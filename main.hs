{-# LANGUAGE ExistentialQuantification #-}
import Codec.Picture
import Data.IORef
import System.IO
import Control.Monad
import Control.Monad.Error
import Control.Applicative ((<$>),(<*>))
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric
import Data.Complex
import Data.Array
import Debug.Trace
import System.IO.Unsafe

------------------------------
-- REPL
------------------------------
readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

readExpr = readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr spaces)

flushStr :: String -> IO()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr =  evalString env expr >>= putStrLn

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives
                                              ++ map (makeFunc PrimitiveFunc) primitives)
    where makeFunc constructor (var, func) = (var, constructor func)

runOne :: [String] -> IO()
runOne args = do
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)] 
    (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)])) 
         >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
  result <- prompt
  if pred result 
     then return ()
     else action result >> until_ pred prompt action

main :: IO ()
main = do args <- getArgs
          if null args then runRepl else runOne $ args

------------------------------
-- Language representation
------------------------------
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Float
             | String String
             | Bool Bool
             | Character Char
             | Ratio Rational
             | Vector [LispVal]
             | Complex (Complex Float)
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle
             | Image (Image PixelRGB8)
             | Func {
                 params :: [String],
                 vararg :: (Maybe String),
                 body :: [LispVal],
                 closure :: Env
             }

instance Show LispVal where show = showVal
showVal :: LispVal -> String
showVal (Atom name) = name
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (Number contents) = show contents
showVal (Float contents) = show contents
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Character c) = "#\\" ++ [c]
showVal (Ratio r) = "#\\" ++ show r
showVal (Complex c) = "#\\" ++ show c
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Port _) = "<IO port>"
showVal (Main.Image _) = "<Image>"
showVal (IOFunc _) = "<IO primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) = 
  "(lambda (" ++ unwords (map show args) ++ 
     (case varargs of 
        Nothing -> ""
        Just arg -> " . " ++ arg) ++ ") ...)"

------------------------------
-- Error handling
------------------------------

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where show = showError
showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected 
                                  ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (Default error) = "Error: " ++ error

instance Error LispError where
     noMsg = Default "An error has occurred"
     strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

------------------------------
-- Environment
------------------------------
type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ErrorT LispError IO
liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var  =  do env <- liftIO $ readIORef envRef
                         maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                               (liftIO . readIORef)
                               (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable" var) 
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do 
    alreadyDefined <- liftIO $ isBound envRef var 
    if alreadyDefined 
       then setVar envRef var value >> return value
       else liftIO $ do 
          valueRef <- newIORef value
          env <- readIORef envRef
          writeIORef envRef ((var, valueRef) : env)
          return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
          addBinding (var, value) = do ref <- newIORef value
                                       return (var, ref)

--------------------------------------------------------------------------------
-- Evaluator
--------------------------------------------------------------------------------
eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(Float _) = return val
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) = 
    do result <- eval env pred
       case result of
         Bool False -> eval env alt
         otherwise -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) =
    eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
    eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
    makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
    makeVarargs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
    makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
    makeVarargs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
    makeVarargs varargs env [] body
eval env (List [Atom "load", String filename]) = 
    load filename >>= liftM last . mapM (eval env)
eval env (List (function : args)) = do 
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (IOFunc func) args = func args
apply (Func params varargs body closure) args = 
    if num params /= num args && varargs == Nothing
       then throwError $ NumArgs (num params) args
       else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
    where remainingArgs = drop (length params) args
          num = toInteger . length
          evalBody env = liftM last $ mapM (eval env) body 
          bindVarArgs arg env = case arg of
              Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
              Nothing -> return env 

------------------------------------------------------------
-- Built-in functions
------------------------------------------------------------

------------------------------
-- Helper-functions for
-- builtins
------------------------------
makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarargs = makeFunc . Just . showVal

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f [v] = return (f v)
unaryOp f (x:_) = throwError $ Default
  "Unary function expected a single argument. Multiple provided."
unaryOp f [] = throwError $ Default "Unary function expected an argument"

unaryFlOp :: (Float -> Float) -> [LispVal] -> ThrowsError LispVal
unaryFlOp op params = mapM unpackFloat params >>= return . Float . op . head

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2 
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

floatBoolBinop = boolBinop unpackNum
numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

floatBinop :: (Float -> Float -> Float) -> [LispVal] -> ThrowsError LispVal
floatBinop op           []  = throwError $ NumArgs 2 []
floatBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
floatBinop op params = mapM unpackFloat params >>= return . Float . foldl1 op

unpackFloat :: LispVal -> ThrowsError Float
unpackFloat (Float n) = return n
unpackFloat x = throwError $ TypeMismatch "float" $ x

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op           []  = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in 
                          if null parsed 
                            then throwError $ TypeMismatch "number" $ String n
                            else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

------------------------------
-- Built-in functions
------------------------------
ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll),
                ("read-image", readimage),
                ("write-image", writeimage),
                ("create-image", createimage)]

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("+f", floatBinop (+)),
              ("-f", floatBinop (-)),
              ("*f", floatBinop (*)),
              ("/f", floatBinop (/)),
              ("sqrt", unaryFlOp sqrt),
              ("sin", unaryFlOp sin),
              ("cos", unaryFlOp cos),
              ("exp", unaryFlOp exp),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("=f", floatBoolBinop (==)),
              ("<f", floatBoolBinop (<)),
              (">f", floatBoolBinop (>)),
              ("/=f", floatBoolBinop (/=)),
              (">=f", floatBoolBinop (>=)),
              ("<=f", floatBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("symbol?", unaryOp isSymbol),
              ("number?", unaryOp isNumber),
              ("string?", unaryOp isString),
              ("null?", unaryOp isNull),
              ("bool?", unaryOp isBool),
              ("list?", unaryOp isList),
              ("symbol2string", unaryOp symbol2string),
              ("string2symbol", unaryOp string2symbol),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal),
              ("f2i", f2i),
              ("i2f", i2f),
              ("make-string", makestring),
              ("string-length", stringlength),
              ("string-ref", stringref),
              ("substring", substring),
              ("string-append", stringappend),
              ("string->list", string2list),
              ("list->string", list2string),
              ("string-copy", stringcopy),
              ("image-width", imagewidth),
              ("image-height", imageheight),
              ("get-pixel", getpixel)]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _ = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename

fromEither :: Either a b -> b
fromEither (Right a) = a

readimage :: [LispVal] -> IOThrowsError LispVal
readimage [String filename] = lift (Main.Image <$> img)
        where img :: IO (Image PixelRGB8)
              img = head <$> fromEither <$> readGifImages filename
readimage badArgList = throwError $ NumArgs 1 badArgList

writeimage :: [LispVal] -> IOThrowsError LispVal
writeimage (String filename : img@(Main.Image i) :[]) =
  (lift $ fromEither $ saveGifImage filename (ImageRGB8 i)) >> return img
writeimage badArgList = throwError $ NumArgs 2 badArgList

isSymbol, isNumber, isString, isNull, isBool, isList :: LispVal -> LispVal
isSymbol (Atom _) = Bool True
isSymbol _ = Bool False
isNumber (Number _) = Bool True
isNumber _ = Bool False
isString (String _) = Bool True
isString _ = Bool False
isNull (List []) = Bool True
isNull _ = Bool False
isBool (Bool _) = Bool True
isBool _ = Bool False
isList (List _) = Bool True
isList (DottedList _ _) = Bool True
isList _ = Bool False

symbol2string, string2symbol :: LispVal -> LispVal
symbol2string (Atom s)   = String s
symbol2string _          = String ""
string2symbol (String s) = Atom s
string2symbol _          = Atom ""

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqvList :: ([LispVal] -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
eqvList eqvFunc [(List arg1), (List arg2)] = 
    return $ Bool $ (length arg1 == length arg2) && (all eqvPair $ zip arg1 arg2)
    where eqvPair (x1, x2) = case eqvFunc [x1, x2] of
                                  Left err -> False
                                  Right (Bool val) -> val

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [l1@(List arg1), l2@(List arg2)] = eqvList eqv [l1, l2]
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)
unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = 
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
        `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [l1@(List arg1), l2@(List arg2)] = eqvList equal [l1, l2]
equal [(DottedList xs x), (DottedList ys y)] = equal [List $ xs ++ [x], List $ ys ++ [y]]
equal [arg1, arg2] = do
  primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                       [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [arg1, arg2]
  return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

f2i :: [LispVal] -> ThrowsError LispVal
f2i [Float f] = return $ Number (floor f)

i2f :: [LispVal] -> ThrowsError LispVal
i2f [Number n] = return $ Float (fromIntegral n)

makestring :: [LispVal] -> ThrowsError LispVal
makestring [Number k] = makestring (Number k : [Character '!'])
makestring (Number k : [Character c]) = return (String (replicate (fromInteger k) c))
makestring badArgList = throwError $ NumArgs 2 badArgList

stringlength :: [LispVal] -> ThrowsError LispVal
stringlength [String s] = return $ Number $ toInteger (length s)
stringlength badArgList = throwError $ NumArgs 1 badArgList

stringref :: [LispVal] -> ThrowsError LispVal
stringref ((String s) : [Number k]) = 
    let k' = fromInteger k
    in
    if k' < length s then
        return $ Character $ s !! (fromInteger k)
        else
            throwError $ Default $
                "string-ref out of bounds (" ++ show k' ++ " > length " ++ show s ++ ")"
stringref badArgList = throwError $ NumArgs 2 badArgList

substring :: [LispVal] -> ThrowsError LispVal
substring (String s : (Number start) : [Number end]) =
    let start' = fromInteger start
        end' = fromInteger end
    in
    if (0 <= start') && (start' <= end') && (end' <= (length s)) then
        let finalS = take (end' - start') $ drop start' s in
        return $ String $ finalS
        else
            throwError $ Default $
                "substring out of bounds"

stringappend :: [LispVal] -> ThrowsError LispVal
stringappend [] = return $ String ""
stringappend [String s] = return $ String s
stringappend (String s : rest) = do
    String rest' <- stringappend rest
    return $ String (s ++ rest')
stringappend badArgList = throwError $ TypeMismatch "String" $ head badArgList

string2list :: [LispVal] -> ThrowsError LispVal
string2list [String s] = return $ List $ map (\c -> Character c) s
string2list [badArg] = throwError $ TypeMismatch "String" badArg
string2list badArgList = throwError $ NumArgs 1 badArgList

list2string :: [LispVal] -> ThrowsError LispVal
list2string [List cs] = return $ String $ map (\(Character c) -> c) cs
list2string [Vector cs] = return $ String $ map (\(Character c) -> c) cs
list2string [badArg] = throwError $ TypeMismatch "List of Characters" badArg
list2string badArgList = throwError $ NumArgs 1 badArgList

stringcopy :: [LispVal] -> ThrowsError LispVal
stringcopy [String s] = return $ String s

createimage :: [LispVal] -> IOThrowsError LispVal
createimage ((Number w) : (Number h) : f@(Func _ _ _ _) :[]) = Main.Image <$> ans
  where arry = sequence [ monf x y >>= return . ((,) (x,y)) | x <- [0..w], y<-[0..h]] >>= return . (array ((0,0),(w,h)))
        monf :: Integer -> Integer -> IOThrowsError PixelRGB8
        monf x y = do (Float r) <- apply f [Number (fi x),Number (fi y), Number 0]
                      (Float g) <- apply f [Number (fi x),Number (fi y), Number 1]
                      (Float b) <- apply f [Number (fi x),Number (fi y), Number 2]
                      return $ PixelRGB8 (floor$ 255*r) (floor$ 255*g) (floor$ 255*b)
        ans :: IOThrowsError (Image PixelRGB8)
        ans = do thearray <- arry
                 return $ generateImage (\x y -> thearray ! (fi x, fi y)) (fi w) (fi h)
        fi a = fromIntegral a
createimage badArgList = throwError $ NumArgs 3 badArgList

imagewidth :: [LispVal] -> ThrowsError LispVal
imagewidth [Main.Image i] = return $ Number $ toInteger $ imageWidth i
imagewidth badArgList = throwError $ NumArgs 1 badArgList

imageheight :: [LispVal] -> ThrowsError LispVal
imageheight [Main.Image i] = return $ Number $ toInteger $ imageHeight i
imageheight badArgList = throwError $ NumArgs 1 badArgList

getpixel :: [LispVal] -> ThrowsError LispVal
getpixel ((Main.Image i) : (Number x) : (Number y) : (Number c) : []) =
    if (x >= 0) && (x < (toInteger $ imageWidth i)) && (y >= 0) && (y < (toInteger $ imageHeight i)) then
      let PixelRGB8 r g b = pixelAt i (fromIntegral x) (fromIntegral y)
          f = case c of
              0 -> r
              1 -> g
              2 -> b
      in return $ Float (fromIntegral f / 255.0)
    else return $ Float 0.0
getpixel badArgList = throwError $ NumArgs 4 badArgList

--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseBool :: Parser LispVal
parseBool = do
  char '#'
  (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseString :: Parser LispVal
parseString = do 
  char '"'
  x <- many (noneOf "\"\\" <|> (char '\\' >> oneOf "nrt\\\""))
  char '"'
  return$ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  return$ case first:rest of 
      "#t" -> Bool True
      "#f" -> Bool False
      _ -> Atom$ first:rest

parseDigital1 :: Parser LispVal
parseDigital1 = many1 digit >>= (return . Number . read)

parseDigital2 :: Parser LispVal
parseDigital2 = do try $ string "#d"
                   x <- many1 digit
                   return (Number (read x))

parseHex :: Parser LispVal
parseHex = do try $ string "#x"
              x <- many1 hexDigit
              return (Number (hex2dig x))

parseOct :: Parser LispVal
parseOct = do try $ string "#o"
              x <- many1 octDigit
              return $ Number (oct2dig x)

parseBin :: Parser LispVal
parseBin = do try $ string "#b"
              x <- many1 (oneOf "10")
              return $ Number (bin2dig x)

parseCharacter :: Parser LispVal
parseCharacter = do try $ string "#\\"
                    value <- try (string "newline" <|> string "space")
                             <|> do { x <- anyChar; notFollowedBy alphaNum ; return [x] }
                    return $ Character $ case value of
                                         "space" -> ' '
                                         "newline" -> '\n'
                                         otherwise -> (value !! 0)

oct2dig x = fst $ readOct x !! 0
hex2dig x = fst $ readHex x !! 0
bin2dig = bin2dig' 0
bin2dig' digint "" = digint
bin2dig' digint (x:xs) =
  let old = 2 * digint + (if x == '0' then 0 else 1) in
  bin2dig' old xs

parseNumber :: Parser LispVal
parseNumber =  parseDigital1
           <|> parseDigital2
           <|> parseHex
           <|> parseOct
           <|> parseBin

parseFloat :: Parser LispVal
parseFloat = do i <- many1 digit
                char '.'
                f <- many1 digit
                return $ Float (read (i++'.':f) :: Float)

parseRatio :: Parser LispVal
parseRatio = do n <- many1 digit
                char '/'
                d <- many1 digit
                return $ Ratio (read (n++'%':d) :: Rational)

parseComplex :: Parser LispVal
parseComplex = do Float r <- parseFloat
                  char '/'
                  Float i <- parseFloat
                  return $ Complex ( r :+ i )

parseVector :: Parser LispVal
parseVector = do string "#("
                 vals <- sepBy parseExpr spaces
                 string ")"
                 return $ Vector vals

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
                char '`'
                x <- parseExpr
                return $ List [Atom "quasiquote", x]

parseUnQuote :: Parser LispVal
parseUnQuote = do
                char ','
                x <- parseExpr
                return $ List [Atom "unquote", x]

parseDottedList :: Parser LispVal
parseDottedList = do
  h <- endBy parseExpr spaces
  t <- char '.' >> spaces >> parseExpr
  return$ DottedList h t

parseQuoted :: Parser LispVal
parseQuoted =
  char '\'' >>
  parseExpr >>=
  return . List . ((Atom "quote"):) . (:[])

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> try parseComplex
         <|> try parseFloat
         <|> try parseNumber
         <|> try parseBool
         <|> try parseQuoted
         <|> try parseCharacter
         <|> try parseRatio
         <|> try parseQuasiQuoted
         <|> try parseUnQuote
         <|> try parseVector
         <|> do char '('
                x <- (try parseList) <|> parseDottedList
                char ')'
                return x

--------------------------------------------------------------------------------
-- Misc. Functions
--------------------------------------------------------------------------------

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal
