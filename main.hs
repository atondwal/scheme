import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

main :: IO ()
main = getArgs >>= putStrLn . readExpr . head 

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~#"

readExpr :: String -> String
readExpr input = case parse symbol "lisp" input of
    Left e -> "Parse Error: " ++ show e
    Right _ -> "You Parse!"

