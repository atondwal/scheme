(define (at lst idx) (if (list? lst) (if (<= idx 0) (car lst) (at (cdr lst) (- idx 1))) lst))

(define (foldr func end lst) (if (null? lst) end (func (car lst) (foldr func end (cdr lst)))))

(define (foldl func accum lst) (if (null? lst) accum (foldl func (func accum (car lst)) (cdr lst))))

(define (map func lst) (foldr (lambda (x y) (cons (func x) y)) '() lst))

(define (range i) (if (<= i 0) 0 (cons i (range (- i 1)))))

(define (waveDx x y t r spread) (define d2 (/f (+f (*f x x) (*f y y)) spread)) (define d (sqrt d2)) (define e (exp (/f (-f 0.0 d2) r))) (/f (*f (*f (-f 0.0 e) x) (+f (*f 2.0 (*f d (cos (+f t d)))) (*f r (sin (+f t d))))) (*f r d)))

(define (waveDy x y t r spread) (define d2 (/f (+f (*f x x) (*f y y)) spread)) (define d (sqrt d2)) (define e (exp (/f (-f 0.0 d2) r))) (/f (*f (*f (-f 0.0 e) y) (+f (*f 2.0 (*f d (cos (+f t d)))) (*f r (sin (+f t d))))) (*f r d)))

(define (warpOffset w h x y wave time) (define wx (-f x (*f w (at wave 0)))) (define wy (-f y (*f h (at wave 1)))) (define t (*f time (-f 0.0 0.6283))) (define spread (at wave 2)) (define r (*f (*f w (at wave 3)) (*f w (at wave 3)))) (define sx (*f (-f 0.0 (waveDx x y t r spread)) (at wave 4))) (define sy (*f (-f 0.0 (waveDy x y t r spread)) (at wave 4))) (cons sx sy))

(define (genWarpFunc inputImg t waves) (lambda (x y c) (define xf (i2f x)) (define yf (i2f y)) (define w (i2f (image-width inputImg))) (define h (i2f (image-height inputImg))) (define off (warpOffset w h xf yf (car waves) t)) (define xp (+f xf (car off))) (define yp (+f yf (cdr off))) (get-pixel inputImg (f2i xp) (f2i yp) c)))

(define waves (cons (cons (-f 0.0 0.1) (cons (-f 0.0 0.15) (cons 10.0 (cons 0.2 3.0)))) (cons (-f 0.0 0.1) (cons (-f 0.0 0.15) (cons 10.0 (cons 0.2 3.0))))))

(define img (read-image "test.gif"))

(define t 2)

(define (comment) (define img0 (create-image (image-width img) (image-height img) (genWarpFunc img 0.0 waves))))

(define (comment) (write-image "warp.gif" img0))

(define (comment) (define times '(0 1 2 3 4 5 6 7 8 9)))

(define warped (create-image (image-width img) (image-height img) (genWarpFunc img (/f 1.0 (i2f t)) waves)))
(define outfilename (string-append (string-append "warp" t) ".gif"))
(write-image outfilename warped)
