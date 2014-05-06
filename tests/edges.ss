(define img (read-image "kitten.gif"))

(define (clamp x min max) (if (<f x min) min (if (>f x max) max x)))

(define comment "foo")

(define (fabs n) (if (<f n 0.0) (*f n (i2f (- 0 1))) n))

(define (avgpxl img x y) (/f (+f (get-pixel img x y 0) (+f (get-pixel img x y 1) (get-pixel img x y 2))) 3.0))

(define (genedge inputImg) (lambda (x y c) (define xd (fabs (-f (avgpxl img (+ x 1) y) (avgpxl img (- x 1) y)))) (define yd (fabs (-f (avgpxl img x (+ y 1)) (avgpxl img x (- y 1))))) (define gr (sqrt (+f (*f xd xd) (*f yd yd)))) (if (>f gr 0.2) 1.0 0.0)))

(define img2 (create-image (image-width img) (image-height img) (genedge img)))

(write-image "edges.gif" img2)
