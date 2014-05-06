(define img (read-image "kitten.gif"))

(define (clamp x min max) (if (<f x min) min (if (>f x max) max x)))

(define comment "foo")

(define (func x y c) (define cur 0.0)
  (set! cur (+f cur (get-pixel img (+ x 1) y c))) (set! cur (+f cur (get-pixel img (- x 1) y c))) (set! cur (+f cur (get-pixel img x y c))) cur)

(define img2 (create-image (image-width img) (image-height img) func))

(write-image "blur.gif" img2)
