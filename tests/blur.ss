(define img (read-image "kitten.gif"))

(define (clamp x min max) (if (<f x min) min (if (>f x max) max x)))

(define comment "foo")

(define (blur x y c) (define cur 0.0) (set! cur (+f cur (get-pixel img (+ x 1) y c))) (set! cur (+f cur (get-pixel img (- x 1) y c))) (set! cur (+f cur (get-pixel img x (+ y 1) c))) (set! cur (+f cur (get-pixel img x (- y 1) c))) (/f cur 4.0))

(define img2 (create-image (image-width img) (image-height img) blur))
(define img3 (create-image (image-width img2) (image-height img) blur))
(define img4 (create-image (image-width img3) (image-height img) blur))
(define img5 (create-image (image-width img4) (image-height img) blur))

(write-image "blur.gif" img5)
