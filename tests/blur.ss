(define img (read-image "kitten.gif"))

(define (clamp x min max) (if (<f x min) min (if (>f x max) max x)))

(define comment "foo")

(define (genblur inputImg) (lambda (x y c) (define cur 0.0) (set! cur (+f cur (get-pixel inputImg (+ x 1) y c))) (set! cur (+f cur (get-pixel inputImg (- x 1) y c))) (set! cur (+f cur (get-pixel inputImg x (+ y 1) c))) (set! cur (+f cur (get-pixel inputImg x (- y 1) c))) (/f cur 4.0)))

(define img2 (create-image (image-width img) (image-height img) (genblur img)))
(define img3 (create-image (image-width img) (image-height img) (genblur img2)))
(define img4 (create-image (image-width img) (image-height img) (genblur img3)))
(define img5 (create-image (image-width img) (image-height img) (genblur img4)))

(write-image "blur.gif" img5)
