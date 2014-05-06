(define img (read-image "kitten.gif"))
(define img2 (create-image (image-width img) (image-height img) (lambda (x y c) (+f (i2f x) (i2f y)))))
(write-image "out.gif" img2)
