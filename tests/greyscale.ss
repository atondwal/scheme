(define img (read-image "kitten.gif"))
(define img2 (create-image (image-width img) (image-height img) (lambda (x y c) (/f (+f (get-pixel img x y 0) (+f (get-pixel img x y 1) (get-pixel img x y 2))) (i2f 3)))))
(write-image "greyscale.gif" img2)
