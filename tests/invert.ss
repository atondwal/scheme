(define img (read-image "kitten.gif"))
(define img2 (create-image (image-width img) (image-height img) (lambda (x y c) (-f 1.0 (get-pixel img x y c)))))
(write-image "invert.gif" img2)
