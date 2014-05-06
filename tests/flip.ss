(define img (read-image "kitten.gif"))
(define img2 (create-image (image-width img) (image-height img) (lambda (x y c) (get-pixel img (- (image-width img) x) (- (image-height img) y) c))))
(write-image "flip.gif" img2)
