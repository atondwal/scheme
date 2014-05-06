;; // The equation itself was created via trial/error while playing with mathematica
;; float waveDx(float x, float y, float t, float r, float spread) {
	;; float d2 = (x*x + y*y)/spread;
	;; float d = sqrt(d2);
	;; float e = exp(-d2/r);
	;; return (float) (-e*x*(2*d*cos(t + d) + r*sin(t + d))/(r*d));
;; }
;; float waveDy(float x, float y, float t, float r, float spread) {
	;; float d2 = (x*x + y*y)/spread;
	;; float d = sqrt(d2);
	;; float e = exp(-d2/r);
	;; return (float) (-e*y*(2*d*cos(t + d) + r*sin(t + d))/(r*d));
;; }
;; int Image32::FunFilter(float time, Image32& outputImage) const {
	;; // each row represents a wave with (x, y, spread, radius, magnitude scale factor)
	;; // note that x, y, and spread are relative to the image dimensions
	;; float points[3][5] = {
		;; {-0.1f,-0.15f, 10.0f, 0.2f, 3.0f},
		;; {1.3f, 0.25, 20.0f, 0.4f, 1.0f},
		;; {0.3f, 1.33, 30.0f, 0.6f, 1.0f}
	;; };
	;; for(int x = 0; x < width(); x++) {
		;; for(int y = 0; y < height(); y++) {
			;; float sx = 0;
			;; float sy = 0;
			;; // add the sum of gradient vectors from 5 different waves
			;; for(int i = 0; i < 3; i++) {
				;; float wx = (float) x - width() * points[i][0];
				;; float wy = (float) y - height() * points[i][1];
				;; float t = - time * 2.0f*PI/10.0f;
				;; float spread = points[i][2];
				;; float r = points[i][3]*width() * points[i][3]*width();
				;; sx += -waveDx(wx, wy, t, r, spread) * points[i][4];
				;; sy += -waveDy(wx, wy, t, r, spread) * points[i][4];
			;; }
			;; float srcX = sx + (float) x;
			;; float srcY = sy + (float) y;
			;; if(inBounds((int) srcX, (int) srcY, width(), height()))
				;; outputImage.pixel(x,y) = BilinearSample(srcX, srcY);
			;; else {
				;; if(srcX < 0)
					;; srcX = 0;
				;; if(srcX > width())
					;; srcX = width() - 1;
				;; if(srcY < 0)
					;; srcY = 0;
				;; if(srcY > width())
					;; srcY = width() - 1;
				;; outputImage.pixel(x,y) = NearestSample(srcX, srcY);
			;; }
		;; }
	;; }
	;; return 1;
;;}

;; (define (at lst idx) (if (list? lst) (if (<= idx 0) (car lst) (at (cdr lst) (- idx 1))) lst))

;; (define (foldl func accum lst) (if (null? lst) accum (foldl func (func accum (car lst)) (cdr lst))))

;; (define (map func lst) (foldr (lambda (x y) (cons (func x) y)) '() lst))

;; (define (range i) (if (<= i 0) 0 (cons i (range (- i 1)))))

;; (define (waveDx x y t r spread) (define d2 (/ (+ (* x x) (* y y)) spread)) (define d (sqrt d2)) (define e (exp (/ (- 0.0 d2) r))) (/ (* (* (- 0.0 e) x) (+ (* 2.0 (* d (cos (+ t d)))) (* r (sin (+ t d))))) (* r d)))

;; (define (waveDy x y t r spread) (define d2 (/ (+ (* x x) (* y y)) spread)) (define d (sqrt d2)) (define e (exp (/ (- 0.0 d2) r))) (/ (* (* (- 0.0 e) y) (+ (* 2.0 (* d (cos (+ t d)))) (* r (sin (+ t d))))) (* r d)))

;; (define (genWarpFunc inputImg t points) ())

;; (define points '( '(-0.1 -0.15 10 0.2 3.0) '(1.3 0.25 20.0 0.4 1.0) '(0.3 1.33 30.0 0.6 1.0)))

;; (define img (read-image "kitten.gif"))

;; (define times '(0 1 2 3 4 5 6 7 8 9))

;; (define img0 (create-image (image-width img) (image-height img) (genWarpFunc img 0.0 points)))

;; (write-image "warp.gif" img)

(define (at lst idx) (if (list? lst) (if (<= idx 0) (car lst) (at (cdr lst) (- idx 1))) lst))

(define (foldr func end lst) (if (null? lst) end (func (car lst) (foldr func end (cdr lst)))))

(define (foldl func accum lst) (if (null? lst) accum (foldl func (func accum (car lst)) (cdr lst))))

(define (map func lst) (foldr (lambda (x y) (cons (func x) y)) '() lst))

(define (range i) (if (<= i 0) 0 (cons i (range (- i 1)))))

(define (waveDx x y t r spread) (define d2 (/f (+f (*f x x) (*f y y)) spread)) (define d (sqrt d2)) (define e (exp (/f (-f 0.0 d2) r))) (/f (*f (*f (-f 0.0 e) x) (+f (*f 2.0 (*f d (cos (+f t d)))) (*f r (sin (+f t d))))) (*f r d)))

(define (waveDy x y t r spread) (define d2 (/f (+f (*f x x) (*f y y)) spread)) (define d (sqrt d2)) (define e (exp (/f (-f 0.0 d2) r))) (/f (*f (*f (-f 0.0 e) y) (+f (*f 2.0 (*f d (cos (+f t d)))) (*f r (sin (+f t d))))) (*f r d)))

(define (warpOffset w h x y wave time) (define wx (-f x (*f w (at wave 0)))) (define wy (-f y (*f h (at wave 1)))) (define t (*f time (- 0.0 0.6283))) (define spread (at wave 2)) (define r (*f (*f w (at wave 3)) (*f w (at wave 3)))) (define sx (*f (-f 0.0 (waveDx x y t r spread)) (at wave 4))) (define sy (*f (-f 0.0 (waveDy x y t r spread)) (at wave 4))) (cons sx sy))

(define (genWarpFunc inputImg t waves) (lambda (x y c) (define w (i2f (image-width inputImg))) (define h (i2f (image-height inputImg))) (define off (warpOffset w h x y (car waves) t)) (define xp (+f x (car off))) (define yp (+f y (cdr off))) (get-pixel inputImg xp yp c)))

(define waves (cons '( (-f 0.0 0.1) (-f 0.0 0.15) 10.0 0.2 3.0) (cons '(1.3 0.25 20.0 0.4 1.0) '(0.3 1.33 30.0 0.6 1.0))))

(define img (read-image "kitten.gif"))

(define img0 (create-image (image-width img) (image-height img) (genWarpFunc img 0.0 waves)))

(write-image "warp.gif" img0)

(define times '(0 1 2 3 4 5 6 7 8 9))
