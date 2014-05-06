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

(define (at lst idx) (if
                       (list? lst)
                       (if
                         (<= idx 0) 
                         (car lst)
                         (at (cdr lst) (- idx 1))
                         )
                       lst))

(define (waveDx x y t r spread)
  (define d2 (/ (+ (* x x) (* y y)) spread))
  (define d (sqrt d2))
  (define e (exp (/ (- 0.0 d2) r)))
  (/ 
    (*
      (* (- 0.0 e) x)
      (+
        (* 2.0 (* d (cos (+ t d))))
        (* r (sin (+ t d))))
      )
    (* r d)))

(define (waveDy x y t r spread)
  (define d2 (/ (+ (* x x) (* y y)) spread))
  (define d (sqrt d2))
  (define e (exp (/ (- 0.0 d2) r)))
  (/ 
    (*
      (* (- 0.0 e) y)
      (+
        (* 2.0 (* d (cos (+ t d))))
        (* r (sin (+ t d))))
      )
    (* r d)))

(define points '((-0.1 -0.15 10 0.2 3.0) (1.3 0.25 20.0 0.4 1.0) (0.3 1.33 30.0 0.6 1.0)))

(waveDx 1.0 2.0 3.0 4.0 5.0)
