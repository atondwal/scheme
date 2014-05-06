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

(define points ())

(waveDx 1.0 2.0 3.0 4.0 5.0)
