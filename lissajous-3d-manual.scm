;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lissajous-3d-manual.scm (Simple Scheme 専用)

(define (rot-operator c dx dy)
  (let* ((dxx (* dx dx))       (dyy (* dy dy))       (dxx+dyy (+ dxx dyy))
         (dxx (/ dxx dxx+dyy)) (dyy (/ dyy dxx+dyy)) (ss (- 1 (* c c)))
         (t (* 1/2 (+ 1 c))) (u (* 1/2 (- dxx dyy) (- 1 c)))
         (a11 (+ t u)) (a12 (* (/ (* dx dy) dxx+dyy) (- 1 c)))
         (a13 ((if (< dy 0) + -) (sqrt (* dyy ss)))) (a22 (- t u))
         (a23 ((if (< dx 0) - +) (sqrt (* dxx ss)))))
    (lambda (x y z)
      (values (+ (* a11 x) (* a12 y) (* a13 z))
              (+ (* a12 x) (* a22 y) (* a23 z))))))

(define offset-x (/ (image-width (empty-scene)) 2))
(define offset-y (/ (* (image-height (empty-scene)) 65) 148))
(define magnification (* 1/2 (min offset-x offset-y)))
(define (conv-x x) (+ (* magnification x) offset-x))
(define (conv-y y) (+ (* magnification y) offset-y))

; x = cos c1*θ, y = sin c2*θ, z = sin c3*θ
(define c1 5) (define c2 5) (define c3 6) 
(define xs (build-list 401 (lambda (i) (cos (* c1 1/200 pi i)))))
(define ys (build-list 401 (lambda (i) (sin (* c2 1/200 pi i)))))
(define zs (build-list 401 (lambda (i) (sin (* c3 1/200 pi i)))))

(define (plot scn rot color)
  (letrec ((lp (lambda (scn x0 y0 xs ys zs)
             (if (null? xs) scn
               (let-values (((x y) (rot (car xs) (car ys) (car zs))))
                 (let ((x (conv-x x)) (y (conv-y y)))
                   (lp (add-line scn x0 y0 x y color)
                       x y (cdr xs) (cdr ys) (cdr zs))))))))
    (let-values (((x y) (rot (car xs) (car ys) (car zs))))
      (lp scn (conv-x x) (conv-y y) (cdr xs) (cdr ys) (cdr zs)))))

(big-bang
  (lambda (x y z) (values x y))
  (on-draw (lambda (w) (plot (empty-scene "black") w "white")))
  (on-orient (lambda (w x y z) (rot-operator (/ z 10) y x))))
