;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lissajous-2d-simple.scm (Simple Scheme 専用)

(define offset-x (/ (image-width (empty-scene)) 2))
(define offset-y (/ (* (image-height (empty-scene)) 65) 148))
(define magnification (* 3/4 (min offset-x offset-y)))
(define (conv-x x) (+ (* magnification x) offset-x))
(define (conv-y y) (+ (* magnification y) offset-y))

(define (draw-lissajous phi)
  (letrec ((lp (lambda (theta x0 y0 scn)
             (if (> theta (* 2 pi)) scn
               (let ((x (conv-x (cos (* 2 theta))))
                     (y (conv-y (cos (+ (* 3 theta) phi)))))
                 (lp (+ theta (/ pi 200)) x y
                     (add-line scn x0 y0 x y "white")))))))
    (lp 0 (conv-x 1) (conv-y (cos phi)) (empty-scene "black"))))

(big-bang 0 (on-draw draw-lissajous) (on-tick (lambda (w) (+ w (/ pi 40))) 0.3))
