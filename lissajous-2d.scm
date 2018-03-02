;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lissajous-2d.scm (Simple Scheme 専用)

(define (ceiling x) (round (+ x 1/2)))
(define (gcd x y) (if (= x 0) y (gcd (% y x) x)))

(define (divide-by-2or3 x1 y1 x2 y2)
  (let* ((dx (- x2 x1)) (dy (- y2 y1)) (abs-dx (abs dx)) (abs-dy (abs dy))
         (normalize-by-dx
          (lambda (one) (values one (* one (/ dy dx)) (ceiling (/ dx one)))))
         (normalize-by-dy
          (lambda (one) (values (* one (/ dx dy)) one (ceiling (/ dy one))))))
    (if (>= abs-dx abs-dy)
      (if (<= (/ abs-dy abs-dx) 2/3)
        (normalize-by-dx (if (> dx 0) 3.0 -3.0))
        (normalize-by-dy (if (> dy 0) 2.0 -2.0)))
      (if (<= (/ abs-dx abs-dy) 2/3)
        (normalize-by-dy (if (> dy 0) 3.0 -3.0))
        (normalize-by-dx (if (> dx 0) 2.0 -2.0))))))

(define (add-bold-line scn x1 y1 x2 y2 color)
  (let-values (((dx dy n) (divide-by-2or3 x1 y1 x2 y2)))
    (letrec ((lp (lambda (x y i scn)
               (if (>= i n) scn
                 (lp (+ x dx) (+ y dy) (++ i)
                     (place-image (circle 3 "solid" color) x y scn))))))
      (lp x1 y1 0 scn))))

(define offset-x (/ (image-width (empty-scene)) 2))
(define offset-y (/ (* (image-height (empty-scene)) 65) 148))
(define magnification (* 3/4 (min offset-x offset-y)))
(define (conv-x x) (+ (* magnification x) offset-x))
(define (conv-y y) (+ (* magnification y) offset-y))

(define c1 5) (define c2 6) ; x = cos c1*θ, y = cos c1*θ+φ
(define (draw-lissajous theta-min theta-max phi)
  (letrec ((lp (lambda (theta x0 y0 scn)
             (if (> theta theta-max) scn
               (let ((x (conv-x (cos (* c1 theta))))
                     (y (conv-y (cos (+ (* c2 theta) phi)))))
                 (lp (+ theta (/ pi 200)) x y
                     (add-bold-line scn x0 y0 x y "white")))))))
    (lp theta-min (conv-x (cos theta-min)) (conv-y (cos (+ theta-min phi)))
        (empty-scene "black"))))

(define nframes/2 20)
(define axis-of-symmetry (* (/ pi c1) (gcd c1 c2)))
(define image-1st (draw-lissajous 0 pi 0))
(define image-middle (draw-lissajous 0 pi axis-of-symmetry))
(define images
  (build-list (-- nframes/2)
              (lambda (i)
                (draw-lissajous
                  0 (* 2 pi) (* (+ i 1) (/ axis-of-symmetry nframes/2))))))
(define frames
  (append (cons image-1st images) (cons image-middle (reverse images))))

(big-bang frames (on-draw (lambda (w) (car w)))
          (on-tick (lambda (w) (if (null? (cdr w)) frames (cdr w))) 0.05))
