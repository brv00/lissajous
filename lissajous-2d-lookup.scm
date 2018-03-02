;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lissajous-2d-lookup.scm (Simple Scheme 専用)

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

(define (plot scn xs ys color)
  (letrec ((lp (lambda (scn x0 y0 xs ys)
             (if (null? xs) scn
               (let ((x (car xs)) (y (car ys)))
                 (lp (add-bold-line scn x0 y0 x y color)
		     x y (cdr xs) (cdr ys)))))))
    (lp scn (car xs) (car ys) (cdr xs) (cdr ys))))

(define offset-x (/ (image-width (empty-scene)) 2))
(define offset-y (/ (* (image-height (empty-scene)) 65) 148))
(define magnification (* 3/4 (min offset-x offset-y)))
(define (conv-x x) (+ (* magnification x) offset-x))
(define (conv-y y) (+ (* magnification y) offset-y))

(define c1 5) (define c2 6) ; x = cos c1*θ, y = cos c2*θ
(define nframes/2 20)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 座標テーブル
(define one-round (* 2 (/ c1 (gcd c1 c2)) nframes/2))
(define nsteps (/ 400 one-round))
(define npoints (* one-round nsteps))

(define cos-table1/4
  (build-list (-- (/ npoints 4))
              (lambda (i) (cos (/ (* (++ i) 2 pi) npoints)))))
(define cos-table3/4 (map - cos-table1/4))
(define cos-table
  (foldr append '()
         (list '(1.0) cos-table1/4 '(0.0) (reverse cos-table3/4)
               '(-1.0) cos-table3/4 '(0.0) (reverse cos-table1/4))))

(define xs (map conv-x cos-table))
(define xs (build-list npoints (lambda (i) (list-ref xs (% (* i c1) npoints)))))

(define ys (map conv-y cos-table))
(define yss
  (build-list
    c2 (lambda (i)
         (build-list npoints
                     (lambda (j) (list-ref ys (% (+ (* j c2) i) npoints)))))))
(define (kth-ys k)
  (let ((ys (list-ref yss (% (* nsteps k) c2))) (i (/ (* nsteps k) c2)))
    (append (drop ys i) (take ys i))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define line-color "white") (define background (empty-scene "black"))
(define npoints/2 (/ npoints 2)) (define xs-1st-half (take xs npoints/2))
(define image-1st
  (plot background xs-1st-half (take (car yss) npoints/2) line-color))
(define image-middle
  (plot background xs-1st-half (take (kth-ys nframes/2) npoints/2) line-color))
(define images
  (build-list (-- nframes/2)
	      (lambda (i)
		(let ((ys (kth-ys (++ i))))
		  (plot background
			(cons (list-ref xs (-- npoints)) xs)
			(cons (list-ref ys (-- npoints)) ys)
			line-color)))))
(define frames
  (append (cons image-1st images) (cons image-middle (reverse images))))

(big-bang frames (on-draw (lambda (w) (car w)))
          (on-tick (lambda (w) (if (null? (cdr w)) frames (cdr w))) 0.05))
