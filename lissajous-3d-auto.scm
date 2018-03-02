;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lissajous-3d-auto.scm (Simple Scheme 専用)

(define (list->matrix lis m n)
  (build-list
    n (lambda (i)
        (build-list m (lambda (j) (list-ref lis (+ i (* n j))))))))

(define (mat*vec mat vec)
  (build-list
    (length (car mat))
    (lambda (i)
      (letrec ((lp (lambda (j res)
                 (if (< j 0) res
                   (lp (-- j)
                       (+ (* (list-ref (list-ref mat j) i) (list-ref vec j))
                          res))))))
        (lp (-- (length mat)) 0.0)))))

(define (mat*mat mat1 mat2) (map (lambda (v) (mat*vec mat1 v)) mat2))

(define (sign x) (cond ((< x 0) -1) ((= x 0) 0) (else 1)))
(define (make-rotation-matrix theta dx dy dz)
  (let* ((dxx (* dx dx)) (dyy (* dy dy)) (dzz (* dz dz))
         (delta^2 (+ dxx dyy dzz))

         (dxx (/ dxx delta^2)) (dyy (/ dyy delta^2)) (dzz (/ dzz delta^2))
         (dx (* (sign dx) (sqrt dxx))) (dy (* (sign dy) (sqrt dyy)))
         (dz (* (sign dz) (sqrt dzz)))

         (t (+ 1 (cos theta))) (u (- 2 t)) (sine (sin theta))
         (f (lambda (x y z) (* 1/2 (+ t (* (- x y z) u)))))
         (g (lambda (x y z) (+ (* x y u) (* z sine)))))

    (list->matrix (list (f dxx dyy dzz)  (g dx dy (- dz)) (g dz dx dy)
                        (g dx dy dz)     (f dyy dzz dxx)  (g dy dz (- dx))
                        (g dz dx (- dy)) (g dy dz dx)     (f dzz dxx dyy))
                  3 3)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ceiling x) (round (+ x 1/2)))

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
(define magnification (* 1/2 (min offset-x offset-y)))
(define (conv-x x z) (+ (* (expt 0.95 z) magnification x) offset-x))
(define (conv-y y z) (+ (* (expt 0.95 z) magnification y) offset-y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; グリッド
(define (add-grid scn x0 y0 x1 y1 x2 y2 x3 y3 n color)
  (letrec ((lp (lambda (i scn)
             (if (> i n) scn
               (let* ((j (- n i)) (x0j (* x0 j)) (y0j (* y0 j))
                      (x3i (* x3 i)) (y3i (* y3 i)))
                 (lp (++ i)
                     (add-line
                       (add-line scn
                                 (/ (+ x0j (* x1 i)) n) (/ (+ y0j (* y1 i)) n)
                                 (/ (+ (* x2 j) x3i) n) (/ (+ (* y2 j) y3i) n)
                                 color)
                       (/ (+ x0j (* x2 i)) n) (/ (+ y0j (* y2 i)) n)
                       (/ (+ (* x1 j) x3i) n) (/ (+ (* y1 j) y3i) n)
                       color)))))))
    (lp 0 scn)))

(define (farther? p1 p2)
  (or (> (third p1) (third p2))
      (and (= (third p1) (third p2))
           (or (> (second p1) (second p2))
               (and (= (second p1) (second p2)) (< (first p1) (first p2)))))))

(define (find-farthest-vertex vertices)
  (letrec ((lp (lambda (s fv ifv)
             (if (>= s 8) ifv
               (let* ((i (^ ifv s)) (v (list-ref vertices i)))
                 (if (farther? v fv)
                   (lp (<< s 1) v i)
                   (lp (<< s 1) fv ifv)))))))
    (lp 1 (car vertices) 0)))

(define (add-rear-wall scn vertices color)
  (let* ((ifv (find-farthest-vertex vertices))
         (vertices (build-list
		     7 (lambda (i)
			 (let ((v (list-ref vertices (^ ifv i))))
			   (list (conv-x (first  v) (third v))
				 (conv-y (second v) (third v))))))))
    (foldr (lambda (di scn)
             (let ((v0 (car vertices)) (v1 (list-ref vertices di))
		   (v2 (list-ref vertices (% (* 2 di) 7)))
		   (v3 (list-ref vertices (% (* 3 di) 7))))
               (add-grid scn
			 (first v0) (second v0) (first v1) (second v1)
			 (first v2) (second v2) (first v3) (second v3)
			 4 color)))
           scn '(1 2 4))))

(define cube-vertices
  (let ((vecs (foldr (lambda (lis dsts)
                       (foldr append '()
                              (map (lambda (dst)
                                     (map (lambda (e) (cons e dst)) lis))
                                   dsts)))
                     '(()) '((-1 1) (-1 1) (-1 1)))))
    (lambda (rot-mat) (mat*mat rot-mat vecs))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (plot3d scn mat pts color)
  (letrec ((lp (lambda (scn x0 y0 pts)
             (if (null? pts) scn
               (let* ((p (mat*vec mat (car pts))) (z (third p))
                      (x (conv-x (first p) z)) (y (conv-y (second p) z)))
                 (lp (add-bold-line scn x0 y0 x y color) x y (cdr pts)))))))
    (let* ((p (mat*vec mat (car pts))) (z (third p)))
      (lp scn (conv-x (first p) z) (conv-y (second p) z) (cdr pts)))))

; x = cos c1*θ, y = sin c2*θ, z = sin c3*θ
(define c1 3) (define c2 2) (define c3 5) 
(define points
  (build-list 401 (lambda (i)
                    (list (cos (* c1 1/200 pi i)) (sin (* c2 1/200 pi i))
                          (sin (* c3 1/200 pi i))))))

(define (make-image mat)
  (plot3d (add-rear-wall (empty-scene "black") (cube-vertices mat) "gray")
          mat points "white"))
(define images1
  (build-list
    8 (lambda (i)
	(make-image (make-rotation-matrix (* 1/16 pi (- 8 i)) -1.0 0.0 0.0)))))
(define images2
  (build-list
    8 (lambda (i)
	(make-image (make-rotation-matrix (* 1/16 pi i) 0.0 -1.0 0.0)))))
(define images3
  (build-list
    8 (lambda (i)
	(make-image
	  (mat*mat (make-rotation-matrix (* 1/16 pi (- 8 i)) 0.0 -1.0 0.0)
		   (make-rotation-matrix (* 1/16 pi i) -1.0 0.0 0.0))))))
(define images (append images1 (append images2 images3)))

(big-bang
  images
  (on-draw (lambda (w) (car w)))
  (on-tick (lambda (w) (if (null? (cdr w)) images (cdr w))) 0.15))
