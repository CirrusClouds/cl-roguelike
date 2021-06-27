
(in-package :cl-rltut
	    )

(defparameter cl-unittest:*log-level* 'debug)

;; Dijkstras

(setf *dummy-tile* (make-instance 'tile
				  :blocked t
				  :block-sight t
				  :x 100
				  :y 100
				  :weight 9999
				  :visited t
				  :predecessor nil))

(defmethod reset-weights ((map game-map))
  (cl-unittest:make-log 'debug "Resetting weights")
  (terpri)
  (mapc (lambda (x)
	  (mapc (lambda (y)
		  (setf (tile/weight (aref (map/tiles map) x y)) 100000)
		  (setf (tile/visited (aref (map/tiles map) x y)) t))
		(unitrange 0 (- (map/height map) 1))))
	(unitrange 0 (- (map/width map) 1)))
  (mapc (lambda (tile)
	  (setf (tile/visited tile) nil))
	(list-unblocked-tiles map)))

(defun safe-aref-2d (array i j)
  (cl-unittest:make-log 'debug "Finding a neighbour tile safely")
  (terpri)
  (handler-case (aref array i j)
    (error (c)
      nil)))

(defmethod determine-neighbours ((current tile) (map game-map))
  (cl-unittest:make-log 'debug "Determining neighbours")
  (terpri)
  (remove-if #'null (list
		     (safe-aref-2d (map/tiles map) (+ 1 (tile/x current)) (+ 1 (tile/y current)))
		     (safe-aref-2d (map/tiles map) (+ 1 (tile/x current)) (tile/y current))
		     (safe-aref-2d (map/tiles map) (+ 1 (tile/x current)) (- (tile/y current) 1))
		     (safe-aref-2d (map/tiles map) (tile/x current) (+ 1 (tile/y current)))
		     (safe-aref-2d (map/tiles map) (tile/x current) (- 1 (tile/y current)))
		     (safe-aref-2d (map/tiles map) (- (tile/x current) 1) (+ (tile/y current) 1))
		     (safe-aref-2d (map/tiles map) (- (tile/x current) 1) (tile/y current))
		     (safe-aref-2d (map/tiles map) (- (tile/x current) 1) (- (tile/y current) 1))
		     )))


(defmethod init-dijkstra (current target (map game-map))
  (cl-unittest:make-log 'debug "Initiating Dijkstra's Algorithm for Pathfinding at ~A ~A" (tile/x current) (tile/y current))
  (terpri)
  (reset-weights map)
  (setf (tile/weight current) 0)
  (setf (tile/predecessor current) nil)
  (run-dijkstra current target map))


(defmethod run-dijkstra (current target (map game-map))
  (cl-unittest:make-log 'debug "Running Dijkstra's Algorithm")
  (terpri)
  (let ((neighbours (determine-neighbours current map)))
    (cond ((eq (tile/visited current) nil)
	   (setf (tile/visited current) t)
	   (iterate-dijkstra current neighbours map)
	   (run-dijkstra (find-lowest-weight-unvisited (list-unblocked-tiles map) *dummy-tile*)
			 target
			 map))
	  (t
	   (find-path-from-target target nil)))))


(defmethod iterate-dijkstra (current neighbours (map game-map))
  (cl-unittest:make-log 'debug "Iterating dijkstras")
  (terpri)
  (mapc (lambda (neighbour)
	  (if (and (> (tile/weight neighbour) (+ (tile/weight (aref (map/tiles map) (tile/x current) (tile/y current))) 1)) (eq (tile/blocked neighbour) nil))
	      (progn
		(setf (tile/weight neighbour) (+ (tile/weight (aref (map/tiles map) (tile/x current) (tile/y current))) 1))
		(setf (tile/predecessor neighbour) current))
	      nil))
	neighbours))


(defun find-lowest-weight-unvisited (tiles acc)
  (cl-unittest:make-log 'debug "Finding lowest-weight unvisited node")
  (terpri)
  (cond ((null tiles)
	 acc)
	(t
	 (if (tile/visited (car tiles))
	     (find-lowest-weight-unvisited (cdr tiles) acc)
	     (if (>= (tile/weight (car tiles)) (tile/weight acc))
		 (find-lowest-weight-unvisited (cdr tiles) acc)
		 (find-lowest-weight-unvisited (cdr tiles) (car tiles)))))))


(defun find-path-from-target (target acc)
  (cl-unittest:make-log 'debug "Finding path from target")
  (terpri)
  (cond ((null target)
	 acc)
	(t
	 (find-path-from-target (tile/predecessor target) (cons target acc)))))

