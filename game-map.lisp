;;;; game-map.lisp

(in-package :cl-rltut)

(defclass tile ()
  ((blocked
    :initarg :blocked
    :accessor tile/blocked
    :initform nil)
   (block-sight
    :initarg :block-sight
    :accessor tile/block-sight
    :initform nil)
   (x
    :initarg :x
    :accessor tile/x)
   (y
    :initarg :y
    :accessor tile/y)
   (weight
    :initarg :weight
    :accessor tile/weight)
   (visited
    :initarg :visited
    :accessor tile/visited)
   (predecessor
    :initarg :predecessor
    :accessor tile/predecessor)))


(defclass game-map ()
  ((width
    :initarg :w
    :accessor map/width)
   (height
    :initarg :h
    :accessor map/height)
   (tiles
    :initarg :tiles
    :accessor map/tiles)
   (rooms
    :initarg :rooms
    :accessor map/rooms)
   (entities
    :initarg :entities
    :accessor map/entities)))

(defmethod initialize-map ((map game-map))
  (setf (map/tiles map) (make-array (list (map/width map) (map/height map)))))

(defmethod initialize-tiles ((map game-map))
  (dotimes (x (map/width map))
    (dotimes (y (map/height map))
      (setf (aref (map/tiles map) x y) (make-instance 'tile :blocked t :block-sight t :x x :y y :weight 100000)))))

(defmethod list-unblocked-tiles ((map game-map))
  (remove-if #'null
	     (mapcan
	      (lambda (x)
		(mapcar
		 (lambda (y)
		   (cond ((null (tile/blocked (aref (map/tiles map) x y)))
			  (aref (map/tiles map) x y))
			 (t
			  nil)))
		 (unitrange 0 (- (map/height map) 1))))
	      (unitrange 0 (- (map/width map) 1)))))

(defclass rect-room ()
  ((x1
    :initarg :x
    :accessor room/x)
   (y1
    :initarg :y
    :accessor room/y)
   (width
    :initarg :width
    :accessor room/width)
   (height
    :initarg :height
    :accessor room/height)))


(defun unitrange (x1 x2)
  (cond ((> x1 x2)
	 nil)
	(t
	 (cons x1 (unitrange (+ 1 x1) x2)))))


(defmethod generate-room ((map game-map) (room rect-room))
  (mapc (lambda (y)
	  (mapc
	   (lambda (x)
	     (setf (tile/blocked (aref (map/tiles map) x y)) nil)
	     (setf (tile/block-sight (aref (map/tiles map) x y)) nil))
	   (unitrange (room/x room) (+ (room/x room) (room/width room)))))
	(unitrange (room/y room) (+ (room/y room) (room/height room)))))


(defmethod create-h-tunnel ((map game-map) x1 x2 y)
  (let ((start-x (min x1 x2))
	(end-x (max x1 x2)))
    (mapc (lambda (x)
	    (setf (tile/blocked (aref (map/tiles map) x y)) nil)
	    (setf (tile/block-sight (aref (map/tiles map) x y)) nil))
	  (unitrange start-x end-x))))


(defmethod create-v-tunnel ((map game-map) y1 y2 x)
  (let ((start-y (min y1 y2))
	(end-y (max y1 y2)))
    (mapc (lambda (y)
	    (setf (tile/blocked (aref (map/tiles map) x y)) nil)
	    (setf (tile/block-sight (aref (map/tiles map) x y)) nil))
	  (unitrange start-y end-y))))


(defmethod rect-room-centre ((room rect-room))
  (let ((centre-x (* (+ (+ (room/x room) (room/width room)) (room/x room)) 0.5))
	(centre-y (* (+ (+ (room/y room) (room/height room)) (room/y room)) 0.5)))
    (values (round centre-x) (round centre-y))))


(defmethod intersect ((room1 rect-room) (room2 rect-room))
  "Returns T if this RECT intersects with OTHER"
  (and (<= (+ (room/x room1) 1) (- (+ (room/x room2) (room/width room2)) 1))
       (>= (+ (+ (room/x room1) (room/width room1)) 1) (- (room/x room2) 1))
       (<= (+ (room/y room1)) (- (+ (room/y room2) (room/height room2)) 1))
       (>= (+ (+ (room/y room1) (room/height room1)) 1) (- (room/y room2) 1))))


(defmethod can-place-p (room rooms) 
  (cond
    ((null rooms)
     t)
    ((intersect room (car rooms))
     nil)
    (t
     (can-place-p room (cdr rooms)))))



;; Generate a number of rooms randomly that have x and y within map and place if there's no intersection
;; connect tunnels

(defmethod make-random-room ((map game-map) max-size min-size)
  (let* ((w (+ (random (- max-size min-size)) min-size))
	(h (+ (random (- max-size min-size)) min-size))
	(x (random (- (map/width map) w)))
	(y (random (- (map/height map) h))))
    (make-instance 'rect-room :x x :y y :width w :height h)))


;; Returns a list of all the rooms generated 
(defmethod make-all-rooms ((map game-map) max-size min-size max-rooms roomlist)
  (let ((new-room (make-random-room map max-size min-size)))
    (cond ((null roomlist)
	   (make-all-rooms map max-size min-size (- max-rooms 1) (cons new-room nil)))
	  ((< max-rooms 0)
	   roomlist)
	  (t
	   (cond
	     ((can-place-p new-room roomlist)
	      (make-all-rooms map max-size min-size (- max-rooms 1) (cons new-room roomlist)))
	     (t
	      (make-all-rooms map max-size min-size (- max-rooms 1) roomlist)))))))


;; Returns a list of all the tunnels generated
(defmethod make-all-tunnels ((map game-map) roomlist)
  (reduce (lambda (room1 room2)
	    (multiple-value-bind (room1-xc room1-yc) (rect-room-centre room1)
	      (multiple-value-bind (room2-xc room2-yc) (rect-room-centre room2)
		(progn
		  (create-v-tunnel map room1-yc room2-yc room1-xc)
		  (create-h-tunnel map room1-xc room2-xc room2-yc)
		  room2))))
	  roomlist))

;; Border around the map

(defmethod draw-border ((map game-map))
  (mapc (lambda (x)
	  (mapc (lambda (y)
		  (setf (tile/blocked (aref (map/tiles map) (- (map/width map) 1) y)) t)
		  (setf (tile/blocked (aref (map/tiles map) 0 y)) t)
		  (setf (tile/blocked (aref (map/tiles map) x (- (map/height map) 1))) t)
		  (setf (tile/blocked (aref (map/tiles map) x 0)) t)
		  (setf (tile/block-sight (aref (map/tiles map) (- (map/width map) 1) y)) t)
		  (setf (tile/block-sight (aref (map/tiles map) 0 y)) t)
		  (setf (tile/block-sight (aref (map/tiles map) x (- (map/height map) 1))) t)
		  (setf (tile/block-sight (aref (map/tiles map) x 0)) t))
		(unitrange 0 (- (map/height map) 1))))
	(unitrange 0 (- (map/width map) 1))))

;; Now finally let's generate the dungeon


(defmethod generate-dungeon ((map game-map) room-max-size room-min-size max-rooms)
  (let ((rooms (make-all-rooms map room-max-size room-min-size max-rooms nil)))
    (setf (map/rooms map) rooms)
    (mapc (lambda (room)
	    (generate-room map room))
	  rooms)
    (make-all-tunnels map rooms)
    (draw-border map)
    ))
