;;;; entity.lisp

(in-package :cl-rltut)


(defclass entity ()
  ((x
    :initarg :x
    :accessor entity/x)
   (y
    :initarg :y
    :accessor entity/y)
   (char
    :initarg :char
    :accessor entity/char)
   (color
    :initarg :color
    :accessor entity/color)))


(defclass creature (entity)
  ((statustoplayer
    :initarg :playerstatus
    :accessor creature/playerstatus)
   (statustotroll
    :initarg :trollstatus
    :accessor creature/trollstatus)
   (statustoboxlin
    :initarg :boxlinstatus
    :accessor creature/boxlinstatus)))


(defclass troll (creature)
  ())


(defclass boxlin (creature)
  ())


(defclass player (creature)
  ())


(defclass thing (entity)
  ((walkable
    :initarg :walkable
    :accessor thing/walkthru)))


(defmethod move ((obj entity) dx dy)
  (incf (entity/x obj) dx)
  (incf (entity/y obj) dy)
  )

(defmethod canmovep ((obj entity) (map game-map) movement)
  (let ((movedirectionx (+ (car movement) (entity/x obj)))
	(movedirectiony (+ (cdr movement) (entity/y obj))))
    (cond ((eq (tile/blocked (aref (map/tiles map) movedirectionx movedirectiony)) t)
	   nil)
	  (t t))))


(defmethod place-center-starting-room ((obj entity) (map game-map))
  (let ((starting-room (car (map/rooms map))))
    (multiple-value-bind (room-xc room-yc) (rect-room-centre starting-room)
      (setf (entity/x obj) room-xc)
      (setf (entity/y obj) room-yc))))


(defmethod place-center-random-room ((obj entity) (map game-map))
  (let ((random-room (nth (random (list-length (map/rooms map))) (map/rooms map))))
    (multiple-value-bind (room-xc room-yc) (rect-room-centre random-room)
      (setf (entity/x obj) room-xc)
      (setf (entity/y obj) room-yc))))


(defmethod place-random-unblocked ((obj entity) (map game-map) acc)
  (let* ((unblocked-tiles (list-unblocked-tiles map))
	 (random-tile (nth (random (list-length unblocked-tiles)) unblocked-tiles)))
    ;; If any entity in the list has x, y of random tile, recall the function
    ;; Else place the entity
    (cond ((null acc)
	   (setf (entity/x obj) (tile/x random-tile))
	   (setf (entity/y obj) (tile/y random-tile)))
	  ((member t (mapcar (lambda (entity)
			       (and (= (entity/x entity) (tile/x random-tile)) (= (entity/y entity) (tile/y random-tile))))
			     acc))
	   (place-random-unblocked obj map acc))
	  (t 
	   (setf (entity/x obj) (tile/x random-tile))
	   (setf (entity/y obj) (tile/y random-tile))))))


(defmethod place-all-entities (entities (map game-map) acc)
  (cond ((not (null entities))
	 (place-random-unblocked (car entities) map acc)
	 (place-all-entities (cdr entities) map (cons (car entities) acc)))
	(t
	 nil)))


(defmethod bump-entity-p ((obj entity) (obj2 entity) movement)
  (let ((movedirectionx (+ (car movement) (entity/x obj)))
	(movedirectiony (+ (cdr movement) (entity/y obj))))
    (cond ((and (= movedirectionx (entity/x obj2))
		(= movedirectiony (entity/y obj2)))
	   t)
	  (t
	   nil))))


(defmethod interact-entity ((obj creature) (obj2 thing))
  nil)


(defmethod interact-entity ((obj creature) (obj2 troll))
  (cond ((and (>= (creature/trollstatus obj) 0) (< (creature/trollstatus obj) 251))
	 (format t "~A kicks ~A~%" obj obj2))
	(t
	 nil)))


(defmethod interact-entity ((obj creature) (obj2 player))
  (cond ((and (>= (creature/playerstatus obj) 0) (< (creature/playerstatus obj) 251))
	 (format t "~A kicks ~A~%" obj obj2))
	(t
	 nil)))


(defmethod interact-entity ((obj creature) (obj2 boxlin))
  (cond ((and (>= (creature/boxlinstatus obj) 0) (< (creature/boxlinstatus obj) 251))
	 (format t "~A kicks ~A~%" obj obj2))
	(t
	 nil)))


(defmethod handle-creature-movement ((obj creature) other-entities movement (map game-map))
  (if (canmovep obj map movement)
      (cond ((null other-entities)
	     (move obj (car movement) (cdr movement)))
	    (t
	     (if (bump-entity-p obj (car other-entities) movement)
		 (interact-entity obj (car other-entities))
		 (handle-creature-movement obj (cdr other-entities) movement map))))))


(defmethod handle-creature-movement ((obj thing) other-entities movement (map game-map))
  nil)


(defun random-of (list)
  (multiple-value-bind (f r) (floor (random (length list)))
    (nth f list)))


(defun random-movement ()
  (cons (random-of (list -1 0 1)) (random-of (list -1 0 1))))


(defmethod determine-abs-distance ((obj entity) (obj2 entity))
  (sqrt (+ (expt (- (entity/x obj) (entity/x obj2)) 2) (expt (- (entity/x obj) (entity/x obj2)) 2))))

(defmethod can-see-p ((obj entity) (obj2 entity) distance)
  (if (<= (determine-abs-distance obj obj2) 12)
	t
	nil))

(defun move-towards (obj obj2)
  (let ((xdsp (- (entity/x obj2) (entity/x obj)))
	(ydsp (- (entity/y obj2) (entity/y obj))))
    (cond ((and (= xdsp 0) (= ydsp 0))
	   (cons 0 0))
	  ((= xdsp 0)
	   (cons 0 (/ ydsp (abs ydsp))))
	  ((= ydsp 0)
	   (cons (/ xdsp (abs xdsp)) 0))
	  (t
	   (cons (/ xdsp (abs xdsp)) (/ ydsp (abs ydsp)))))))




(defmethod determine-movement ((obj creature) entities (map game-map))
  (let ((distcansee (mapcar (lambda (ent)
			      (list (determine-abs-distance obj ent)
				    (can-see-p obj ent (determine-abs-distance obj ent))
				    ent)) ; Distance can-see target-entity
			    entities)))
    (sort distcansee (lambda (x y)
		       (<= (car x) (car y))))
    (cond ((car (cdr (car distcansee)))
	   ;; If instance of certain object take specific action
	   ;; If thing cdr
	   ;; If friendly cdr
	   ;; If move action (where move action is whatever youre supposed to do here
	   ;;  
	   ;; (init-dijkstra (aref (map/tiles map) (entity/x obj) (entity/y obj))
	   ;; 		  (aref (map/tiles map)
	   ;; 			(entity/x (car (cdr (cdr (car distcansee)))))
	   ;; 			(entity/y (car (cdr (cdr (car distcansee))))))
	   ;; 		  map)
	   (move-towards obj (car (cdr (cdr (car distcansee)))))
	   )
	  (t
	   (random-movement)))))


(defmethod determine-movement ((obj thing) entities map)
  (cons 0 0))

(defmethod handle-other-entity-movement (entities (map game-map))
  (mapc (lambda (entity)
	  (handle-creature-movement entity (remove entity entities) (determine-movement entity (remove entity entities) map) map))
	(cdr entities)))


(defun random-colour ()
  (random-of (list (blt:yellow) (blt:red) (blt:green))))


(defun generate-enemies (entities num)
  (cond ((<= num 0)
	 nil)
	((< (random 1000) 250)
	 (nconc entities (list (make-instance 'troll
					      :char #\T
					      :color (random-colour)
					      :playerstatus (random-of (unitrange 0 250))
					      :trollstatus (random-of (unitrange 251 500))
					      :boxlinstatus (random-of (unitrange 0 250)))))
	 (generate-enemies entities (- num 1)))
	(t
	 (nconc entities (list (make-instance 'boxlin
					      :char #\b
					      :color (random-colour)
					      :playerstatus (random-of (unitrange 0 250))
					      :trollstatus (random-of (unitrange 0 250))
					      :boxlinstatus (random-of (unitrange 251 500)))))
	 (generate-enemies entities (- num 1)))))
