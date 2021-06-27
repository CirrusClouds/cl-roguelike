;;;; cl-rltut.lisp

(in-package :cl-rltut)

(defparameter *screen-width* 80)
(defparameter *screen-height* 50)
(defparameter *map-width* 80)
(defparameter *map-height* 45)
(defparameter *map* nil)
(defparameter *room-max-size* 10)
(defparameter *room-min-size* 6)
(defparameter *max-rooms* 30) 
(defparameter *num-enemies* 10)

(defmethod draw ((obj entity))
  (with-slots (x y char color) obj
    (setf (blt:color) color
	  (blt:cell-char x y) char)))

(defparameter *color-map* (list :dark-wall (blt:rgba 0 0 100)
                                :dark-ground (blt:rgba 50 50 150)
				:light-wall (blt:rgba 130 110 50)
				:light-ground (blt:rgba 200 180 50)))


(defun render-all (entities map)
  (blt:clear)
  (dotimes (y (map/height map))
    (dotimes (x (map/width map))
      (let* ((tile (aref (map/tiles map) x y))
             (wall (tile/blocked tile)))
            (if wall
		(setf (blt:background-color) (getf *color-map* :dark-wall))
		(setf (blt:background-color) (getf *color-map* :dark-ground))))
      (setf (blt:cell-char x y) #\Space)))
  (mapc #'draw entities)

  (setf (blt:background-color) (blt:black))
  (blt:refresh))

(defun config ()
  (blt:set "window.resizeable = true")
  (blt:set "window.size = ~Ax~A" *screen-width* *screen-height*)
  (blt:set "window.title = Roguelike"))


(defun handle-keys ()
  (let ((action nil))
    (blt:key-case (blt:read)
                  (:up (setf action (list :move (cons 0 -1))))
                  (:down (setf action (list :move (cons 0 1))))
                  (:left (setf action (list :move (cons -1 0))))
                  (:right (setf action (list :move (cons 1 0))))
                  (:escape (setf action (list :quit t)))
                  (:close (setf action (list :quit t)))
		  (:n (setf action (list :move (cons 1 1))))
		  (:b (setf action (list :move (cons -1 1))))
		  (:y (setf action (list :move (cons -1 -1))))
		  (:u (setf action (list :move (cons 1 -1))))
		  (:space (setf action (list :move (cons 0 0)))))
    action))


(defun main()
  (blt:with-terminal
    (config)
    (setf *map* (make-instance 'game-map :w *map-width* :h *map-height*))
    (initialize-map *map*)
    (initialize-tiles *map*)
    (generate-dungeon *map* *room-max-size* *room-min-size* *max-rooms*)
    
    (setf *player* (make-instance 'player
				  :char #\@
				  :color (blt:white)
				  :playerstatus 999
				  :trollstatus 0
				  :boxlinstatus 200))
    (setf *thing* (make-instance 'thing
				 :char #\x
				 :color (blt:yellow)))

    
    
    (setf (map/entities *map*) (list *player* *thing*))
    (generate-enemies (map/entities *map*) *num-enemies*)
    (place-all-entities (map/entities *map*) *map* nil)
    
    (loop :with entities = (map/entities *map*)
	  :do
             (render-all entities *map*)
             (let* ((action (handle-keys))
                    (movement (getf action :move))
                    (exit (getf action :quit)))
               (when exit
                 (return))
               (when movement
		 (handle-creature-movement *player* (cdr entities) movement *map*))
	       (handle-other-entity-movement entities *map*)))))
