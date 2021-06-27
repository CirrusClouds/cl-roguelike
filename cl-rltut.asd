;;;; cl-rltut.asd

(asdf:defsystem #:cl-rltut
  :description "Describe cl-rltut here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-unittest #:cl-blt)
  :components ((:file "package")
	       (:file "game-map")
	       (:file "pathfinding")
	       (:file "entity")
               (:file "cl-rltut")))
