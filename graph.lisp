;;; cl-typesetting/cl-typegraph copyright 2003-2004 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-typesetting is here: http://www.fractalconcept.com/asp/html/cl-typesetting.html

(in-package #:typeset)

;(defparameter *dot-command* "dot -Tps ~s -o ~s")
(defparameter *dot-command* "dot")
(defparameter *dot-command-args* '("-Tplain-ext"))
(defparameter *graph-file-prefix* "/tmp/")
(defparameter *arrow-width* 2)
(defparameter *arrow-length* 6)
(defparameter *edge-label-font* (pdf:get-font "helvetica"))
(defparameter *edge-label-font-size* 9)
(defparameter *node-label-font* (pdf:get-font "helvetica"))
(defparameter *node-label-font-size* 12)
(defparameter *box-padding* '(4 2 4 2))


(defvar *graph-id-counter* 0)

(defun make-graph-node-id ()
  (format nil "N~d" (incf *graph-id-counter*)))

(defun make-graph-file-id ()
  (format nil "F~d" (incf *graph-id-counter*)))

(defclass graph-node-decoration-box ()
  ((background-color :accessor background-color :initarg :background-color :initform '(1.0 1.0 1.0))
   (border-color :accessor border-color :initarg :border-color :initform '(0.0 0.0 0.0))
   (border-width :accessor border-width :initarg :border-width :initform 1)
   (size-adjust :accessor size-adjust :initarg :size-adjust :initform 0)))

(defparameter +box+ (make-instance 'graph-node-decoration-box))

(defclass graph-node (box)
  ((id :accessor id :initform (make-graph-node-id))
   (data :accessor data :initarg :data :initform nil)
   (dot-attributes :accessor dot-attributes :initarg :dot-attributes :initform nil)
   (decoration :accessor decoration :initarg :decoration :initform +box+)
   (padding :accessor padding :initarg :padding :initform *box-padding*)
   (x :accessor x :initform 0)
   (y :accessor y :initform 0))
  (:default-initargs :dx nil :dy nil))

(defclass graph-edge ()
  ((id :accessor id :initform (make-graph-node-id))
   (label :accessor label :initarg :label :initform nil)
   (head :accessor head :initarg :head)
   (tail :accessor tail :initarg :tail)
   (direction :accessor direction :initarg :direction :initform :forward)
   (edge-arrows :accessor edge-arrows :initarg :edge-arrows :initform '(:head :arrow))
   (data :accessor data :initarg :data :initform nil)
   (dot-attributes :accessor dot-attributes :initarg :dot-attributes :initform nil)
   (color :accessor color :initarg :color :initform '(0.0 0.0 0.0))
   (width :accessor width :initarg :width :initform 1)
   (label-color :accessor label-color :initarg :color :initform '(0.0 0.0 0.0))
   (label-x :accessor label-x)
   (label-y :accessor label-y)
   (points :accessor points :initform ())))

;;; dot-attributes is a list of (attribute value) pairs ex: ("rankdir" "LR")

(defclass graph ()
  ((nodes :accessor nodes :initform (make-hash-table :test #'equal))
   (edges :accessor edges :initform (make-hash-table :test #'equal))
   (dot-attributes :accessor dot-attributes :initarg :dot-attributes :initform nil)
   (rank-constraints :accessor rank-constraints :initform nil)
   (background-color :accessor background-color :initarg :background-color :initform '(1.0 1.0 1.0))
   (border-color :accessor border-color :initarg :border-color :initform '(0.0 0.0 0.0))
   (border-width :accessor border-width :initarg :border-width :initform 1)
   (landscape-layout :accessor landscape-layout :initarg :landscape-layout :initform nil)
   (max-dx :accessor max-dx :initarg :max-dx :initform 400)
   (max-dy :accessor max-dy :initarg :max-dy :initform 400)
   (scale :accessor scale :initform 1)
   (dx :accessor dx)
   (dy :accessor dy)))

(defmethod initialize-instance :after ((node graph-node) 
				       &key fixed-height fixed-width graph &allow-other-keys)
  (adjust-graph-node-size node (data node) fixed-width fixed-height)
  (when graph (add-node graph node)))

(defmethod initialize-instance :after ((edge graph-edge)
				       &key graph  &allow-other-keys)
  (when graph (add-edge graph edge)))

(defun add-node (graph node)
  (setf (gethash (id node) (nodes graph)) node))

(defun get-node (graph id)
  (etypecase id
    (string (gethash id (nodes graph)))
    (symbol (gethash (symbol-name id) (nodes graph)))))

(defun add-edge (graph edge)
  (setf (gethash (id edge) (edges graph)) edge))

(defun add-rank-constraint (graph constraint nodes)
  (push (cons constraint nodes) (rank-constraints graph)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Size and location functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod size-adjust (thing)
  0)

(defmethod adjust-graph-node-size ((node graph-node) data fixed-width fixed-height)
  (tt::with-quad (l-a t-a r-a b-a) (size-adjust (decoration node))
    (tt::with-quad (l-p t-p r-p b-p) (padding node)
      (unless fixed-width
	(setf (dx node) (+ (pdf::text-width (format nil "~a" data) *node-label-font* *node-label-font-size*)
			   l-a l-p r-a r-p)))
      (unless fixed-height
	(setf (dy node) (+ *node-label-font-size* t-a t-p b-a b-p))))))

(defmethod adjust-graph-node-size ((node graph-node) (box box) fixed-width fixed-height)
  (tt::with-quad (l-a t-a r-a b-a) (size-adjust (decoration node))
    (tt::with-quad (l-p t-p r-p b-p) (padding node)
      (if fixed-width
	  (setf (dx node) (or (dx node) (+  (dx box) l-a l-p r-a r-p)))
	  (setf (dx node) (+ (compute-boxes-natural-size (boxes box) #'dx) l-a l-p r-a r-p)))
      (if fixed-height
	  (setf (dy node) (or (dy node) (+ (dy box) t-a t-p b-a b-p)))
	  (setf (dy node) (+ (compute-boxes-natural-size (boxes box) #'dy) t-a t-p b-a b-p))))))

(defmethod content-x ((node graph-node))
  (with-quad (l-d) (size-adjust (decoration node))
    (with-quad (l-p) (padding node)
      (+ (x node) l-d l-p ))))

(defmethod content-y ((node graph-node))
  (with-quad (l-d t-d) (size-adjust (decoration node))
    (with-quad (l-p t-p) (padding node)
      (- (y node) t-d t-p ))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Writing dot files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun gen-dot-attributes (s attributes &optional comma)
  (loop for (attribute value) in attributes do
	(if comma
	    (write-string ", " s)
	    (setf comma t))
	(write-string attribute s)
	(write-char #\= s)
	(write-line value s)))

(defmethod gen-graph-dot-data ((graph graph) s)
  (let ((*print-readably* nil))
    (format s "digraph G {
size=\"~a,~a\";
edge [fontname=~a,fontsize=~a];
"
	    (/ (max-dx graph) 72.0)(/ (max-dy graph) 72.0)
	    (pdf:name *edge-label-font*) *edge-label-font-size*)
    (loop for (rank-constraint . nodes) in (rank-constraints graph) do
      (format s "{rank = ~a; ~{~s;~^ ~}};~%" rank-constraint (mapcar 'id nodes)))
    (format s "graph [")
    (gen-dot-attributes s (dot-attributes graph))
    (format s "];")
    (iter (for (id node) in-hashtable (nodes graph))
      (gen-graph-dot-data node s))
    (iter (for (id edge) in-hashtable (edges graph))
      (gen-graph-dot-data edge s))
    (format s "}~%")))

(defmethod gen-graph-dot-data ((node graph-node) s)
  (format s "~s [fixedsize=true, width=~a, height=~a"
	  (id node)(/ (dx node) 72.0)(/ (dy node) 72.0))
  (gen-dot-attributes s (dot-attributes node) t)
  (format s "];~%"))

(defmethod gen-graph-dot-data ((edge graph-edge) s)
  (format s "~s -> ~s [label=\"~a\", arrowhead=none, color=\"~a\""
	  (id (head edge)) (id (tail edge))
	  (if (label edge) (label edge) "")
	  (id edge))
  (gen-dot-attributes s (dot-attributes edge) t)
  (format s "];~%"))

(defun read-graph-line-values (string)
  (when string
    (let ((*package* (find-package :keyword)))
      (iter (for position first 0 then end)
	    (for (values object end) = (read-from-string string nil nil :start position))
	    (while object)
	    (collect object)))))

(defun process-graph-line (graph values)
  (setf (scale graph)  (first values)
	(dx graph)(* (second values) (scale graph) 72.0)
	(dy graph)(* (third values) (scale graph) 72.0)))

(defun process-graph-node-line (graph values)
  (let ((node (get-node graph (pop values))))
    (setf (x node) (- (* (pop values) 72.0) (* (dx node) 0.5))
	  (y node) (+ (* (pop values) 72.0) (* (dy node) 0.5)))))

(defun process-graph-edge-line (graph values)
  (let* ((id  (symbol-name (car (last values))))
	 (edge (gethash id (edges graph))))
    (pop values)
    (pop values)
    (setf (points edge) (iter (repeat (pop values))
			      (collect (* (pop values) 72.0))
			      (collect (* (pop values) 72.0))))
    (when (label edge)
      (pop values)
      (setf (label-x edge) (* (pop values) 72.0)
	    (label-y edge) (* (pop values) 72.0)))))

;;; this should be changed to use pipes instead of files and adapted to other Lisp implementations.
(defun compute-graph-layout (graph)
  (let* ((file-id (make-graph-file-id))
	 (dot-file (concatenate 'string *graph-file-prefix* file-id ".dot"))
	 (result-file  (concatenate 'string *graph-file-prefix* file-id ".txt")))
    (unwind-protect
	 (progn
	   (with-open-file (s dot-file :direction :output :if-exists :supersede)
	     (gen-graph-dot-data graph s))
#+lispworks (sys:call-system (format nil "~a~{ ~s~} ~s -o ~s" *dot-command* *dot-command-args* dot-file result-file) :wait t)
#+cmu (ext:run-program *dot-command* `(,@*dot-command-args* ,dot-file "-o" ,result-file) :wait t)
#+sbcl (sb-ext:run-program *dot-command* `(,@*dot-command-args* ,dot-file "-o" ,result-file) :wait t :search t )
	   (with-open-file (s result-file :direction :input)
	     (iter (for line = (read-line s nil))
		   (while line)
		   (for (line-type . values) = (read-graph-line-values line))
		   (case line-type
		     (:node (process-graph-node-line graph values))
		     (:edge (process-graph-edge-line graph values))
		     (:graph (process-graph-line graph values))
		     (:stop (finish))))))
      (progn (ignore-errors (delete-file dot-file))
	     (ignore-errors (delete-file result-file))))))

(defun graph-box (graph &rest args)
  (let ((dx (dx graph))
	(dy (dy graph)))
    (when (landscape-layout graph)
      (rotatef dx dy))
    (add-box (apply 'make-instance 'user-drawn-box
		    :stroke-fn #'(lambda (box x y)
				   (if (landscape-layout graph)
				       (pdf:with-saved-state
					   (pdf:translate x (- y dy))
					   (pdf:rotate 90)
					   (stroke graph 0 0))
				       (stroke graph x y)))
		    :inline t :dx dx :dy dy
		    :allow-other-keys t args))))

(defmethod stroke ((graph graph) x y)
  (pdf:with-saved-state
      (pdf:set-color-fill (background-color graph))
      (when (border-width graph)
	(pdf:set-color-stroke (border-color graph))
	(pdf:set-line-width (border-width graph))
	(pdf:basic-rect x y (dx graph)(- (dy graph)))
	(pdf:fill-and-stroke))
      (pdf:translate x (- y (dy graph)))
      (pdf:scale (scale graph)(scale graph))
      (iter (for (id edge) in-hashtable (edges graph))
	    (stroke-edge edge (data edge)))
      (iter (for (id node) in-hashtable (nodes graph))
	    (stroke-node node (data node)))))

(defmethod stroke-node ((node graph-node) data)
  (stroke-node-decoration node (decoration node))
  (stroke-node-content node data))


(defmethod stroke-node-decoration ((node graph-node) decoration))

(defmethod stroke-node-decoration ((node graph-node) (decoration graph-node-decoration-box))
  (pdf:with-saved-state
      (pdf:set-color-fill (background-color decoration))
      (when (border-width decoration)
	(pdf:set-color-stroke (border-color decoration))
	(pdf:set-line-width (border-width decoration))
	(pdf:basic-rect (x node) (y node)(dx node)(- (dy node)))
	(pdf:fill-and-stroke))))


(defmethod stroke-node-content ((node graph-node) data)
  (when data
    (pdf:set-color-fill '(0.0 0.0 0.0))
    (pdf:draw-centered-text (+ (x node) (* (dx node) 0.5))
			    (- (y node) (* (dy node) 0.5) (* 0.3 *node-label-font-size*))
			    (format nil "~a" data)
			    *node-label-font* *node-label-font-size*)))

(defmethod stroke-node-content ((node graph-node) (box box))
  (stroke box (content-x node) (content-y node)))


(defmethod stroke-edge ((edge graph-edge) data)
  (pdf:with-saved-state
      (pdf:set-color-stroke (color edge))
      (pdf:set-color-fill (color edge))
      (pdf:set-line-width (width edge))
    (let ((points (points edge))
	  (head-arrow-type (getf (edge-arrows edge) :head))
	  (tail-arrow-type (getf (edge-arrows edge) :tail))
	  x1 y1 x2 y2 x3 y3 prev-x1 prev-y1)
      (stroke-arrow edge tail-arrow-type (points edge))
      (pdf:move-to (pop points)(pop points))
      (iter (while points)
	(setf prev-x1 x1 prev-y1 y1)
	(setf x1 (pop points) y1 (pop points)
	      x2 (pop points) y2 (pop points)
	      x3 (pop points) y3 (pop points))
        (pdf:bezier-to x1 y1 x2 y2 x3 y3))
      (pdf:stroke)
      (stroke-arrow edge head-arrow-type (reverse-path (points edge)))
      (when (label edge)
	(pdf:set-color-fill (label-color edge))
	(pdf:draw-centered-text (label-x edge)(label-y edge)(label edge)
				*edge-label-font* *edge-label-font-size*)))))

(defmethod stroke-arrow (edge arrow-type path))

(defmethod stroke-arrow ((edge graph-edge) (arrow-type (eql :arrow)) path)
  (let* ((x2 (pop path))
	 (y2 (pop path))
	 (x1 (pop path))
	 (y1 (pop path))
	 (nx (- x1 x2))
	 (ny (- y1 y2))
	 (l (/ (sqrt (+ (* nx nx) (* ny ny)))))
	 (x0 (+ x2 (* nx *arrow-length* l)))
	 (y0 (+ y2 (* ny *arrow-length* l)))
	 (dx (* nx *arrow-width* l))
	 (dy (* ny *arrow-width* l)))
    (pdf:move-to x2 y2)
    (pdf:line-to (+ x0 dy) (- y0 dx))
    (pdf:line-to (- x0 dy) (+ y0 dx))
    (pdf:line-to x2 y2)
    (pdf:fill-and-stroke)))

(defmethod stroke-arrow ((edge graph-edge) (arrow-type (eql :circle)) path)
  (let* ((x2 (pop path))
	 (y2 (pop path))
	 (x1 (pop path))
	 (y1 (pop path))
	 (nx (- x1 x2))
	 (ny (- y1 y2))
	 (l (/ (sqrt (+ (* nx nx) (* ny ny)))))
	 (x0 (+ x2 (* nx *arrow-length* l 0.5)))
	 (y0 (+ y2 (* ny *arrow-length* l 0.5))))
    (pdf:circle x0 y0 (*  *arrow-length* 0.5))
    (pdf:fill-and-stroke)))
