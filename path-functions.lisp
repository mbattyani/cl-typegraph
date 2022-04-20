(in-package #:typeset)


(defun vector-length (x y)
  (sqrt (+ (* x x) (* y y))))

(defun normalized-vector (x y)
  (let ((l (vector-length x y)))
    (list (/ x l) (/ y l))))

(defun normalized-path-direction (path)
  "Returns as a (x y) pair the direction of the bezier path from the start"
  (let ((x0 (pop path))
	(y0 (pop path)))
    (loop :for x1 = (pop path)
	  :for y1 = (pop path)
	  :while (and (= x0 x1) (= y0 y1))
	  :finally (return (normalized-vector (- y1 y0) (- x1 x0))))))
