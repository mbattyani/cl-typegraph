(in-package #:typeset)


(defun reverse-path (path)
  "Reverse the PATH.

PATH is a list of alternating x y coordinates, e.g.
(x1 y1 x2 y2 x3 y3).

The result is
(x3 y3 x2 y2 x1 y2)"
  (loop
    :with result = (list)
    :for (x y) :on path :by #'cddr
    :do (push y result) (push x result)
    :finally (return result)))

(defun vector-length (x y)
  "Euclidian length of vector ((0 0) --> (x y)).

So basically sqrt (x^2 + y2)"
  (sqrt (+ (* x x) (* y y))))

(defun normalized-vector (x y)
  "Returns vector (x y) normalized to length 1

Will error out when length is zero (that is, when x=0 and y=0)."
  (let ((l (vector-length x y)))
    (list (/ x l) (/ y l))))

(defun normalized-path-direction (path)
  "Returns as a (x y) pair the normalized direction of the bezier path from the start.

That is, direction (x y) is the tangent direction of the be bezier
curve PATH at first point."
  (let ((x0 (pop path))
	(y0 (pop path)))
    (loop :for x1 = (pop path)
	  :for y1 = (pop path)
	  :while (and (= x0 x1) (= y0 y1))
	  :finally (return (normalized-vector (- y1 y0) (- x1 x0))))))


