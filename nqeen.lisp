;;;; N Queen solver

(defparameter *bord-size* 8)

(defun initial-candidates ()
  (let ((result nil)
        (n *bord-size*))
    (dotimes (x (* n n))
      (multiple-value-bind (line row) (floor x n)
        (push  (cons line row) result)))
    (nreverse result)))

(defun out-of-bound (place)
  (let ((line (car place))
        (row (cdr place))
        (n *bord-size*))
    (or (> 0 line) (> 0 row) (>= line n) (>= row n))))

(defun teritory-x (place next-fn)
  (labels ((teritory-x1 (place acc)
             (if (out-of-bound place)
                 acc
                 (teritory-x1 (funcall next-fn place) (push place acc)))))
        (teritory-x1 (funcall next-fn place) nil)))

(defun next-x (place fn-car fn-cdr)
  (cons (funcall fn-car (car place)) (funcall fn-cdr (cdr place))))

(defun next-north (place)
  (next-x place #'1- #'identity))

(defun next-south (place)
  (next-x place #'1+ #'identity))

(defun next-east (place)
  (next-x place #'identity #'1+))

(defun next-west (place)
  (next-x place #'identity #'1-))

(defun next-north-west (place)
  (next-x place #'1- #'1-))

(defun next-south-west (place)
  (next-x place #'1+ #'1-))

(defun next-north-east (place)
  (next-x place #'1- #'1+))

(defun next-south-east (place)
  (next-x place #'1+ #'1+))

(defun teritory (place)
  (let ((funcs (list #'next-north #'next-north-east #'next-east #'next-south-east
                     #'next-south #'next-south-west #'next-west #'next-north-west)))
    (cons place (apply #'append (mapcar #'(lambda (fn) (teritory-x place fn)) funcs)))))

(defun solve-nqeen (candidates result index len)
  (cond 
    ((= (length result) *bord-size*) (values result t))
    ((>= index len) (values result nil))
    (t 
      (let* ((place (nth index candidates))
             (remains (set-difference candidates (teritory place) :test #'equal)))
        (multiple-value-bind (ret successed)
          (solve-nqeen remains (cons place result) 0 (length remains))
          (if successed
              (values ret t)
              (solve-nqeen candidates result (1+ index) len)))))))

(defun nqeen (n)
  (setf *bord-size* n)
  (let ((candidates (initial-candidates)))
    (solve-nqeen candidates nil 0 (length candidates))))

