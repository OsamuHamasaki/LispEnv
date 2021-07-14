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
        (row (cdr place)))
    (or (> 0 line) (> 0 row) (>= line *bord-size*) (>= row *bord-size*))))

(defun next-cell (place fn-pair)
  (cons (funcall (car fn-pair) (car place)) (funcall (cdr fn-pair) (cdr place))))

(defun teritory-x (place fn-pair)
  (labels ((teritory-x1 (place acc)
             (if (out-of-bound place)
                 acc
                 (teritory-x1 (next-cell place fn-pair) (cons place acc)))))
        (teritory-x1 (next-cell place fn-pair) nil)))

(defun teritory (place)
  (let ((funcs (list (cons #'1- #'identity) (cons #'1- #'1+) (cons #'identity #'1+) (cons #'1+ #'1+) 
                     (cons #'1+ #'identity) (cons #'1+ #'1-) (cons #'identity #'1-) (cons #'1- #'1-))))
    (cons place (apply #'append (mapcar #'(lambda (fn) (teritory-x place fn)) funcs)))))

(defun solve-nqueen (candidates result index len)
  (cond 
    ((= (length result) *bord-size*) (values result t))
    ((>= index len) (values result nil))
    (t 
      (let* ((place (nth index candidates))
             (remains (set-difference candidates (teritory place) :test #'equal)))
        (multiple-value-bind (ret successed)
          (solve-nqueen remains (cons place result) 0 (length remains))
          (if successed
              (values ret t)
              (solve-nqueen candidates result (1+ index) len)))))))

(defun nqueen (n)
  (setf *bord-size* n)
  (let ((candidates (initial-candidates)))
    (solve-nqueen candidates nil 0 (length candidates))))

