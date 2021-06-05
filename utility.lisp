;;;; Utilities

(proclaim '(optimize speed))

;;;; Utilties from Paul Graham's ANSI Common Lisp

;;; Function single?
;;; list -> bool
;;; if list has only one element, return t

(defun single? (lst)
 (and (consp lst) (null (cdr lst))))

;;; Fuction addpend1
;;; list, object -> list
;;; add object to last of list

(defun append1 (lst obj)
 (append lst (list obj)))

;;; Function map-int
;;; function, int -> list
;;; apply function to 0..(n - 1),  then collect result to output list

(defun map-int (fn n)
 (let ((acc nil))
  (dotimes (i n)
   (push (funcall fn i) acc))
  (nreverse acc)))

;;; Function filter
;;; function, list -> list
;;; apply function to each list element, and collect result to output list when result is not nil

(defun filter (fn lst)
 (let ((acc nil))
  (dolist (x lst)
   (let ((val (funcall fn x)))
    (if val (push val acc))))
  (nreverse acc)))

;;; Function most
;;; function, list -> value, value
;;;

(defun most (fn lst)
 (if (null lst)
     (values nil nil)
     (let* ((wins (car lst))
            (max (funcall fn wins)))
      (dolist (obj (cdr lst))
       (let ((score (funcall fn obj)))
        (when (> score max)
         (setf wins obj
               max score))))
      (values wins max))))


;;;; My Original 

(defun trim32bit (x)
    (logand #xffffffff x))

(defun printNbase32 (x n)
    (let ((*print-base* n)
          (*print-radix* nil))
        (format nil "~S" (trim32bit x))))

(defun printHex32 (x)
    (printNbase32 x 16))

(defun printBinary32 (x)
    (printNbase32 x 2))

(defun factrial (x)
    (labels ((fact (n acc)
                (if (zerop n)
                    acc
                    (fact (- n 1) (* acc n)))))
            (fact x 1)))

(defun mappend (fn &rest lsts)
    (apply #'append (apply #'mapcar fn lsts)))

(defun rmapcar (fn &rest args)
    (if (some #'atom args)
        (apply fn args)
        (apply #'mapcar
               #'(lambda (&rest args)
                    (apply #'rmapcar fn args))
               args)))

(defun compose (&rest fns)
    (if fns
        (let ((fn1 (car (last fns)))
              (fns (butlast fns)))
            #'(lambda (&rest args)
                (reduce #'funcall fns :from-end t :initial-value (apply fn1 args))))
        #'identity))

