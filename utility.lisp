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

;;;; Macro Utilities from Paul Graham's ANSI Common Lisp

;;; for

(defmacro for (var start stop &body body)
 (let ((gstop (gensym)))
  `(do ((,var ,start (1+ ,var))
        (,gstop ,stop))
       ((> ,var ,gstop))
    ,@body)))

(defmacro in (obj &rest choices)
 (let ((insym (gensym)))
  `(let ((,insym ,obj))
    (or ,@(mapcar #'(lambda (c) `(eql ,insym ,c))
           choices)))))

(defmacro random-choice (&rest exprs)
 `(case (random ,(length exprs))
     `@(lst ((key -1))
         (mapcar #'(lambda (expr)
                    `(,(incf key) ,expr))
                 exprs))))

(defmacro avg (&rest args)
 `(/ (+ ,@args) ,(length args)))

(defmacro with-gensym (syms &body body)
 `(let ,(mapcar #'(lambda (s)
                    `(,s (gensym)))
                syms)
     ,@body))

(defmacro aif (test then &optional else)
 `(let ((it ,test))
     (if it ,then ,else)))

;;;; Function Utilities from Paul Graham's On Lisp

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

(defun runlen (x)
  (labels ((rln (x n lst acc)
             (if (null lst)
                 (nreverse (push (cons x n) acc))
                 (if (eql x (car lst))
                     (rln x (1+ n) (cdr lst) acc)
                     (rln (car lst) 1 (cdr lst) (push (cons x n) acc))))))
    (if (null x)
        x
        (rln (car x) 1 (cdr x) nil))))

(defun drun (pare)
  (labels ((drn (x n acc)
             (if (zerop n)
                 (nreverse acc)
                 (drn x (1- n) (push x acc)))))
    (drn (car pare) (cdr pare) nil)))

(defun derunlen (lst)
  (labels ((dernln (lst acc)
             (if (null lst)
                 acc
                 (dernln (cdr lst) (nconc acc (drun (car lst)))))))
    (dernln lst nil)))

