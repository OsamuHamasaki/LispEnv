;;;;; Sudoku Solver

;;;; Cell
(defstruct (cell (:print-object print-cell))
    line
    row
    (groups ()) 
    (candidate (copy-list '(1 2 3 4 5 6 7 8 9))))

(defun print-cell (c stream)
    (format stream "l<#~A>r<#~A>#<~A>" (cell-line c) (cell-row c) (cell-candidate c)))

(defun cell-addGroup (c g)
    (push g (cell-groups c))
    c)

(defun cell-remove (c x)
    (setf (cell-candidate c) (remove x (cell-candidate c)))
    c)

;;;; CellGroup: Group of 9 cells like 1 line, 1 row, 1 blk.


;;;; Solver
(defstruct solver
    (lines (make-array 9 :initial-element (make-array 9 :initial-element nil)))
    (rows (make-array 9 :initial-element (make-array 9 :initial-element nil)))
    (blks (make-array 9 :initial-element (make-array 9 :initial-element nil)))
    (cells (make-array '(9 9) :initial-element nil)))

(defun blkindex (line row)
    (+ (* (floor line 3) 3) (floor row 3)))

(defun inblkindex (line row)
    (+ (* (rem line 3) 3) (rem row 3)))

(defun solver-initialize (solver)
    (dotimes (x 9)
        (setf (svref (solver-lines solver) x) (make-array 9 :initial-element nil))
        (setf (svref (solver-rows solver) x) (make-array 9 :initial-element nil))
        (setf (svref (solver-blks solver) x) (make-array 9 :initial-element nil)))

    (dotimes (x 81)
        (multiple-value-bind (line row) (floor x 9)
            (let ((groupl (svref (solver-lines solver) line))
                  (groupr (svref (solver-rows solver) row))
                  (groupb (svref (solver-blks solver) (blkindex line row)))
                  (cell (make-cell :line line :row row)))
                (setf (svref groupl row) cell)
                (setf (svref groupr line) cell)
                (setf (svref groupb (inblkindex line row)) cell)
                (setf (aref (solver-cells solver) line row) cell)
                (cell-addGroup cell groupl)
                (cell-addGroup cell groupr)
                (cell-addGroup cell groupb)))))
