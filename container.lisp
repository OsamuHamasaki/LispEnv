;;;; Containers

;;;;

(defstruct rbuf
  maxSize
  (rindex 0)
  (windex 0)
  buffer
  )

(defun new-rbuf (size)
  (make-rbuf :buffer (make-array size :initial-element nil) :maxSize size))

(defmacro rbuf-index-update (buf index)
  `(let ((result (,index ,buf)))
     (incf (,index ,buf))
     (if (eql (,index ,buf) (rbuf-maxSize ,buf))
         (setf (,index ,buf) 0))
     result))

(defun rbuf-rindex-update (buf)
  (rbuf-index-update buf rbuf-rindex))

(defun rbuf-windex-update (buf)
  (rbuf-index-update buf rbuf-windex))

(defun rbuf-empty? (buf)
  (eql (rbuf-rindex buf) (rbuf-windex buf)))

(defun rbuf-ref (buf n)
  (svref (rbuf-buffer buf) n))

(defun (setf rbuf-ref) (val buf n)
  (setf (svref (rbuf-buffer buf) n) val))

(defun rbuf-get (buf)
  (if (rbuf-empty? buf)
      nil
      (rbuf-ref buf (rbuf-rindex-update buf))))

(defun rbuf-put (buf val)
  (setf (rbuf-ref buf (rbuf-windex-update buf)) val))


;;;; Double Linked List

;;; Double Linked List Node
(defstruct dlnode
  prev
  value
  next)

(defun dlnode-initialize (node)
  (setf (dlnode-prev node) node)
  (setf (dlnode-next node) node))

(defun dlnode-attachNext (node next-node)
  (setf (dlnode-prev next-node) node)
  (setf (dlnode-next next-node) (dlnode-next node))
  (setf (dlnode-prev (dlnode-next node)) next-node)
  (setf (dlnode-next node) next-node))

(defun dlnode-attachPrev (node prev-node)
  (setf (dlnode-next prev-node) node)
  (setf (dlnode-prev prev-node) (dlnode-prev node))
  (setf (dlnode-next (dlnode-prev node)) prev-node)
  (setf (dlnode-prev node) prev-node))

(defun dlnode-detach (node)
  (setf (dlnode-next (dlnode-prev node)) (dlnode-next node))
  (setf (dlnode-prev (dlnode-next node)) (dlnode-prev node)))

;;; Double Linked List
(defstruct (dlist (:print-function print-dlist)) 
  (root (make-dlnode)))

(defun print-dlist (lst stream depth)
  (declare (ignore depth))
  (format stream "#<DLIST ~A>" (dlist->list lst)))

(defun dlist-initialize (lst)
  (dlnode-initialize (dlist-root lst)))

(defun dlist->list (lst)
  (let ((root (dlist-root lst)))
    (labels ((dlist->l (node acc)
               (if (eql node root)
                   (reverse acc) 
                   (dlist->l (dlnode-next node)
                             (push (dlnode-value node) acc)))))
        (dlist->l (dlnode-next root) nil))))

(defun dlist-getFirst (lst)
  (let* ((root (dlist-root lst))
         (firstNode (dlnode-next root)))
    (if (eql firstNode root)
        nil
        (dlnode-value firstNode))))

(defun dlist-addFirst (lst value)
  (let ((root (dlist-root lst))
        (new-node (make-dlnode)))
    (setf (dlnode-value new-node) value)
    (dlnode-attachNext root new-node)))

(defun dlist-removeFirst (lst)
  (let* ((root (dlist-root lst))
         (first (dlnode-next root)))
   (dlnode-detach first)
   (dlnode-value first)))

(defun dlist-getLast (lst)
  (let* ((root (dlist-root lst))
         (lastNode (dlnode-prev root)))
    (if (eql lastNode root)
        nil
        (dlnode-value lastNode))))

(defun dlist-addLast (lst value)
  (let ((root (dlist-root lst))
        (new-node (make-dlnode)))
    (setf (dlnode-value new-node) value)
    (dlnode-attachPrev root new-node)))

(defun dlist-removeLast (lst)
  (let* ((root (dlist-root lst))
         (last (dlnode-prev root)))
   (dlnode-detach last)
   (dlnode-value last)))


