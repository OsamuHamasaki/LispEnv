(require 'sb-bsd-sockets)
(defparameter *localhost-address* '(127 0 0 1))
(defparameter *port* 8080)

(defun make-server (address port)
  (let ((server (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address server) t)
    (sb-bsd-sockets:socket-bind server address port)
    (sb-bsd-sockets:socket-listen server 1)
    server))

(defun recieve-request (server fn)
  (let ((connection (sb-bsd-sockets:socket-accept server)))
    (unwind-protect
      (let ((stream (sb-bsd-sockets:socket-make-stream connection :input t :output t)))
        (funcall fn stream)
        (finish-output stream))
      (sb-bsd-sockets:socket-close connection))))

(defun server-loop (server fn)
  (recieve-request server fn)
  (server-loop server fn))

(defun server-start (fn &optional (address *localhost-address*) (port *port*))
  (let ((server (make-server address port)))
    (unwind-protect
      (server-loop server fn)
      (progn
        (format t "~&Closing listen socket~%")
        (sb-bsd-sockets:socket-close server)))))

(defun make-client (address port)
  (let ((client (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (sb-bsd-sockets:socket-connect client address port)
    client))

(defun send-request (client reqfn resfn)
  (let ((stream (sb-bsd-sockets:socket-make-stream client :input t :output t)))
    (funcall reqfn stream)
    (finish-output stream)
    (funcall resfn stream)))

(defun client-run (reqfn resfn &optional (address *localhost-address*) (port *port*))
  (let ((client (make-client address port)))
    (unwind-protect
      (send-request client reqfn resfn)
      (sb-bsd-sockets:socket-close client))))

