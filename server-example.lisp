(load "client-server.lisp")

(server-start
  #'(lambda (stream)
    (format t "Client Message: ~A~%" (read-line stream))
    (write-line "Hello, Client!" stream)))
