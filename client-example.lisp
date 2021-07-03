(load "client-server.lisp")

(client-run 
  #'(lambda (stream)
    (write-line "Hello!" stream))
  #'(lambda (stream)
    (format t "Server message: ~A~%" (read-line stream))))

