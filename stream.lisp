;; Stream Types:
;; Console
;; File
;; Socket
;; String - black sheep? String manipulation

;; Output stream: 1) Check validity 2) Push an item
;; Input stream: 1) Check validity 2) Pop an item

(output-stream-p *standard-output*) ;; check validity of output stream
(input-stream-p *standard-output*) ;; check validity of input stream

(with-open-file (my-stream "data.txt" :direction :output) ;; my-stream is the handle
                (print "my data" my-stream))
;; Safe resource allocation, the handler lives only within the scope of the with-open-file function

(let ((animal-noises '((dog . woof)
                       (cat . meow))))
  (with-open-file (my-stream "animal-noises.txt" :direction :output)
    (print animal-noises my-stream)))

;; (with-open-file (my-stream "data.txt" :direction :output :if-exists :error)
;;   (print "my data" my-stream))

(with-open-file (my-stream "data.txt" :direction :output
                                      :if-exists :supersede)
  (print "my superseded data" my-stream))

;; Server socket
(defparameter my-socket (socket-server 4321))
(defparameter my-stream (socket-accept my-socket))
(print "Yo Server!" my-stream)
(close my-stream)
(socket-server-close my-socket)

;; Client socket
(defparameter my-stream (socket-connect 4321 "127.0.0.1"))
(read my-stream)
(close my-stream)

;; String stream - debugging and creating complex strings efficiently
(defparameter foo (make-string-output-stream))
(princ "This goes into foo. " foo)
(princ "This also goes into foo. " foo)
(get-output-stream-string foo)

(with-output-to-string (*standard-output*)
  (princ "the sum of ")
  (princ 5)
  (princ " and ")
  (princ 2)
  (princ " is ")
  (princ (+ 2 5)))
