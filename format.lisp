(load "loop")

;; format allows advance printing
(format t "Add onion rings for only ~$ dollars more!" 1.5)

;; Destination parameter can be
;; nil: create string
;; t: print to console
;; stream: output to stream

(princ (reverse
        (format nil "Add onion rings for only ~$ dollars more!" 1.5)))

;; Control string parameter can be a variety of formats
;; ~$ is for monetary floating-point values
(format t "I am printing ~s in the middle of this sentence." "foo") ;; includes delimiters
(format t "I am printing ~a in the middle of this sentence." "foo") ;; removes delimiters
(format t "I am printing ~10a in the middle of this sentence." "foo") ;; removes delimiters an adds 10 spaces to the right
(format t "I am printing ~10@a in the middle of this sentence." "foo") ;; removes delimiters an adds 10 spaces to the left
(format t "I am printing ~10,3a within ten (or more) spaces of room." "foo") ;; extra parameters to control string parameters are provided with comma. Rarely used.

;; More examples for numbers


(progn (prin 22)
       (terpri)) ;; terminate current line and start a new one

;; fresh-line will only work for one line unless there's output before

;; like terpri
(progn (format t "this is on one line ~%")
       (format t "~%this is on another line"))

;; like fresh-line
(progn (format t "this is on one line ~&")
       (format t "~&this is on another line"))

;; they also take parameters
(format t "this will print ~5%on two lines spread far apart")

;; justification can also be controlled

;; The control sequences can be a DSL
(defparameter *animals* (loop repeat 10 collect (random-animal)))
(format t "~{I see a ~a! ~}" *animals*) ;; loops through the list to print the values

;; lotsa crazy stuff can be done, check references
