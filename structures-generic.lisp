;; Create array with 3 NIL slots
(defparameter x (make-array 3))

;; Get value in position 1
(aref x 1)

;; Lisp is said to support generic setters
;; I.e. getters can be use to create setters
;; E.g. setf's first arg is a generalized reference
(set foo (make-array 4))
(setf (aref foo 2) '(x y z)) ;; set the '(x y z) list in the 2 index of the array
(setf (car (aref foo 2)) (make-hash-table)) ;; replace the first element of the array for a hash-table
(setf (gethash 'zoink (car (aref foo 2))) 5) ;; insert into the the hash table the value (ZOINK . 5)

;; Clisp can return multiple values
round(2.4) ;; returns the rounded value 2 and the reminder

;; Function that returns two values
(defun foo()
  (values 1 2))

;; Operations are typically done on the first return value
((+) (foo) 5) ;; adds 1 and 5

;; To work with two return values, bind them
(multiple-value-bind (a b) (foo)
  (* a b))

;; Modern Lisps (like Clojure) do not support multiple values

;; Array and Hashtable require only constant time to fetch an element
;; Still, they have downsides. Use them carefully when performance is needed.

;; Convert alist to hash table
(defun hash-edges (edge-list)
  (let ((tab (make-hash-table))) ;; create hash table
    (mapc (lambda (x)
            (let ((node (car x)))
              (push (cdr x) (gethash node tab)))) ;; push can insert into the desidred place via generalized references / "general variables system"
          edge-list)
    tab)) ;; return populated table

;; Get connected hash
(defun get-connected-hash (node edge-tab)
  (let ((visited (make-hash-table)))
    (labels ((traverse (node)
               (unless (gethash node visited)
                 (setf (gethash node visited) t)
                 (mapc (lambda (edge)
                         (traverse edge))
                       (gethash node edge-tab)))))
      (traverse node))
    visited))

;; Structures are objects with properties
;; Properties are also called -slots-
(defstruct person
  name
  age
  waist-size
  favorite-color)

;; make-whatever is used to create a structure
;; The slots can be set using key parameters syntax
(defparameter *bob* (make-person :name "Bob"
                                 :age 35
                                 :waist-size 32
                                 :favorite-color "blue"))

;; Getters for slots are used with whatever-slot
(person-age *bob*)

;; setf can be used to update a slot
(setf (person-age *bob*) 36)

;; homoiconicity can be used to create a structure
(defparameter *that-guy* #S(person :name "Bob" :age 35 :waist-size 32
                                   :favorite-color "blue"))

;; Higher-order functionl programming & domain-specific language programming over OOP (?)
;; Common Lisp Object System (CLOS) to have a more OOP approach.
;; CLOS has been used to study OOP ideas.


;; Sequence functions work on sequencing objects: list, array & strings
(length '(a b c)) ;; list-length is more performant
(length "blub")
(length (make-array 5))

;; fold1' in Haskell (basically)
(reduce #'+ '(3 4 6 5 2))

;; foldr, :initial-value being the base case
(reduce (lambda (best item)
          (if (and (evenp item) (> item best))
              item
              best))
        '(7 4 6 5 2)
        :initial-value 0)

;; Retrieve a subsequence
(subseq "america" 2 6)

;; Sort, via a function
(sort '(5 8 2 4 9 3 6) #'<)

;; Type Predicates
(numberp 5)

;; type dispatching: different versions of a function based on argument types
;; defstruct + defmethod = simple object system
(defmethod add ((a number) (b number))
  (+ a b))

(defmethod add ((a list) (b list))
  (append a b))
