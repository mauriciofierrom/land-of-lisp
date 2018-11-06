(defun add (a b)
  (let ((x (+ a b)))
    (format t "The sum is ~a" x)
    x))

;; defmacro: call the macro expander
;; var: a variable
;; val: the value of the variable
;; &body: get the rest of the parameters as a list
;; Quasiquoting

(defmacro let1 (var val &body body)
  `(let ((,var ,val))
     ,@body))

(let1 foo (+ 2 5)
      (princ "Lisp is awesome!")
      (* foo foo))

;; Bugged
(defmacro split (val yes no)
  `(if ,val
       (let ((head (car ,val))
             (tail (cdr ,val)))
         ,yes)
       ,no))


(macroexpand '(split (progn (princ "Lisp rocks!") ;; shows how the macros are tranlated to code
                            '(2 3))
               (format t "This can be split into ~a and ~a." head tail)
               (format t "This cannot be split.")))

;; Still bugged - x gets captured
(defmacro split (val yes no)
  `(let1 x ,val
         (if x
             (let ((head (car x))
                   (tail (cdr x)))
               ,yes)
             ,no)))

;; Using gensym we avoid the possibility
;; of variables getting shadowed
;; Anaphoric macro, because it intentionally captures outside
;; behavior (head, tail)
(defmacro split (val yes no)
  (let1 g (gensym) ;; generates random symbol e.g. #:G8765
        `(let1 ,g ,val
               (if ,g
                   (let ((head (car ,g))
                         (tail (cdr ,g)))
                     ,yes)
                   ,no))))

;; get the length of a list. Tail recursive
(defun my-length (lst)
  (labels ((f (lst acc)
             (split lst
                    (f tail (1+ acc))
                    acc)))
    (f lst 0)))

(defun my-length (lst)
  (labels ((f (lst acc)
             (split lst
                    (f tail (1+ acc))
                    acc)))
    (f lst 0)))

;; Get pairs from a list
(defun pairs (lst)
  (labels ((f (lst acc)
             (split lst
                    (if tail
                        (f (cdr tail) (cons (cons head (car tail)) acc))
                        (reverse acc))
                    (reverse acc))))
    (f lst nil)))

(defmacro recurse (vars &body body)
  (let1 p (pairs vars)
        `(labels ((self ,(mapcar #'car p)
                    ,@body))
           (self ,@(mapcar #'cdr p)))))

(defun my-length (lst)
  (recurse (lst lst
                acc 0)
           (split lst
                  (f tail (1+ acc))
                  acc)))

;; metaprogramming tradeoffs
