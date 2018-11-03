(load "graph") ;; loads external files

(defparameter *congestion-cty-nodes* nil)
(defparameter *congestion-cty-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *worm-num* 3)
(defparameter *cop-odds* 15)

;; Generates a random number from 1 to 30
(defun random-node ()
  (1+ (random *node-num*))) ;; Generates a random number up to the parameter (non-inclusive)

;; Creates edge pairs as bi-directional directed edges
(defun edge-pair (a b)
  (unless (eql a b) ;; only if the parameters aren't equal. Returns NIL if not.
    (list (cons a b) (cons b a)))) ;; Generates a list of dotted pair lists

;; Create the list of edges to represent the road system
(defun make-edge-list ()
  (apply #'append (loop repeat *edge-num* ;; loop parameter-times
                        collect (edge-pair (random-node) (random-node))))) ;; collect returns the values provided on each iteration

;; Get edges in edge-list that start from the node
(defun direct-edges (node edge-list)
  (remove-if-not (lambda (x) ;; filter with negation of the predicate given
                   (eql (car x) node))
                 edge-list))

;; Get connected nodes from the node
;; Visits the edges of the node by mapc-ing the direct-edges list
(defun get-connected (node edge-list)
  (let ((visited nil))
    (labels ((traverse (node) ;; labels allow the functions in the scope reference each other
               (unless (member node visited) ;; member checks wether an element exists in the list
                 (push node visited) ;; prepend anything to visited list
                 (mapc (lambda (edge) ;; map the function to the list
                         (traverse (cdr edge)))
                       (direct-edges node edge-list)))))
      (traverse node))
    visited))

;; Find the unconnected nodes
(defun find-islands (nodes edge-list)
  (let ((islands nil))
    (labels ((find-island (nodes)
               (let* ((connected (get-connected (car nodes) edge-list)) ;; gets the connected edges from the head of the list
                      (unconnected (set-difference nodes connected))) ;; set-difference gets elements present in the first list but not the second.
                 (push connected islands) ;; prepend the connected edges to islands
                 (when unconnected ;; find island in unconnected nodes too
                   (find-island unconnected)))))
      (find-island nodes))
    islands))

;; Connect the islands
(defun connect-with-bridges (islands)
  (when (cdr islands) ;; only when island has a tail, i.e. land masses
    (append (edge-pair (caar islands) (caadr islands)) ;; connect them with edge-pair
            (connect-with-bridges (cdr islands))))) ;; append concatenates copies. Do this recursively

;; Append connected islands to the original edge-list
(defun connect-all-islands (nodes edge-list)
  (append (connect-with-bridges (find-islands nodes edge-list)) edge-list))

(defun make-city-edges ()
  (let* ((nodes (loop for i from 1 to *node-num* ;; let* allows shadowing variablesand reference previously defined variables. loop using -for- syntax
                      collect i)) ;; collect the counter each iteration
         (edge-list (connect-all-islands nodes (make-edge-list))) ;; create fully connected edge-list
         (cops (remove-if-not (lambda (x) ;; lambda goodness
                                (zerop (random *cop-odds*))) ;; zero predicate
                              edge-list))) ;; edge list containing cops
    (add-cops (edges-to-alist edge-list) cops))) ;; add cops in the a-list

;; Converts the list of edges to an association list
(defun edges-to-alist (edge-list)
  (mapcar (lambda (node1) ;; applies function to first element of each list
            (cons node1
                  (mapcar (lambda (edge)
                            (list (cdr edge)))
                          (remove-duplicates (direct-edges node1 edge-list)
                                             :test #'equal)))) ;; especify the test function
          (remove-duplicates (mapcar #'car edge-list)))) ;; by default is uses the -eql- function

;; Add COPS to the alist
(defun add-cops (edge-alist edges-with-cops)
  (mapcar (lambda (x)
            (let ((node1 (car x))
                  (node1-edges (cdr x)))
              (cons node1
                    (mapcar (lambda (edge)
                              (let ((node2 (car edge)))
                                (if (intersection (edge-pair node1 node2)
                                                  edges-with-cops
                                                  :test #'equal) ;; especify the test function
                                    (list node2 'cops)
                                    edge)))
                            node1-edges))))
          edge-alist))

;; Get the neighbors of some node
(defun neighbors (node edge-alist)
  (mapcar #'car (cdr (assoc node edge-alist))))

;; Neightbors within one unit of distance
(defun within-one (a b edge-alist)
  (member b (neighbors a edge-alist)))

;; Neightbors within two unit of distance
(defun within-two (a b edge-alist)
  (or (within-one a b edge-alist)
      (some (lambda (x)
              (within-one x b edge-alist))
            (neighbors a edge-alist))))

;; Make the final map of the city
(defun make-city-nodes (edge-alist)
  (let ((wumpus (random-node))
        (glow-worms (loop for i below *worm-num* ;;generate random places for glow-worms
                          collect (random-node))))
    (loop for n from 1 to *node-num*
          collect (append (list n) ;; add signals according to objects in the map
                          (cond ((eql n wumpus) '(wumpus))
                                ((within-two n wumpus edge-alist) '(blood!)))
                          (cond ((member n glow-worms)
                                 '(glow-worm))
                                ((some (lambda (worm)
                                         (within-one n worm edge-alist))
                                       glow-worms)
                                 '(lights!)))
                          (when (some #'cdr (cdr (assoc n edge-alist)))
                            '(sirens!))))))

;; Find an empty node to put the player on
(defun find-empty-node ()
  (let ((x (random-node)))
    (if (cdr (assoc x *congestion-city-nodes*))
        (find-empty-node)
        x)))

;; Draw the city via GraphViz
(defun draw-city ()
  (ugraph->png "city" *congestion-city-nodes* *congestion-city-edges*))

;; Mark traversed and unvisited nodes
(defun known-city-nodes ()
  (mapcar (lambda (node)
            (if (member node *visited-nodes*)
                (let ((n (assoc node *congestion-city-nodes*)))
                  (if (eql node *player-pos*)
                      (append n '(*))
                      n))
                (list node '?)))
          (remove-duplicates
           (append *visited-nodes*
                   (mapcan (lambda (node) ;; mapcar that assumes list results and append them
                             (mapcar #'car
                                     (cdr (assoc node
                                                 *congestion-city-edges*))))
                           *visited-nodes*)))))
;; Alist stripped of any unvisited cop sirens
(defun known-city-edges ()
  (mapcar (lambda (node)
            (cons node (mapcar (lambda (x)
                                 (if (member (car x) *visited-nodes*)
                                     x
                                     (list (car x))))
                               (cdr (assoc node *congestion-city-edges*)))))
          *visited-nodes*))

;; Draw known city
(defun draw-known-city ()
  (ugraph->png "known-city" (known-city-nodes) (known-city-edges)))

;; Create a new game
(defun new-game ()
  (setf *congestion-city-edges* (make-city-edges))
  (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (setf *player-pos* (find-empty-node))
  (setf *visited-nodes* (list *player-pos*))
  (draw-city)
  (draw-known-city))

(defun walk (pos)
  (handle-direction pos nil))

(defun charge (pos)
  (handle-direction pos t))

;; Check if a move is legal
(defun handle-direction (pos charging)
  (let ((edge (assoc pos ;; get legal directions
                     (cdr (assoc *player-pos* *congestion-city-edges*)))))
    (if edge
        (handle-new-place edge pos charging)
        (princ "That location does not exist!"))))

;; Handle what happens after arriving to a new position
(defun handle-new-place (edge pos charging)
  (let* ((node (assoc pos *congestion-city-nodes*)) ;; get the node in the given position
         (has-worm (and (member 'glow-worm node) ;; check if position has a worm
                        (not (member pos *visited-nodes*)))))
    (pushnew pos *visited-nodes*) ;; pushnew prepends if the elemen doesn't exist yet
    (setf *player-pos* pos) ;; setf changes a value for another one, i.e. update the player position
    (draw-known-city)
    (cond ((member 'cops edge) (princ "You ran into the cops. Game Over."))
          ((member 'wumpus node) (if charging
                                     (princ "You found the Wumpus!")
                                     (princ "You ran into the Wumpus")))
          (charging (princ "You wasted your last bullet. Game Over."))
          (has-worm (let ((new-pos (random-node)))
                      (princ "You ran into a Glow Worm Gang! You're now at ")
                      (princ new-pos)
                      (handle-new-place nil new-pos nil))))))
