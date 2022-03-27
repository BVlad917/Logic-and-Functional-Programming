; 8. Return the list of nodes of a tree of type (2) accessed inorder

; Tree of type (2):
;     (A (B) (C (D) (E)))
;           < = >
;             A
;         B       C
;               D   E

; Observation: Using the type (2) representation of a tree, each list has at most 3 elements: the root of the subtree, the list representing the left subtree, and the list representing the right subtree

; INORDER TRAVERSAL: - Traverse the left subtree (if it exists)
;                    - Visit the root of the tree
;                    - Traverse the right subtree (if it exists)



(defun firstElem (l)
    (car l)
)

; tests:  (firstElem '(1 2 3))     => 1
;         (firstElem '())          => NIL

(defun secondElem (l)
    (car (cdr l))
)

; tests:  (secondElem '(1 2 3))    => 2
;         (secondElem '(1))        => NIL

(defun thirdElem (l)
    (car (cdr (cdr l)))
)

; tests:  (thirdElem '(1 2 3))   => 3
;         (thirdElem '(1 2))     => NIL


; secondFirstThirdOrder(l1 l2 l3) = { [], if l1, l2, and l3 are NIL
;                                   { [ l1 ], if l2 NIL and l3 NIL
;                                   { [ l2 l1 ], if l2 not NIL and l3 NIL
;                                   { [ l1 l3 ], if l2 NIL and l3 not NIL
;                                   { [ l2 l1 l3 ], otherwise

(defun secondFirstThirdOrder (l)
    (cond
        ((null l) nil)
        ((and (null (secondElem l)) (null (thirdElem l))) (list (firstElem l)))
        ((null (thirdElem l)) (myAppend (list (secondElem l)) (list (firstElem l))))
        ((null (secondElem l)) (myAppend (list (firstElem l)) (list (thirdElem l))))
        (t (myAppend (myAppend (list (secondElem l)) (list (firstElem l))) (list (thirdElem l))))
    )
)

; tests: (secondFirstThirdOrder '(1 2 3))         => (2 1 3)
;        (secondFirstThirdOrder '(1 2))           => (2 1)
;        (secondFirstThirdOrder '(1 NIL 3))       => (1 3)
;        (secondFirstThirdOrder '(1 2 NIL))       => (2 1)
;        (secondFirstThirdOrder '(1))             => (1)
;        (secondFirstThirdOrder '())              => (NIL)
;         => ((2) (1) (3))




; listLength(l1 l2 ... ln) = { 0, if n = 0
;                            { 1 + listLength(l2 ... ln), otherwise

;(defun listLength (l)
;    (cond
;        ((null l) 0)
;        (t (+ 1 (listLength (cdr l))))
;    )
;)

; tests: (listLength '(1 2 3))    => 3
;        (listLength '(4))        => 1
;        (listLength '())         => 0



; myAppend(l1 l2 ... ln, e) = { {e}, if n = 0 AND e atom
;                             { e, if n = 0 AND e list
;                             { l1 U myAppend(l2 ... ln, e), otherwise

(defun myAppend (l e)
    (cond
        ((and (null l) (atom e)) (list e))
        ((and (null l) (listp e)) e)
        (t (cons (car l) (myAppend (cdr l) e)))
    )
)

; tests: (myAppend '(1 2 3) 4)      => (1 2 3 4)
;        (myAppend '(1) 2)          => (1 2)
;        (myAppend '() 1)           => (1)
;        (myAppend '(1 2 3) '(4 5)) => (1 2 3 4 5)



; inorder(l) = { {l}, if l is an atom
;              { inorder(l2) U inorder(l1) U inorder(l3), if l = l1 l2 l3 list

(defun inorder (l)
    (cond
        ((atom l) (list l))
        (t (apply #'append (mapcar #'inorder (secondFirstThirdOrder l))))
    )
)

; tests: (inorder '(A (B) (C (D) (E))))    => (B A D C E)
;        (inorder '(1 (2 (4) (5)) (3)))    => (4 2 5 1 3)

;          1
;     2       3
;   4    5

;        (inorder '(1 (2 (3 (4)))))        => (4 3 2 1)

;           1
;         2
;       3
;     4


;        (inorder '(1 () (2 () (3 () (4)))))  => (1 2 3 4)

;      1
;        2
;          3
;            4

