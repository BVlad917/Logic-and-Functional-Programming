; L1.
; 2)
; a) Write a function to return the product of two vectors

; dotProduct(l1l2...ln, p1p2...pn) = { 0, if n = 0
;                                    { (l1 * p1) + dotProduct(l2...ln, p2...pn), otherwise

; dotProductMain(l1l2...ln, p1p2...pm) = { dotProduct(l1l2...ln, p1p2...pm), if n = m and n > 0

; listLength(l1l2...ln) = { 0, if n = 0
;                         { 1 + listLength(l2...ln), otherwise

(defun listLength (l)
    (cond
        ((null l) 0)
        (t (+ 1 (listLength (cdr l))))
    )
)

; tests: (listLength '(1 2 3))      => 3
;        (listLength '(1))          => 1
;        (listLength '())           => 0


(defun dotProduct (l p)
    (cond
        ((null l) 0)
        (t (+ (* (car l) (car p)) (dotProduct (cdr l) (cdr p))))
    )
)

(defun dotProductMain (l p)
    (cond
        ((and (equal (listLength l) (listLength p)) (> (listLength l) 0)) (dotProduct l p))
    )
)

; tests: (dotProductMain '(1 2 3) '(2 2 2))      => 2 + 4 + 6 = 12
;        (dotProductMain '(1) '(10))             => 10
;        (dotProductMain '(1 2 3) '(2 2))        => the vectors should be of the same dimension
;        (dotProductMain '() '())                => the vectors should have a length > 0






; b) Write a function to return the depth of a list. Example: the depth of a linear list is 1.

; depth(l1l2...ln, current_depth, max_depth) = { max_depth, if n = 0
;                     { depth(l2...ln, current_depth, max_depth), if l1 atomic
;                     { depth(l2...ln, current_depth, max(current_depth, depth(l1, current_depth + 1, max(current_depth + 1, max_depth)))), if l1 is a list

; max(a, b) = { a, if a > b
;             { b, otherwise

(defun maxim (a b)
    (cond
        ((> a b) a)
        (t b)
    )
)

; test: (maxim 5 6)


(defun depth (l current_depth max_depth)
    (cond
        ((null l) max_depth)
        ((atom (car l)) (depth (cdr l) current_depth max_depth))
        (t (depth (cdr l) current_depth (maxim current_depth (depth (car l) (+ current_depth 1) (max (+ current_depth 1) max_depth)))))
    )
)

(defun depthMain (l)
    (cond
        ((listp l) (depth l 1 1))
    )
)

; tests: (depthMain '(1 2 3))                                                             => 1
;        (depthMain '(1 2 (4 5) 3))                                                       => 2
;        (depthMain '(1 2 (4 (6 7) 5) 3))                                                 => 3
;        (depthMain '(1 2 (4 (6 (8 9) 7) 5) 3))                                           => 4
;        (depthMain '(1 2 (4 (6 (8 9) 7) 5) (11 12) (13 14) 3))                           => 4
;        (depthMain '(1 2 (4 (6 (8 9) 7) 5) (11 (15 16) 12) (13 14) 3))                   => 4
;        (depthMain '(1 2 (4 (6 (8 9) 7) 5) (11 (15 (17 (19 20) 18) 16) 12) (13 14) 3))   => 5




; c) Write a function to sort a linear list without keeping the double values.

; insertInSortedList(l1l2...ln, e) = { (e), if n = 0
;                                    { e U l1l2...ln, if e < l1
;                                    { l1l2...ln, if e = l1
;                                    { l1 U insertInSortedList(l2...ln, e), if e > l1

(defun insertInSortedList (l e)
    (cond
        ((null l) (list e))
        ((< e (car l)) (cons e l))
        ((equal e (car l)) l)
        (t (cons (car l) (insertInSortedList (cdr l) e)))
    )
)

; tests: (insertInSortedList '(2 4 6 8) 5)               => (2 4 5 6 8)
;        (insertInSortedList '(2 4 6 8) 1)               => (1 2 4 6 8)
;        (insertInSortedList '(2 4 6 8) 10)              => (2 4 6 8 10)
;        (insertInSortedList '(2) 10)                    => (2 10)
;        (insertInSortedList '() 10)                     => (10)



; sortWithoutDoubles(l1l2...ln) = { [], if n = 0
;                                 { insertInSortedList( sortWithoutDoubles(l2...ln), l1), otherwise

(defun sortWithoutDoubles (l)
    (cond
        ((null l) nil)
        (t (insertInSortedList (sortWithoutDoubles (cdr l)) (car l)))
    )
)

; tests: (sortWithoutDoubles '(5 2 9 19 3 1))                 => (1 2 3 5 9 19)
;        (sortWithoutDoubles '(2 5 1 2 9 19 3 19 1))          => (1 2 3 5 9 19)
;        (sortWithoutDoubles '(1))                            => (1)
;        (sortWithoutDoubles '())                             => ()




; d) Write a function to return the intersection of two sets.

; isSet(l1l2...ln, p1p2...pm) = { true, if n = 0
;                               { isSet(l2...ln, p1p2...pm), if nrOcc(p1p2...pm, l1) = 1
;                               { false, otherwise

; isSetMain(l1l2...ln) = { isSet(l1l2...ln, l1l2...ln)

; nrOcc(l1l2...ln, e) = { 0, if n = 0
;                       { 1 + nrOcc(l2...ln, e), if l1 = e
;                       { nrOcc(l2...ln, e), otherwise

(defun nrOcc (l e)
    (cond
        ((null l) 0)
        ((equal (car l) e) (+ 1 (nrOcc (cdr l) e)))
        (t (nrOcc (cdr l) e))
    )
)

; tests:  (nrOcc '(2 1 3 6 1) 1)     => 2
;         (nrOcc '(2 1 3 6) 1)       => 1


(defun isSet (l p)
    (cond
        ((null l) t)
        ((equal (nrOcc p (car l)) 1) (isSet (cdr l) p))
        (t nil)
    )
)

(defun isSetMain (l)
    (isSet l l)
)

; tests:  (isSetMain '(1 2 3 4 5))    => T
;         (isSetMain '(1 2 3 4 2 5))  => NIL





; intersect(l1l2...ln, p1p2...pm) = { [], if n = 0
;                                   { l1 U intersect(l2...ln, p1p2...pm), if nrOcc(p1p2...pm, l1) = 1
;                                   { intersect(l2...ln, p1p2...pm), otherwise

; intersectMain(l1l2...ln, p1p2...pm) = { intersect(l1l2...ln, p1p2...pm), if l set AND p set

(defun intersect (l p)
    (cond
        ((null l) nil)
        ((equal (nrOcc p (car l)) 1) (cons (car l) (intersect (cdr l) p)))
        (t (intersect (cdr l) p))
    )
)

(defun intersectMain (l p)
    (cond
        ((and (isSetMain l) (isSetMain p)) (intersect l p))
    )
)

; tests:  (intersectMain '(7 2 4 6 8 5 10) '(1 3 5 7 9))      => (7 5)
;         (intersectMain '(2) '(2))                           => (2)
;         (intersectMain '(2 4 6 8 10) '(1 3 5 7 9))          => NIL
