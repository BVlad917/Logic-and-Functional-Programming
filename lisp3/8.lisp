; Write a function to determine the number of nodes on the level k from a n-tree represented as follows:
; (root list_nodes_subtree1 ... list_nodes_subtreen)
; Eg: tree is (a (b (c)) (d) (e (f))) and k=1 => 3 nodes

;        a         level 0
;    b   d   e     level 1
;    c       f     level 2


; countNodes(tree, level, current_level) = { 1, if tree atom AND current_level = level
;                                          { 0, if tree atom AND current_level != level
;                                          { countNodes(tree_1, level, current_level + 1) + countNodes(tree_2, level, current_level + 1) + ... + countNodes(tree_n, level, current_level + 1), if tree = [tree_1 tree_2 ... tree_n] list


(defun countNodes (tree level current_level)
    (cond
        ((and (atom tree ) (= current_level level)) 1)
        ((atom tree) 0)
        (t (apply #'+ (mapcar #'(lambda (a) (countNodes a level (+ current_level 1))) tree)))
    )
)


(defun countNodesMain (tree k)
    (countNodes tree k -1)    ; initialize current_level = -1, take into account the initial list
)


; tests:  (countNodesMain '(a (b (c)) (d) (e (f))) 1)    => 3
;         (countNodesMain '(a (b (c)) (d) (e (f))) 2)    => 2
;         (countNodesMain '(a (b (c)) (d) (e (f))) 0)    => 1
;         (countNodesMain '(a (b (c)) (d) (e (f))) 3)    => 0


;               A                    level 0
;    B     C         D       E       level 1
;  F  G   I J      K L M    N O      level 2
;  P  Q   R S      T U V    W X      level 3

; tree representation: (A (B (F (P)) (G (Q))) (C (I (R)) (J (S))) (D (K (T)) (L (U)) (M (V))) (E (N (W)) (O (X))))

; tests: (countNodesMain '(A (B (F (P)) (G (Q))) (C (I (R)) (J (S))) (D (K (T)) (L (U)) (M (V))) (E (N (W)) (O (X)))) 0)    => 1
;        (countNodesMain '(A (B (F (P)) (G (Q))) (C (I (R)) (J (S))) (D (K (T)) (L (U)) (M (V))) (E (N (W)) (O (X)))) 1)    => 4
;        (countNodesMain '(A (B (F (P)) (G (Q))) (C (I (R)) (J (S))) (D (K (T)) (L (U)) (M (V))) (E (N (W)) (O (X)))) 2)    => 9
;        (countNodesMain '(A (B (F (P)) (G (Q))) (C (I (R)) (J (S))) (D (K (T)) (L (U)) (M (V))) (E (N (W)) (O (X)))) 3)    => 9
;        (countNodesMain '(A (B (F (P)) (G (Q))) (C (I (R)) (J (S))) (D (K (T)) (L (U)) (M (V))) (E (N (W)) (O (X)))) 4)    => 0
