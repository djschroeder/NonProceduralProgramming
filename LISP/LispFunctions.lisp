;QUESTION 1

;Counts the number of atomic elements in a list L
;Second condition originally written as: ((atom (car L)) (+ 1 (xcount(cdr L))))
;Checked original solution against chatgpt and it suggested ((atom L) 1) which is simpler and produces same result.

;Test Input: (xcount '(a (a b) ((c) a)))
;Expected Output: 5

(defun xcount(L)
    (cond 
        ((null L) 0)
        ((atom L) 1)
        (t (+ (xcount(car L)) (xcount(cdr L))))))


;QUESTION 2

;Removes duplicate values from a list X of atoms while preserving the order
;If list X is empty then returns nil as there are no duplicates in an empty list
;else if a duplicate of the first element is found then skip that element and recurse on rest on list
;else if a duplicate of the first element is not found then construct a list with that element in it

;Test Input: (remove-duplicate '(a b c a d b))
;Expected Output: (a b c d)

(defun remove-duplicate(X)
    (cond 
        ((null X) nil)
        ((duplicate (car X) (cdr X)) (remove-duplicate (cdr X)))
        (t (cons (car X) (remove-duplicate (cdr X))))))

;Iteratively compares and atom x against each element in a list L
;If return is nil, no duplicate for the atom x was found
;Otherwise if a duplicate was found, returns true
(defun duplicate(x L)
    (cond
        ((null L) nil)
        ((equal x (car L)) t)
        (t (duplicate x (cdr L)))))


;Question 3a

;Alternatingly mixes elements of L1 and L2 into a single list
;If L1 is empty, returns the remaining values of L2 and visa versa
;Otherwise, combines the first two elements of the list with the assumed already mixed remaining elements of the list

;Test Input: (mix '((a) (b c)) '(d e f g h)) 
;Expected Output: (d (a) e (b c) f g h)

(defun mix(L1 L2)
    (cond
        ((null L1) L2)
        ((null L2) L1)
        (t (append (cons (car L2) (cons (car L1) ())) (mix (cdr L1) (cdr L2))))))


;Question 3b

;Splits a list of two sublists
;First sublist contains elements at even index's of L
;Second sublist contains elements at odd index's of L
;Condition 1 returns a list of two emply lists if L is empty
;Condition 2 and 3 are mostly the same, except:
;Condition 2 handles lists of odd sizes (by removing portion of condition 3 that adds an even element to the even sublist)
;Condition 3 handles lists of even sizes

;Test Input: (split '((a) (b c) (d e f) g h)) 
;Expected Output: (((b c) g) ((a) (d e f) h))

(defun split(L)
    (cond 
        ((null L) (list nil nil))
        ((null (cadr L))
            (list (car (split (cddr L)))
                  (cons (car L) (cadr (split (cddr L))))))
        (t  (list (cons (cadr L) (car (split (cddr L))))
                  (cons (car L) (cadr (split (cddr L))))))))


;Question 4

;Creates a list of all subsets of L whose size is S
;appends two lists:
;       First list is all sublists of length S which dont include car L
;       Second list is all sublists of length S - 1 which include car L by adding it to accumulator

;Test Input: (subsets '(a b c) 2)
;Expected Output: ((a b) (a c) (b c)

(defun subsets(L S)
    (subsetsAcc L S nil))

(defun subsetsAcc(L S acc)
    (cond
        ((= S 0) (list acc))
        ((or (null L) (< S 0)) nil)
        (t (append
            (subsetsAcc (cdr L) S acc)
            (subsetsAcc (cdr L) (- S 1) (cons (car L) acc))))))


;Question 5

;Replaces each occurrence of expressing E1 in list L with expression E2. 
;If the first element equals E1 then add E2 the the remainder of the list
;Otherwise if the first element is an atom then there are no more nested lists so add the first element back to the front of the remainder
;Lastly, if it reaches the end this means that the first element is nested so recurse the function over the first element and add it to the recurse of the remainder

;Test Input: (substitute-exp '(c e) '(1 2) '(a ((c e) 3) (b a) (c e) (a)))
;Expected Output: (a ((1 2) 3) (b a) (1 2) (a))

(defun substitute-exp(E1 E2 L)
    (cond
        ((null L) nil)
        ((equal (car L) E1) (cons E2 (substitute-exp E1 E2 (cdr L))))
        ((atom (car L)) (cons (car L) (substitute-exp E1 E2 (cdr L))))
        (t (cons (substitute-exp E1 E2 (car L)) (substitute-exp E1 E2 (cdr L))))))


;Question 6

;Counts the number of distinct atoms in an un-nested list L
;Duplicate function is defined in Question 2 above
;If first element has a duplicate then ignores and moves on to remainder of list
;else first element has no duplicate and +1 added to count

;Test Input: (my-count '(a b a c d c))
;Expected Output: 4

(defun my-count (L)
    (cond
        ((null L) 0)
        ((duplicate (car L) (cdr L)) (my-count (cdr L)))
        (t (+ 1 (my-count (cdr L))))))


;Question 7a

;L is a list of pairs representing linkage where 
;a web page A containing a link to another one B is represented by a pair, (A  B)
;returns a list of all web pages that can be reached from x

;Test Input: (reached 'google '( (google shopify) (google aircanada) (amazon aircanada) (aircanada delta) (google google) )) 
;Expected Output: (SHOPIFY AIRCANADA DELTA)

(defun reached(x L)
    (removeMatching x (reaches x L)))

;Generates a list of all elements matching query and may contain unwanted elements which match the query
(defun reaches(x L)
    (cond
        ((null L) (list x))
        ;Not equal prevents infinite recursion when first element = second element
        ((and (equal x (caar L)) (not (equal x (cadar L)))) 
            ;If link found, then also find all links from the one just found and then
            ;append to all links found in remainder of list matching original query 
            (append (reaches (cadar L) L) (reaches x (cdr L))))
        ;If first element doesnt match query, recurse on remainder of the list
        (t (reaches x (cdr L)))))

;removes all elements of given list L that matches element x
(defun removeMatching(x L)
    (cond
        ((null L) nil)
        ((equal x (car L)) (removeMatching x (cdr L)))
        (t (cons (car L) (removeMatching x (cdr L))))))


;Question 7b

;Provides a sorted ranking of a list S of webpages according to how many times each
;element of S was referenced in a list L of pairs (A, B) where B is referenced by A

;Test Input: (rank '(google shopify aircanada amazon delta) '((google shopify) (google aircanada) (amazon aircanada) (aircanada delta) (google google)))
;Expected Output: (AIRCANADA SHOPIFY DELTA GOOGLE AMAZON)

(defun rank(S L)
   (removeRanks (mySort (getRankings S (remove-duplicate L)))) ;remove-duplicate defined in question 1
)

;Returns a list of pairs where the first element in each pair cooresponds to 
;an element of S and the second element is an integer representing its rank.
(defun getRankings(S L)
    (if (null S) 
        nil
        (cons
            (list (car S) (elementLinkCount (car S) L)) 
            (getRankings (cdr S) L))))

;Counts how many times a given atom x equals B in each (A, B) element of L
;Does not count elements where A = B
(defun elementLinkCount(x L)
    (cond
        ((null L) 0)
        ((and (equal x (cadar L)) (not (equal x (caar L))))
            (+ 1 (elementLinkCount x (cdr L))))
        (t (elementLinkCount x (cdr L)))))

;Sorts a given list L of pairs by their rankings given by the second element in each pair
(defun mySort (L)
            (sort L 'greaterThan))

;Custom sort function to provide to built-in sort function
(defun greaterThan (L1 L2)
    (> (cadr L1) (cadr L2)))

;Given a list L of pairs, constructs a new list containing the first element of every pair
(defun removeRanks(L)
    (if (null L)
        nil
        (cons (caar L) (removeRanks (cdr L)))))
