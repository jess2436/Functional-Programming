#| 
File: proj3.lisp
Authors: Jessica Chen & Shadiya Akhter
CSC 173 | George Ferguson | Project 3
Last modified: 10/21/24
|#

; LIST FUNCTIONS

; (.LENGTH L) - returning number of elements in list L
(defun .LENGTH (L)
  (if (null L)
      0
      (+ 1 (.LENGTH (cdr L)))))

; (.REMOVE-ALL X L) - return list with all the elements of L that are not X
(defun .REMOVE-ALL (X L)
  (cond ((null L) nil)
        ((equalp X (car L)) (.REMOVE-ALL X (cdr L)))
        (t (cons (car L) (.REMOVE-ALL X (cdr L))))))

; (.MAP F L) - return list whose elements are result of applying function F
(defun .MAP (F L)
  (if (null L)
      nil
      (cons (funcall (if (and (listp F) (eq (car F) 'LAMBDA))
                         (eval F)   
                         F)         
                    (car L))
            (.MAP F (cdr L)))))


; (.MERGE L1 L2) - return single sorted list
(defun .MERGE (L1 L2)
  (cond ((null L1) L2)
        ((null L2) L1)
        ((<= (car L1) (car L2))
         (cons (car L1) (.MERGE (cdr L1) L2)))
        (t (cons (car L2) (.MERGE L1 (cdr L2))))))


; SET FUNCTIONS

; (.ELEMENT-OF X S) - return true if X is an element of set S
(defun .ELEMENT-OF (X S)
  (cond ((null S) nil)
        ((equalp X (car S)) t)
        (t (.ELEMENT-OF X (cdr S)))))

; (.INSERT S X) - return set resulting from adding element X to set S
(defun .INSERT (S X)
  (if (.ELEMENT-OF X S)
      S
      (cons X S)))

; (.DIFFERENCE S1 S2) - return set that is .DIFFERENCE of sets S1 and S2
(defun .DIFFERENCE (S1 S2)
  (cond ((null S1) nil)
        ((.ELEMENT-OF (car S1) S2) (.DIFFERENCE (cdr S1) S2))
        (t (cons (car S1) (.DIFFERENCE (cdr S1) S2)))))

; (.SUPERSETEQ S1 S2) - return true if set S1 is superset or equal to S2
(defun .SUPERSETEQ (S1 S2)
  (cond ((null S2) t)
        ((.ELEMENT-OF (car S2) S1) (.SUPERSETEQ S1 (cdr S2)))
        (t nil)))


; MATH FUNCTIONS

; (.FACTORIAL N) - return .FACTORIAL of positive integer N
(defun .FACTORIAL (N)
  (if (<= N 1)
      1
      (* N (.FACTORIAL (- N 1)))))

; (.RIGHT-TRI A B C) - return true if positive integers A, B, and C can be sides of a right triangle
(defun .RIGHT-TRI (A B C)
  (and (= (+ (* A A) (* B B)) (* C C))
       (> A 0) (> B 0) (> C 0)))

; (.NTH-FIBO N) - return Nth Fibonacci number for non-negative integer N
(defun .NTH-FIBO (N)
  (cond ((= N 0) 0)
        ((= N 1) 1)
        (t (+ (.NTH-FIBO (- N 1)) (.NTH-FIBO (- N 2))))))

; (.POW X Y) - return value of X to power Y
(defun .POW (X Y)
  (cond ((= Y 0) 1)
        ((> Y 0) (* X (.POW X (- Y 1))))
        (t (/ 1 (.POW X (- Y))))))


; ADDITIONAL FUNCTIONS

; (.ADD3 N) - return N plus 3
(defun .ADD3 (N)
 (+ N 3))