;; mergsort implementation that needs to be passed a comparator that returns true or false


;; the merge function

(define (merge list1 list2 comp)
   ( cond 
      ((null? list1) list2)
      ((null? list2) list1)
      ((comp (car list1)(car list2))
       (cons (car list1) (merge (cdr list1) list2 comp)))
      (else
       (cons (car list2) (merge (cdr list2) list1 comp)))))
        

;;( merge '(1 2 6) '(4 3 8) <)


;; returns the first k elts of a list
(define (firstK seq k)
  ( if (or (zero? k) (null? seq)) '()
       (cons (car seq) (firstK (cdr seq) (- k 1)))))

;;(firstK '(1 2 3 4 5 6 7) 8)

(define (mergeSort seq comp)
  (if (<= (length seq) 1) seq
      (let (
            (frontLength (quotient (length seq) 2))
            (backLength  (- (length seq) (quotient (length seq) 2))))
        (merge (mergeSort (firstK seq frontLength) comp)
             (mergeSort (firstK (reverse seq) backLength) comp)
             comp))))

(mergeSort '(1 3 5 7 2 78 89 6 54 ) <)
