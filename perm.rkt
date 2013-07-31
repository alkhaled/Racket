;; returns all the possible permutations of the given list 

(define (rm x seq)
  (cond 
    ((null? seq) '()) 
    ((= x (car seq)) (rm  x (cdr seq)))
    (else
     (cons (car seq) (rm  x (cdr seq))))))
  


(define (perm seq)
  (if (null? seq) '(())
      (apply append ;; append all the results of the map
             ;; remove an element from the list and append it to all combinations of the cdr 
             (map (lambda (r) ;;map remove onto our list
                    (map (lambda (permutation) ;; append the removed element to all permutations of the cdr 
                           (cons r permutation))
                  (perm (rm r seq)))) 
                  seq)))) 

(perm '(1 2 3))




