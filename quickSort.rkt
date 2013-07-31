;;quicksort

(define (quickSort lst)
  (if (<= (length lst) 1) lst
      (append 
       (quickSort (removes <= (cdr lst) (car lst))) 
            (cons 
             (car lst)(quickSort (removes >  (cdr lst) (car lst)))))))

               
 

(define (removes comp lst x)
  (cond 
  ((null? lst) '())
  ((list? lst) 
   (if (comp (car lst) x) (cons (car lst) (removes comp (cdr lst) x))
       (removes comp (cdr lst) x)))
  (else
   (if (<= lst x) lst))))


  
(removes <= '( 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17) 6)
(quickSort '( 9 7 8 6 5 4 3 2 1) )