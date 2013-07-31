

(define (unaryMap fn seq)
  (if ( null? seq) '()
      (cons (fn (car seq)) (unaryMap fn (cdr seq)))))

(define (manMap fn list1 . listN)
  ( if (null? list1) '()    
       (cons
        (apply fn 
               (cons (car list1) (unaryMap car listN)))
      ;; do not use this =>  (manMap fn (cdr list1) (unaryMap cdr listN)) because we need to flatten the lists using the cons below
        (apply manMap (cons fn 
                            (cons (cdr list1) (unaryMap cdr listN)))))))

(manMap + '( 1 2 3) '( 1 2 3))