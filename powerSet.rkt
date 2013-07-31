;; finds the power set of a given set 

(define (pSet set)
  (if (null? set) '(())
      (let ((psetRest (pSet (cdr set)))) ;; let psetRest be all obtained subsets (ps (cdr set)) so we dont have to call it twice below
        (append psetRest ;; append the list of all current subsets to the new subsets obtained by adding (car set) to all current subsets
                (map (lambda (subset) ;; cons car to all subsets of pset rest to obtain the new subsets
                       (cons (car set) subset))
                     psetRest)))))
