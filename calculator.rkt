;; implementation of a calculator
;; given an valid infix infix airthmetic expression returns its value

;; maintian two lists one for numbers and another for operators
;; as we parse the list push any number we see on the numbers list and any operation into the op list

;; every time we remove an oprator we compare it to the last element placed in the op list, if the new element is 
;; of lower precidence apply the old operator to the last two elements in the numbers list and put the result back into
;; the list in their place.
;; if the new op is of higher precedence we apply it to the last two numbers



(define ( evaluate expr)
  (parse expr '() '()))

;;;;;;;;evaluate-expr & helper function;;;;;;;;;
(define (to-procedure op)
  (cond
    ((equal? op '*) *)
    ((equal? op '/) /)
    ((equal? op '+) +)
    ((equal? op '-) -)
    (else (error "INVALID OP: " op))))

(define (evaluate-expr op num1 num2)
  ((to-procedure op) num1 num2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;precedes & helper functions ;;;;;;;;;;;
(define (member? elt lst)
  (cond
    ((null? lst) #f)
    ((eq? elt (car lst)) #t)
    (else
     (member? elt (cdr lst)))))

( define (priority op)
   ( if (member? op '(* /)) 1 
                 2))

;; given two operators returns true if op1 has a higher priority than op2 and false otherwise.
(define (precedes? op1 op2)
  (< (priority op1) (priority op2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse expr op-list num-list)
  (cond
    ((null? expr)
     (if (null? op-list) ;; if the list of ops is empty we should be done.
         (car num-list) ;; num is now the final value of the expression for all well formed expressions
         (evaluate-expr  (car op-list) (car num-list) ( cadr num-list)))) ;; else there is one last op to evaluate. Evaluate it and return the result.
    
    ((list? (car expr)) ;; if the next expression is in parenthesis evaluate it first and return the answer. We do this so parens can be used to specify order of ops. 
     (parse (cdr expr) op-list (cons (evaluate (car expr)) num-list)))
    
    ((number? (car expr)) ;; Simply place any number on the num list if it is at the head of the expression. T
     (parse (cdr expr) op-list (cons (car expr) num-list)))
    
    (else ;; => (car expr) is an operator. If it takes precedence over the operator in op-list evaluate it, if not evaluate the operation in op-list.
     (if (null? op-list) ;; Saftey check to make sure the op-list is not empty. Only an edge case for the first op encountered.
         (parse (cdr expr) (list (car expr)) num-list)
        
         (if (precedes? (car expr) (car op-list))
             ;;parse
             ;;       expression with the last operator and the number folowing ( both will be used in the evaluation) it removed
             ;;         |      same op-list since we take our expression from expr
             ;;         |          |     combine the evaluated expression with the cdr of numlist (we use cdr to remove the elt that was used in the evaluation from num-list)        
             ;;         |          |       |   evaluate the car of numlist with the operator from expression and the number following the oprator.
             ;;         |          |       |                   |
             (parse (cddr expr) op-list (cons (evaluate-expr (car expr) (car num-list) ( cadr expr)) 
                                                   (cdr num-list)))
             ;;parse
             ;;      Expression with operator removed ( leave the number after since we will use two numbers from numlist with the operator already in op-list
             ;;         |             Set the op-list to be the new operator obtained from expression. The previous op in op-list will be evaluated so we can remove it from this list
             ;;         |               |          Remove the top two numbers from num-list (leaving it empty) and return the result of evaluating it with the op from op-list. 
             ;;         |               |            |     
             (parse (cdr expr)  (list(car expr)) (list (evaluate-expr (car op-list) (car num-list) (cadr num-list)))))))))