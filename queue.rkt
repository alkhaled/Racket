;;Implementation of a queue in scheme
;; with ammortized constant time Query  and remove and constant time insert
;;
;; This alorithm works by maintaining two queues a HEAD and a TAIL.
;; TAIL - constant insert:
;; elements are inserted by cons'ing them on to the tail, which is constant time
;; to query the head of the list or remove it, however, would take linear time if we 
;; only have the single tail list.
;; HEAD - ammortized constant query and remove:
;; To get constant ammortized time for query and remove we maintain another queue called the head
;; If head is ever empty when we query or remove we reverse tail and make it the head queue
;; this takes linear time, and gives us constant time queries for all the elements
;; we reversed. leading to ammortized O(1) time. 
;;Note this implementation of a queue is equivalent to maintaing two stacks and 
;;popping from one to push onto the other.

;; car of queue is the head, and cdr is the tail.

(define (insert queue x)
  (list (car queue) (cons x (cadr queue))))

(define (head queue)
  (car (car queue)))

(define (delete queue)
  (cond
    ((is-empty? queue) ( error "Error in delete: removing from an empty queue"))
    ((null? (car queue)) (cons (cdr (reverse (cadr queue))) '(()) ));; if the head is empty reverse the tail and then make its cdr the head ( taking the cdr excludes the first element, thus deleting it.
    (else
     (cons ( cdar queue) ( cdr queue)))))

(define (is-empty? queue)
  (and (null? (car queue)) (null? (cdr queue))));; If head is empty the queue must be empty since the tail is transfered to head if it is empty.

(define (make-queue)
  (list '() '()))

(define q '( (7) (1 2 3)))
(is-empty? (insert (delete (delete q)) 5)) 

;(head q)
