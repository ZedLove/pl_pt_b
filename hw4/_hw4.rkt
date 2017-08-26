#lang racket

(provide (all-defined-out))

(define (sequence low high stride)
  (cond
    [(> high low) (cons low (sequence (+ low stride) high stride))]
    [(= high low) (list low)]
    [(< high low) null]))

(define (string-append-map xs suffix)
  (map (lambda (str) (string-append str suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(empty? xs) (error "list-nth-mod: empty list")]
        [else (let ([i (remainder n (length xs))])                    
                (car (list-tail xs i)))]))

(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

(define funny-number-stream
  (letrec ([f (lambda (x i)
                (cons x (lambda ()
                          (let ([n (+ i 1)])
                            (if (equal? 0 (remainder n 5))
                                (f (* -1 n) n)
                                (f n n))))))])
    (lambda () (f 1 1))))

(define dan-then-dog
  (lambda ()
    (cons "dan.jpg"
	  (lambda ()
	    (cons "dog.jpg"
		  dan-then-dog)))))

(define (stream-add-zero s)
  (let ([x (s)])
    (lambda () (cons (cons 0 (car x))
		     (stream-add-zero (cdr x))))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
		(cons (cons (list-nth-mod xs n) (list-nth-mod ys n))
		      (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

(define (vector-assoc v vec)
  (letrec ([f (lambda (n)
		(cond [(equal? n (vector-length vec)) #f]
		      [(pair? (vector-ref vec n)) (let ([p (vector-ref vec n)])
						    (if (equal? (car p) v)
							p
							(f (+ n 1))))]
		      [#t (f (+ n 1))]))])
    (f 0)))

(define (cached-assoc xs n)
  (letrec ([cache       (make-vector n #f)]
	   [cache-index 0])
    (lambda (v)
      (letrec ([cached (vector-assoc v cache)])
	(if cached
	    (cdr cached)
	    (letrec ([res (assoc v xs)])
	      (begin
		(set! cache-index (if (= (+ cache-index 1) n)
				      0
				      (+ cache-index 1)))
		(vector-set! cache cache-index (cons v res))
		res)))))))

;; Challenge Problem - not working
;; (define-syntax while-less
;;   (syntax-rules (do)
;;     [(while-less e1 do e2)
;;      (letrec ([f (lambda (v1 e2)
;; 		   (let ([v2 (e2)])
;; 		     (if (< v1 v2)
;; 			 (f v1 e2)
;; 			 #t)))])
;;        (f e1 e2))]))

