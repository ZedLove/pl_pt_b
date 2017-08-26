;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var       (string)              #:transparent) ;; a variable, e.g., (var "foo")
(struct int       (num)                 #:transparent) ;; a constant number, e.g., (int 17)
(struct add       (e1 e2)               #:transparent) ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)         #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun       (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call      (funexp actual)       #:transparent) ;; function call
(struct mlet      (var e body)          #:transparent) ;; a local binding (let var = e in body) 
(struct apair     (e1 e2)               #:transparent) ;; make a new pair
(struct fst       (e)                   #:transparent) ;; get first part of a pair
(struct snd       (e)                   #:transparent) ;; get second part of a pair
(struct aunit     ()                    #:transparent) ;; unit value -- good for ending a list
(struct isaunit   (e)                   #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

(define (racketlist->mupllist l)
  (if (pair? l)
      (apair (car l) (racketlist->mupllist (cdr l)))
      (aunit)))

(define (mupllist->racketlist l)
  (if (apair? l)
      (cons (apair-e1 l) (mupllist->racketlist (apair-e2 l)))
      null))


;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (let ([val-env (lambda (e) (eval-under-env e env))])
    (cond [(var? e) 
           (envlookup env (var-string e))]
          [(int? e)
           e]
          [(add? e) 
           (let ([v1 (val-env (add-e1 e))]
                 [v2 (val-env (add-e2 e))])
             (if (and (int? v1)
                      (int? v2))
                 (int (+ (int-num v1) 
                         (int-num v2)))
                 (error "MUPL addition applied to non-number")))]
          [(ifgreater? e)
           (let ([v1 (val-env (ifgreater-e1 e))]
                 [v2 (val-env (ifgreater-e2 e))]
                 [e3 (ifgreater-e3 e)]
                 [e4 (ifgreater-e4 e)])
             (if (and (int? v1) (int? v2))
                 (if (> (int-num v1) (int-num v2))
                     (val-env e3)
                     (val-env e4))
                 (error "MUPL ifgreater applied to non-number")))]
	  [(closure? e)
	   e]
          [(fun? e)
	   ;; nameopt formal body
           (closure env e)]
          [(call? e)
	   ;; funexp actual
           (let ([exp   (val-env (call-funexp e))]
		 [param (val-env (call-actual e))])
	     (if (closure? exp)
		 (let* ([old-env (closure-env exp)]
			[fun     (closure-fun exp)]
			[body    (fun-body fun)]
			[new-env (cons (cons (fun-formal fun) param) old-env)]
			[nameopt (fun-nameopt fun)])
		   (if nameopt
		       (eval-under-env body (cons (cons nameopt exp) new-env))
		       (eval-under-env body new-env)))
		 (error "Not a valid MUPL function")))]
          [(mlet? e)
	   (let* ([new-var (mlet-var e)]
		  [new-exp (val-env (mlet-e e))]
		  [new-env (cons (cons new-var new-exp) env)])
	     (eval-under-env (mlet-body e) new-env))]
          [(apair? e)
           (let ([v1 (val-env (apair-e1 e))]
                 [v2 (val-env (apair-e2 e))])
             (apair v1 v2))]
          [(fst? e)
           (let ([v (val-env (fst-e e))])
	     (if (apair? v)
		 (apair-e1 v)
		 (error "Cannot get fst of non-apair")))]
          [(snd? e)
	   (let ([v (val-env (snd-e e))])
	     (if (apair? v)
		 (apair-e2 v)
		 (error "Cannot get 2nd of non-apair")))]
          [(aunit? e)
           e]
          [(isaunit? e)
           (if (aunit? (val-env (isaunit-e e)))
               (int 1)
               (int 0))]
          [#t (error (format "bad MUPL expression: ~v" e))])))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; Problem 3

(define (ifaunit e1 e2 e3)
  (ifgreater
   (isaunit e1) (int 0)
   e2
   e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (let ([v (car lstlst)])
	(mlet (car v) (cdr v) (mlet* (cdr lstlst) e2)))))

(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1)
	       (cons "_y" e2))
	 (ifgreater (var "_x") (var "_y") e4
		    (ifgreater (var "_y") (var "_x") e4 e3))))

;; Problem 4

(define mupl-map
  (fun "fn-map" "fn"
       (fun "lst-map" "lst"
	    (ifeq (isaunit (var "lst")) (int 1)
		  (aunit)
		  (apair (call (var "fn") (fst (var "lst")))
			 (call (var "lst-map") (snd (var "lst"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
	(fun "add-fn" "i"
	     (call (var "map")
		   (fun #f "x"
			(add (var "i") (var "x")))))))


;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e)
  (int 69))

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env)
  (int 69))

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
