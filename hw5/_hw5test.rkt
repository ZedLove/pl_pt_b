#lang racket
;; Programming Languages Homework 5 Simple Test
;; Save this file to the same directory as your homework file
;; These are basic tests. Passing these tests does not guarantee that your code will pass the actual homework grader

;; Be sure to put your homework file in the same folder as this test file.
;; Uncomment the line below and, if necessary, change the filename
(require "_hw5.rkt")

(define l (list (int 1) (int 2) (int 3)))

(require rackunit)

(define tests
  (test-suite
   "Sample tests for Assignment 5"
   
   ;; check racketlist to mupllist with normal list
   (check-equal?
    (racketlist->mupllist (list (int 3) (int 4)))
    (apair (int 3) (apair (int 4) (aunit)))
    "racketlist->mupllist test")

   (check-equal?
    (racketlist->mupllist l)
    (apair (int 1) (apair (int 2) (apair (int 3) (aunit))))
    "racketlist->mupllist test 2")
   
   ;; check mupllist to racketlist with normal list
   (check-equal?
    (mupllist->racketlist (apair (int 3) (apair (int 4) (aunit))))
    (list (int 3) (int 4)) "racketlist->mupllist test")

   (check-equal?
    (mupllist->racketlist (racketlist->mupllist l))
    l
    "mupllist->racketlist test 2")

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; eval-exp tests
   
   ;; Test add
   (check-equal?
    (eval-exp
     (add (int 3) (int 5)))
    (int 8)
    "add test")

   (check-equal?
    (eval-exp
     (add (aunit) (int 5)))
    ;; TODO - how to check for error?
    ;;(error "MUPL addition applied to non-number")
    "MUPL addition applied to non-number"
    "add test2")

   ;; tests if ifgreater returns (int 2)
   (check-equal?
    (eval-exp
     (ifgreater (int 3) (int 4)
                (int 3)
                (int 2)))
    ;; return val
    (int 2)
    "ifgreater test")

   (check-equal?
    (eval-exp (ifgreater (int 3) (int 2)
                         (int 10)
                         (int 20)))
    ;; return val
    (int 10)
    "ifgreater test2")
   
   (check-equal?
    (eval-exp
     (ifgreater (int 3) (apair (int 1) (int 2))
   		(add (int 10) (int 12))
   		(int 40)))
    "MUPL ifgreater applied to non-number"
    "ifgreater test3")
      
   ;; mlet test
   (check-equal?
    (eval-exp
     (mlet "x" (int 1)
           (add (int 5) (var "x"))))
    ;; return val
    (int 6)
    "mlet test")
   
   ;; TODO - strings
   
   ;; call test
   (check-equal?
    (eval-exp
     (call (closure
            '()
            (fun #f "x"
                 (add
                  (var "x") (int 7))))
           (int 1)))
    ;; return val
    (int 8)
    "call test")

   (check-equal?
    (eval-exp
     (call (closure '()
   		    (fun #t "x"
   			 (add (var "x") (int 7))))
           (int 1)))
    ;; return val
    (int 8)
    "call test2")
   
   (check-equal?
    (eval-exp
     (call (add
   	    (int 10) (int 7))
           (int 1)))
    ;; return val
    ;; TODO - error
    "Not a valid MUPL function"
    "call test3")

   
   ;;fst test
   (check-equal?
    (eval-exp
     (fst (apair (int 1) (int 2))))
    ;; return val
    (int 1)
    "fst test")

   (check-equal?
    (eval-exp
     (fst (int 1)))
    ;; return val
    ;; TODO - error
    "Cannot get fst of non-apair"
    "fst test2")

   ;;snd test
   (check-equal?
    (eval-exp
     (snd (apair (int 1) (int 2))))
    ;; return val
    (int 2)
    "snd test")

   (check-equal?
    (eval-exp
     (snd (int 1)))
    ;; return val
    ;; TODO - error
    "Cannot get 2nd of non-apair"
    "snd test2")
   
   ;; isaunit test
   (check-equal?
    (eval-exp
     (isaunit (closure
               '()
               (fun #f "x"
                    (aunit)))))
    ;; return val
    (int 0)
    "isaunit test")

   (check-equal?
    (eval-exp
     (isaunit (aunit)))
    ;; return val
    (int 1)
    "isaunit test2")
   
   ;; ifaunit test
   (check-equal?
    (eval-exp
     (ifaunit (int 1)
              (int 2)
              (int 3)))
    ;; return val
    (int 3)
    "ifaunit test")

   ;; ifaunit test
   (check-equal?
    (eval-exp
     (ifaunit (aunit)
              (int 2)
              (int 3)))
    ;; return val
    (int 2)
    "ifaunit test2")
   
   ;; mlet* test
   (check-equal?
    (eval-exp
     (mlet* (list (cons "x" (int 10)))
            (var "x")))
    ;; return val
    (int 10)
    "mlet* test")
   
   (check-equal?
    (eval-exp
     (mlet* (list (cons "x" (int 10))
		  (cons "y" (int 100)))
            (add (var "x") (var "y"))))
    ;; return val
    (int 110)
    "mlet* test2")
   
   ;; ifeq test
   (check-equal?
    (eval-exp
     (ifeq (int 1) (int 2)
           (int 3)
           (int 4)))
    ;; return val
    (int 4)
    "ifeq test")

   (check-equal?
    (eval-exp
     (ifeq (int 1) (int 1)
           (int 3)
           (int 4)))
    ;; return val
    (int 3)
    "ifeq test2")
   
   ;; mupl-map test
   (check-equal?
    (eval-exp
     (call
      (call mupl-map
            (fun #f "x"
                 (add (var "x") (int 7))))
      (apair (int 1) (aunit)))) 
    (apair (int 8) (aunit))
    "mupl-map test")
   
   ;; TODO - bigger list, handle non-list
   (check-equal?
    (eval-exp
     (call
      (call mupl-map
            (fun #f "x"
                 (add (var "x") (int 7))))
      (apair (int 1) (apair (int 10) (aunit))))) 
    (apair (int 8) (apair (int 17) (aunit)))
    "mupl-map test2")
   
   (check-equal?
    (eval-exp
     (call
      (call mupl-mapAddN (int 10))
      (racketlist->mupllist (list (int 1) (int 2) (int 3) (int 4)))))
    (apair (int 11) (apair (int 12) (apair (int 13) (apair (int 14) (aunit)))))
    "mupl-mapAddN test")
   
   ;; problems 1, 2, and 4 combined test
   (check-equal?
    (mupllist->racketlist
     (eval-exp
      (call
       (call mupl-mapAddN (int 7))
       (racketlist->mupllist 
        (list (int 3) (int 4) (int 9))))))
    (list (int 10) (int 11) (int 16))
    "combined test")))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
