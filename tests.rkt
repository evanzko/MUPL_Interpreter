;; CSE413 16au, Programming Languages, Homework 5
;;Evan Ko
;;CSE 413
;;Homework 5
#lang racket

(require "hw5.rkt")

; This file uses Racket's unit-testing framework.

; Note we have provided [only] 3 tests, but you can't run them until do some of the assignment.
; You will want to add more tests.

(require rackunit)

(define tests
  (test-suite
   "Homework 5 Tests"

   (check-equal? (eval-exp (add (int 2) (int 2))) (int 4) "add simple test")

   (check-exn (lambda (x) (string=? (exn-message x) "MUPL addition applied to non-number"))
              (lambda () (eval-exp (add (int 2) (munit))))
              "add bad argument")

   ;;Problem 2
   
   ;;var check
   (check-equal? (eval-exp (int 3)) (int 3) "simple int")
   (check-equal? (eval-exp (int 0)) (int 0) "zero test")
   
   ;;munit check
   (check-equal? (eval-exp (munit)) (munit) "simple munit check")
   
   ;;isgreater check
   ;   (check-equal? (eval-exp (isgreater (munit) (int 0))) ("passed in a non integer value") "error check")
   (check-equal? (eval-exp (isgreater (int 3) (int 2))) (int 1) "simple greater check")
   (check-equal? (eval-exp (isgreater (int 2) (int 3))) (int 0) "check the other ways")
   (check-equal? (eval-exp (isgreater (add (int 2) (int 3)) (int 4))) (int 1) "complex check")
   
   ;;ifnz check
   (check-equal? (eval-exp (ifnz (int 0) (int 1) (int 2))) (int 2) "zero test")
   (check-equal? (eval-exp (ifnz (int 1) (int 1) (int 2))) (int 1) "non-zero test")
   (check-equal? (eval-exp (ifnz (add (int 1) (int -1)) (int 1) (int 2))) (int 2) "complex zero test")
   
   ;;apair check
   (check-equal? (eval-exp (apair (int 3) (int 3))) (apair (int 3) (int 3)) "simple pair test")
   (check-equal? (eval-exp (apair (add (int 1) (int 2)) (int 3))) (apair (int 3) (int 3)) "complex pair test")
   
   ;;first check
   (check-equal? (eval-exp (first (apair (int 5) (int 3)))) (int 5) "first test")
   
   ;;second check
   (check-equal? (eval-exp (second (apair (int 5) (int 3)))) (int 3) "second test")
   
   ;;ismunit check
   (check-equal? (eval-exp (ismunit (munit))) (int 1) "simple ismunit test")
   (check-equal? (eval-exp (ismunit (closure '() (fun "f" "x" (munit))))) (int 0) "complex ismunit test")
   
   ;;fun check
   (check-equal? (eval-exp (fun "x" "i" (int 1))) (closure '() (fun "x" "i" (int 1)))) 
   
   ;;mlet check
   (check-equal? (eval-exp (mlet "x" (int 1) (var "x"))) (int 1) "mlet test")
   (check-equal? (eval-exp (mlet "x" (int 1) (add (int 5) (var "x")))) (int 6) "mlet test")
   
   ;;call check
   (check-equal? (eval-exp (call (fun "f" "x" (int 6)) (int 1))) (int 6) "Should return 7")
   (check-equal? (eval-exp (call (closure '() (fun "f" "x" (add (var "x") (int 107)))) (int 2))) (int 109) "call test")

   ;;Prblem 3

   ;;ifmunit check
   (check-equal? (eval-exp (ifmunit (int 1) (int 2) (int 35))) (int 35) "simple ifmunit test")
   (check-equal? (eval-exp (ifmunit (munit) (add (int 2) (int 10)) (int 3))) (int 12) "complex ifmunit test")

   ;;mlet* check
    (check-equal? (eval-exp (mlet* (list (cons "n" (int 15))) (var "n"))) (int 15) "mlet* test")
    (check-equal? (eval-exp (mlet* (list (cons "x" (int 10)) (cons "y" (int -1))) (add (var "x") (var "y")))) (int 9) "testing with two vars")

    ;;ifeq check
    (check-equal? (eval-exp (ifeq (int 1) (int 2) (int 3) (int 4))) (int 4) "ifeq test")
    (check-equal? (eval-exp (ifeq (int 1) (int 1) (int 3) (int 4))) (int 3) "ifeq test")
    (check-equal? (eval-exp (ifeq (add (int 3) (int 4)) (add (int 6) (int 1)) (add (int 3) (int 2)) (int 4))) (int 5) "ifeq test")

    ;;problem 4

    ;;mupl-filter and  check
         (check-equal? (mupllist->racketlist
                  (eval-exp (call (call mupl-all-gt (int 9))
                                  (racketlist->mupllist 
                                   (list (int 10) (int 9) (int 15))))))
                 (list (int 10) (int 15))
                 "provided combined test using problems 1, 2, and 4")))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
