;; CSE413 16au, Programming Languages, Homework 5

;;Evan Ko
;;CSE 413
;;Homework 5
#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct isgreater (e1 e2)    #:transparent) ;; if e1 > e2 then 1 else 0
(struct ifnz (e1 e2 e3) #:transparent) ;; if not zero e1 then e2 else e3
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair   (e1 e2) #:transparent) ;; make a new pair
(struct first   (e)     #:transparent) ;; get first part of a pair
(struct second  (e)     #:transparent) ;; get second part of a pair
(struct munit   ()      #:transparent) ;; unit value -- good for ending a list
(struct ismunit (e)     #:transparent) ;; if e1 is unit then 1 else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

;; CHANGE (put your solutions here)
(define (racketlist->mupllist lst)
  (if (null? lst)
      (munit)
      (apair (car lst) (racketlist->mupllist (cdr lst)))))

(define (mupllist->racketlist mlst)
  (if (munit? mlst)
      null
      (cons (apair-e1 mlst) (mupllist->racketlist (apair-e2 mlst)))))

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
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]        
        ;; CHANGE add more cases here
        [(int? e) e]
        [(munit? e) e]
        [(isgreater? e)
         (let ([n1 (eval-under-env (isgreater-e1 e) env)]
               [n2 (eval-under-env (isgreater-e2 e) env)])
           (if (and (int? n1) (int? n2))
               (if (> (int-num n1) (int-num n2))
                   (int 1)
                   (int 0))
               (error "passed in a non integer value")))]
        [(ifnz? e)
         (let ([n1 (eval-under-env (ifnz-e1 e) env)])
         (if (not (equal? n1 (int 0)))
             (eval-under-env (ifnz-e2 e) env)
             (eval-under-env (ifnz-e3 e) env)))]
        [(apair? e)
         (let ([n1 (eval-under-env (apair-e1 e) env)]
               [n2 (eval-under-env (apair-e2 e) env)])
           (apair n1 n2))]
        [(first? e)
         (let ([n1 (eval-under-env (first-e e) env)])
           (if (apair? n1) (apair-e1 n1) '()))]
        [(second? e)
         (let ([n1 (eval-under-env (second-e e) env)])
           (if (apair? n1) (apair-e2 n1) '()))]
        [(ismunit? e)
         (let ([n1 (eval-under-env (ismunit-e e) env)])
           (if (munit? n1)
               (int 1)
               (int 0)))]
        [(fun? e)
         (closure env e)]
        [(mlet? e)
         (letrec ([n1 (eval-under-env (mlet-e e) env)]
                  [newEnv (cons (cons (mlet-var e) n1)  env)])
           (eval-under-env (mlet-body e) newEnv))]
        [(closure? e) (eval-under-env (closure-fun e) (closure-env e))]
        [(call? e)
         (let ([n1 (eval-under-env (call-funexp e) env)]) ;;first param
           (if (closure? n1)
               (let* ([func (closure-fun n1)]
                     [var (eval-under-env (call-actual e) env)]
                     [newEnv (cons (cons (fun-formal func) var) (closure-env n1))])
                 (if (fun-nameopt func)
                     (let ([en (cons (cons (fun-nameopt func) n1) newEnv)])
                       (eval-under-env (fun-body func) en))
                     (eval-under-env (fun-body func) newEnv)))                            
           (error "First param in call is not a closure")))]
         [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

;;if e1 is a munit then evaluate e2 else e3
(define (ifmunit e1 e2 e3)
  (ifnz (ismunit e1) e2 e3))
  
;;binds all bs pairs with each other
(define (mlet* bs e2)
  (if (null? bs)
      e2
      (let ([n1 (car bs)])
        (mlet (car n1) (cdr n1) (mlet* (cdr bs) e2)))))

;;if e1 = e2 the evaluates e3 else e4
;;throws error if e1 and e2 are not integers
(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons "_y" e2))
     (ifnz (isgreater (var "_x") (var "_y")) e4
            (ifnz (isgreater (var "_y") (var "_x")) e4 e3))))
      

;; Problem 4

;;takes a MUPL function and returns a MUPL function that takes a MUPL list
;;applies the function to every element of the list
;;returns a new list with all numbers other than zero
(define mupl-filter
  (fun null "fn"
       (fun "fil-lst" "lst"
            (ifmunit (var "lst")
                     (munit)
                     (mlet "now" (first (var "lst"))
                           (ifnz (call (var "fn") (var "now"))
                                       (apair (var "now") (call (var "fil-lst") (second (var "lst"))))
                                              (call (var "fil-lst") (second (var "lst")))))))))

                    
;;takes an integer i and returns a MUPL list with all
;;integers that are greater than i
(define mupl-all-gt
  (mlet "filter" mupl-filter
        (fun null "i"
             (call (var "filter")
                   (fun null "curr"
                        (isgreater (var "curr") (var "i")))))))


;; Challenge Problem (extra credit)

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make grading
;; more difficult, so copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
