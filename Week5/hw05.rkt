;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

;; CHANGE (put your solutions here)
(define (racketlist->mupllist  xs)
  (if (null? xs)
      (aunit)
      (apair (car xs)
             (racketlist->mupllist (cdr xs)))))
;; Problem 2
;; CHANGE (put your solutions here)
(define (mupllist->racketlist xs)
  (if (aunit? xs)
      null
      (cons (apair-e1 xs)
            (mupllist->racketlist (apair-e2 xs)))))
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
        [(fst? e) (let ([pr (eval-under-env (fst-e e) env)])
                    (if (apair? pr)
                        (eval-under-env (apair-e1 pr) env)
                        (error "fst applied to a non-pair")))]
        [(snd? e) (let ([pr (eval-under-env (snd-e e) env)])
                    (if (apair? pr)
                        (eval-under-env (apair-e2 pr) env)
                        (error "snd applied to a non-pair")))]  
        [(mlet? e)
         (let ([val (eval-under-env (mlet-e e) env)])
           (eval-under-env (mlet-body e) (cons (cons (mlet-var e) val) env)))]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)]
               [greater? (lambda (int1 int2)
                           (> (int-num int1) (int-num int2)))])
           (if (and (int? v1) (int? v2))
               (if (greater? v1 v2)
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error (format "ifgreater applied to non-ints: ~v" v1))))]
        [(apair? e) (apair
                     (eval-under-env (apair-e1 e) env)
                     (eval-under-env (apair-e2 e) env))]
        [(call? e) ;; TODO Clean up!!
         (let
             ([cl (eval-under-env (call-funexp e) env)])
           (cond [(closure? cl)     
                  (let ([param-name (fun-formal (closure-fun cl))]
                        [param-val (eval-under-env (call-actual e) env)])
                    (eval-under-env (fun-body (closure-fun cl)) (cons (cons param-name param-val) (closure-env cl))))]
                 [(fun? cl) (let ([param-name (fun-formal cl)]
                                  [param-val (eval-under-env (call-actual e) env)])
                              (eval-under-env (fun-body cl) (cons (cons param-name param-val) env)))]    
                 [#t     (error (format "not a closure: ~v" cl))]))]
        [(fun? e) (closure
                   (cons (cons (fun-nameopt e) e) env)
                   e)]
        [(isaunit? e) (if
                       (equal? (aunit) (eval-under-env (isaunit-e e) env))
                       (int 1)
                       (int 0))]
        [(or (int? e)
             (closure? e)
             (aunit? e)) e]  ;; a value evaluates to itself
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; Problem 3

(define (ifaunit e1 e2 e3) (if (equal? (int 1) (eval-exp (isaunit e1))) e2 e3))

(define (mlet* lstlst e2)
  (letrec ([f (lambda (bindings env)
                (if
                 (null? bindings)
                 (eval-under-env e2 env)
                 (let ([new-binding (cons (car (car bindings)) (eval-under-env (cdr (car bindings)) env))])
                   
                   (f (cdr bindings) (cons new-binding env)))))])
    (f lstlst '())))


(define (ifeq e1 e2 e3 e4)
  (let ([_x e1]
        [_y e2])
    (ifgreater _x _y e4 (ifgreater _y _x e4 e3))))

;; Problem 4

(define mupl-map (fun "mupl-map" "f" (fun "mupl-map" "lst"
                                          (ifeq (isaunit (var "lst")) (int 1)
                                                (aunit)
                                                (apair
                                                 (call (var "f") (fst (var "lst")))
                                                 (call (var "mupl-map") (snd (var "lst"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun "mupl-mapAddN" "n"  (fun "mupl-mapAddN" "lst"
                                     (call (call (var "map") (fun "add-i" "i"
                                                            (add (var "i") (var "n")))) (var "lst"))))))

                                    

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
