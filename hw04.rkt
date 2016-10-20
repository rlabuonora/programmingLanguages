#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; 1
(define (sequence lo hi stride)
  (if (> lo hi)
      null
      (cons lo (sequence (+ lo stride) hi stride))))

;; 2
(define (string-append-map xs suffix)
  (map (lambda (str) (string-append str suffix)) xs))

;; 3
(define (list-nth-mod xs n)
  (let ([n-mod (remainder n (length xs))])
    (cond [(< n 0) (error "list-nth-mod: negative number")]
          [(null? xs) (error "list-nth-mod: empty list")]
          [(= n-mod 0)  (car xs)] 
          [#t (list-nth-mod (cdr xs) (- n-mod 1))])))


;; 4
(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

;; 5
(define funny-number-stream
  (letrec ([f (lambda (x) (cons (funny x) (lambda () (f (+ x 1)))))]
           [funny
            (lambda (n)
              (if (not (= (remainder n 5) 0))
                  n
                  (* -1 n)))])
    (lambda () (f 1))))

;; 6
(define dan-then-dog
  (letrec ([dan (lambda () (cons "dan.jpg" dog))]
           [dog (lambda () (cons "dog.jpg" dan))])
    (lambda () (dan))))

;; 7
(define (stream-add-zero s)
  (lambda ()
    (cons (cons 0 (car (s)))
          (lambda () (stream-add-zero (cdr (s)))))))

;; 8
(define (cycle-lists xs ys)
  (letrec ([foo (lambda (n)
                  (cons
                   (cons (list-nth-mod xs n) (list-nth-mod ys n))
                   (lambda() (foo (+ n 1)))))])
    (lambda () (foo 0))))

;; 9
(define (vector-assoc v vec)
  (letrec ([f (lambda (n)
                (cond [(> n (- (vector-length vec) 1)) #f]
                      [(and
                        (pair? (vector-ref vec n))
                        (equal? (car (vector-ref vec n)) v)) (vector-ref vec n)]
                      [#t (f (+ n 1))]))])
    (f 0)))

;; 10
(define (cached-assoc xs n)
  (let ([cache (make-vector n #f)]
        [last 0])
    (lambda (v)
      (let ([cached-result (vector-assoc v cache)])
          (if cached-result
          cached-result
          (let ([calculated-result (assoc v xs)])
            (if calculated-result
                (begin
                  (vector-set! cache last calculated-result) 
                  (set! last
                        (if
                         (< last (- n 1))
                         (+ last 1)
                         0))
                  (assoc v xs))
                calculated-result))))))) 
