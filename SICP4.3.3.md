# 4.50
```
;ramb
(define (ramb? exp) (tagged-list? exp 'ramb))
(define (rand-car lst) (list-ref lst (random (length lst))))
(define (remove item lst)
  (cond ((null? lst) '())
        ((eq? item (car lst)) (cdr lst))
        (else (cons (car lst) (remove item (cdr lst))))))
(define (analyze-ramb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            (let ((item (rand-car choices)))
                (first env
                       succeed
                       (lambda ()
                             (try-next (remove item choices)))))))
      (try-next cprocs))))
```

実行結果

```
;;; Amb-Eval input:
(ramb 1 2 3 4 5)

;;; Starting a new problem
;;; Amb-Eval value:
3

;;; Amb-Eval input:
try-again

;;; Amb-Eval value:
5

;;; Amb-Eval input:
try-again

;;; Amb-Eval value:
1
```

4.49の実行結果


# 4.51
```
(define (permanent-assignment? exp)  (tagged-list? exp 'permanent-set!))
(define (analyze-permanent-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
                 (set-variable-value! var val env)
                 (succeed 'ok
                          fail2))
             fail))))
```

実行結果

```
;;; Amb-Eval input:
(define (require p)
  (if (not p) (amb)))

;;; Starting a new problem
;;; Amb-Eval value:
ok

;;; Amb-Eval input:
(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

;;; Starting a new problem
;;; Amb-Eval value:
ok

;;; Amb-Eval input:
(define count 0)

;;; Starting a new problem
;;; Amb-Eval value:
ok

;;; Amb-Eval input:
(let ((x (an-element-of '(a b c)))
        (y (an-element-of '(a b c))))
    (set! count (+ count 1))
    (require (not (eq? x y)))
    (list x y count))

;;; Starting a new problem
;;; Amb-Eval value:
(a b 1)

;;; Amb-Eval input:
try-again

;;; Amb-Eval value:
(a c 1)

;;; Amb-Eval input:
try-again

;;; Amb-Eval value:
(b a 1)

;;; Amb-Eval input:
try-again

;;; Amb-Eval value:
(b c 1)

;;; Amb-Eval input:
try-again

;;; Amb-Eval value:
(c a 1)

;;; Amb-Eval input:
try-again

;;; Amb-Eval value:
(c b 1)

;;; Amb-Eval input:
try-again

;;; There are no more values of
(let ((x (an-element-of '(a b c))) (y (an-element-of '(a b c)))) (set! count (+ count 1)) (require (not (eq? x y))) (list x y count))
```

setを用いた場合は毎度環境が戻されるので、１から変わらない


# 4.52
```
;if-fail
(define (if-fail? exp) (tagged-list? exp 'if-fail))

(define (analyze-if-fail exp)
  (let ((success-proc (analyze (cadr exp)))
        (fail-proc (analyze (caddr exp))))
    (lambda (env succeed fail)
      (success-proc env succeed
            (lambda ()
              (fail-proc env succeed fail))))))
```


# 4.53
```
(define (require p)
  (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define (square n) (* n n))
(define (divides? a b) (= (remainder b a) 0))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (smallest-divisor n)
        (find-divisor n 2))
(define (prime? n)
  (= n (smallest-divisor n)))
(define (prime-sum-pair list1 list2)
  (let ((a (an-element-of list1))
        (b (an-element-of list2)))
       (require (prime? (+ a b)))
       (list a b)))

(let ((pairs '()))
     (if-fail (let ((p (prime-sum-pair '(1 3 5 8) '(20 35 110))))
                   (permanent-set! pairs (cons p pairs))
                   (amb))
              pairs))
```

```
;;; Amb-Eval input:
(define (require p)
  (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define (square n) (* n n))
(define (divides? a b) (= (remainder b a) 0))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (smallest-divisor n)
        (find-divisor n 2))
(define (prime? n)
  (= n (smallest-divisor n)))
(define (prime-sum-pair list1 list2)
  (let ((a (an-element-of list1))
        (b (an-element-of list2)))
       (require (prime? (+ a b)))
       (list a b)))

(let ((pairs '()))
     (if-fail (let ((p (prime-sum-pair '(1 3 5 8) '(20 35 110))))
                   (permanent-set! pairs (cons p pairs))
                   (amb))
              pairs))

;;; Starting a new problem
;;; Amb-Eval value:
ok

(略）

;;; Starting a new problem
;;; Amb-Eval value:
((8 35) (3 110) (3 20))
```

# 4.54
```
;require
(define (require? exp) (tagged-list? exp 'require))
(define (require-predicate exp) (cadr exp))
(define (analyze-require exp)
  (let ((pproc (analyze (require-predicate exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (not (true? pred-value))
                   (fail2)
                   (succeed 'ok fail2)))
             fail))))
```
