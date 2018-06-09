# 4.1
もともとの list-of-values
```
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))
```

cons の引数をあらかじめ評価してやれば良いので

左から右

```
(define (list-of-values-left-to-right exps env)
  (if (no-operands? exps)
      '()
      (let ((first-eval (eval (first-operand exps) env)))
        (cons first-eval
              (list-of-values-left-to-right (rest-operands exps) env)))))
```

右から左

```
(define (list-of-values-right-to-left exps env)
  (if (no-operands? exps)
      '()
      (let ((first-eval (list-of-values-right-to-left (rest-operands exps) env)))
        (cons  (eval (first-operand exps) env)
               first-eval))))
```

# 4.2
もともとのeval

```
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))
```

Louisが修正したeval

(application? が assignment?の前に来る)

```
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        (else
         (error "Unknown expression type -- EVAL" exp))))
```

## 4.2a

(define x 3) に対する挙動

define を関数として認識してしまい、うまく動かないのではないか。

代入のシンボルをシンボルとして認識できず、関数として解釈してしまう。

## 4.2b

applicaiton で評価したいときは必ず call がつく、と前提できるので、applicationの処理を変える。

```
(define (application? exp)(tagged-list? exp 'call))
```

演算子についても callがつくので追従して直す。

具体的には、carにあるcallを無視するようにする。

```
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))
```

# 4.3
パス

# 4.4
```
(define (eval-and exp env)
  (define (eval-and-iter exp result)
    (if (null? exp)
        result
        (let ((first-eval (eval (car exp) env)))
          (if (true? first-eval)
              (eval-and-iter (cdr exp) 'true)
              'false))))
  (eval-and-iter exp 'true))
```

```
(define (eval-or exp env)
  (if (null? exp)
      'false
      (let ((first-eval (eval (car exp) env)))
        (if (true? first-eval)
            'true
            (eval-or (cdr exp) env)))))
```

# 4.5

expand-clauses の if分作成前に => を含む構文を評価する

```
(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; else節なし
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (if (eq? (car (cond-actions first)) '=>)
                         (list (cadr (cond-actions first)) (cond-predicate first))
                         (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))
```

# 4.6

```
#lang racket
(define (let? exp)
  (tagged-list? exp 'let))

;(let ((var1 exp1) (var2 exp2) ...) body)
(define (let-variables exp) (map car (cadr exp)))
(define (let-expressions exp) (map cdr (cadr exp)))
(define (let-body exp) (cddr exp))

;((lambda (var1 var2 ...) body) exp1 exp2 ...)
(define (let->combination exp)
   (if (null? (let-parameters exp))
       '()
       (cons
         (make-lambda (let-variables exp) (let-body))
         (let-expressions exp))))
```

# 4.7
```
(let* ((var1 exp1) (var2 exp2) ...) body)
```

は

```
(let ((var1 exp1))
  (let* ((var2 exp2) ...) body))
```

と展開できる。

よって
```
(define (let*->nested-lets exp)
  (define (iter params)
    (if (null? params)
        (let*-body exp)
        (make-let (car params) (iter (cdr params)))))
  (iter (let*->parameters exp)))
```

あるいは

```
(define (let*-parameters exp) (cadr exp))
(define (let*-body exp) (cddr exp))
(define (let*->nested-lets exp)
  (define (iter params result)
    (if (null? params)
        result
        (iter (cdr params)
              (make-let (list (car params)) result))))
  (iter (reverse (let*-parameters exp)) (let*-body exp)))
```

# 4.8
ギブ

# 4.9
ギブ

# 4.10
ギヴ
