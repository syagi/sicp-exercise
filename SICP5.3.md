# 5.20

```
(define x (cons 1 2))
```

|index|0|1|2|
|the-cars| |n1| |
|the-cdrs| |n2| |

```
(define y (list x x))
```

|index|0|1|2|3|
|the-cars| |n1|p1|p1|
|the-cdrs| |n2|p3|e0|


# 5.21
題意からすると、本文中のメモリ演算を使った実装をしろという事に思えたが、  
動作確認できないので、使わずコメントに注記。

## a
```
(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves (car tree))
                 (count-leaves (cdr tree))))))
```

フィボナッチと似てるのでうまくいじれないか考える。
```
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))
```

コードは 5.21.rkt　の count-leaves-machine-a


## b
```
(define (count-leaves tree)
  (define (count-iter tree n)
    (cond ((null? tree) n)
          ((not (pair? tree)) (+ n 1))
          (else (count-iter (cdr tree)
                            (count-iter (car tree) n)))))
  (count-iter tree 0))
```

末尾再帰になっているのが違い.
アセンブラで実装すると、戻り先を保持する必要がないので、
ラベルひとつ分少なくなるのが分かる。

コードは 5.21.rkt　の count-leaves-machine-b


# 5.22

## append
```
(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))
```

```

(define append-machine
  (make-machine
     (list (list 'null? null?) (list 'cons cons) (list 'car car) (list 'cdr cdr))
     '(start
         (assign continue (label append-done))
       append-loop
         ;; xがnullの場合
         (test (op null?) (reg x))
         (branch (label null-lst))
         ;; cons
         (save continue)
         (assign continue (label after-append))
         ;; (append (cdr x) y)
         (save x)
         (assign x (op cdr) (reg x)) ;(assign x (op vector-ref) (reg the-cdrs) (reg x))
         (goto (label append-loop))
       null-lst
         (assign result (reg y))
         (goto (reg continue))
       after-append
         (restore x)
         (restore continue)
         (assign tmp (op car) (reg x)) ;(assign x (op vector-ref) (reg the-cdrs) (reg x))
         (assign result (op cons) (reg tmp) (reg result))
         (goto (reg continue))
       append-done)))
```

## append!
```
(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))
```

```
;(define (append! x y)
;  (set-cdr! (last-pair x) y)
;  x)
;(define (last-pair x)
;  (if (null? (cdr x))
;      x
;      (last-pair (cdr x))))

(define append!-machine
  (make-machine
    (list (list 'set-cdr! set-cdr!) (list 'null? null?) (list 'cdr cdr))
    '(start
        (assign continue (label append!-done))
        (save x)
      append!-loop
        ; last-pair
        ; (null? (cdr x))
        (assign tmp (op cdr) (reg x))
        (test (op null?) (reg tmp))
        (branch (label null))
        ; (last-pair (cdr x))
        (assign x (op cdr) (reg x))
        (goto (label append!-loop))
      null
        ; (set-cdr! (last-pair x) y)
        (assign x (op set-cdr!) (reg x) (reg y))
        (goto (reg continue))
      append!-done
        (restore x))))
```
