# 5.23

```
eval-dispatch
  (test (op cond?) (reg exp)) ;add
  (branch (label ev-cond))    ;add
  (test (op let?) (reg exp))  ;add
  (branch (label ev-let))     ;add

...

;;cond
ev-cond
  (assign exp (op cond->if) (reg exp))
  (goto (label eval-dispatch))
;;let
ev-let
  (assign exp (op let->combination) (reg exp))
  (goto (label eval-dispatch))

```

# 5.24
```
;exp1
(cond
  ;;unev1 (car exp1)
    ;;exp2 (car unev1)
    (
      (> x 0)    ;; exp3
      'positive  ;; unev3
    )
    ;;exp2
    ;;unev2 (cdr unev1)
    ( (= x 0)
      'zero
    )
    (else
     'negative ;; unev4
    )
    ;;unev2
  ;;unev1
)
;exp1
```

```
;;cond
ev-cond
  (assign unev (op cond-clauses) (reg exp)) ;unev1
  (save continue)
  (goto (label eval-cond-loop))
ev-cond-loop
  (test (op null?) (reg unev)) ; cond-clauses がnullか判定
  (branch (label ev-cond-null))
  (assign exp (op car) (reg unev)) ;exp2 次に評価する条件式
  (test (op cond-else-clause?) (reg exp)) ; else節 か判定
  (branch (label ev-cond-else-clause))
  (save exp) ;exp2
  (assign exp (op cond-predicate) (reg exp)) ;exp3
  (save unev) ;uenv1
  (save env)
  (assign continue (label ev-cond-decide))
  (goto (label eval-dispatch)) ; 次の条件式を評価
ev-cond-decide
  (restore env)
  (restore unev) ; unev1
  (restore exp) ; exp2
  (test (op true?) (reg val)) ; 条件にヒットしたか
  (branch (label ev-cond-consequent))
  (assign unev (op cdr) (reg unev)) ; unev2 未評価の条件
  (goto (label ev-cond-loop))
ev-cond-consequent
  (assign unev (op cond-actions) (reg exp)) ; unev3 ヒットした条件の命令部
  (goto (label ev-sequence))
ev-cond-else-clause
  (assign unev (op cond-actions) (reg exp)) ; unev4 else節の命令部
  (goto (label ev-sequence))
ev-cond-null
  (restore continue)
  (assign val (const #f))
  (goto (reg continue)
```

# 5.25

# 5.26

print-stack-statistics に対応した評価器は 5.26.rkt

実行結果

```

;;; EC-Eval input:
(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

(total-pushes = 3 maximum-depth = 3)
;;; EC-Eval value:
ok

;;; EC-Eval input:
(factorial 3)

(total-pushes = 134 maximum-depth = 10)
;;; EC-Eval value:
6

;;; EC-Eval input:
(factorial 4)

(total-pushes = 169 maximum-depth = 10)
;;; EC-Eval value:
24

;;; EC-Eval input:
(factorial 5)

(total-pushes = 204 maximum-depth = 10)
;;; EC-Eval value:
120

;;; EC-Eval input:
(factorial 6)

(total-pushes = 239 maximum-depth = 10)
;;; EC-Eval value:
720

;;; EC-Eval input:
(factorial 10)

(total-pushes = 379 maximum-depth = 10)
;;; EC-Eval value:
3628800

```

## a
最大深さ(maximum-depth) は 常に10

## b
（数学ダメなのでカンニング）
```
35n + 29
```

# 5.27

実行結果

```
;;; EC-Eval input:
(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))

(total-pushes = 3 maximum-depth = 3)
;;; EC-Eval value:
ok

;;; EC-Eval input:
(factorial 3)

(total-pushes = 80 maximum-depth = 18)
;;; EC-Eval value:
6

;;; EC-Eval input:
(factorial 4
)
(total-pushes = 112 maximum-depth = 23)
;;; EC-Eval value:
24

;;; EC-Eval input:
(factorial 5
)
(total-pushes = 144 maximum-depth = 28)
;;; EC-Eval value:
120

;;; EC-Eval input:
(factorial 6
)
(total-pushes = 176 maximum-depth = 33)
;;; EC-Eval value:
720

;;; EC-Eval input:
(factorial 10
)
(total-pushes = 304 maximum-depth = 53)
;;; EC-Eval value:
3628800
```

プッシュ回数
```
32n-16
```

最大深さ
```
5n+3
```

表にすると

| |最大深さ | プッシュ回数 |
|----|----|----|
| 再帰的 | 5n+3 | 32n-16 |
| 反復的 | 10 | 35n+29 |

# 5.28

## factorial 反復

maximum-depth が線形増加している。

```
;;; EC-Eval input:
(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

(total-pushes = 3 maximum-depth = 3)
;;; EC-Eval value:
ok

;;; EC-Eval input:
(factorial 3)

(total-pushes = 144 maximum-depth = 23)
;;; EC-Eval value:
6

;;; EC-Eval input:
(factorial 4)

(total-pushes = 181 maximum-depth = 26)
;;; EC-Eval value:
24

;;; EC-Eval input:
(factorial 5)

(total-pushes = 218 maximum-depth = 29)
;;; EC-Eval value:
120

;;; EC-Eval input:
(factorial 6)

(total-pushes = 255 maximum-depth = 32)
;;; EC-Eval value:
720

;;; EC-Eval input:
(factorial 10)

(total-pushes = 403 maximum-depth = 44)
;;; EC-Eval value:
3628800
```


## factorial 再帰

こちらは元々線形増加。変化なし。

```
;;; EC-Eval input:
(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))

(total-pushes = 3 maximum-depth = 3)
;;; EC-Eval value:
ok

;;; EC-Eval input:
(factorial 3)

(total-pushes = 86 maximum-depth = 27)
;;; EC-Eval value:
6

;;; EC-Eval input:
(factorial 4
)
(total-pushes = 120 maximum-depth = 35)
;;; EC-Eval value:
24

;;; EC-Eval input:
(factorial 5
)
(total-pushes = 154 maximum-depth = 43)
;;; EC-Eval value:
120

;;; EC-Eval input:
(factorial 6
)
(total-pushes = 188 maximum-depth = 51)
;;; EC-Eval value:
720

;;; EC-Eval input:
(factorial 10
)
(total-pushes = 324 maximum-depth = 83)
;;; EC-Eval value:
3628800
```

# 5.29
```

;;; EC-Eval input:
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(total-pushes = 3 maximum-depth = 3)
;;; EC-Eval value:
ok

;;; EC-Eval input:
(fib 3)

(total-pushes = 128 maximum-depth = 18)
;;; EC-Eval value:
2

;;; EC-Eval input:
(fib 4)

(total-pushes = 240 maximum-depth = 23)
;;; EC-Eval value:
3

;;; EC-Eval input:
(fib 5)

(total-pushes = 408 maximum-depth = 28)
;;; EC-Eval value:
5

;;; EC-Eval input:
(fib 6)

(total-pushes = 688 maximum-depth = 33)
;;; EC-Eval value:
8

;;; EC-Eval input:
(fib 10)

(total-pushes = 4944 maximum-depth = 53)
;;; EC-Eval value:
55
```

## a
最大深さ
```
5n+3
```

## b
プッシュ数

問題文より
```
S(n) = S(n-1) + S(n-2) + k
```
と仮定すると、
```
k=40
```

また、
```
S(n) = a * Fib(n+1) + b
```
と仮定すると
```
S(n) = 56 * Fib(n-1) - 40
```

# 5.30
相当な仕事と大プロジェクト・・・

## a

題意の「相当な仕事」は、未束縛変数以外にも諸々エラーになり得る処理を見つけて、対応しろ、ということだと思うが、まあ無理なので、未束縛変数だけに絞って解答を作る。

実際は syntax-error なども考えるべき？

現状
```
;;; EC-Eval input:
(x + 1)
. . Unbound variable x
```

schemeのエラーにより、評価期が終わってしまう。

これを、 評価期の中でハンドリングできるようにする。

```
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))
```

現状、 error になっている部分をなんとかする

カンニングを重ねて理解。

lookup-variable-value の中では、束縛の有無を判断し、
出力を以下のいずれかにする。
- ('bound 値)
- ('unbound '())

```
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (cons 'bound (car vals))) ;; changed
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (cons 'unbound '()) ;; changed
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))
```

eceval 側では、帰ってきた bound/unbound のペアを評価し、
 bound なら値を格納、
 unbound ならエラー処理に回すようにする。

```
(define (bounded? v-pair) ;;add
  (and (pair? v-pair) (eq? (car v-pair) 'bound)))

(define (bounded-value v-pair) ;;add
  (cdr v-pair))

...

(define eceval-operations
   ...
   (list 'bounded? bounded?)  ;;add
   (list 'bounded-value bounded-value)))  ;;add

...

ev-variable  ;; changed
  (assign val (op lookup-variable-value) (reg exp) (reg env))
  (test (op bounded?) (reg val))
  (branch (label ev-bounded-value) (reg val))
  (goto (label unbounded))

ev-bounded-value ;;add
  (assign val (op bounded-value) (reg val))
  (goto (reg continue))

unbounded ;;add
  (assign val (const unbounded-variable-error))
  (goto (label signal-error))

```

エラー処理ではメッセージを表示後、処理を継続する。

```
signal-error  ;;add
  (perform (op user-print) (reg val))
  (goto (label read-eval-print-loop))
```

結果
```
;;; EC-Eval input:
(x + 1)
unbounded-variable-error

;;; EC-Eval input:
```

## b

基本手続きのエラーについてもハンドルできるようにするのが目標。

のようだが、パス。
