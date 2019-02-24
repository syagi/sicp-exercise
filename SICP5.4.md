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
