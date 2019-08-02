# 5.31

saveとrestore = 退避と回復

それぞれの遅延回復を以下に分類。
1. 演算子の評価前後に env レジスタを退避回復
2. (最後のものを除いて)各被演算子の前後で env を退避回復
3. 被演算子の前後で argl を退避回復
4. 被演算子列の評価の前後でprocを退避回復

ただし、 演算子、被演算子が変数または自己評価式の場合は、遅延回復は不要。


```
(f 'x 'y)
```
退避なし.

コンパイル結果
```
> (parse-compiled-code
  (compile
    '(f 'x 'y)
    'val
    'next))

(env)
(env proc argl continue val)
  (assign proc (op lookup-variable-value) (const f) (reg env))
  (assign val (const y))
  (assign argl (op list) (reg val))
  (assign val (const x))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch38))
compiled-branch39
  (assign continue (label after-call40))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch38
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call40
```
不要な save/restoreはない


```
((f) 'x 'y)
```
退避なし.

```
> (parse-compiled-code
  (compile
    '((f) 'x 'y)
    'val
    'next))

(env)
(env proc argl continue val)
  (assign proc (op lookup-variable-value) (const f) (reg env))
  (assign argl (const ()))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch41))
compiled-branch42
  (assign continue (label proc-return44))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
proc-return44
  (assign proc (reg val))
  (goto (label after-call43))
primitive-branch41
  (assign proc (op apply-primitive-procedure) (reg proc) (reg argl))
after-call43
  (assign val (const y))
  (assign argl (op list) (reg val))
  (assign val (const x))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch45))
compiled-branch46
  (assign continue (label after-call47))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch45
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call47
```
不要な save/restoreはない


```
(f (g 'x) y)
```

 (f (g 'x) y) の被演算子 (g 'x) 評価前後に argl, proc の退避回復が必要

コンパイル結果
```
> (parse-compiled-code
  (compile
    '(f (g 'x) y)
    'val
    'next))

(env)
(env proc argl continue val)
  (assign proc (op lookup-variable-value) (const f) (reg env))
  (save proc) ;; 退避
  (assign val (op lookup-variable-value) (const y) (reg env))
  (assign argl (op list) (reg val))
  (save argl) ;; 退避
  (assign proc (op lookup-variable-value) (const g) (reg env))
  (assign val (const x))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch48))
compiled-branch49
  (assign continue (label after-call50))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch48
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call50
  (restore argl) ;; 回復
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc) ;; 回復
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch51))
compiled-branch52
  (assign continue (label after-call53))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch51
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call53
```
最低限の退避回復のみ


```
(f (g 'x) 'y)
```
(f (g 'x) 'y) の被演算子 (g 'x) 評価前後に argl, proc を退避回復

コンパイル結果
```
> (parse-compiled-code
  (compile
    '(f (g 'x) 'y)
    'val
    'next))

(env)
(env proc argl continue val)
  (assign proc (op lookup-variable-value) (const f) (reg env))
  (save proc) ;; 退避
  (assign val (const y))
  (assign argl (op list) (reg val))
  (save argl) ;; 退避
  (assign proc (op lookup-variable-value) (const g) (reg env))
  (assign val (const x))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch54))
compiled-branch55
  (assign continue (label after-call56))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch54
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call56
  (restore argl) ;; 回復
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc) ;; 回復
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch57))
compiled-branch58
  (assign continue (label after-call59))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch57
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call59
```

最低限の退避回復のみ


# 5.32

演算子が記号である式を最適化するようにする。

演算子が記号の場合は、 env/unevを退避せず、 ev-appl-symbol-operator に分岐。

その後 ev-appl-did-symbol-operator に入り、  
env/unevの回復なしに ev-appl-did-operator と同じ処理を行う。

```
ev-application
  (save continue)
  (assign unev (op operands) (reg exp))
  (assign exp (op operator) (reg exp))
  (test (op variable?) (reg exp)) ;; add
  (branch (label ev-appl-symbol-operator)) ;; add
  (save env)
  (save unev)
  (assign continue (label ev-appl-did-operator))
  (goto (label eval-dispatch))

ev-appl-symbol-operator ;; add
  (assgin continue (label ev-appl-did-symbol-operator))
  (goto (label eval-dispatch))

ev-appl-did-operator
  (restore unev)
  (restore env)

ev-appl-did-symbol-operator ;; add
  (assign argl (op empty-arglist))
  (assgin proc (reg val))
  (test (op no-operands?) (reg unev))
  (branch (label apply-dispatch))
  (save proc)
  (goto (label ev-appl-operand-loop))
```

# 5.33
本文の factorial
```
> (parse-compiled-code
 (compile
   '(define (factorial n)
       (if (= n 1)
           1
           (* (factorial (- n 1)) n)))
    'val
    'next))
```


問題文の factorial
```
> (parse-compiled-code
 (compile
   '(define (factorial-alt n)
      (if (= n 1)
          1
          (* n (factorial-alt (- n 1)))))
    'val
    'next))
```

コード上の差分は、かけ算の 引数順序として、 factorial(alt)と n のどちらを先にするか.

それぞれの翻訳結果は factorial.rktとfactorial-alt.rkt

ラベル名を統一してからdiffを取ると
```
35,38c35,36
;; 本文
<   (assign val (op lookup-variable-value) (const n) (reg env))
<   (assign argl (op list) (reg val))
<   (save argl)
<   (assign proc (op lookup-variable-value) (const factorial) (reg env))
---
:: 問題文
>   (save env)
>   (assign proc (op lookup-variable-value) (const factorial-alt) (reg env))

65c63,65
;; 本文
<   (restore argl)
---
;; 問題文
>   (assign argl (op list) (reg val))
>   (restore env)
>   (assign val (op lookup-variable-value) (const n) (reg env))
```

本文の実装は、 先に n が評価されるので、 argl の待避回復が行われる.  

問題文の実装は、先に (factorial-alt (- n 1)) が評価されるので、 env の退避回復が必要となる。

効率の観点で言うと、 save と restore の回数は（対象は違えど）同じなので、大差ないように見える。

# 5.34

```
> (parse-compiled-code
 (compile
    '(define (factorial n)
      (define (iter product counter)
        (if (> counter n)
            product
            (iter (* counter product)
                  (+ counter 1))))
      (iter 1 1))
    'val
    'next))

    (env)
    (val)
      (assign val (op make-compiled-procedure) (label entry21) (reg env))
      (goto (label after-lambda22))
    entry21
      (assign env (op compiled-procedure-env) (reg proc))
      (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
      (assign val (op make-compiled-procedure) (label entry23) (reg env))
      (goto (label after-lambda24))
    entry23
      (assign env (op compiled-procedure-env) (reg proc))
      (assign env (op extend-environment) (const (product counter)) (reg argl) (reg env))
      (save continue)
      (save env)
      ;; (> counter n)
      (assign proc (op lookup-variable-value) (const >) (reg env))
      (assign val (op lookup-variable-value) (const n) (reg env))
      (assign argl (op list) (reg val))
      (assign val (op lookup-variable-value) (const counter) (reg env))
      (assign argl (op cons) (reg val) (reg argl))
      (test (op primitive-procedure?) (reg proc))
      (branch (label primitive-branch28))
    compiled-branch29
      (assign continue (label after-call30))
      (assign val (op compiled-procedure-entry) (reg proc))
      (goto (reg val))
    primitive-branch28
      (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
    after-call30
      (restore env)
      (restore continue)
      (test (op false?) (reg val))
      (branch (label false-branch26))
    true-branch25
      (assign val (op lookup-variable-value) (const product) (reg env))
      (goto (reg continue))

    false-branch26
    ;;  (iter (* counter product)
    ;;      (+ counter 1)))) の計算
      (assign proc (op lookup-variable-value) (const iter) (reg env))
      (save continue)
      (save proc) ;; iter 退避
      (save env)  ;; 環境退避*
      ;; (+ counter 1)
      (assign proc (op lookup-variable-value) (const +) (reg env))
      (assign val (const 1))
      (assign argl (op list) (reg val))
      (assign val (op lookup-variable-value) (const counter) (reg env))
      (assign argl (op cons) (reg val) (reg argl))
      (test (op primitive-procedure?) (reg proc))
      (branch (label primitive-branch34))
    compiled-branch35
      (assign continue (label after-call36))
      (assign val (op compiled-procedure-entry) (reg proc))
      (goto (reg val))
    primitive-branch34
      (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
    after-call36
      (assign argl (op list) (reg val))
      (restore env) ;; 環境回復*
      (save argl)   ;; 引数リストの退避*
      ;; (* counter product)
      (assign proc (op lookup-variable-value) (const *) (reg env))
      (assign val (op lookup-variable-value) (const product) (reg env))
      (assign argl (op list) (reg val))
      (assign val (op lookup-variable-value) (const counter) (reg env))
      (assign argl (op cons) (reg val) (reg argl))
      (test (op primitive-procedure?) (reg proc))
      (branch (label primitive-branch31))
    compiled-branch32
      (assign continue (label after-call33))
      (assign val (op compiled-procedure-entry) (reg proc))
      (goto (reg val))
    primitive-branch31
      (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
    after-call33
      (restore argl)  ;; 引数リスト回復*
      (assign argl (op cons) (reg val) (reg argl))
      (restore proc)  ;; iter 回復
      (restore continue)
      ;; iter
      (test (op primitive-procedure?) (reg proc))
      (branch (label primitive-branch37))
    compiled-branch38
      (assign val (op compiled-procedure-entry) (reg proc))
      (goto (reg val))
    primitive-branch37
      (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
      (goto (reg continue))
    after-call39
    after-if27
    after-lambda24
      (perform (op define-variable!) (const iter) (reg val) (reg env))
      (assign val (const ok))
      ;; (iter 1 1)
      (assign proc (op lookup-variable-value) (const iter) (reg env))
      (assign val (const 1))
      (assign argl (op list) (reg val))
      (assign val (const 1))
      (assign argl (op cons) (reg val) (reg argl))
      (test (op primitive-procedure?) (reg proc))
      (branch (label primitive-branch40))
    compiled-branch41
      (assign val (op compiled-procedure-entry) (reg proc))
      (goto (reg val))
    primitive-branch40
      (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
      (goto (reg continue))
    after-call42
    after-lambda22
      (perform (op define-variable!) (const factorial) (reg val) (reg env))
      (assign val (const ok))
```

コメントで * をつけた部分で退避回復が完結しているので、スタックの深さが一定になる.


# 5.35
```
(define (f x) (+ x (g (+ x 2))))
```

元の翻訳結果
```
  (assign val (op make-compiled-procedure) (label entry16)
                                           (reg env))
  (goto (label after-lambda15))
entry16
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env
          (op extend-environment) (const (x)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const +) (reg env))
  (save continue)
  (save proc) ;; 演算子 +
  (save env)  
  (assign proc (op lookup-variable-value) (const g) (reg env))
  (save proc) ;; 演算子 g
  ;; (+ x 2)
  (assign proc (op lookup-variable-value) (const +) (reg env))
  (assign val (const 2))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const x) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch19))
compiled-branch18
  (assign continue (label after-call17))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch19
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call17
  (assign argl (op list) (reg val))
  (restore proc) ;; 演算子 g
  ;; (g (+ x 2))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch22))
compiled-branch21
  (assign continue (label after-call20))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch22
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call20
  (assign argl (op list) (reg val))
  (restore env)
  ;; (+ x (g (+ x 2)))
  (assign val (op lookup-variable-value) (const x) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc) ;; 演算子 +
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch25))
compiled-branch24
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch25
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
after-call23
after-lambda15
  ;; (define (f x)  (+ x (g (+ x 2))))
  (perform (op define-variable!) (const f) (reg val) (reg env))
  (assign val (const ok))
```

# 5.36

現在は　右から左　の順で評価している

順番が決まるのは
```
(define (construct-arglist operand-codes)
```
の中の
```
  (let ((operand-codes (reverse operand-codes)))
```
の部分。

左から右に評価する場合は上記箇所を削除して関連する実装を修正する。

```
(define (construct-arglist operand-codes)
  ;;(let ((operand-codes (reverse operand-codes))) ;; delete
    (if (null? operand-codes)
        (make-instruction-sequence '() '(argl)
         '((assign argl (const ()))))
        (let ((code-to-get-first-arg ;; rename
               (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence '(val) '(argl)
                 '((assign argl (op list) (reg val)))))))
          (if (null? (cdr operand-codes))
              code-to-get-first-arg ;; changed
              (preserving '(env)
               code-to-get-first-arg ;; changed
               (code-to-get-rest-args
                (cdr operand-codes)))))))
(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         (preserving '(argl)
          (car operand-codes)
          (make-instruction-sequence '(val argl) '(argl)
           '((assign val (op list) (reg val)) ;; add
             (assign argl
                (op append) (reg argl) (reg val))))))) ;; chaged cons -> append
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving '(env)
         code-for-next-arg
         (code-to-get-rest-args (cdr operand-codes))))))
```

効率については、　コンパイル時は reverse が無くなる分効率が良くなる。

実行時は、 cons が append に変わるため、効率が落ちる。

# 5.37
```
(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
;;        (if (and (needs-register? seq2 first-reg)  ;; delete
;;                 (modifies-register? seq1 first-reg)) ;; delete
            (preserving (cdr regs)
             (make-instruction-sequence
              (list-union (list first-reg)
                          (registers-needed seq1))
              (list-difference (registers-modified seq1)
                               (list first-reg))
              (append `((save ,first-reg))
                      (statements seq1)
                      `((restore ,first-reg))))
             seq2)
;;            (preserving (cdr regs) seq1 seq2)) ;; delete
        )))
```

動作確認

```
> (parse-compiled-code
  (compile
    '(f 'x 'y)
    'val
    'next))

(env continue)
(env proc argl continue val)
  (save continue)
  (save env)
  (save continue)
  (assign proc (op lookup-variable-value) (const f) (reg env))
  (restore continue)
  (restore env)
  (restore continue)
  (save continue)
  (save proc)
  (save env)
  (save continue)
  (assign val (const y))
  (restore continue)
  (assign argl (op list) (reg val))
  (restore env)
  (save argl)
  (save continue)
  (assign val (const x))
  (restore continue)
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch21))
compiled-branch22
  (assign continue (label after-call23))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch21
  (save continue)
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (restore continue)
after-call23
```

ひたすら save と restore が増えている

# 5.38

https://wat-aro.hatenablog.com/entry/2016/02/08/173201
ここ参考に作ろうとしたが挫折。。。

# 5.39

```
(define (lexical-address-frame-number lexical-address)
  (car lexical-address))

(define (lexical-address-displacement-number lexical-address)
  (cadr lexical-address))

(define (make-lexical-address frame-number displacement-number)
  (list frame-number displacement-number))

(define (lexical-address-lookup lexical-address env)
  (let ((frame-num (lexical-address-frame-number lexical-address))
        (displacement-num (lexical-address-displacement-number lexical-address)))
       (let ((frame (list-ref env frame-num)))
             (let ((value (list-ref (frame-values frame) displacement-num)))
                  (if (eq? value '*unassigned*)
                      (error "Unassigned variable")
                      value)))))

(define (lexical-address-set! lexical-address val env)
  (let ((frame-num (lexical-address-frame-number lexical-address))
        (displacement-num (lexical-address-displacement-number lexical-address)))
       (let ((frame (list-ref env frame-num)))
             (define (iter vrs vls count)
               (cond ((null? vrs)
                      (error "Unbound variable - LEXICAL-ADDRESS-SET!"))
                     ((= count 0)
                      (set-car! vls val))
                     (else
                       (iter (cdr vrs) (cdr vls) (- count 1)))))
             (iter (frame-variables frame) (frame-values frame) displacement-num))))
```

# 5.40

全体は 5.40.rkt

以下の関数に ct-env 引数を追加する。
```
compile
compile-assingment
compile-definition
compile-if
compile-lambda
compile-sequence
compile-application
compile-lambda-body
```

ct-env は compile-labda-body で拡張する
```

(define (compile-lambda-body exp proc-entry ct-env) ;;changed
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
     (make-instruction-sequence '(env proc argl) '(env)
      `(,proc-entry
        (assign env (op compiled-procedure-env) (reg proc))
        (assign env
                (op extend-environment)
                (const ,formals)
                (reg argl)
                (reg env))))
     (compile-sequence (lambda-body exp) 'val 'return (cons formals ct-env))))) ;;拡張
```

# 5.41
```
(define (find-variable var ct-env)
  (define (frame-iter var frames frame-number)
    (if (null? frames)
        'not-found
        (let ((addr (scan-iter var (car frames) frame-number 0)))
             (if (null? addr)
                 (frame-iter var (cdr frames) (+ frame-number 1))
                 addr))))
  (define (scan-iter var frame frame-number displacement-number)
    (if (null? frame)
        '()
        (if (eq? var (car frame))
            (make-lexical-address frame-number displacement-number)
            (scan-iter var (cdr frame) frame-number (+ displacement-number 1)))))
  (frame-iter var ct-env 0))
```

# 5.42
```
(define (compile-variable exp target linkage ct-env) ;; changed
  (let ((addr (find-variable exp ct-env)))
    (end-with-linkage linkage
     (make-instruction-sequence
       '(env)
       (list target)
       (if (eq? addr 'not-found)
           `((assign ,target
                     (op lookup-variable-value)
                     (const ,exp)
                     (reg env)))
           `((assign ,target
                     (op lookup-variable-value)
                     (const ,addr)
                     (reg env))))))))
```

```
(define (compile-assignment exp target linkage ct-env)
  (let ((var (assignment-variable exp))
        (get-value-code
         (compile (assignment-value exp) 'val 'next ct-env)))
    (let ((addr (find-variable var ct-env))) ;; changed
      (end-with-linkage
        linkage
        (preserving
          '(env)
          get-value-code
          (make-instruction-sequence
             '(env val)
             (list target)
             (if (eq? addr 'not-found)
                 `((perform (op set-variable-value!)
                            (const ,var)
                            (reg val)
                            (reg env))
                   (assign ,target (const ok)))
                 `((perform (op lexical-address-set!)
                                 (const ,addr)
                                 (reg val)
                                 (reg env))
                   (assign ,target (const ok))))))))))
```

# 5.43

```
(define (compile-lambda-body exp proc-entry ct-env)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
     (make-instruction-sequence '(env proc argl) '(env)
      `(,proc-entry
        (assign env (op compiled-procedure-env) (reg proc))
        (assign env
                (op extend-environment)
                (const ,formals)
                (reg argl)
                (reg env))))
     (compile-sequence
      (scan-out-defines (lambda-body exp)) ;; changed
      'val
      'return
      (cons formals ct-env)))))
```

# 5.44
