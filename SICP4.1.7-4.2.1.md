# 4.22

問題4.6では、 let->combinationを実装することで実現している。
ここでも同じアプローチを使う。

let->combination は 4.6 で定義済みなので、
analyze手続きに 以下を追記する
```
(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
         ;;(中略)
        ((let? exp) (analyze (let->combination exp))) ;; 追記
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))
```

# 4.23
本文の定義
```
(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))
```

Alyssaの定義
```
(define (analyze-sequence exps)
  (define (execute-sequence procs env)
    (cond ((null? (cdr procs)) ((car procs) env))
          (else ((car procs) env)
                (execute-sequence (cdr procs) env))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (lambda (env) (execute-sequence procs env))))
```

## 並びが一つの場合

### 本文の実装
 loop に評価される rest-procs がnilになる。
そのため、 first-procそのものが返ってくる。

### alyssaの実装
 lambda式     (lambda (env) (execute-sequence procs env))　が返ってくる

## 並びが2つの場合
### 本文の実装
 説明しにくいが、 loopによって 順次 sequectially 手続きが適用された lambda 式が返ってくる
 要は 各式に対して
 (loop (sequentially first-proc (car rest-procs))
       (cdr rest-procs))))
 によって、
 (lambda (env) (proc1 env) (proc2 env)))
 が順次適用された結果が返ってくる。

### alyssaの実装
 1つの場合と同じなので、 式のリスト(procs)が含まれた
 lambda式     (lambda (env) (execute-sequence procs env))　が返ってくる

## 2つの比較
まとめると、 本文の実装は

```
(begin
 <procA>
 <procB>
 <procC>)
```

のような入力に対して、

```
(lambda (env)
  (lambda (env)
    (lambda (env) (<eval-procA> env))
    (<eval-procB> env))
  (<eval-procC> env))
```

という手続きが作られる。


Alyssaの場合は

```
(labmda (env)
  (execute-sequence
    (list
      (lambda (env) (<eval-procA> env))
      (lambda (env) (<eval-procB> env))
      (lambda (env) (<eval-procC> env)))))
```

という手続きになる。
この結果、個々のシーケンスの解釈が実行時に都度行われてしまう。

つまり本文にある
```
lambda式の解析は, 効率に大きな利益をもたらす: lambdaの評価の結果の手続きが多数回作用されようとも, lambda本体は一回だけ解析される.
```
この恩恵が失われてしまうことになる。


# 4.24


# 4.25
停止しない。

作用的順序では n=1の場合にも (factorial (- n 1)) を評価してしまうため、再帰が終わらない。

正規順序の場合、 n=1 のときには　(factorial (- n 1)) が評価されないので、正常動作する。

# 4.26
unless->if を作って実装する
```
; analyze に ((unless? exp) (analyze (unless->if exp))) を追加

(define (unless? exp) (tagged-list? exp 'unless))

;;(unless condition usual-value exceptional-value)
(define (unless-condition exp) (cadr exp))
(define (unless-usual-value exp) (caddr exp))
(define (unless-exceptional-value exp)
  (if (null? (cdddr exp))
      #f ;; exceptional-valueを省略した場合
      (cadddr exp)))

;;(unless condition usual-value exceptional-value)
;;  ->  (if condition exceptional-value usual-value)
(define (unless->if exp)
  (make-if (unless-condition exp)
           (unless-exceptional-value exp)
           (unless-usual-value exp)))
```

# 4.27
```
(define count 0)

(define (id x)
  (set! count (+ count 1))
  x)

(define w (id (id 10)))

;;; L-Eval input:
count
;;; L-Eval value:
1

;;; L-Eval input:
w
;;; L-Eval value:
10

;;; L-Eval input:
count
;;; L-Eval value:
2
```

ちなみに作用的順序の場合は
wを定義した時点で (id (id 10))がすべて評価されるので、 count は最初から2になる


# 4.28


# 4.29

フィボナッチ数の計算

(square (id 10)) の評価

メモ化なし
```
;;; L-Eval input:
(square (id 10))
;;; L-Eval value:
100
;;; L-Eval input:
count
;;; L-Eval value:
2
```

メモ化あり
```
;;; L-Eval input:
(square (id 10))
;;; L-Eval value:
100
;;; L-Eval input:
count
;;; L-Eval value:
1
```

# 4.30
## a
問題中の式
```
(define (for-each proc items)
  (if (null? items)
      'done
      (begin (proc (car items))
             (for-each proc (cdr items)))))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))
```

最初のbeginまで評価すると
```
(begin ((lambda(x) (newline) (display x)) 57)
       (for-each (lambda(x) (newline) (display x))
                 '(321 88)))
```
ここで、元々の評価器なら、newlineがevalされ、displayがforceされる。
つまり、　(改行) 57 と印字される。
以下同じ繰り返しなので、正しく動く。
Benの場合もこの場合は同じ。

## b
```
(define (p1 x)
  (set! x (cons x '(2)))
  x)

(define (p2 x)
  (define (p e)
    e
    x)
  (p (set! x (cons x '(2)))))
```
元々の場合
p1は基本手続きなので遅延無く評価されるため、
(p1 1) は　(1 2) を出力する。

p2の p は複合手続きなので引数(set! x (cons x '(2))が遅延され実行されない。

Ｃｙの場合は、両方評価される。
(p2 1) は (1 2) を出力する。

## c
すべて基本手続きを使っており、元々遅延していないから

## d
好きかどうかと言われても・・・

# 4.31

lazy や lazy-memo を解釈する処理を追加する。
lazyやlazy-memo月居ていた場合にそれぞれ別のタグをつけておいて、
force-itで解釈して処理を分岐する。

実装する気力が無かったのでカンニング。
いくつか実装方針がある模様。
http://sioramen.sub.jp/blog/2008/02/sicp-422.html
では、 仮引数がpair であればその中をみてタグをつける
http://sioramen.sub.jp/blog/2008/02/sicp-422.html
では、procedure文の形を変えて中に保存している。
コードの変更は後者のが少ないが、実装的には前者のがわかりやすい気がした。


# 4.32
ググったところ、 car部も遅延されているので未定義の関数を格納したリストを定義できる　らしい

# 4.33
クオートされた式を評価する機構が無いので、これを追加してやれば良い。
クオートをどう解釈するか一瞬悩んだが、quoted?ってのがあるらしい。

```
;eval に追加
((quoted? exp) (eval-quote exp env))
;前後は略

(define (eval-quote exp env)
  (if (list? (cadr exp))
      (eval (make-quotation-list (cadr exp)) env)
      (cadr exp)))

(define (make-quotaiton-list obj)
   (if (null? obj)
       '()
       (let ((first-list (car obj))
             (rest-list  (cdr obj)))
            (list 'cons
                  (list 'quote first-list)
                  (make-quotation-list rest-list)))))
```

# 3.34
何のことか分からなかったのでカンニング。

http://wat-aro.hatenablog.com/entry/2016/01/10/204424

が、ちょっとついていけなかった。
