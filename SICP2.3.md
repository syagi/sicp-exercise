
# 2.53
```
(list 'a 'b 'c)
;=> '(a b c)
(list (list 'george))
;=> '((george))
(cdr '((x1 x2) (y1 y2)))
;=> '((y1 y2))
(cadr '((x1 x2) (y1 y2)))
;=> '(y1 y2)
(pair? (car '(a short list)))
;=> #f
(memq 'red '((red shoes) (blue socks)))
;=> #f
(memq 'red '(red shoes blue socks))
;=> '(red shoes blue socks)
```

# 2.54
```

(define (equal lst1 lst2)
  (cond ((null? lst1 lst2) #t)
        ((eq? (car lst1) (car lst2)) (equal (cdr lst1) (cdr lst2)))
        (else #f)))
```

# 2.55
''hoge は '('hoge)を返す。
つまり、 (quote (quote hoge))

```
(display ''hoge)
;=> 'hoge
```

そのため、(car ''hoge) の返り値は 'quote（記号の')となる。
なお、(cdr ''hoge)は'(hoge) （記号のhoge）となる
```
(cdr ''hoge)
;=> '(hoge)
(cadr ''hoge)
;=> 'hoge
```


# 2.56
```
#lang racket

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (make-product
                          (exponent exp)
                          (make-exponentiation
                            (base exp)
                            (make-sum (exponent exp) '-1)))
                       (deriv (base exp) var)))
        (else
         (error "unknown expression type --DERIV" exp))))


;べき乗
;base^expを (** base exp) と表現
(define (make-exponentiation base exp)
  (cond ((or (=number? base 1) (=number? exp 0)) 1)
        ((=number? exp 0) 0)
        ((=number? base 1) exp)
        ((and (number? exp) (number? base) (expt base exp)))
        (else (list '** base exp))))
(define (exponentiation? e)
  (and (pair? e) (eq? (car e) '**)))
(define (base e) (cadr e))
(define (exponent e) (caddr e))


;数値、変数
(define (=number? exp num)
  (and (number? exp) (= exp num)))
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;加算
(define (sum? a) (and (pair? a) (eq? (car a) '+)))
(define (addend a) (cadr a))
(define (augend a) (caddr a))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

;乗算
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))
(define (product? m) (and (pair? m) (eq? (car m) '*)))
(define (multiplier m) (cadr m))
(define (multiplicand m) (caddr m))

;test
(deriv '(+ x 3) 'x)

(deriv '(* x y) 'x)

(deriv '(* (* x y) (+ x 3)) 'x)

;(x^5-3x+y+2)
(define test-exp1
  (make-sum
    (make-exponentiation 'x '5)
    (make-sum
      (make-product '-3 'x)
      (make-sum 'y '2))))

(deriv test-exp1 'x)
```


# 2.57
```
#lang racket

;加算
;リストを受け取り、和を作る関数
(define (make-sum-list sum-list)
  (define (iter rest result)
    (if (null? rest)
        (cons '+ (reverse result))
        (iter (cdr rest) (cons (car rest) result))))
  (iter sum-list '()))
(define (sum? a) (and (pair? a) (eq? (car a) '+)))
(define (addend a) (cadr a))
(define (augend a)
  (let ((augend (cddr a)))
    (if (> (length augend) 1)
        (make-sum-list augend)
        (car augend))))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (make-sum-list (list a1 a2)))))
;乗算
(define (make-product-list product-list)
  (define (iter rest result)
    (if (null? rest)
        (cons '* (reverse result))
        (iter (cdr rest) (cons (car rest) result))))
  (iter product-list '()))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (make-product-list (list m1 m2)))))
(define (product? m) (and (pair? m) (eq? (car m) '*)))
(define (multiplier m) (cadr m))
(define (multiplicand m)
  (let ((multiplicand (cddr m)))
    (if (> (length multiplicand) 1)
        (make-product-list multiplicand)
        (car multiplicand))))
;べき乗
(define (make-exponentiation base exp)
  (cond ((or (=number? base 1) (=number? exp 0)) 1)
        ((=number? exp 0) 0)
        ((=number? base 1) exp)
        ((and (number? exp) (number? base) (expt base exp)))
        (else (list '** base exp))))
(define (exponentiation? e) (and (pair? e) (eq? (car e) '**)))
(define (base e) (cadr e))
(define (exponent e) (caddr e))
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (make-product
                          (exponent exp)
                          (make-exponentiation
                            (base exp)
                            (make-sum (exponent exp) '-1)))
                       (deriv (base exp) var)))
        (else
         (error "unknown expression type --DERIV" exp))))
;数値、変数
(define (=number? exp num)
  (and (number? exp) (= exp num)))
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

```

テストコード
```
(deriv '(** x 3) 'x)
(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)
(deriv '(* x y (+ x 3)) 'x)
;(x^5-3xy+y+2)
(define test-exp1
  (make-sum-list
   (list
     (make-exponentiation 'x '5)
     (make-product-list '(-3 x y))
     'y
     '2)))
(deriv test-exp1 'x)

# 2.58

## 2.58a

加算、蒸散、べき乗について、
演算子の位置を変えるだけで良い(car, cadrを入れ替える）

```
#lang racket

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (make-product
                          (exponent exp)
                          (make-exponentiation
                            (base exp)
                            (make-sum (exponent exp) '-1)))
                       (deriv (base exp) var)))
        (else
         (error "unknown expression type --DERIV" exp))))


;べき乗
;base^expを (** base exp) と表現
(define (make-exponentiation base exp)
  (cond ((or (=number? base 1) (=number? exp 0)) 1)
        ((=number? exp 0) 0)
        ((=number? base 1) exp)
        ((and (number? exp) (number? base) (expt base exp)))
        (else (list base '** exp))))
(define (exponentiation? e)
  (and (pair? e) (eq? (cadr e) '**)))
(define (base e) (car e))
(define (exponent e) (caddr e))


;数値、変数
(define (=number? exp num)
  (and (number? exp) (= exp num)))
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;加算
(define (sum? a) (and (pair? a) (eq? (cadr a) '+)))
(define (addend a) (car a))
(define (augend a) (caddr a))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

;乗算
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))
(define (product? m) (and (pair? m) (eq? (cadr m) '*)))
(define (multiplier m) (car m))
(define (multiplicand m) (caddr m))

;test
(deriv '(x + 3) 'x)

(deriv '(x * y) 'x)

(deriv '((x * y) * (x + 3)) 'x)

;(x^5-3x+y+2)
(define test-exp1
  (make-sum
    (make-exponentiation 'x '5)
    (make-sum
      (make-product '-3 'x)
      (make-sum 'y '2))))

(deriv test-exp1 'x)
```
## 2.58b

式の解釈
1)左から順に走査して、演算子を探す。
2)演算子が乗算記号なら乗算とみなす。
3)演算子が加算記号なら、乗算記号がないか継続走査。
4)最後まで乗算記号がなかったら加算とみなす。

乗算、加算、共に、最も左に出現した演算子を基準に式を分割する。

なお、結果の出力についてはカッコの省略を行わない。

```
#lang racket

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "unknown expression type --DERIV" exp))))

;式解釈(注目すべき演算子ってなんて言えば良いのだろう・・・）
(define (1st-priori-ope exp)
  (define (iter rest result)
    (if (or (not (pair? rest)) (eq? result '*))
        result
        (iter
          (cdr rest)
          (cond ((eq? (car rest) '*) '*)
                ((eq? (car rest) '+) '+)
                (else result)))))
  (iter exp '()))

;数値、変数
(define (=number? exp num)
  (and (number? exp) (= exp num)))
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;加算
(define (sum? a) (eq? (1st-priori-ope a) '+))
;左から'+まで
(define (addend a) (car a))
;'+から右まで
(define (augend a)
  (define (iter rest)
    (if (eq? (car rest) '+)
        (cdr rest)
        (iter (cdr rest))))
  (if (= (length a) 3)
      (caddr a)
      (iter a)))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

;乗算
(define (product? m) (eq? (1st-priori-ope m) '*))
;左から'*まで
(define (multiplier m)
  (define (iter rest result)
    (if (eq? (car rest) '*)
        (if (= (length result) 1)
            (car result)
            (reverse result))
        (iter (cdr rest) (cons (car rest) result))))
  (iter m '()))
;'*から右まで
(define (multiplicand m)
  (define (iter rest)
    (if (eq? (car rest) '*)
        (if (= (length rest) 2)
            (cadr rest)
            (cdr rest))
        (iter (cdr rest))))
  (iter m))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

;test
(display "d(x + 3)/dx -> ")
(deriv '(x + 3) 'x)
(display "d(x * y)/dx -> ")
(deriv '(x * y) 'x)
(display "d((x * y) * (x + 3))/dx -> ")
(deriv '((x * y) * (x + 3)) 'x)
(display "d(x + 3 * (x + y + 2))/dx -> ")
(deriv '(x + 3 * (x + y + 2)) 'x)
(display "d(x + 3 * (2 * x + y + 2))/dx -> ")
(deriv '(x + 3 * (2 * x + y + 2)) 'x)
```

# 2.59
```
#lang racket

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
           (union-set (cdr set1) set2))
         (else (cons (car set1) (union-set (cdr set1) set2)))))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

;test
(define s1 '(a b c d))
(define s2 '(c d e f))

(union-set s1 s2)

```

# 2.60
element-of-set, intersection-set は変わらない。
adjoin-set, union-setは
```
(define (adjoin-set2 x set) (cons x set))
(define (union-set2 set1 set2) (append set1 set2))
```
となる。

効率は、element-of-set, intersection-setは落ち、
リストの要素数m, 正味の要素数n (m>n)とすると、O(m),O(m^2)になる。

adjoin-set, union-setは改善する。
リスト内容の参照が不要となるので、共にO(1)となる。

よって、要素追加が要素参照よりも多い場合に有用といえる。


# 2.61

```
(define (adjoin-set3 x set)
  (cond ((null? set) (list x))
        ((< (car set) x )
          (cons (car set) (adjoin-set3 x (cdr set))))
        ((= (car set) x) set)
        ((< x (car set))
          (cons x set))))
```

リストに要素が存在している場合、elements-of-set同様に半分のステップ数で発見できる。


# 2.62
```
(define (union-set3 set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((< (car set1) (car set2))
          (cons (car set1) (union-set3 (cdr set1) set2)))
        ((= (car set1) (car set2))
          (cons (car set1) (union-set3 (cdr set1) (cdr set2))))
        ((> (car set1) (car set2))
          (cons (car set2) (union-set3 set1 (cdr set2))))))
```

# 2.63
## a
同じリストを出力する
'(1 3 5 7 9 11)
## b
後者のほうがステップ数の増加が遅い。
前者はappend処理(O(n))がある分ステップ数が増加するため。

# 2.64

## a
（「明快で簡潔」のレベル感がわからないけれど。。。）
まず、左の部分器を作るために再帰。
左の部分木に含まれなかった要素を根(entry)に。
残りの要素から右部分木を作るために再帰。
最後に、根と左右の部分木をconsで結合して木を作る。

```
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
		; 左の部分木を作る
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            ; 根を決める
			(let ((this-entry (car non-left-elts))
                  ; 右の部分木を作る
				  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                ; 根に左右の部分木を結合する
				(cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))
```

 (list 1 3 5 7 9 11) から生成される木は以下
```
(list->tree (list 1 3 5 7 9 11))
; => '(5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))
```

## b
左右の木の要素数はそれぞれ n/2 となる。
2T(n/2) なので、 O(n)

# 2.65
```
#lang Racket

(define (union-set4 set1 set2)
  (list->tree
    (union-set3 (tree->list-2 set1)
                (tree->list-2 set2))))

(define (intersection-set4 set1 set2)
  (list->tree
    (intersection-set3 (tree->list-2 set1)
                       (tree->list-2 set2))))

(define (union-set3 set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((< (car set1) (car set2))
          (cons (car set1) (union-set3 (cdr set1) set2)))
        ((= (car set1) (car set2))
          (cons (car set1) (union-set3 (cdr set1) (cdr set2))))
        ((> (car set1) (car set2))
          (cons (car set2) (union-set3 set1 (cdr set2))))))


(define (intersection-set3 set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set3 (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set3 (cdr set1) set2))
              ((< x2 x1)
               (intersection-set3 set1 (cdr set2)))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))


(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))


(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right) (list entry left right))

```

# 2.66

```
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))
```


# 2.67
'(A D A B B C A)

# 2.68
```
#lang racket

(define (encode-symbol symbol tree)
  (define (element-of-branch? branch)
    (if (leaf? branch)
        (equal? symbol (symbol-leaf branch))
        (element-of-set? symbol (symbols branch))))
  (let ((left (left-branch tree))
        (right (right-branch tree)))
    (cond ((element-of-branch? left)
           (if (leaf? left)
               '(0)
               (cons 0 (encode-symbol symbol left))))
          ((element-of-branch? right)
           (if (leaf? right)
               '(1)
               (cons 1 (encode-symbol symbol right))))
          (else
           (error "bad symbol"symbol)))))

;既存コード
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (element-of-set? x set)
   (cond ((null? set) false)
         ((equal? x (car set)) true)
         (else (element-of-set? x (cdr set)))))
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;動作確認
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(encode (decode sample-message sample-tree) sample-tree)
```

# 2.69
```
#lang racket

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge set)
  (if (<= (length set) 1)
      set
      (let ((left (car set))
            (right (cadr set)))
        (successive-merge
          (adjoin-set
            (make-code-tree left right)
            (cddr set))))))

;既存コード
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set
           (make-leaf (car pair) (cadr pair))
           (make-leaf-set (cdr pairs))))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

;test
(generate-huffman-tree '((C 1)  (D 1)  (E 1)  (F 1)  (G 1)  (H 1) (B 3) (A 8)))
```

# 2.70

元の曲？は36ワード。
エンコード結果は 84 bit
固定長符号の場合は、8種のワードがあるので1ワードあたり3bit必要。
よって 36*8=108ワード
ハフマン符号の方がより少ないビット数で符号化できることがわかる。

# 2.71
n=5
```
'((((((leaf a 1)
      (leaf b 2)
      (a b) 3)
     (leaf c 4)
     (a b c) 7)
    (leaf d 8)
    (a b c d) 15)
  (leaf e 16)
  (a b c d e) 31))
```

n=10
```
'(((((((((((leaf a 1)
           (leaf b 2)
           (a b) 3)
          (leaf c 4)
          (a b c) 7)
         (leaf d 8)
         (a b c d) 15)
        (leaf e 16)
        (a b c d e) 31)
       (leaf f 32)
       (a b c d e f) 63)
      (leaf g 64)
      (a b c d e f g) 127)
     (leaf h 128)
     (a b c d e f g h) 255)
    (leaf i 256)
    (a b c d e f g h i) 511)
  (leaf j 512)
  (a b c d e f g h i j) 1023))
```

一般のnでは、n-1

# 2.72
2.71のような出現頻度の場合、
最高頻度の記号は O(n)　（木の探索1 * シンボルの探索n）
最低頻度の記号は O(n^2)　（木の探索n * シンボルの探索n)

問題後半の、「アルファベットの最高頻度と最低頻度の符号」の
問題設定がよくわからないが、
2.71の出現頻度（最低:1 最大:2^25）と仮定できるなら、上の通り
仮に
http://www7.plala.or.jp/dvorakjp/hinshutu.htm
こういう出現率を考慮しろと言うことだとしたらわからない。。。
（最高：e: 11.40%, 最低：z 0.07%）
