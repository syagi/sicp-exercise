# 2.73

## a
else節では、 operator 抽出した演算子に対応した演算処理関数を呼び出し、
operands で抽出した式に適用している。

number, valiableについては、 operator で演算子を抽出できないので吸収できない。

## b
## c
get, put が標準関数にないので、3.3.3 の実装を借用した。

```
#lang planet neil/sicp

;記号微分
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)                                            var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (=number? exp num)
  (and (number? exp) (= exp num)))
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;表の実装(3.3.3 から流用)
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))
(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))
(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

; ここから回答
; b
(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-product m1 m2) (list '* m1 m2))
;;sum
(define (install-sum-package)
  (define (addend s) (car s))
  (define (augend s) (cadr s))
  (put 'deriv
       '+
       (lambda (exp var)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))))

;;product
(define (install-product-package)
  (define (multiplier p) (car p))
  (define (multiplicand p) (cadr p))
  (put 'deriv
       '*
       (lambda (exp var)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))))
; c
;;exponent
(define (install-exponent-package)
  (define (make-exponentiation base exp)
    (cond ((or (=number? base 1) (=number? exp 0)) 1)
          ((=number? exp 0) 0)
          ((=number? base 1) exp)
          ((and (number? exp) (number? base) (expt base exp)))
          (else (list '** base exp))))
  (define (exponentiation? e)
    (cond ((or (pair? e) (eq? (car e) '**)))))
  (define (base e) (car e))
  (define (exponent e) (cadr e))
  (put 'deriv
       '**
       (lambda (exp var)
         (make-product
          (make-product
           (exponent exp)
           (make-exponentiation
            (base exp)
            (make-sum (exponent exp) '-1)))
          (deriv (base exp) var)))))


;test
(install-sum-package)
(install-product-package)
(install-exponent-package)
(get 'deriv '+)
(get 'deriv '*)
(get 'deriv '**)

(deriv 'y 'x)
(deriv '(+ x 1) 'x)
(deriv '(* x 3) 'x)
(deriv '(+ (* x x) (* x 3)) 'x)
(deriv '(+ (** x 4) (** x 3)) 'x)
```

## d
putの順序を入れ替える。  
例）
```
  (put '+
       'deriv
       (lambda (exp var)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))))
```


# 2.74

## a
以下を前提として実装する
・各事業所毎に従業員名から対応するレコードを取得する関数を用意する。
・事業者名と従業員レコード取得関数を紐付けてテーブルに登録する

```
(define (get-record name divison)
  ((get divison 'record) name))
```

## b
以下を前提として実装する
・各事業所毎に従業員レコードから給料を取得する関数を用意する。
・事業者名と給与取得関数を紐付けてテーブルに登録する

```
(define (get-salary name division)
  ((get divison 'salary) ((get-record name division)))
```

## c

```
(define (find-employee-record name divisions)
  (if (null? divisions)
      #f
      (or (get-record name (car divisions))
          (find-employee-record name (cdr divisions)))))
```

## d
そのカイシャの record取得関数と給与取得関数を合併後の名前とひも付けて、テーブルに登録する。
例）
```
  (put 'satiable-company 'record satiable-get-record)
  (put 'satiable-company 'salaly satiable-get-salary)
```

# 2.75
```
#lang planet neil/sicp
(define (make-from-mag-ang r th)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos th)))
          ((eq? op 'imag-part) (* r (sin th)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) th)
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

;test
(define pi 3.14159265359879)
(define complex1 (make-from-mag-ang 2 (/ pi 6)))
(complex1 'real-part)
(complex1 'imag-part)
(complex1 'magnitude)
(complex1 'angle)
```

# 2.76

## 明白な振分けを持つ汎用演算,
### 新しい型
すべての手続について、新しい型タグと対応する処理を実装する
（例）
整数の四則演算に有理数の四則演算を加える場合、
加減乗除それぞれに、有理数を取る場合を実装する。

### 新しい演算
新しい演算処理を実装するだけ。（既存の演算には手を加えない）
（例）
整数、有理数の四則演算にべき乗計算を加える場合、
べき乗計算の関数を実装すれば良い。
（既存の加減乗除関数には影響しない）

## データ主導流
### 新しい型
パッケージを追加する。
（例）
整数の四則演算に有理数の四則演算を加える場合、
有理数のパッケージを実装する。

### 新しい演算
新しい演算を使用するパッケージ全てに、演算処理を追加する。
（例）
整数、有理数の四則演算にべき乗計算を加える場合、
整数パッケージ、有理数パッケージそれぞれにべき乗計算を実装する。

## メッセージパッシング流
### 新しい型
新たなデータオブジェクト構成子を定義する
（例）
整数の四則演算に有理数の四則演算を加える場合、
有理数のデータオブジェクトを生成する関数を実装する

### 新しい演算
既存のデータオブジェクト構成子に、新しい演算メッセージを受ける処理を定義する
（例）
整数、有理数の四則演算にべき乗計算を加える場合、
整数、有理数の構成子それぞれに メッセージ exponentを受けたらべき乗を行う処理を追加する。

## 新しい型が絶えず追加されるシステム
パッケージ追加だけで対応でき、既存処理が影響受けないデータ手動流が適切。
同様に、構成子の追加だけで対応できるメッセージパッシングも適切。

## 新しい演算が絶えず追加されるシステム
演算処理を関数として実装すれば対応でき、既存処理が影響を受けない、明確な振り分けを持つ汎用演算が適切。

