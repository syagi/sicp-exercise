# 3.24
key-1とkey-2の関係性を考慮して#t/#fを返す仕様も考えたが、
それは 3.25 に包含されるので、ここではkey-1とkey-2を独立して評価する仕様にする。

```
#lang racket
(require r5rs)

;; same-key? は2つの引数を取り、#t/#fを返す関数
(define (make-table same-key?)
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
    (define (assoc key records)
      (cond ((null? records) false)
        ((same-key? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

;test

; 差が5未満なら一致と見なす
(define (my-same-key? x y)
  (if (> 5 (abs (- x y)))
      #t
      #f))

(define test-table (make-table my-same-key?))
((test-table 'insert-proc!) 10 10 100)
((test-table 'lookup-proc) 10 10)
((test-table 'lookup-proc) 10 15)
((test-table 'lookup-proc) 6 6)
((test-table 'lookup-proc) 5 10)
```

# 3.25
```
#lang racket
(require r5rs)

;; same-key? はキーのリストを取り、#t/#fを返す関数
;; key-list はキーのリストを格納する
;; 表自体はリスト型のuキーを取る1次元の表　とみなす
(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (lookup key-list)
      (let ((record (assoc key-list (cdr local-table))))
        (if record
            (cdr record)
            false)))
    (define (insert! key-list value)
      (let ((record (assoc key-list (cdr local-table))))
        (if record
            (set-cdr! record value)
            (set-cdr! local-table
                      (cons (cons key-list value)
                            (cdr local-table)))))
      'ok)
    (define (assoc key records)
      (cond ((null? records) false)
        ((same-key? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

;test

; リストが完全一致なら一致と見なす
(define (my-same-key? x y) (equal? x y))

(define test-table (make-table my-same-key?))
((test-table 'insert-proc!) (list 10 10) 100)
((test-table 'insert-proc!) (list 10 10 11) 1100)
((test-table 'lookup-proc) (list 10 10))
((test-table 'lookup-proc) (list 10 10 11))
((test-table 'lookup-proc) (list 10 11))
```

# 2.26
assocが表全体を走査してしまうので、これを二分探索で評価できるようにする。
