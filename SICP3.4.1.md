# 3.38

## 3.38a

1. Peter => Paul  => Mary 45
1. Peter => Mary  => Paul 35
1. Paul  => Peter => Mary 45
1. Paul  => Mary  => Peter 50
1. Mary  => Peter => Paul 40
1. Mary  => Paul  => Peter 40

## 3.38b
以下のパターンが考えられる
* 並列実行により、いずれか1人の残高反映が失われる
* 同いずれか二人の残高反映が失われる

( ) が失われた処理
1. Peter => Paul  (Mary)  90
1. Peter => Mary  (Paul)  55
1. Paul  => Peter (Mary)  90
1. Paul  => Mary  (Peter) 40
1. Mary  => Peter (Paul)  60
1. Mary  => Paul  (Peter) 30
1. Peter (Paul)  (Mary) 110
1. Paul  (Peter) (Mary) 80
1. Mary  (Peter) (Mary) 50

# 3.39

起こりえるパターンは
1. (* 10 10) => set! 100 => (+ 100 1) => set! x 101 101
1. (* 10 10) => (+ 10 1) => set! 100 => set! x 11 11
1. (* 10 10) => (+ 10 1) => set! x 11 => set! 100 101
1. (+ 10 1) => set! x 11 => (* 11 11) => set! 121 121

# 4.40
1. (* 10 10) => set 100 => (* 100 100 100) => set 1000000  1000000
1. (* 10 10) => (* 10 10 10) => set 100 => set 1000  1000
1. (* 10 10) => (* 10 10 10) => set 1000 => set 100  100
1. (* 10 10 10) => set 1000 => (* 1000 1000) => set 1000000  1000000
1. (* 10 10 10) => (* 10 10) => set 1000 => set 100  100
1. (* 10 10 10) => (* 10 10) => set 100 => set 1000  1000

なお、serializeすると、1000000 だけになる。

# 3.41
不要。
balance は、処理が完了するまでは処理前の値を返す。
balance 自体は副作用を起こさないので、同時アクセスによる影響は無い。

# 3.42
安全かつ、差分はないように見える。
時間の浪費？という観点で言うと、make-accountの時点で serializeするので、
確かにdispatchの時点での処理時間は減るかもしれない。

# 3.43
手書き

# 3.44
Louisは誤っている。
送金の場合は、 differenceを計算する必要が無いので、3.43のような問題は発生しない

# 3.45
serialized-exchangeを行う際
```
(serializer1 (serializer2
  ((account1 'withdraw) difference)
  ((account2 'deposit) difference)))
=>
(serializer1 (serializer2
  ((serializer1 (withdraw account1 amount))
  (serializer2 (deposit account2 amount)))))
```
serializer1の中にserializer1があるので、この処理は永遠に終わらない

# 3.46
両方のプロセスが mutexを獲得できてしまう

# 3.47
## a
mutexを使った実装。
acquire されたら countを減らす。
count が 0 になるまでは acquireを許容。
release されたら countを増やす。
```
(define (make-semaphore)
  (let ((count n)
        (mutex1 make-mutex))
       (define (the-semaphore m)
         (cond
           ((eq m 'aquire)
              (mutex1 'aquire)
              (if (> count 0)
                  (begin
                    (set! count (- count 1))
                    (muxtex1 'release))
                  (begin
                    (mutex1 'release)
                    (the-semaphore m))))
           ((eq m 'release)
              (mutex1 'aquire)
              (set! count (+ count 1))
              (mutex1 'release))))
    the-semaphore))
```

## b
方針は同じ.
ただし、 test-and-set は 排他を採れたときにfalseを返すので分岐の順序が変わる
```
(define (make-semaphore)
  (let ((count n)
        (cell (list false)))
       (define (the-semaphore m)
         (cond
           ((eq m 'aquire)
              (if (test-and-set! cell)
                  (the-semaphore m)
                  (begin
                    (if (> count 0)
                      (begin
                        (set! count (- count 1))
                        (clear! cell))
                      (the-semaphore m)))))
           ((eq m 'release)
              (set! count (+ count 1)))))
    the-semaphore))
```


# 3.48
デッドロックが起きるのは、二者間で　保護した資源と　保護しようとした資源とが
相互に入れ替わってしまった場合である。
本文中で言うと、Peterのa1, Paulのa2 が保護した資源。Peterのa2とPaulのa1が保護しようとした資源。
この入れ替わりが発生しないようにするためには、保護する順番を全員が共通化する必要がある。
本文中で言うと、二人ともa1から保護しに行けば、デッドロックは発生し得ない。

```
#lang racket

(define (make-account balance id)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ((eq? m 'balance) balance)
            ((eq? m 'id) id)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))


(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    (if (< (account1 'id) (account2 'id))
        ((serializer1 (serializer2 exchange)) account1 account2)
        ((serializer2 (serializer1 exchange)) account2 account1)
        )))
```

# 3.49
先行して保護した共有資源の処理結果に応じて、次に保護すべき資源が決まるような処理。
例）
