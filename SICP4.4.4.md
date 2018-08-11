# 4.70

元の実装
```
(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
       (set! THE-ASSERTIONS
             (cons-stream assertion old-assertions))
       'ok))
```

問題文の実装
```
(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (set! THE-ASSERTIONS
        (cons-stream assertion THE-ASSERTIONS))
  'ok)
```

違いは、 old-assersions を let で束縛するかどうか。

cons-streamは、carを評価したあと、 cdrを遅延評価して consする。
(3.5.1節、 (cons-stream a b) は (cons a (delay b)) と等価)

そのため、THE-ASSERTIONSは評価されないまま、新しいset!が行われてしまう。
結果、(car THE-ASSERTIONS)で期待した値が返ってこない。


本文中では、 letで古い値を他の変数に束縛し、改めてTHE-ASSERTIONSに束縛することで、
