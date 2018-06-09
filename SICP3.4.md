
# 3.28

```
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)
```

# 3.29

OR を and-gate と inverter で実現すると

aをa1, bをb1, fをoutputと見なせば
```
(define (or-gate a1 b1 output)
  (let ((c make-wire)
        (d make-wire)
        (e make-wire))
    (inverter a1 c)
    (inverter b1 d)
    (and-gate c d e)
    (inverter e output)
    'ok))
```

遅延時間は
 inverter-dalay*2 + and-delay*1

# 3.30

full-adderの定義
```
(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))
```

これを使って
```
; aとbは同じ桁と仮定
; aとbは(a1,a2,...,an) のような順序で格納されたリスト
(define (ripple-carry-adder a b s c)
  (let ((c-tmp make-wire))
    (if (null? (cdr a))
        (set-signal! c-tmp 0) ;最下位桁のcarry
        (ripple-carry-adder (cdr a) (cdr b) (cdr s) c-tmp))
    (full-adder (car a) (car b) c-tmp (car s) c)))
```
全体の遅延は
 full-adder-delay * n

全加算器1段の遅延は
half-adder-delay * 2 + or-delay

半加算器の遅延は
and-gate-delay * 2 + or-gate-delay * 1 + inverter-daly

よって、

and-gate-dlay * 4n + or-gate-delay * 3n + inverter-dlay * 2n


# 3.31

本文の実装
```
(define (accept-action-procedure! proc)
  (set! action-procedures (cons proc action-procedures))
  (proc))
```

問題の実装
```
(define (accept-action-procedure! proc)
  (set! action-procedures (cons proc action-procedures)))
```

(proc)の実行有無で何が変わるか。

半加算器の例
```
(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))
```

全部追う時間が無いのでカンニング
http://labs.timedia.co.jp/2014/12/sicp-ex-3.31-ex-3.32.html

ポイントは、 set時に初期値を正しく評価できるか。
単にsetした値をそのまま使うのであれば、procなしでも問題ない。
しかし、invertを含む場合、初期値を反転して評価する必要がある。
そのため、procを走らせる必要がある。

# 3.32
キューイングされた処理をFIFOで行わなければならない理由。
先に入ったイベントに後の処理が依存していれば、当然先のイベントを評価した後で無いと、
依存する後発処理は正しく評価できない。という話かと。。。

問題文
```
In particular, trace the behavior of an and-gate
 whose inputs change from 0,1 to 1,0 in the same segment
 and say how the behavior would differ
 if we stored a segment's procedures in an ordinary list,
 adding and removing procedures only at the front (last in, first out).
```

and-gateのinputが 0,1 -> 1,0 と変わった場合
FIFO:
初期時点 (a=0, b=1) -> 1 and 0 -> 0
a変更   (a=1, b=1) -> 1 and 1 -> 1
b変更　　(a=1, b=0) -> 1 and 0 -> 0

LIFO:
初期時点 (a=0, b=1) -> 1 and 0 -> 0
b変更　　(a=0, b=0) -> 1 and 0 -> 0
a変更   (a=1, b=0) -> 1 and 0 -> 0
