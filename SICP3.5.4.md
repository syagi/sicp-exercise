# 3.77
```
#lang racket
(define (integral delayed-integrand initial-value dt)
  (let ((integrand (froce delayed-integrand)))
    (cons-stream initial-value
      (if (stream-null? integrand)
          the-empty-stream
          (integral (stream-cdr integrand)
                    (+ (* dt (stream-car integrand))
                       initial-value)
                    dt)))))
```

# 3.78
```
(define (solve-2nd a b dt y0 dy0)
  (define (y (integral dy) y0 dt))
  (define (dy (integral ddy) dy0 dt))
  (define (ddy (+ (scale-stream dy a)
                  (scale-stream y b))))
  y)
```

# 3.79
```
(define (solve-2nd f dt y0 dy0)
  (define (y (integral dy) y0 dt))
  (define (dy (integral ddy) dy0 dt))
  (define (ddy (stream-map f dy y))))
  y)
```

# 3.80
```
#lang racket
(require (prefix-in strm: racket/stream))

;helper
(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (strm:stream-cons a b))))
(define stream-car strm:stream-first)
(define stream-cdr strm:stream-rest)
(define stream-null? strm:stream-empty?)
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))
(define the-empty-stream strm:empty-stream)
(define (display-stream s) (stream-for-each display-line s))
(define (add-streams s1 s2) (stream-map + s1 s2))
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))
(define (display-stream-head s n)
  (define (iter s n)
    (if (<= n 0)
      'done
      (begin
        (display (stream-car s))
        (newline)
        (iter (stream-cdr s) (- n 1)))))
  (iter s n))
(define (display-line x)
  (newline)
  (display x))

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)

(define (RLC R L C dt)
  (define (rlc vC0 iL0)
    (define vC (integral (delay dvC) vC0 dt))
    (define iL (integral (delay diL) iL0 dt))
    (define dvC (scale-stream iL (/ -1 C)))
    (define diL (add-streams
                  (scale-stream vC (/ 1 L))
                  (scale-stream iL (- (/ R L)))))
    (stream-map (lambda (x y) (cons x y)) vC iL))
  rlc)

(define RLC1 (RLC 1 1 0.2 0.1))

(display-stream-head (RLC1 10 0) 10)
```

結果
```
(10 . 0)
(10 . 1.0)
(9.5 . 1.9)
(8.55 . 2.66)
(7.220000000000001 . 3.249)
(5.5955 . 3.6461)
(3.77245 . 3.84104)
(1.8519299999999999 . 3.834181)
(-0.0651605000000004 . 3.6359559)
(-1.8831384500000004 . 3.2658442599999997)
'done
```

# 3.81
入力として 'generate が来たら乱数を生成。

数値が来たらその数値を使ってresetという動作。

```
(define (rand-stream s seed)
  (define (generate last-val)
    ;lcdとか使うと乱数っぽくなるが、テストを楽にするために簡略化
    (+ last-val 1))
  (define (reset new-seed) new-seed)
  (define (dispatch req last-val)
    (cond ((eq? req 'generate) (generate last-val))
          ((number? req) (reset req))
          (else (error "unknown message" req))))
  (define (iter req-stream last-val)
    (let ((new-val (dispatch (car req-stream) last-val)))
      (cons-stream
        new-val
        (iter (stream-cdr req-stream) new-val))))
  (iter s seed))

;test
(define test-stream (list 'generate 5 'generate 'generate 10 20 'generate))

(display-stream-head (rand-stream test-stream 1) 7)
```

結果。（実行時は3.80と同じhelperを追加する）
```
2
5
6
7
10
20
21
'done
```

# 3.82
