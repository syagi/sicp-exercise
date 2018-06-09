# 3.9

# 3.10

# 3.11

# 3.12

# 3.13

# 3.14

# 3.15

# 3.16

# 3.17
~~~~~~
#lang racket
; to use set-car! set-cdr!
(require r5rs)

(define (count-pairs lst)
  (let ((counted '()))
    (define (counter x)
      (if (or (not (pair? x))
              (memq x counted))
          0
          (begin
              (set! counted (cons x counted))
              (+ (counter (car x))
                 (counter (cdr x))
                 1))))
    (counter lst)))


;test

(define x (cons 'a (cons 'b (cons 'c '()))))
(display x)
(count-pairs x) ;3

(define y (cons 'd (cons 'a '())))
(set-car! y (cons 'b (cdr y)))
(display y)
(count-pairs y) ;3

(define z (cons 'a (cons 'b (cons 'c '()))))
(set-car! (cdr z) (cdr (cdr z)))
(set-car! z (cdr z))
(display z)
(count-pairs z) ;3
~~~~~~~~~~~

# 3.18
~~~~~~~~~~~
#lang racket
; to use set-car! set-cdr!
(require r5rs)

(define (check-loop lst)
  (let ((encountered '()))
    (define (checker x)
      (cond ((not (pair? x)) #f)
            ((memq x encountered) #t)
            (else
              (set! encountered (cons x encountered))
              (or (checker (car x))
                  (checker (cdr x))))))
    (checker lst)))


;test

(check-loop '()) ;f

(define x (cons 'a (cons 'b (cons 'c '()))))
(display x)
(check-loop x) ;f

(define y (cons 'd (cons 'a '())))
(set-car! y (cons 'b (cdr y)))
(display y)
(check-loop y) ;t

(define z (cons 'a (cons 'b (cons 'c '()))))
(set-car! (cdr z) (cdr (cdr z)))
(set-car! z (cdr z))
(display z)
(check-loop z) ;t
~~~~~~~~~~~~~

# 3.19
リストが有限なら3.18も有限の空間で動くじゃん。
と思いつつも空気を読んでより効率的な実装にする。

使うのはフロイドの循環検出法。
wikipediaはわかりにくかったが、ココがわかりやすかった。
http://hidekazu.hatenablog.jp/entry/2016/04/01/204153

要は、
ウサギ：cddrで2個ずつ巡る。
亀：cdrで1個ずつ巡る。
という方針

~~~~~~~~~
#lang racket
; to use set-car! set-cdr!
(require r5rs)

(define (smart-check-loop lst)
  (define (checker x y)
    x
    y
    (cond ((or (not (pair? x)) (not (pair? y))) #f)
          ((not (pair? (cdr y))) #f)
          ((or (eq? x y) (eq? x (cdr y))) #t)
          (else
           (checker (cdr x) (cddr y)))))
    (if (pair? lst)
        (checker lst (cdr lst))
        #f))

;test

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(smart-check-loop '()) ;f

(define x (cons 'a (cons 'b (cons 'c '()))))

(smart-check-loop x) ;f
(define y (make-cycle x))
y
(smart-check-loop y) ;t
~~~~~~~~~~~~~~~~

# 3.20

# 3.21
キューのcar（リスト全体）と cdr（リスト末尾）が両方印字されているため、末尾要素が2度挿入されているように見えるだけ。
正しい印字プログラムは
~~~~~~~
#lang racket
(require r5rs)

(define (print-queue queue)
  (display (front-ptr queue)))

;helper
(define (make-queue) (cons '() '()))
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))
(define (empty-queue? queue) (null? (front-ptr queue)))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

;test
(define q1 (make-queue))
(insert-queue! q1 'a)
(display q1)
(newline)
(print-queue q1)

(insert-queue! q1 'b)
(display q1)
(newline)
(print-queue q1)

(delete-queue! q1)
(display q1)
(newline)
(print-queue q1)

(delete-queue! q1)
(display q1)
(newline)
(print-queue q1)
~~~~~~~~~~~~

#3.22

#lang racket
(require r5rs)

(define (make-queue)
  (let ((front-ptr '() )
        (rear-ptr  '() ))
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))
    (define (empty-queue?) (null? front-ptr))
    (define (front)
      (if (empty-queue?)
          (error "empty")
          (car front-ptr)))
    (define (rear)
      (if (empty-queue?)
          (error "empty")
          (car rear-ptr)))
    (define (insert item)
      (let ((new-item (cons item '())))
        (if (empty-queue?)
              (begin
                (set-front-ptr! new-item)
                (set-rear-ptr! new-item)
                (print))
              (begin
                (set-cdr! rear-ptr new-item)
                (set-rear-ptr! new-item)
                (print)))))
    (define (delete)
      (if (empty-queue?)
          (error "empty")
          (set-front-ptr! (cdr front-ptr))))
    (define (print)
      (display front-ptr))
    (define (dispatch m)
      (cond ((eq? m 'insert-queue!) insert)
            ((eq? m 'delete-queue!) (delete))
            ((eq? m 'print-queue)  (print))
            ((eq? m 'front-queue) front)
            ((eq? m 'rear-queue) rear)
            (else (error "undefined " m))))
    dispatch))

;test
~~~~~~~~~~~~~~~
(define q1 (make-queue))
((q1 'insert-queue!) 'a)
((q1 'insert-queue!) 'b)
(q1 'delete-queue!)
(q1 'print-queue)
(q1 'delete-queue!)
(q1 'print-queue)
~~~~~~~~~~~~~

# 3.23

各要素を
（値、（前要素、次要素）
という形に変更した。

~~~~~~~~~~
#lang racket
(require r5rs)

(define (make-queue)
  (let ((front-ptr '() )
        (rear-ptr  '() ))
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))
    (define (empty-queue?) (null? front-ptr))
    (define (front)
      (if (empty-queue?)
          (error "empty")
          (car front-ptr)))
    (define (rear)
      (if (empty-queue?)
          (error "empty")
          (car rear-ptr)))
    (define (prev item)
      (if (null? item)
          '()
          (cadr item)))
    (define (next item)
      (if (null? item)
          '()
          (cddr item)))
    (define (set-prev! item new-prev)
      (if (null? item)
          '()
          (set-cdr! item (cons new-prev (next item)))))
    (define (set-next! item new-next)
      (if (null? item)
          '()
          (set-cdr! item (cons (prev item) new-next))))
    (define (rear-insert item)
      (let ((new-item (cons item (cons rear-ptr '() ))))
        (if (empty-queue?)
              (begin
                (set-front-ptr! new-item)
                (set-rear-ptr! new-item)
                (print))
              (begin
                (set-next! rear-ptr new-item)
                (set-rear-ptr! new-item)
                (print)))))
    (define (front-delete)
      (if (empty-queue?)
          (error "empty")
          (begin
            (set-front-ptr! (next front-ptr))
            (if (null? front-ptr)
                (set-rear-ptr! '())
                (set-prev! front-ptr '()))
            (print))))
    (define (front-insert item)
      (if (empty-queue?)
          (rear-insert item)
          (begin
            (let ((new-item (cons item (cons '() front-ptr))))
              (set-prev! front-ptr new-item)
              (set-front-ptr! new-item))
            (print))))
    (define (rear-delete)
      (if (empty-queue?)
          (error "empty")
          (begin
            (set-rear-ptr! (prev rear-ptr))
            (if (null? rear-ptr)
                (set-front-ptr! '())
                (set-next! rear-ptr '()))
            (print))))
    (define (print)
      (define (iter item)
        (if (null? item)
            (newline)
            (begin
              (display (car item))
              (iter (next item)))))
      (iter front-ptr))
    (define (dispatch m)
      (cond ((eq? m 'rear-insert-queue!) rear-insert)
            ((eq? m 'front-delete-queue!) (front-delete))
            ((eq? m 'front-insert-queue!) front-insert)
            ((eq? m 'rear-delete-queue!) (rear-delete))
            ((eq? m 'print-queue)  (print))
            (else (error "undefined " m))))
    dispatch))

;test
(define q1 (make-queue))
((q1 'rear-insert-queue!) 'a)
((q1 'rear-insert-queue!) 'b)
(q1 'front-delete-queue!)
(q1 'front-delete-queue!)
((q1 'front-insert-queue!) 'c)
((q1 'front-insert-queue!) 'd)
(q1 'rear-delete-queue!)
(q1 'rear-delete-queue!)
~~~~~~~~~~~~
