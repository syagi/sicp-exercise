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


本文中では、 letで古い値を他の変数に束縛し、改めてTHE-ASSERTIONSに束縛することで、問題を回避している

# 4.71

本文の定義
```
(define (simple-query query-pattern frame-stream)
  (stream-flatmap
   (lambda (frame)
     (stream-append-delayed
      (find-assertions query-pattern frame)
      (delay (apply-rules query-pattern frame))))
   frame-stream))

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed
       (qeval (first-disjunct disjuncts) frame-stream)
       (delay (disjoin (rest-disjuncts disjuncts)
                       frame-stream)))))
```

Louisの定義
```
(define (simple-query query-pattern frame-stream)
  (stream-flatmap
   (lambda (frame)
     (stream-append (find-assertions query-pattern frame)
                    (apply-rules query-pattern frame)))
   frame-stream))

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave
       (qeval (first-disjunct disjuncts) frame-stream)
       (disjoin (rest-disjuncts disjuncts) frame-stream))))
```

差分は、 stream-append-delaye / interleave-delayed を使うか否か。
遅延を使わない場合、全ての結果が演算された後にならないと、結果が出力されない。

遅延がないことで問題になるのは、無限ループになる質問。
遅延がある場合、逐次評価結果が出力される（終了はしない）
遅延がない場合は、評価結果が出力されない。


# 4.72
3.5.3節に書いてあるとおり
```
interleaveは二つのストリームから要素を交互にとるので,
第一のストリームが無限であっても, 第二のストリームのすべての要素は,
いつかは混ぜ合されたストリームへ行く道を見つける.
```
片方、または両方のストリームが無限でも対応するため。

# 4.73
本来の定義
```
(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave-delayed
       (stream-car stream)
       (delay (flatten-stream (stream-cdr stream))))))
```
こうしない理由
```
(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave
       (stream-car stream)
       (flatten-stream (stream-cdr stream)))))
```

要は、 flatten-stream を遅延評価する必要性。
flatten stream の引数は、streamなので、
無限ストリームでも評価が逐次行われるようにするため。
（ 4.71と同じ）

# 4.74
## a
元の定義
```
(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave-delayed
       (stream-car stream)
       (delay
          (flatten-stream (stream-cdr stream))))))
```

Alyssaの変更
```
(define (simple-stream-flatmap proc s)
  (simple-flatten (stream-map proc s)))

(define (simple-flatten stream)
  (stream-map
    stream-car
    (stream-filter
      (lambda (s) (not (stream-null? s)))
      stream )))
```

## b
変わらないように見えるが、理由が説明できない、、、


# 4.75
カンニングしながら理解。

解が1つということをチェックするために、
- result が null でない
- stream-cdr result が null
というのをチェックしている。

```
(define (unique-query exps) (car exps))

(define (uniquely-asserted contents frame-stream)
  (stream-flatmap
    (lambda (frame)
      (let ((result (qeval (unique-query contents)
                           (singleton-stream frame))))
        (if (and (not (stream-null? result))
                 (stream-null? (stream-cdr result)))
            result
            the-empty-stream)))
    frame-stream))

(put 'unique 'qeval uniquely-asserted)
```



# 4.76
戦略としては、各質問の結果を次の質問の入力とする、直列な適用をするのではなく、
各質問はデータベースに適用し、結果をマージする並列的な適用とする。


実装はカンニング。。。

```
(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (merge-frame-streams
        (qeval (first-conjunct conjuncts) frame-stream)
        (conjoin (rest-conjuncts conjuncts) frame-stream))))

(define (merge-frame-streams s1 s2)
  (stream-flatmap
     (lambda (frame1)
       (stream-filter
          (lambda (frame)
             (not (equal? frame 'failed)))
          (stream-map
            (lambda (frame2)
               (merge-frames frame1 frame2))
            s2)))
        s1))

(define (merge-frames frame1 frame2)
  (if (null? frame1)
      frame2
      (let ((var (caar frame1))
            (val (cdar frame1)))
         (let
            ((extension
               (extend-if-possible var val frame2)))
            (if (equal? extension 'failed)
                'failed
                (merge-frames (cdr frame1) extension))))))

(put 'and 'qeval conjoin)

```

# 4.77
さっぱり分からず。
カンニングしつつ解釈すると、
notの実行を後回しにして、他の演算によって変数が束縛されるのを待つ、ということ？
http://wat-aro.hatenablog.com/entry/2016/01/21/181839
http://himoiku.cocolog-nifty.com/blog/2008/03/sicp477_db0f.html


# 4.78
ambを使って質問言語を実装する,という問題らしいが・・・
自力は諦めてカンニングしようとしたが、ほとんど無い。

ちなみに唯一見つけた回答
https://github.com/phoeninee/SICP-solutions/tree/master/Chapter%204-Metalinguistic%20Abstraction/4.Logical%20Programing/4.78



# 4.79

パス
