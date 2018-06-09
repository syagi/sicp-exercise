# 4.35
```
(define (an-integer-between low high)
  (require (< low high)
  (amb low (an-integer-between (+ low 1) high))
```

# 4.36

3.69では　3つの値すべてに integersを入れて、filterによってすべての三角形を見つけようとしていた。
http://syagi.hatenablog.com/entry/2017/07/22/211415

問題文でも同じように
```
define (a-pythagorean-triple-starting-from low)
  (let ((i (an-integer-starting-from low)))
    (let ((j (an-integer-starting-from i)))
      (let ((k (an-integer-starting-from j)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))
```
とすると、 kが無限のため、 i と j の再試行が永遠に行われない。
そのため、 i と j を有限にして、 kがj、jがiの範囲を決定するようにする。
(iやjの範囲をtryし終えたら、上位のambが範囲を更新する )
```
(define (a-pythagorean-triple-starting-from low)
  (let ((k (an-integer-starting-from low)))
    (let ((i (an-integer-between low k)))
      (let ((j (an-integer-between i k)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))
```

#4.37
形として、 4.35は
```
amb1
amb2
amb3
require1
```

4.37(Ben)は
```
amb4
amb5
require2
require3
```

という形になっている。
requireの数が異なるが、評価しなければいけない可能性の数は
amb1,2,3 で出来る木 vs amb4,5 で出来る木
となる。

木の大きさは後者の方が小さいので、Benの方が効率が良い。


#4.38
各人の可能性は
| Baker | 1 | 2 | 3 | 4 |
| Cooper | | 2 | 3 | 4 | 5 |(Fletherと隣接しない)|
| Flether | | 2 | 3 | 4 | |(Cooperと隣接しない)|
| Miller | | | 3 | 4 | 5 |(Cooperより上)|
| Smith | 1 | 2 | 3 | 4 | 5 |

Fletcherが可能性が一番少ないのでここを起点に解く

|F|2| |4| | |
|C|4| |2| | |
|M|5| |3|5| |
|B|1|2|1|1|3|
|S|2|1|5|3|1|

5通り

# 4.39
順序は解に影響しない。
条件の順番が変わっても、木の枝を切り落とす順序が変わるだけで、全体としてできあがる木は同じになるため。

解を見出す時間には影響する。
影響としては、偽が多く発生する条件が前にある方が効率よくなる。
（require評価がANDなので、後段の評価が不要となる。)
ただし、評価する木の大きさが変わるわけではないのでおそらく僅差。
（現時点では実行できないので検証はしない）

よって効率の良いプログラムは
```
(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require
     (distinct? (list baker cooper fletcher miller smith)))
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))
```
直感的には3.37とだいぶ異なるが、人間はそもそもすべての可能性を考慮しないためだと思われる。
（無意識に distinctしてるし、 fletcherは 2 3 4 しか考慮しない）


# 4.40
一瞬日本語でおｋ・・・と思ったが
## 前半部
要求前はすべての人が全ての階を選びうるので、 階数^人数
要求語は順列問題になるので、 階数P人数 = 階数!/人数!

## 後半部
階を選択する前に制限を課す = requireに合致したときだけ amb を評価する　なので

```
(define (multiple-dwelling)
  (let ((fletcher (amb 1 2 3 4 5)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (let ((cooper (amb 1 2 3 4 5)))
      (require (not (= cooper 1)))
      (require (not (= (abs (- fletcher cooper)))))
      (let ((miller (amb 1 2 3 4 5)))
        (require (> miller cooper))
        (let ((baker (amb 1 2 3 4 5)))
          (require (not (= baker 5)))
          (let ((smith (amb 1 2 3 4 5)))
            (require (not (= (abs (- smith flethcer)))))
            (require (distinct? (list baker cooper fletcher miller smith)))
            (list 'fletcher fletcher 'cooper cooper
                  'miller miller 'baker baker 'smith smith)))))))
```

3.38で自分が評価した順番と同じになったので気持ちいい。
ちなみに an-integer-between とか使えばもっと減る？かと思ったが、
結局内部でrequireを呼ぶので意味がないと気づいた。


# 4.41
5人が取り得る組み合わせをfilterする。
順列を生成する部分は省略。
https://qiita.com/nakataSyunsuke/items/15c151ea88a44e7c4a8e
あたりが流用できるはず
```
;居住条件
(define (dwelling? x)
  (let ((baker (car x))
        (cooper (cadr x))
        (fletcher (caddr x))
        (miller (cadddr x))
        (smith (car (cddddr x))))
    (and
         (not (= baker 5))
         (not (= cooper 1))
         (not (= fletcher 5))
         (not (= fletcher 1))
         (> miller cooper)
         (not (= (abs (- smith fletcher)) 1))
         (not (= (abs (- fletcher cooper)) 1)))))

(define dwelling-list
  permutations '(1 2 3 4 5))
;https://qiita.com/nakataSyunsuke/items/15c151ea88a44e7c4a8e

(filter dwelling? dwelling-list)
```

# 4.42
女子生徒なのに彼ら、という訳。

以下の前提を置く
 - 5人全員が本当のことをいっている場合以外は矛盾が生じて順位が重複する。
   (例えば全員が順位を1つずつずらして言えば矛盾が生じなくなってしまうが、そういうケースはそんざいしないと仮定)

各自の発言を xor で結び、それをANDでつなげて、矛盾が起きなかった組み合わせが真実。
（はじめ orでつなげば良いかと思ったが、両方真実になってしまうケースが出る）

```
(let ((betty (amb 1 2 3 4 5))
      (ethel (amb 1 2 3 4 5))
      (joan (amb 1 2 3 4 5))
      (kitty (amb 1 2 3 4 5))
      (mary (amb 1 2 3 4 5)))
  (require (xor (= kitty 2) (= betty 3)))
  (require (xor (= ethel 1) (= joan 2)))
  (require (xor (= joan 3) (= ethel 5)))
  (require (xor (= kitty 2) (= mary 4)))
  (require (xor (= mary 4) (= betty 1)))
  (list 'betty betty 'ethel ethel 'joan joan 'kitty kitty 'mary mary))
```

# 4.43

父,娘
  (Moore, MaryAnn)
  (Barnacle, Melissa)   ; not Downing
  (g_father, Gabrielle) ; not Barnacle, Parker
  (l_father, Lorna)     ; not Moore
  (r_father, Rosalind)  ; not Hall

条件を整理して
```
(let ((father-of-mary (amb 'downing 'hall 'barnacle 'paker 'moor))
  (require (eq? father-of-mary 'moor)) ; maryの姓が不明ならここを削除
  (let ((father-of-melissa  (amb 'downing 'hall 'barnacle 'paker 'moor)))
    (require (eq? father-of-melissa 'barnacle))
    (require (not (eq? father-of-melissa 'downing)))
    (let ((father-of-gabrielle (amb 'downing 'hall 'barnacle 'paker 'moor)))
      (require (not (eq? father-of-gabrielle 'parker)))
      (require (not (eq? father-of-gabrielle 'barnacle)))
      (let ((father-of-rosalind (amb 'downing 'hall 'barnacle 'paker 'moor)))
         (require (not (eq? father-of-rosalind 'hall)))
         (let ((father-of-lorna (amb 'downing 'hall 'barnacle 'paker 'moor)))
           (require (not (eq? father-of-lorna 'moor)))
           (require (distinct? father-of-mary
                      father-of-gabrielle
                      father-of-lorna
                      father-of-rosalind
                      father-of-melissa)
  (list 'father-of-lorna father-of-lorna))
```

# 4.44
座標の順列を用意して、 requireで同じ動線に配置されないようにすれば良い
縦横は単に x y の distinct
斜めは x y の和（右上から左下）、差（左上から右下）のdistinct

```
(let ((queen1-x (amb 1 2 3 4 5 6 7 8))
      (queen1-y (amb 1 2 3 4 5 6 7 8))
      (queen2-x (amb 1 2 3 4 5 6 7 8))
      (queen2-y (amb 1 2 3 4 5 6 7 8))
      (queen3-x (amb 1 2 3 4 5 6 7 8))
      (queen3-y (amb 1 2 3 4 5 6 7 8))
      (queen4-x (amb 1 2 3 4 5 6 7 8))
      (queen4-y (amb 1 2 3 4 5 6 7 8))
      (queen5-x (amb 1 2 3 4 5 6 7 8))
      (queen5-y (amb 1 2 3 4 5 6 7 8))
      (queen6-x (amb 1 2 3 4 5 6 7 8))
      (queen6-y (amb 1 2 3 4 5 6 7 8))
      (queen7-x (amb 1 2 3 4 5 6 7 8))
      (queen7-y (amb 1 2 3 4 5 6 7 8))
      (queen8-x (amb 1 2 3 4 5 6 7 8))
      (queen8-y (amb 1 2 3 4 5 6 7 8)))
  (require (distinct? queen1-x queen2-x queen3-x queen4-x
                      queen5-x queen6-x queen7-x queen8-x))
  (require (distinct? queen1-y queen2-y queen3-y queen4-y
                      queen5-y queen6-y queen7-y queen8-y))
  (require (distinct? (+ queen1-x queen1-y) (+ queen2-x queen2-y)
                      (+ queen3-x queen3-y) (+ queen4-x queen4-y)
                      (+ queen5-x queen5-y) (+ queen6-x queen6-y)
                      (+ queen7-x queen7-y) (+ queen8-x queen9-y)))
  (require (distinct? (- queen1-x queen1-y) (- queen2-x queen2-y)
                      (- queen3-x queen3-y) (- queen4-x queen4-y)
                      (- queen5-x queen5-y) (- queen6-x queen6-y)
                      (- queen7-x queen7-y) (- queen8-x queen9-y)))
  (list (list queen1-x queen1-y)(list queen2-x queen2-y)
        (list queen3-x queen3-y)(list queen4-x queen4-y)
        (list queen5-x queen5-y)(list queen6-x queen6-y)
        (list queen7-x queen7-y)(list queen8-x queen8-y)))
```

# 4.45

```
The professor lectures to the student in the class with the cat
```

和訳と英語の対応がややこしいのでいったんまとめる。

```
文　sentence ::= verb-phrase + noun-phrase
動詞句　verb-phrase ::= verb + prep-phrase
名詞句　noun-phrase ::= simple-noun-phrase + prep-phrase
前置詞句　prep-phrase ::= preposition + noun-phrase
単純名詞句　simple-noun-phrase ::= article + noun
名詞　noun ::= student|professor|cat|class
動詞　verb ::= studies|lectures|eats|sleeps
冠詞　article ::= the|a
前置詞　preposition ::= for|to|in|by|with
```

1. the professor
2. lectures
3. to the students
4. in the class
5. with the cat
として構文解析すると以下のようになる。
なお、意訳すると差分が不明瞭になるので直訳する。

(1) 1 + (2 + (3 + (4 + 5)))
教授は 生徒(教室(猫が居る)にいる)に 教える
(2) 1 + (2 + (3 + 4) + 5)
教授は 生徒(教室に居る)に 猫と教える
(3) 1 + (2 + (3 + 4 + 5))
教授は 生徒(教室に猫といる)に 教える
(4) 1 + (2 + 3 + (4 + 5))
教授は 教室(猫が居る)で 生徒に 教える
(5) 1 + (2 + 3 + 4 + 5)
教授は 猫と 教室で 生徒に 教える


# 4.46

 unparsed から car で先頭要素を取り出して評価しているため、
 amb も先頭要素から順に評価するようにしないと正しく動作しない。

構文解析器が、被演算子を右から左へ評価する場合を考える。

```
(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*))
    sent))

(define (parse-sentence)
  (list 'sentence
         (parse-noun-phrase)
         (parse-verb-phrase)))

```
ここまでは特に問題なし。

parse-noun-phrase で問題が起きる。
```
(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
         (maybe-extend (list 'noun-phrase
                             noun-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))

  (define (parse-simple-noun-phrase)
    (list 'simple-noun-phrase
          (parse-word articles)
          (parse-word nouns)))
```

左から右に評価する場合は noun-parse が先に評価される。これは問題ない（本文中の動き）。
右から左に評価する場合は、 maybe-extend が先に評価される。
maybe-extend の中では、 parse-prepositional-phraseが評価される。
```
(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))
```

ここで、 parse-word により unparsed が更新され、空になってしまうと、
```
(require (null? *unparsed*))
```
に引っかからなくなってしまい、後続のパターンが評価できなくなってしまう。



# 4.47

parse-verb-phrase が無限ループするため終了しない。

Louis の定義
```
(define (parse-verb-phrase)
  (amb (parse-word verbs)
       (list 'verb-phrase
             (parse-verb-phrase)
             (parse-prepositional-phrase))))
```
は、まず parse-word が評価される。
```
(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))
```
word-list には verbs が渡されるので、
unparsed の先頭要素が動詞であれば
 (list verb 見つけた動詞) が返され、 unparsedにはcdr(見つけた動詞を除いた残りの文)が set! される。
unparsed の先頭要素が動詞以外の場合は
 require にヒットしないので ambの次の要素が評価される。（unparsedは更新されない）
しかし、amb の次の要素は parse-verb-phrase であるため、最初に戻ってまた unparsed が動詞外の場合に入ってしまう。
結果、無限ループになるのでプログラムは終了しない。


# 4.48
副詞(adverb)句、形容詞(adjective)句をそれぞれ追加する。

```
(define adverbs '(adverb 副詞を列挙))
(define adjectives '(adjective 形容詞を列挙))
```

副詞句は、 parse-adverb-phrase を追加し、parse-verb-phrase から呼び出す。
```
(define (parse-adverb-phrase)
  (define (maybe-extend adverb-phrase)
    (amb adverb-phrase
         (maybe-extend (list 'adverb-phrase
                             adverb-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-word adverbs)))

  (define (parse-verb-phrase)
    (define (maybe-extend verb-phrase)
      (amb verb-phrase
           (maybe-extend (list 'verb-phrase
                               verb-phrase
                               (parse-adverb-phrase)))
           (maybe-extend (list 'verb-phrase
                               verb-phrase
                               (parse-prepositional-phrase)))))
    (maybe-extend (parse-word verbs)))
```

形容詞句は、 parse-simple-article-phrase と parse-article-phrase に追加。
```
(define (parse-simple-article-phrase)
  (list 'simple-article-phrase
        (parse-word articles)
        (parse-word adjectives)))

(define (parse-article-phrase)
  (define (maybe-extend article-phrase)
    (amb article-phrase
         (maybe-extend (list 'article-phrase
                             article-phrase
                             (parse-word adjectives)))))
  (maybe-extend (parse-word articles)))
```


# 4.49
unparsed を処理していた部分で単語を出力するようにする。


```
(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (amb (cdr items))))

(define (parse-word word-list)
  (list (car word-list)
        (an-element-of (cdr word-list))))

(define (generate-sentence)
  (parse-sentence))
```

動作するambの実装がまだないので出力分は省略。
