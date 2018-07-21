# 4.55
## a. Ben Bitdiddleに監督されている人すべて;
監督 = supervisor
```
(supervisor ?x (Ben Bitdiddle))
```

## b. 経理部門[accounting division]のすべての人の名前と担当;
担当 = job
```
(job ?x (accounting division))
```

## c. Slumervilleに住む人すべての名前と住所.
Slumervilleに住む = address の第1要素が Slumerville
```
(address ?x (Slumerville . ?type))
```

# 4.56
## a. Ben Bitdiddleが監督している人すべての名前とその住所;
```
(and (supervisor ?person (Ben Bitdiddle))
     (address ?person ?where))
```

## b. 給料がBen Bitdiddleのそれより少ない人のすべてと, その人たちの給料と, Ben Bitdiddleの給料;
```
(and (salaly (Ben Bitdiddle) ?amount-of-ben)
     (salaly ?person ?amount)
     (lisp-value > ?amount ?amount-of-ben))
```

## c. 計算機部門にいない人が監督している人すべてと, その監督者の名前と担当.
計算機部門 = jobがcomputerで始まる
```
(and (job ?non-comp ?section)
     (and (not (job ?non-comp (computer . ?type)))
          (supervisor ?person ?non-comp)))
```

# 4.57
規則
```
(rule (replace ?person-1 ?person-2)
      (and (or (and (job ?person-1 ?job-1)
                    (job ?person-2 ?job-2)
                    (same ?job-1 ?job-2))
               (and (can-do-job ?person1 ?can-1)
                    (can-do-job ?person2 ?can-2)
                   (same ?can-1 ?can-2))
           (not (same person-1 person-2)))))
```

## a. Cy D. Fectに代れる人すべて;
```
(replace ?person (Cy D. Fect))
```

## b. 誰かに代れて, その誰かの方が多くの給料を貰っている人すべてと, 両者の給料.
〜〜人：person1
誰か：person2
```
(and (salaly ?person1 ?amount1)
     (replace ?person1 ?person2)
     (salaly ?person2 ?amount2)
     (lisp-value < ?amount1 ?amount2))
```

# 4.58
```
(rule (big-shot ?person)
      (and (job ?person ?job)
           (supervisor ?person ?supervisor)
           (job ?supervisor ?supervisor-job)
           (not (same ?job ?supervisor-job))))
```

# 4.59
## a 金曜の会合すべて
```
(meeting ?meeting (Friday . ?time))
```

## b
```
(rule (meeting-time ?person ?day-and-time)
      (or (meeting (whole-company) ?day-and-time)
          (and (job ?person (?division . ?rest)
               (meeting ?division ?day-and-time)))))
```

## c
```
(and (meeting-time (Alyssa P. Hacker) (Wednesday ?time)))
```

# 4.60
lives-near は無方向の対象関係なので、AさんがBさんとlives-nearなら、BさんもAさんとlives-nearになるため。

対策としては、各人にIDを割り振り、IDが若い場合だけ出力するようにすれば良い。
```
(id (Hacker Alyssa P) 1)
(id (Fect Cy D) 2)

(rule (lives-near-uniq ?person1 ?person2)
      (and (lives-near ?person1 ?person2)
           (id ?person1 ?id1)
           (id ?person2 ?id2)
           (lisp-value < ?id1 ?id2)))
```

なお、 lives-nearを直接書き換えると、
```
(lives-near BigID ?person)
```
の問い合わせで BigID氏より若いIDの人が表示されなくなるので、ruleを分けるべき

# 4.61

```
(?x next-to ?y in (1 (2 3) 4))
;=> (2 3) next-to 4
;=> 1 next-to (2 3)
```

```
(?x next-to 1 in (2 1 3 1))
;=> 3 next-to 1
;=> 2 next-to 1
```

# 4.62
実装がないのにどうやって調べるのか・・・

```
(rule (last-pair (?x) (?x)))
(rule (last-pair (?x . ?y) ?z)
      (last-pair? ?y ?z))
```

```
(last-pair (3) ?x)
;=> (last-pair (?x) (?x)) にマッチ。
;=> 3

(last-pair (1 2 3) ?x)
;=> (last-pair (?x . ?y) ?z) にマッチ
;=> ?x=1, ?y=(2 3)
;=> (last-pair (?x . ?y) ?z) にマッチ
;=> ?x=2, ?y=3
:=> (last-pair ?x ?x)にマッチ
;=> 3

(last-pair (2 ?x) (3))
;=> (last-pair (?x . ?y) ?z) にマッチ
;=> ?x=2 ?y=?x ?z=3
;=> (last-pair ?x 3) にマッチ
;=> 3
;=> しかし、(last-pair (?x . ?y) ?z) にもマッチ
;=> （3で終わる全ての配列がマッチするので無限ループ）
```

# 4.63
```
(rule (grandson ?g ?s)
      (and (son ?g ?f)
           (son ?f ?s)))
```

```
(rule (son ?m ?s)
      (and (wife ?w ?m)
           (son ?w ?s)))
```
