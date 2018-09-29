# 4.64
元の定義
```
(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (supervisor ?staff-person ?middle-manager)
               (outranked-by ?middle-manager ?boss))))
```

変更した定義
```
(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (outranked-by ?middle-manager ?boss)
               (supervisor ?staff-person ?middle-manager))))
```

Benの質問
```
(outranked-by (Bitdiddle Ben) ?who)
```

違いは、 and で結ばれた2つの質問の順番.
Louisの実装では
1. ?staff-person が Benに、?boss が ?who にbindされる
2. (supervisor (Bitdiddle Ben) ?boss) で Benの上司が ?boss にbindされる
3. (outranked-by ?middle-manager ?boss) が呼ばれる
4. ?middle-managerも?bossも未束縛なので、終了条件に一致しない

となり、無限ループ

元の実装では
1. ?staff-person が Benに、?boss が ?who にbindされる
2. (supervisor (Bitdiddle Ben) ?boss) で Benの上司が ?boss にbindされる
3. (supervisor (Bitdiddle Ben) ?middle-manager) で Ben上司が ?middle-manager に bind される
4. (outranked-by Ben上司 ?boss) が呼ばれる
5. 以下、上司がいない人物にぶつかるまで繰り返して止まる

# 4.65
wheelの定義
```
(rule (wheel ?person)
      (and (supervisor ?middle-manager ?person)
           (supervisor ?x ?middle-manager)))
```

質問
```
(wheel ?who)
```

本文に出てくるレコードから、
(supervisor ?middle-manager ?person)
の結果を出すと
```
(supervisor (Hacker Alyssa P) (Bitdiddle Ben))
(supervisor (Fect Cy D) (Bitdiddle Ben))
(supervisor (Tweakit Lem E) (Bitdiddle Ben))
(supervisor (Reasoner Louis) (Hacker Alyssa P))
(supervisor (Bitdiddle Ben) (Warbucks Oliver))
(supervisor (Scrooge Eben) (Warbucks Oliver))
(supervisor (Cratchet Robert) (Scrooge Eben))
(supervisor (Aull DeWitt) (Warbucks Oliver))
```

さらに各 ?middle-managerについて
(supervisor ?x ?middle-manager)
の結果を出すと
```
; (supervisor ?x (Hacker Alyssa P))
(supervisor (Reasoner Louis) (Hacker Alyssa P))
; (supervisor ?x (Bitdiddle Ben))
(supervisor (Hacker Alyssa P) (Bitdiddle Ben))
(supervisor (Fect Cy D) (Bitdiddle Ben))
(supervisor (Tweakit Lem E) (Bitdiddle Ben))
; (supervisor ?x (Scrooge Eben))
(supervisor (Cratchet Robert) (Scrooge Eben))
```
となる。（該当がない質問は省略）

2つをまとめると
```
; Alyssa の上司
(Bitdiddle Ben)
; Ben の上司 (Ben の列が3つあったので3つ出る)
(Warbucks Oliver)
(Warbucks Oliver)
(Warbucks Oliver)
; Eben の上司
(Warbucks Oliver)
```
となる。

結果について、重複を排除しないので、このような出力になる。


# 4.66

4.65のように重複したレコードが出てしまうので、二重計算してしまう.

対策としては、重複を排除するような処理が必要。
1. 結果セットを格納するリストを用意
2. 結果がリスト中に存在したら無視
3. 存在しなかったらリストに追加
4. 全ての結果を評価したら、リストを演算
とする。

# 4.67
4.4.4節やってから解く方がいい、と書いてあるように見える。
何故もっと後で出題しないのかｗ

1. 処理した質問を格納するリストを用意
2. 質問を処理する前に、リスト中に同じ質問がないか確認
3. あれば無視
4. 無ければ、リストに追加してその質問を評価

みたいな感じだろうか。。。

実装は 4.4.4 節はいらないと出来ないのでパス

# 4.68
```
(rule (reverse () ()))

(rule (reverse (?car . ?cdr) ?list)
         (and (reverse ?cdr ?tmp)
              (append-to-form ?tmp (?car) ?list)))
```

(reverse (1 2 3) ?x)
は動く。

(reverse ?x (1 2 3)) は、
(reverse ?cdr ?tmp) が束縛なしで呼ばれるので無限ループに陥る

# 4.69

```
(rule ((great . ?rel) ?x ?y)
      (and (son ?x ?w)
         (?rel ?w ?y)))

(rule ((great . ?rel) ?x ?y)
      (and (sun ?x ?z)
           (?rel ?z ?y)))
```


チェックは動かせる実装がないのでスキップ
