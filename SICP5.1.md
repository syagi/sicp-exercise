# 5.1

# 5.2
```
(controller
    (assign product (const 1))
    (assign counter (const 1))
  test-iter
    (test (op >) (reg counter) (reg n))
    (branch (label fact-done))
    (assign j (op *) (reg product) (reg counter)
    (assign product (reg j)
    (assign k (op +) (reg counter) (const 1))
    (assign counter (reg k))
    (goto (label test-iter))
  fact-done)
```

# 5.3

good-enough と improve が基本演算の場合
```
(controller
    (assign guess (const 1.0))
  sqrt-iter
    (test (op good-enough?) (reg guess))
    (branch (label sqrt-done))
    (assign i (op improve) (reg guess))
    (assign guess (reg i))
    (goto (label sqrt-iter))
  sqrt-done)
```

基本演算ではない場合
```
(controller
    (assign guess (const 1.0))
  sqrt-iter
    (assign a (op square) (reg guess))
    (assign b (op -) (reg a) (reg x))
    (assign c (op abs) (reg b)
    (test (op <) (reg c) (const 0.001))
    (branch (label sqrt-done))
    (assign j (op /) (reg x) (reg guess))
    (assign guess (op average) (reg guess) (reg j))
    (assign guess (reg k))
    (goto sqrt-iter)
  sqrt-done)
```

# 5.4

## a
```
(controller
    (assign continue (label expt-done))
  expt-loop
    (test (op =) (reg n) (const 0))
    (branch (label base-case))
    (save continue)
    (save n)
    (assign n (op -) (reg n) (const 1))
    (assign continue (label after-expt))
    (goto (label expt-loop))
  after-expt
    (restore n)
    (assgin val (op *) (reg b) (reg val))
    (goto (reg continue))
  base-case
    (assign val (const 1))
    (goto (reg continue))
  expt-done)
```

## b
```
(controller
    (assign product (const 1))
    (assign counter (reg n))
  expt-loop
    (test (op =) (reg counter) (const 0))
    (branch (label expt-done))
    (assign counter (op -) (reg counter) (const 1))
    (assign product (op *) (reg b) (reg product))
    (goto expt-loop)
  expt-done)
```

# 5.5

実行される行を逐次書き出してみる


fact(3)の場合

|命令|n|continue|val|nのstack|continueのstack|valのstack|
|----|-|--------|---|-------|--------------|----------|
|   (assign continue (label fact-done)) |3|fact-done| | | | |
|fact-loop|
|   (test (op =) (reg n) (const 1))|
|   (save continue)| | | |[fact-done]| | |
|   (save n)| | | |[3]| | |
|   (assign n (op -) (reg n) (const 1)) |2| | | | | |
|   (assign continue (label after-fact))| |after-fact| | | | | |
|   (goto (label fact-loop))|
|fact-loop|
|   (test (op =) (reg n) (const 1))|
|   (save continue)| | | |[after-fact, <br> fact-done]| | |
|   (save n)| | | |[2, <br> 3]| | |
|   (assign n (op -) (reg n) (const 1)) |1| | | | | |
|   (assign continue (label after-fact))| |after-fact| | | | |
|   (goto (label fact-loop))|
|fact-loop|
|   (test (op =) (reg n) (const 1))|
|   (branch (label base-case))|
|base-case|
|   (assign val (const 1))| | |1| | | |
|   (goto (reg continue))|
|after-fact|
|   (restore n)|2| | |[3]| | |
|   (restore continue)| |after-fact| | |[fact-done]| |
|   (assign val (op *) (reg n) (reg val))| |2| | | | |
|   (goto (reg continue))|
|after-fact|
|   (restore n)|3| | |[]| | |
|   (restore continue)| |fact-done| | | | |
|   (assign val (op *) (reg n) (reg val))| |6| | | | |
|   (goto (reg continue))|
|fact-done|


fib(4)の場合

|命令|n|continue|val|nのstack|continueのstack|valのstack|
|----|-|--------|---|-------|--------------|----------|
|   (assign continue (label fib-done))| |fib-done| | | | |
| fib-loop|
|   (test (op <) (reg n) (const 2))|
|   (save continue) | | | | |[fib-done]| |
|   (assign continue (label afterfib-n-1))| |afterfib-n-1| | | | |
|   (save n) | | | | [4]| | |
|   (assign n (op -) (reg n) (const 1)); |3| | | | | |
|   (goto (label fib-loop)) |
| fib-loop|
|   (test (op <) (reg n) (const 2))|
|   (save continue) | | | | |[afterfib-n-1, <br> fib-done]| |
|   (assign continue (label afterfib-n-1))| |afterfib-n-1| | | | |
|   (save n) | | | |[3, <br> 4]| | |
|   (assign n (op -) (reg n) (const 1)); |2| | | | | |
|   (goto (label fib-loop)) |
| fib-loop|
|   (test (op <) (reg n) (const 2))|
|   (branch (label immediate-answer))|
|immediate-answer|
|  (assign val (reg n))| | |2| | | |
|  (goto (reg continue))|
| afterfib-n-1 |
|   (restore n)|3| | |[4]| | |
|   (restore continue)| |afterfib-n-1| | |[fib-done]| |
|   (assign n (op -) (reg n) (const 2))|1| | | | | |
|   (save continue)| | | | |[afterfib-n-1, <br> fib-done]| |
|   (assign continue (label afterfib-n-2))| |afterfib-n-2| | | | |
|   (save val) | | |2| | |[2]|
|   (goto (label fib-loop))|
| fib-loop|
|   (test (op <) (reg n) (const 2))|
|   (branch (label immediate-answer))|
|immediate-answer|
|  (assign val (reg n))| | |1| | | |
|  (goto (reg continue))|
| afterfib-n-2|
|   (assign n (reg val))|1| | | | | |
|   (restore val)| | |2| | |[]|
|   (restore continue)| |afterfib-n-1| | |[fib-done]| |
|   (assign val (op +) (reg val) (reg n))| | |3| | | |
|   (goto (reg continue)) |
| afterfib-n-1 |
|   (restore n)|4| | |[]| | |
|   (restore continue)| |fib-done| | |[]| |
|   (assign n (op -) (reg n) (const 2))|2| | | | | |
|   (save continue)| | | | |[fib-done]| |
|   (assign continue (label afterfib-n-2))| |afterfib-n-2| | | | |
|   (save val) | | |3| | |[3]|
|   (goto (label fib-loop))|
| fib-loop|
|   (test (op <) (reg n) (const 2))|
|   (branch (label immediate-answer))|
|immediate-answer|
|  (assign val (reg n))| | |2| | | |
|  (goto (reg continue))|
| afterfib-n-2|
|   (assign n (reg val))|2| | | | | |
|   (restore val)| | |3| | |[]|
|   (restore continue)| |fib-done| | |[]| |
|   (assign val (op +) (reg val) (reg n))| | |5| | | |
|   (goto (reg continue)) |
|fib-done|


# 5.6

afterfib-n-1の中に、 restoreしてそのままsaveしている箇所がある.

```
(controller
   (assign continue (label fib-done))
 fib-loop
   (test (op <) (reg n) (const 2))
   (branch (label immediate-answer))
   ;; Fib(n-1)を計算するよう設定
   (save continue)
   (assign continue (label afterfib-n-1))
   (save n)                           ; nの昔の値を退避
   (assign n (op -) (reg n) (const 1)); nを n-1 に変える
   (goto (label fib-loop))            ; 再帰呼出しを実行
 afterfib-n-1                         ; 戻った時 Fib(n-1)はvalにある
   (restore n)
   (restore continue)                 ;不要
   ;; Fib(n-2)を計算するよう設定
   (assign n (op -) (reg n) (const 2))
   (save continue)                    ;不要
   (assign continue (label afterfib-n-2))
   (save val)                         ; Fib(n-1)を退避
   (goto (label fib-loop))
 afterfib-n-2                         ; 戻った時Fib(n-2)の値はvalにある
   (assign n (reg val))               ; nにはFib(n-2)がある
   (restore val)                      ; valにはFib(n-1)がある
   (restore continue)
   (assign val                        ; Fib(n-1)+Fib(n-2)
           (op +) (reg val) (reg n))
   (goto (reg continue))              ; 呼出し側に戻る. 答えはvalにある
 immediate-answer
   (assign val (reg n))               ; 基底の場合: Fib(n)=n
   (goto (reg continue))
 fib-done)
```
