# 5.7

## a 再帰
計算機モデルの構成
```
(define expt-machine
  (make-machine
   '(b n counter product)
   (list (list '= =) (list '- -) (list '* *))
   '((assign continue (label expt-done))
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
  expt-done)))
```

テスト（実装がないので推測。こう動くはず）
```
(set-register-contents! expt-machine 'b 3)
(set-register-contents! expt-machine 'n 4)
(start expt-machine)
(get-register-contents expt-machine 'product)
=> 81
```

## b 反復
```
(define expt-machine
  (make-machine
   '(b n counter product)
   (list (list '= =) (list '- -) (list '* *))
   '((assign product (const 1))
    (assign counter (reg n))
  expt-loop
    (test (op =) (reg counter) (const 0))
    (branch (label expt-done))
    (assign counter (op -) (reg counter) (const 1))
    (assign product (op *) (reg b) (reg product))
    (goto (label expt-loop))
  expt-done)))
```

テスト（実装がないので推測。こう動くはず）
```
(set-register-contents! expt-machine 'b 3)
(set-register-contents! expt-machine 'n 4)
(start expt-machine)
(get-register-contents expt-machine 'product)
=> 81
```

# 5.8
```
(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
             (if (assoc next-inst labels)
                 (error "label already exists :" next-inst) ;;ADD
                 (receive insts
                          (cons (make-label-entry next-inst
                                                  insts)
                                labels))
                 (receive (cons (make-instruction next-inst)
                                insts)
                          labels)))))))
```

# 5.9
```
(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
         (map (lambda (e)
                (if (label-exp? e)
                    (error "Cannot apply operation to label " e) ;;ADD
                    (make-primitive-exp e machine labels)))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))
```

# 5.10
新しい構文の具体例が思いつかなかったが、おそらく 11のcが概ね題意に沿っていると思う。

(register に pop/pushの構文を追加した状態)

# 5.11
## a
1命令を除去する = 1命令少なくしても同じ結果が得られる　という意味と理解。

書き換え前
```
afterfib-n-2                         ; 戻った時Fib(n-2)の値はvalにある
  (assign n (reg val))               ; nにはFib(n-2)がある
  (restore val)                      ; valにはFib(n-1)がある
  (restore continue)
  (assign val                        ; Fib(n-1)+Fib(n-2)
          (op +) (reg val) (reg n))
  (goto (reg continue))              ; 呼出し側に戻る. 答えはvalにある
```

書き換え後
```
afterfib-n-2                         ; 戻った時Fib(n-2)の値はvalにある
  (restore n)                        ; nにはFib(n-1)がある
  (restore continue)
  (assign val                        ; Fib(n-2)+Fib(n-1)
          (op +) (reg val) (reg n))
  (goto (reg continue))              ; 呼出し側に戻る. 答えはvalにある
```

val と n の値はテレコでも問題ないので、 nにスタックの値(Fib(n-1))を復元すると、1命令減る

## b
saveで、reg-nameを保存する。
```
(define (make-save inst machine stack pc)
  (let ((reg-name  (stack-inst-reg-name inst)))
     (let ((reg (get-register machine reg-name)))
        (lambda ()
           (push stack (cons reg-name (get-contents reg)))
           (advance-pc pc)))))
```

restoreで、引数の reg-name と popされた値に格納された reg-name が一致することを確認する.
```
(define (make-restore inst machine stack pc)
  (let ((reg-name  (stack-inst-reg-name inst)))
     (let ((reg (get-register machine reg-name)))
        (lambda ()
           (let ((popped (pop stack)))
              (if (eq? (car popped) reg-name)
                  (set-contents! reg (cdr popped))
                  (error "Wrong reg-name" reg-name)))
           (advance-pc pc)))))
```

## c
1つのスタックで連想配列的な実装をする案と、
レジスタごとにスタックを持たせる案とが出てきた。

題意に沿うのは前者だが、後者の方が実装がすっきりしそうなので後者を試してみる。

全体は 5.11c.rkt

make-registerの変更
```
(define (make-register name)
  (let ((contents '*unassigned*)
        (stack (make-stack))) ;; 各regster専用のstack
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value) (set! contents value)))
            ((eq? message 'push) (stack 'push)) ;; 自分のregsterへpush
            ((eq? message 'pop) (stack 'pop)) ;; 自分のregsterからpop
            ((eq? message 'initialize) (stack 'initialize))
            (else
             (error "Unknown request -- REGISTER" message))))
    dispatch))
```

make-new-machine 中の allocate-registerを修正
```
(define (make-new-machine)
...
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
              (let ((reg (make-register name)))
                (set! register-table (cons (list name reg) register-table))
                (reg 'initialize))) ;initialzie the stack of allocated register
        'register-allocated)
```

make-save/make-restoreの行き先をstackからregisterに
```
(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push reg (get-contents reg)) ;;各registerのstackへpush
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop reg)) ;;各registerのstackからpop
      (advance-pc pc))))
```


# 5.12

全体は  5.12.rkt 参照.

この方式の欠点は、一度実行しないと analyzerが完成しない点。


make-new-machine に analyzerを追加
```
(define (make-new-machine)
  (let ((pc (make-register 'pc))
       ..
       (stack (make-stack))
       (analyzer (make-analyzer)) ;; ADD
       (the-instruction-sequence '()))
    ..
    (define (dispatch message)
        (cond ((eq? message 'start)
              ..
              ((eq? message 'operations) the-ops)
              ((eq? message 'analyzer) analyzer) ;; ADD
              (else (error "Unknown request -- MACHINE" message))))
..
```

analyzerの定義
```
;; 解析器
(define (make-analyzer)
  (let ((analyze-list '((assgin) (test) (branch) (goto) (save) (restore) (perform))))
    (define (add-analyzer inst label)
      (let (( analyzer (assoc label analyze-list)))
        (if analyzer
            (if (not (member inst analyzer))
                (set-cdr! analyzer (cons inst (cdr analyzer)))))))
    (define (print-analyzer) (print analyze-list))
    (define (dispatch message)
      (cond ((eq? message 'add) add-analyzer)
            ((eq? message 'print) print-analyzer)
            (else (error "Unkown message" message))))
    dispatch))

(define (print-analyzed-result machine)
  (((machine 'analyzer) 'print)))

(define (add-analyzer inst machine label)
  (((machine 'analyzer) 'add) inst label))
```

実行時に analyzerに追加する
```
;; 実行手続き
  (define (make-execution-procedure inst labels machine pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (add-analyzer inst machine 'assgin)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (add-analyzer inst machine 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (add-analyzer inst machine 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (add-analyzer inst machine 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (add-analyzer inst machine 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (add-analyzer inst machine 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (add-analyzer inst machine 'perform)
         (make-perform inst machine labels ops pc))
        (else (error "Unknown instruction type -- ASSEMBLE"
                     inst))))
```

# 5.13

registerの操作は必ず lookup-registerを通るので、ココを修正する。

全体ソースは 5.13.rkt

make-machineの変更（register-names周りの処理を削除)
```
(define (make-machine ops controller-text)
  (let ((machine (make-new-machine)))
;    (for-each (lambda (register-name)
;                ((machine 'allocate-register) register-name))
;              register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))
```

make-new-machineの中の lookup-register を修正
```
(define (lookup-register name)
  (let ((val (assoc name register-table)))
    (if val
        (cadr val)
        (begin ;無ければregisterを追加して再度lookup
          (allocate-register name)
          (lookup-register name)))))
```

# 5.14
全体は 5.14.rkt

試しに動かしてみると
```
(define fact-machine
  (make-machine
    (list (list '= =) (list '- -) (list '* *))
    '(start
       (assign continue (label fact-done))
  fact-loop
    (test (op =) (reg n) (const 1))
    (branch (label base-case))
    (save continue)
    (save n)
    (assign n (op -) (reg n) (const 1))
    (assign continue (label after-fact))
    (goto (label fact-loop))
  after-fact
    (restore n)
    (restore continue)
    (assign val (op *) (reg n) (reg val))
    (goto (reg continue))
  base-case
    (assign val (const 1))
    (goto (reg continue))
  fact-done)))

(define (fact n)
  ((fact-machine 'stack) 'initialize)
  (set-register-contents! fact-machine 'n n)
  (start fact-machine)
  (display n)
  (newline)
  (display (get-register-contents fact-machine 'val))
  ((fact-machine 'stack) 'print-statistics)
  (newline))

(define (print-fact-stack n)
  (define (iter k)
    (if (< k n)
        (begin
          (fact k)
          (iter (+ k 1)))))
  (iter 1))

(print-fact-stack 10)
```

実行結果は
```
1
1
(total-pushes = 0 maximum-depth = 0)
2
2
(total-pushes = 2 maximum-depth = 2)
3
6
(total-pushes = 4 maximum-depth = 4)
4
24
(total-pushes = 6 maximum-depth = 6)
5
120
(total-pushes = 8 maximum-depth = 8)
6
720
(total-pushes = 10 maximum-depth = 10)
7
5040
(total-pushes = 12 maximum-depth = 12)
8
40320
(total-pushes = 14 maximum-depth = 14)
9
362880
(total-pushes = 16 maximum-depth = 16)
```

よって、
```
push回数=最大深さ=(n-1)*2
```

問題文後半が何を言いたいのかよく分からなかったが、上記に定義した fact のような手続きを言うのだろうか？

# 5.15
全体は 5.15.rkt

```
(define (make-new-machine)
  (let ((pc (make-register 'pc))
..
        (instruction-count 0) ;;ADD
        (the-instruction-sequence '()))
..
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                (set! instruction-count (+ 1 instruction-count)) ;;ADD
                (execute)))))
      (define (get-instruction-count) instruction-count)
      (define (initialize-instruction-count) (set! instruction-count 0)) ;ADD
      (define (dispatch message)
        (cond ((eq? message 'start)
..
              ((eq? message 'get-instruction-count) ;;ADD
                 (let ((inst-count (get-instruction-count)))
                     (initialize-instruction-count)
                     inst-count))
              ((eq? message 'initialize-instruction-count) ;;ADD
                 (initialize-instruction-count))
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (get-instruction-counting machine)
  (machine 'get-instruction-count))

(define (initialize-instruction-counting machine)
  (machine 'initialize-instruction-count))
```

# 5.16

全体は 5.16.rkt

```
(define (make-new-machine)
  (let ((pc (make-register 'pc))
        ..
        (instruction-trace #f) ;;ADD
        (the-instruction-sequence '()))
..
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                (if instruction-trace ;;ADD
                    (begin
                      (display (caar insts))
                      (newline)))
                ((instruction-execution-proc (car insts)))
                (set! instruction-count (+ 1 instruction-count))
                (execute)))))
..
      (define (set-instruction-trace flag) (set! instruction-trace flag))
      (define (dispatch message)
        (cond ((eq? message 'start)
..
              ((eq? message 'trace-on) (set-instruction-trace #t))
              ((eq? message 'trace-off) (set-instruction-trace #f))
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))
```

# 5.17

全体は 5.17.rkt

# 5.18
全体は 5.18.rkt

# 5.19
全体は 5.19.rkt
