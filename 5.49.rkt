#lang racket
(require r5rs)

(define true #t)
(define false #f)

;; 基盤の apply への参照を apply-in-underlying-scheme へ退避させる
;;（こうすることで、基盤の apply に apply-in-underlying-scheme という名前でアクセスできる）。
(define apply-in-underlying-scheme apply)

;;;; 手続きの引数
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

;;;; 条件式
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;;;; 並び
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

;;;; 代入と定義
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

;;;; 4.1.2 式の表現

;;;; 自己評価式は数と文字だけ
(define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ((string? exp) #t)
        (else #f)))

;;;; 変数は記号で表現
(define (variable? exp) (symbol? exp))

;;;; クォート式は (quote <text-of-quotation>) の形
(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

;;;; 代入は (set! <var> <value>) の形
(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

;;;; 定義
(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)      ; 仮パラメタ
                   (cddr exp))))    ; 本体

;;;; lambda 式は記号 lambda で始まるリスト
(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;;;; 条件式
(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      #f))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;;;; begin
(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

;;;; 手続き作用
(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

;;;; cond 式
(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      #f
      (let ((first (car clauses))
            (rest (cdr clauses)))
           (if (cond-else-clause? first)
               (if (null? rest)
                   (sequence->exp (cond-actions first))
                   (error "ELSE clause isn't last -- COND->IF"
                          clauses))
               (make-if (cond-predicate first)
                        (sequence->exp (cond-actions first))
                        (expand-clauses rest))))))

;;;; 4.1.3 評価器のデータ構造

;;;; 述語のテスト
(define (true? x)
  (not (eq? x #f)))

(define (false? x)
  (eq? x #f))

;;;; 手続きの表現
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))

;;;; 環境に対する操作
(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))

(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
             (scan (frame-variables frame)
                   (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
             (scan (frame-variables frame)
                   (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
       (define (scan vars vals)
         (cond ((null? vars)
                (add-binding-to-frame! var val frame))
               ((eq? var (car vars))
                (set-car! vals val))
               (else (scan (cdr vars) (cdr vals)))))
       (scan (frame-variables frame)
             (frame-values frame))))

;;;; 4.1.4 評価器をプログラムとして走らせる
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        ;; 基本手続きが続く
        (list '+ +)
        (list '* *)
        (list '/ /)
        (list '- -)
        (list '= =)
        (list '> >)
        (list '< <)
        (list 'exit exit)
        ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (setup-environment)
  (let ((initial-env
          (extend-environment (primitive-procedure-names)
                              (primitive-procedure-objects)
                              the-empty-environment)))
       (define-variable! 'true #t initial-env)
       (define-variable! 'false #f initial-env)
       initial-env))

;add
(define (setup-c-and-r-environment)
  (extend-environment
    (list 'compile-and-run)
    (list (list 'primitive compile-and-run))
    (setup-environment)))


(define the-global-environment (setup-environment))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
    (primitive-implementation proc) args))

;;;; 基盤の Lisp システムの"読み込み-評価-印字"ループをモデル化する"駆動ループ(driver loop)"を用意する。
(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
       (let ((output (eval input the-global-environment)))
            (announce-output output-prompt)
            (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (cond ((compound-procedure? object)
         (display (list 'compound-procedure
                        (procedure-parameters object)
                        (procedure-body object)
                        '<procedure-env>)))
        ((compiled-procedure? object)
         (display '<compiled-procedure>))
        (else (display object))))

;; vim:ft=scheme:fdl=0 fdm=marker:ts=4 sw=4 sts=0:
;;;; 5.5.1 翻訳系の構造
;;; 翻訳系のトップレベルの振り分け処理 compile
(define (compile exp target linkage)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage))
        ((assignment? exp)
         (compile-assingment exp target linkage))
        ((definition? exp)
         (compile-definition exp target linkage))
        ((if? exp) (compile-if exp target linkage))
        ((lambda? exp) (compile-lambda exp target linkage))
        ((begin? exp)
         (compile-sequence (begin-actions exp)
                           target
                           linkage))
        ((cond? exp) (compile (cond->if exp) target linkage))
        ((application? exp)
         (compile-application exp target linkage))
        (else
          (error "Unknown expression type -- COMPILE" exp))))

;;; 命令列とスタックの使用
(define (make-instruction-sequence needs modifies statements)
  (list needs modifies statements))

(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))

;;;; 5.5.2 式の翻訳

;;; 接続コードの翻訳
(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
         (make-instruction-sequence '(continue) '()
                                    '((goto (reg continue)))))
        ((eq? linkage 'next)
         (empty-instruction-sequence))
        (else
          (make-instruction-sequence '() '()
                                     `((goto (label ,linkage)))))))

(define (end-with-linkage linkage instruction-sequence)
  (preserving '(continue)
              instruction-sequence
              (compile-linkage linkage)))

;;; 単純式の翻訳
(define (compile-self-evaluating exp target linkage)
  (end-with-linkage linkage
                    (make-instruction-sequence '() (list target)
                                               `((assign ,target (const ,exp))))))

(define (compile-quoted exp target linkage)
  (end-with-linkage linkage
                    (make-instruction-sequence '() (list target)
                                               `((assign ,target (const ,(text-of-quotation exp)))))))

(define (compile-variable exp target linkage)
  (end-with-linkage linkage
                    (make-instruction-sequence '(env) (list target)
                                               `((assign ,target
                                                         (op lookup-variable-value)
                                                         (const ,exp)
                                                         (reg env))))))

(define (compile-assingment exp target linkage)
  (let ((var (assignment-variable exp))
        (get-value-code
          (compile (assignment-value exp) 'val 'next)))
       (end-with-linkage linkage
                         (preserving '(env)
                                     get-value-code
                                     (make-instruction-sequence '(env val) (list target)
                                                                `((perform (op set-variable-value!)
                                                                           (const ,var)
                                                                           (reg val)
                                                                           (reg env))
                                                                  (assign ,target (const ok))))))))

(define (compile-definition exp target linkage)
  (let ((var (definition-variable exp))
        (get-value-code
          (compile (definition-value exp) 'val 'next)))
       (end-with-linkage linkage
                         (preserving '(env)
                                     get-value-code
                                     (make-instruction-sequence '(env val) (list target)
                                                                `((perform (op define-variable!)
                                                                           (const ,var)
                                                                           (reg val)
                                                                           (reg env))
                                                                  (assign ,target (const ok))))))))

; ラベルの生成
(define label-counter 0)

(define (new-label-number)
  (set! label-counter (+ 1 label-counter))
  label-counter)

(define (make-label name)
  (string->symbol
	(string-append (symbol->string name)
				   (number->string (new-label-number)))))

;;; 条件式の翻訳
(define (compile-if exp target linkage)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))
        (after-if (make-label 'after-if)))
       (let ((consequent-linkage
               (if (eq? linkage 'next) after-if linkage)))
            (let ((p-code (compile (if-predicate exp) 'val 'next))
                  (c-code
                    (compile
                      (if-consequent exp) target consequent-linkage))
                  (a-code
                    (compile (if-alternative exp) target linkage)))
                 (preserving '(env continue)
                             p-code
                             (append-instruction-sequences
                               (make-instruction-sequence '(val) '()
                                                          `((test (op false?) (reg val))
                                                            (branch (label ,f-branch))))
                               (parallel-instruction-sequences
                                 (append-instruction-sequences t-branch c-code)
                                 (append-instruction-sequences f-branch a-code))
                               after-if))))))

;;; 並びの翻訳
(define (compile-sequence seq target linkage)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage)
      (preserving '(env continue)
                  (compile (first-exp seq) target 'next)
                  (compile-sequence (rest-exps seq) target linkage))))

;;; lambda 式の翻訳
(define (compile-lambda exp target linkage)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
       (let ((lambda-linkage
               (if (eq? linkage 'next) after-lambda linkage)))
            (append-instruction-sequences
              (tack-on-instrunction-sequence
                (end-with-linkage lambda-linkage
                                  (make-instruction-sequence '(env) (list target)
                                                             `((assign ,target
                                                                       (op make-compiled-procedure)
                                                                       (label ,proc-entry)
                                                                       (reg env)))))
                (compile-lambda-body exp proc-entry))
              after-lambda))))

(define (compile-lambda-body exp proc-entry)
  (let ((formals (lambda-parameters exp)))
       (append-instruction-sequences
         (make-instruction-sequence '(env proc argl) '(env)
                                    `(,proc-entry
                                       (assign env (op compiled-procedure-env) (reg proc))
                                       (assign env
                                               (op extend-environment)
                                               (const ,formals)
                                               (reg argl)
                                               (reg env))))
         (compile-sequence (lambda-body exp) 'val 'return))))

;;;; 5.5.3 組合せの翻訳
(define (compile-application exp target linkage)
  (let ((proc-code (compile (operator exp) 'proc 'next))
        (operand-codes
          (map (lambda (operand) (compile operand 'val 'next))
               (operands exp))))
       (preserving '(env continue)
                   proc-code
                   (preserving '(proc continue)
                               (construct-arglist operand-codes)
                               (compile-procedure-call target linkage)))))

(define (construct-arglist operand-codes)
  (let ((operand-codes (reverse operand-codes)))
       (if (null? operand-codes)
           (make-instruction-sequence '() '(argl)
                                      '((assign argl (const ()))))
           (let ((code-to-get-last-arg
                   (append-instruction-sequences
                     (car operand-codes)
                     (make-instruction-sequence '(val) '(argl)
                                                '((assign argl (op list) (reg val)))))))
                (if (null? (cdr operand-codes))
                    code-to-get-last-arg
                    (preserving '(env)
                                code-to-get-last-arg
                                (code-to-get-rest-args
                                  (cdr operand-codes))))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
          (preserving '(argl)
                      (car operand-codes)
                      (make-instruction-sequence '(val argl) '(argl)
                                                 '((assign argl
                                                           (op cons) (reg val) (reg argl)))))))
       (if (null? (cdr operand-codes))
           code-for-next-arg
           (preserving '(env)
                       code-for-next-arg
                       (code-to-get-rest-args (cdr operand-codes))))))

;;; 手続きの作用
(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (after-call (make-label 'after-call)))
       (let ((compiled-linkage
               (if (eq? linkage 'next) after-call linkage)))
            (append-instruction-sequences
              (make-instruction-sequence
               '(proc) '()
               `((test (op primitive-procedure?) (reg proc))
                 (branch (label ,primitive-branch))))
              (make-instruction-sequence
               '(proc) '()
               `((test (op compiled-procedure?) (reg proc))
                 (branch (label ,compiled-branch))))
              (parallel-instruction-sequences
                (cond ((and (eq? target 'val)
                            (not (eq? compiled-linkage 'return)))
                       (make-instruction-sequence
                        '(proc)all-regs
                        `((assign continue (label ,compiled-linkage))
                         (save continue)
                         (goto (reg compapp)))))
                      ((and (not (eq? target 'val))
                            (not (eq? compiled-linkage 'return)))
                       (let ((proc-return (make-label 'proc-return)))
                            (make-instruction-sequence
                             '(proc) all-regs
                             `((assign continue (label ,proc-return))
                               (save continue)
                               (goto (reg compapp))
                               ,proc-return
                               (assign ,target (reg val))
                               (goto (label ,compiled-linkage))))))
                      ((and (eq? target 'val)
                            (eq? compiled-linkage 'return))
                       (make-instruction-sequence
                         '(proc continue) all-regs
                         '((save continue)
                           (goto (reg compapp)))))
                      ((and (not (eq? target 'val))
                            (eq? compiled-linkage 'return))
                       (error "return linkage, target not val -- COMPILE"
                              target)))
                  (append-instruction-sequences
                    compiled-branch
                    (compile-proc-appl target compiled-linkage)))
              (parallel-instruction-sequences
                (append-instruction-sequences
                   compiled-branch
                   (compile-proc-appl target compiled-linkage))
                (append-instruction-sequences
                   primitive-branch
                   (end-with-linkage
                     linkage
                     (make-instruction-sequence
                       '(proc argl)
                       (list target)
                       `((assign ,target
                                 (op apply-primitive-procedure)
                                 (reg proc)
                                 (reg argl)))))))
              after-call))))

;;; 翻訳した手続きの作用
(define (compile-proc-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence
           '(proc) all-regs
           `((assign continue (label ,linkage))
             (assign val (op compiled-procedure-entry)
                         (reg proc))
             (goto (reg val)))))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
              (make-instruction-sequence
               '(proc) all-regs
               `((assign continue (label ,proc-return))
                 (assign val
                         (op compiled-procedure-entry)
                         (reg proc))
                 (goto (reg val))
                 ,proc-return
                 (assign ,target (reg val))
                 (goto (label ,linkage))))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence
           '(proc continue) all-regs
           '((assign val
                     (op compiled-procedure-entry)
                     (reg proc))
             (goto (reg val)))))
        ((and (not (eq? target 'val)) (eq? linkage 'return))
         (error "return linkage, target not val -- COMPILE"
                target))))


(define all-regs '(env proc val argl continue))

;;;; 5.5.4 命令列の組合せ
(define (registers-needed s)
  (if (symbol? s) '() (car s)))

(define (registers-modified s)
  (if (symbol? s) '() (cadr s)))

(define (statements s)
  (if (symbol? s) (list s) (caddr s)))

(define (needs-register? seq reg)
  (memq reg (registers-needed seq)))

(define (modifies-register? seq reg)
  (memq reg (registers-modified seq)))

(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
      (list-union (registers-needed seq1)
                  (list-difference (registers-needed seq2)
                                   (registers-modified seq1)))
      (list-union (registers-modified seq1)
                  (registers-modified seq2))
      (append (statements seq1) (statements seq2))))
  (define (append-seq-list seqs)
    (if (null? seqs)
        (empty-instruction-sequence)
        (append-2-sequences (car seqs)
                            (append-seq-list (cdr seqs)))))
  (append-seq-list seqs))

(define (list-union s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2) (list-union (cdr s1) s2))
        (else (cons (car s1) (list-union (cdr s1) s2)))))

(define (list-difference s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2) (list-difference (cdr s1) s2))
        (else (cons (car s1)
                    (list-difference (cdr s1) s2)))))

(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
           (if (and (needs-register? seq2 first-reg)
                    (modifies-register? seq1 first-reg))
               (preserving (cdr regs)
                           (make-instruction-sequence
                             (list-union (list first-reg)
                                         (registers-needed seq1))
                             (list-difference (registers-modified seq1)
                                              (list first-reg))
                             (append `((save ,first-reg))
                                     (statements seq1)
                                     `((restore ,first-reg))))
                           seq2)
               (preserving (cdr regs) seq1 seq2)))))

(define (tack-on-instrunction-sequence seq body-seq)
  (make-instruction-sequence
    (registers-needed seq)
    (registers-modified seq)
    (append (statements seq) (statements body-seq))))

(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
    (list-union (registers-needed seq1)
                (registers-needed seq2))
    (list-union (registers-modified seq1)
                (registers-modified seq2))
    (append (statements seq1) (statements seq2))))

;;; 出力結果をパース
(define (parse-compiled-code lis)
  (if (not (null? lis))
      (begin
        (if (pair? (caar lis))
            (map (lambda (x)
                         (if (symbol? x)
                             (print x)
                             (print "  " x)))
                 (car lis))
            (print (car lis)))
        (parse-compiled-code (cdr lis)))))

;; vim:ft=scheme:fdl=0 fdm=marker:ts=4 sw=4 sts=0:

;; 5.2 レジスタ計算機シミュレータ


;;; 5.2.1 計算機モデル
(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
       (for-each (lambda (register-name)
                         ((machine 'allocate-register) register-name))
                 register-names)
       ((machine 'install-operations) ops)
       ((machine 'install-instruction-sequence)
        (assemble controller-text machine))
       machine))

;;; レジスタ
(define (make-register name)
  (let ((contents '*unassigned*))
       (define (dispatch message)
         (cond ((eq? message 'get) contents)
               ((eq? message 'set)
                (lambda (value) (set! contents value)))
               (else
                 (error "Unknown request -- REGISTER" message))))
       dispatch))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))

;;; スタック
;(define (make-stack)
;  (let ((s '()))
;       (define (push x)
;         (set! s (cons x s)))
;       (define (pop)
;         (if (null? s)
;             (error "Empty stack -- POP")
;             (let ((top (car s)))
;                  (set! s (cdr s))
;                  top)))
;       (define (initialize)
;         (set! s '())
;         'done)
;       (define (dispatch message)
;         (cond ((eq? message 'push) push)
;               ((eq? message 'pop) (pop))
;               ((eq? message 'initialize) (initialize))
;               (else (error "Unknown request -- STACK"
;                            message))))
;       dispatch))

;; 性能監視版 make-stack
(define (make-stack)
  (let ((s '())
        (number-pushes 0)
        (max-depth 0)
        (current-depth 0))
       (define (push x)
         (set! s (cons x s))
         (set! number-pushes (+ 1 number-pushes))
         (set! current-depth (+ 1 current-depth))
         (set! max-depth (max current-depth max-depth)))
       (define (pop)
         (if (null? s)
             (error "Empty stack -- POP")
             (let ((top (car s)))
                  (set! s (cdr s))
                  (set! current-depth (- current-depth 1))
                  top)))
       (define (initialize)
         (set! s '())
         (set! number-pushes 0)
         (set! max-depth 0)
         (set! current-depth 0)
         'done)
       (define (print-statistics)
         (newline)
         (display (list 'total-pushes '= number-pushes
                        'max-depth '= max-depth)))
       (define (dispatch message)
         (cond ((eq? message 'push) push)
               ((eq? message 'pop) (pop))
               ((eq? message 'initialize) (initialize))
               ((eq? message 'print-statistics) (print-statistics))
               (else
                 (error "Unknown request -- STACK" message))))
       dispatch))

(define (pop stack)
  (stack 'pop))

(define (push stack value)
  ((stack 'push) value))

;;; 基本計算機
(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
       (let ((the-ops
               (list
                 (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 (list 'print-stack-statistics ;; 性能監視
                       (lambda () (stack 'print-statistics)))))
             (register-table
               (list (list 'pc pc) (list 'flag flag))))
            (define (allocate-register name)
              (if (assoc name register-table)
                  (error "Multiply defined register: " name)
                  (set! register-table
                        (cons (list name (make-register name))
                              register-table)))
              'register-allocated)
            (define (lookup-register name)
              (let ((val (assoc name register-table)))
                   (if val
                       (cadr val)
                       (error "Unknown register:" name))))
            (define (execute)
              (let ((insts (get-contents pc)))
                   (if (null? insts)
                       'done
                       (begin
                         ((instruction-execution-proc (car insts)))
                         (execute)))))
            (define (dispatch message)
              (cond ((eq? message 'start)
                     (set-contents! pc the-instruction-sequence)
                     (execute))
                    ((eq? message 'install-instruction-sequence)
                     (lambda (seq) (set! the-instruction-sequence seq)))
                    ((eq? message 'allocate-register) allocate-register)
                    ((eq? message 'get-register) lookup-register)
                    ((eq? message 'install-operations)
                     (lambda (ops) (set! the-ops (append the-ops ops))))
                    ((eq? message 'stack) stack)
                    ((eq? message 'operations) the-ops)
                    (else (error "Unknown request -- MACHINE" message))))
            dispatch)))

(define (start machine)
  (machine 'start))

(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))

(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

;;; 5.2.2 アセンブラ
(define (assemble controller-text machine)
  (extract-labels controller-text
                  (lambda (insts labels)
                          (update-insts! insts labels machine)
                          insts)))

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
                      (lambda (insts labels)
                              (let ((next-inst (car text)))
                                   (if (symbol? next-inst)
                                       (receive insts
                                                (cons (make-label-entry next-inst
                                                                        insts)
                                                      labels))
                                       (receive (cons (make-instruction next-inst)
                                                      insts)
                                                labels)))))))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
       (for-each
         (lambda (inst)
                 (set-instruction-execution-proc!
                   inst
                   (make-execution-procedure
                     (instruction-text inst) labels machine
                     pc flag stack ops)))
         insts)))

(define (make-instruction text)
  (cons text '()))

(define (instruction-text inst)
  (car inst))

(define (instruction-execution-proc inst)
  (cdr inst))

(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
       (if val
           (cdr val)
           (error "Undefined label -- ASSEMBLE" label-name))))

;;; 5.2.3 命令の実行手続きの生成
(define (make-execution-procedure inst labels machine
                                  pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        (else (error "Unknown instruction type -- ASSEMBLE"
                     inst))))

;;; assign 命令
(define (make-assign inst machine labels operations pc)
  (let ((target
          (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
       (let ((value-proc
               (if (operation-exp? value-exp)
                   (make-operation-exp
                     value-exp machine labels operations)
                   (make-primitive-exp
                     (car value-exp) machine labels))))
            (lambda () ; assign の実行手続き
                    (set-contents! target (value-proc))
                    (advance-pc pc)))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))

(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

;;; test 命令
(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
       (if (operation-exp? condition)
           (let ((condition-proc
                   (make-operation-exp
                     condition machine labels operations)))
                (lambda ()
                        (set-contents! flag (condition-proc))
                        (advance-pc pc)))
           (error "Bad TEST instruction -- ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (cdr test-instruction))

;;; branch 命令
(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
       (if (label-exp? dest)
           (let ((insts
                   (lookup-label labels (label-exp-label dest))))
                (lambda ()
                        (if (get-contents flag)
                            (set-contents! pc insts)
                            (advance-pc pc))))
           (error "Bad BRANCH instruction -- ASSEMBLE" inst))))

(define (branch-dest branch-instruction)
  (cadr branch-instruction))

;;; goto 命令
(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
       (cond ((label-exp? dest)
              (let ((insts
                      (lookup-label labels
                                    (label-exp-label dest))))
                   (lambda () (set-contents! pc insts))))
             ((register-exp? dest)
              (let ((reg
                      (get-register machine
                                    (register-exp-reg dest))))
                   (lambda ()
                           (set-contents! pc (get-contents reg)))))
             (else (error "Bad GOTO instruction -- ASSEMBLE"
                          inst)))))

(define (goto-dest goto-instruction)
  (cadr goto-instruction))

;;; その他の命令
(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
       (lambda ()
               (push stack (get-contents reg))
               (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
       (lambda ()
               (set-contents! reg (pop stack))
               (advance-pc pc))))

(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
       (if (operation-exp? action)
           (let ((action-proc
                   (make-operation-exp
                     action machine labels operations)))
                (lambda ()
                        (action-proc)
                        (advance-pc pc)))
           (error "Bad PERFORM instruction -- ASSEMBLE" inst))))

(define (perform-action inst) (cdr inst))

;;; 部分式の実行手続き
(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
              (lambda () c)))
        ((label-exp? exp)
         (let ((insts
                 (lookup-label labels
                               (label-exp-label exp))))
              (lambda () insts)))
        ((register-exp? exp)
         (let ((r (get-register machine
                                (register-exp-reg exp))))
              (lambda () (get-contents r))))
        (else
          (error "Unknown expression type -- ASSEMBLE" exp))))

(define (register-exp? exp) (tagged-list? exp 'reg))

(define (register-exp-reg exp) (cadr exp))

(define (constant-exp? exp) (tagged-list? exp 'const))

(define (constant-exp-value exp) (cadr exp))

(define (label-exp? exp) (tagged-list? exp 'label))

(define (label-exp-label exp) (cadr exp))

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
          (map (lambda (e)
                       (make-primitive-exp e machine labels))
               (operation-exp-operands exp))))
       (lambda ()
               (apply op (map (lambda (p) (p)) aprocs)))))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))

(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))

(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
       (if val
           (cadr val)
           (error "Unknown operation -- ASSEMBLE" symbol))))

;; vim:ft=scheme:fdl=0 fdm=marker:ts=4 sw=4 sts=0:
;;; Explicit-Control Evaluator

(define (empty-arglist) '())

(define (last-operand? ops)
  (null? (cdr ops)))

(define (adjoin-arg arg arglist)
  (append arglist (list arg)))

(define (get-global-environment)
  the-global-environment)

(define (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env))

(define (compiled-procedure? proc)
  (tagged-list? proc 'compiled-procedure))

(define (compiled-procedure-entry c-proc)
  (cadr c-proc))

(define (compiled-procedure-env c-proc)
  (caddr c-proc))

(define (compile-and-go expression)
  (let ((instructions
          (assemble (statements
                      (compile expression 'val 'return))
                    eceval)))
       (set! the-global-environment (setup-c-and-r-environment))
       (set-register-contents! eceval 'val instructions)
       (set-register-contents! eceval 'flag true)
       (start eceval)))

(define (compile-and-run? proc)
  (tagged-list? proc 'compile-and-run))

(define (compile-and-run expression)
  (let ((instructions
          (assemble (statements
                      (compile expression 'val 'return))
                    eceval)))
       (set-register-contents! eceval 'val instructions)
       (set-register-contents! eceval 'flag true)
       (start eceval)))

(define (make-register-machine) reg-machine)

(define eceval-operations
  (list (list 'self-evaluating? self-evaluating?)
        (list 'variable? variable?)
        (list 'quoted? quoted?)
        (list 'assignment? assignment?)
        (list 'definition? definition?)
        (list 'if? if?)
        (list 'lambda? lambda?)
        (list 'begin? begin?)
        (list 'application? application?)
        (list 'lookup-variable-value lookup-variable-value)
        (list 'text-of-quotation text-of-quotation)
        (list 'lambda-parameters lambda-parameters)
        (list 'lambda-body lambda-body)
        (list 'make-procedure make-procedure)
        (list 'operands operands)
        (list 'operator operator)
        (list 'empty-arglist empty-arglist)
        (list 'no-operands? no-operands?)
        (list 'first-operand first-operand)
        (list 'last-operand? last-operand?)
        (list 'adjoin-arg adjoin-arg)
        (list 'rest-operands rest-operands)
        (list 'primitive-procedure? primitive-procedure?)
        (list 'compound-procedure? compound-procedure?)
        (list 'apply-primitive-procedure apply-primitive-procedure)
        (list 'procedure-parameters procedure-parameters)
        (list 'procedure-environment procedure-environment)
        (list 'extend-environment extend-environment)
        (list 'procedure-body procedure-body)
        (list 'begin-actions begin-actions)
        (list 'first-exp first-exp)
        (list 'last-exp? last-exp?)
        (list 'rest-exps rest-exps)
        (list 'if-predicate if-predicate)
        (list 'true? true?)
        (list 'if-alternative if-alternative)
        (list 'if-consequent if-consequent)
        (list 'assignment-variable assignment-variable)
        (list 'assignment-value assignment-value)
        (list 'set-variable-value! set-variable-value!)
        (list 'definition-variable definition-variable)
        (list 'definition-value definition-value)
        (list 'define-variable! define-variable!)
        (list 'prompt-for-input prompt-for-input)
        (list 'read read)
        (list 'get-global-environment get-global-environment)
        (list 'announce-output announce-output)
        (list 'user-print user-print)
        (list 'make-compiled-procedure make-compiled-procedure)
        (list 'compiled-procedure? compiled-procedure?)
        (list 'compiled-procedure-entry compiled-procedure-entry)
        (list 'compiled-procedure-env compiled-procedure-env)
        (list 'compile-and-go compile-and-go)
        (list 'compile-and-run? compile-and-run?)
        (list 'compile-and-run compile-and-run)
        (list 'list list)
        (list 'cons cons)
        (list 'false? false?)
        (list 'make-register-machine make-register-machine)
        (list 'compile compile)
        (list 'statements statements)
        (list 'assemble assemble)
        (list 'print-stack-statistics)
        ))

(define eceval
  (make-machine
    '(exp env val proc argl continue unev compapp)
    eceval-operations
    '(
      (assign compapp (label compound-apply))
      (branch (label external-entry)) ; flag が設定してあれば分岐する
      read-eval-print-loop
        (perform (op initialize-stack))
        (perform
          (op prompt-for-input) (const ";;; EC-Eval input:"))
        (assign exp (op read))
        (assign env (op get-global-environment))
        (assign continue (label print-result))
        (goto (label eval-dispatch))
      external-entry
        (perform (op initialize-stack))
        (assign env (op get-global-environment))
        (assign continue (label print-result))
        (goto (reg val))
      print-result
        (perform (op print-stack-statistics)) ;; 評価器の性能表示
        (perform
          (op announce-output) (const ";;; EC-Eval value:"))
        (perform (op user-print) (reg val))
        (goto (label read-eval-print-loop))
      ;; 5.4.1 積極制御評価器の中核
      eval-dispatch
        (test (op self-evaluating?) (reg exp))
        (branch (label ev-self-eval))
        (test (op variable?) (reg exp))
        (branch (label ev-variable))
        (test (op quoted?) (reg exp))
        (branch (label ev-quoted))
        (test (op assignment?) (reg exp))
        (branch (label ev-assignment))
        (test (op definition?) (reg exp))
        (branch (label ev-definition))
        (test (op if?) (reg exp))
        (branch (label ev-if))
        (test (op lambda?) (reg exp))
        (branch (label ev-lambda))
        (test (op begin?) (reg exp))
        (branch (label ev-begin))
        (test (op application?) (reg exp))
        (branch (label ev-application))
        (goto (label unknown-expression-type))
      ;; 5.4.1 単純式の評価
      ev-self-eval
        (assign val (reg exp))
        (goto (reg continue))
      ev-variable
        (assign val (op lookup-variable-value) (reg exp) (reg env))
        (goto (reg continue))
      ev-quoted
        (assign val (op text-of-quotation) (reg exp))
        (goto (reg continue))
      ev-lambda
        (assign unev (op lambda-parameters) (reg exp))
        (assign exp (op lambda-body) (reg exp))
        (assign val (op make-procedure) (reg unev) (reg exp) (reg env))
        (goto (reg continue))
      ;; 5.4.1 手続き作用の評価
      ev-application
        (save continue)
        (save env)
        (assign unev (op operands) (reg exp))
        (save unev)
        (assign exp (op operator) (reg exp))
        (assign continue (label ev-appl-did-operator))
        (goto (label eval-dispatch))
      ev-appl-did-operator
        (restore unev) ; 被演算子
        (restore env)
        (assign argl (op empty-arglist))
        (assign proc (reg val)) ; 演算子
        (test (op no-operands?) (reg unev))
        (branch (label apply-dispatch))
        (save proc)
      ev-appl-operand-loop
        (save argl)
        (assign exp (op first-operand) (reg unev))
        (test (op last-operand?) (reg unev))
        (branch (label ev-appl-last-arg))
        (save env)
        (save unev)
        (assign continue (label ev-appl-accumulate-arg))
        (goto (label eval-dispatch))
      ev-appl-accumulate-arg
        (restore unev)
        (restore env)
        (restore argl)
        (assign argl (op adjoin-arg) (reg val) (reg argl))
        (assign unev (op rest-operands) (reg unev))
        (goto (label ev-appl-operand-loop))
      ev-appl-last-arg
        (assign continue (label ev-appl-accum-last-arg))
        (goto (label eval-dispatch))
      ev-appl-accum-last-arg
        (restore argl)
        (assign argl (op adjoin-arg) (reg val) (reg argl))
        (restore proc)
        (goto (label apply-dispatch))
      ;; 手続き作用
      apply-dispatch
        (test (op primitive-procedure?) (reg proc))
        (branch (label primitive-apply))
        (test (op compound-procedure?) (reg proc))
        (branch (label compound-apply))
        (test (op compiled-procedure?) (reg proc))
        (branch (label compiled-apply))
        (goto (label unknown-procedure-type))
      compiled-apply
        (restore continue)
        (assign val (op compiled-procedure-entry) (reg proc))
        (goto (reg val))
      primitive-apply
        (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
        (restore continue)
        (goto (reg continue))
      compound-apply
        (assign unev (op procedure-parameters) (reg proc))
        (assign env (op procedure-environment) (reg proc))
        (assign env (op extend-environment) (reg unev) (reg argl) (reg env))
        (assign unev (op procedure-body) (reg proc))
        (goto (label ev-sequence))
      ;; 5.4.2 並びの評価と末尾再帰
      ev-begin
        (assign unev (op begin-actions) (reg exp))
        (save continue)
        (goto (label ev-sequence))
      ev-sequence
        (assign exp (op first-exp) (reg unev))
        (test (op last-exp?) (reg unev))
        (branch (label ev-sequence-last-exp))
        (save unev)
        (save env)
        (assign continue (label ev-sequence-continue))
        (goto (label eval-dispatch))
      ev-sequence-continue
        (restore env)
        (restore unev)
        (assign unev (op rest-exps) (reg unev))
        (goto (label ev-sequence))
      ev-sequence-last-exp
        (restore continue)
        (goto (label eval-dispatch))
      ;; 5.4.3 条件式、代入および定義
      ev-if
        (save exp) ; 後のために式を退避
        (save env)
        (save continue)
        (assign continue (label ev-if-decide))
        (assign exp (op if-predicate) (reg exp))
        (goto (label eval-dispatch)) ; 述語を評価
      ev-if-decide
        (restore continue)
        (restore env)
        (restore exp)
        (test (op true?) (reg val))
        (branch (label ev-if-consequent))
      ev-if-alternative
        (assign exp (op if-alternative) (reg exp))
        (goto (label eval-dispatch))
      ev-if-consequent
        (assign exp (op if-consequent) (reg exp))
        (goto (label eval-dispatch))
      ;; 代入
      ev-assignment
        (assign unev (op assignment-variable) (reg exp))
        (save unev) ; 後のために変数を退避
        (assign exp (op assignment-value) (reg exp))
        (save env)
        (save continue)
        (assign continue (label ev-assignment-1))
        (goto (label eval-dispatch)) ; 代入する値を評価
      ev-assignment-1
        (restore continue)
        (restore env)
        (restore unev)
        (perform
          (op set-variable-value!) (reg unev) (reg val) (reg env))
        (assign val (const ok))
        (goto (reg continue))
      ;; 定義
      ev-definition
        (assign unev (op definition-variable) (reg exp))
        (save unev) ; 後のために変数を退避
        (assign exp (op definition-value) (reg exp))
        (save env)
        (save continue)
        (assign continue (label ev-definition-1))
        (goto (label eval-dispatch)) ; 定義する値を評価
      ev-definition-1
        (restore continue)
        (restore env)
        (restore unev)
        (perform
          (op define-variable!) (reg unev) (reg val) (reg env))
        (assign val (const ok))
        (goto (reg continue))
      ev-compile-and-run
        (perform (op compile-and-run) (reg exp))
        (goto (reg continue))
      ;; 5.4.4 評価の実行
      unknown-expression-type
        (assign val (const unknown-expression-type-error))
        (goto (label signal-error))
      unknown-procedure-type
        (restore continue) ; スタックを清掃する (apply-dispatch から)
        (assign val (const unknown-procedure-type-error))
        (goto (label signal-error))
      signal-error
        (perform (op user-print) (reg val))
        (goto (label read-eval-print-loop))
      )))

(define reg-machine
  (make-machine
    '(exp env val proc argl continue unev compapp machine)
    eceval-operations
    '(
        (assign machine (op make-register-machine))
      read-eval-print-loop
        (perform (op initialize-stack))
        (perform
          (op prompt-for-input) (const ";;; RM-Eval input:"))
        (assign exp (op read))
        (assign env (op get-global-environment))
        (assign continue (label print-result))
        (goto (label compile))
      print-result
        (perform (op print-stack-statistics))
        (perform
          (op announce-output) (const ";;; RM-Eval value:"))
        (perform (op user-print) (reg val))
        (goto (label read-eval-print-loop))
      compile
        (assign exp (op compile) (reg exp) (const val) (const return))
        (assign exp (op statements) (reg exp))
        (goto (label assemble))
      assemble
        (assign val (op assemble) (reg exp) (reg machine))
        (goto (reg val))
 )))
      


(define (start-reg-machine)
  (set! the-global-environment (setup-environment))
  (set-register-contents! reg-machine 'flag false)
  (start reg-machine))

