(env)
(val)
  (assign val (op make-compiled-procedure) (label entry38) (reg env))
  (goto (label after-lambda39))
entry38
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const =) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch43))
compiled-branch44
  (assign continue (label after-call45))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch43
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call45
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch41))
true-branch40
  (assign val (const 1))
  (goto (reg continue))
false-branch41
  (assign proc (op lookup-variable-value) (const *) (reg env))
  (save continue)
  (save proc)
  (save env)
  (assign proc (op lookup-variable-value) (const factorial-alt) (reg env))
  (save proc)
  (assign proc (op lookup-variable-value) (const -) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch46))
compiled-branch47
  (assign continue (label after-call48))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch46
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call48
  (assign argl (op list) (reg val))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch49))
compiled-branch50
  (assign continue (label after-call51))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch49
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call51
  (assign argl (op list) (reg val))
  (restore env)
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch52))
compiled-branch53
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch52
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
after-call54
after-if42
after-lambda39
  (perform (op define-variable!) (const factorial-alt) (reg val) (reg env))
  (assign val (const ok))
