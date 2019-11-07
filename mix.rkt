#lang racket

(require racket/trace)

;int-aux;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-namespace-anchor roguelike)
(define ns (namespace-anchor->namespace roguelike))
;(namespace-attach-module (current-namespace) "aux.rkt" ns)
(define (ns-eval expr)
  (eval expr ns))

(define (qu var)
  (cons 'quote (list var)))

(define (zip x y)
  (map cons x y))

(define (unqu l)
  (match l
    [`(,x) x]
    [_     l]))

(define (init-env env blocks)
  (foldr (lambda (a e) (dict-set e (car a) (cdr a))) env blocks))

(define (env<-assign env key value)
  (dict-set env key (int-expr env (unqu value))))

(define (get-next-body env label-env label)
  (int-assigns-jump env label-env (dict-ref label-env label)))

(define find-name
  '((read name namelist valuelist)
   (search (if (equal? name (car namelist)) goto found goto cont))
   (cont (valuelist := cdr valuelist)
         (namelist := cdr namelist)
         (goto search))
   (found (return car valuelist))))

(define tm-example
  '((if 0 goto 3)
    (right)
    (goto 0)
    (write 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;------------------------------------- FlowChart Interpreter -----------------------------------;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;task0;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (int program data)
  (match program
    [`(,read . ,blocks) (let
                            ([env (init-env #hash() (zip (cdr read) data))]
                             [label-env (init-env #hash() blocks)]
                             [assign-jump (cdar blocks)])
                            (int-assigns-jump env label-env assign-jump))]
    [else               (error "int: empty program")]))

;(trace int)

(define (int-assigns-jump env label-env assigns-jump)
  (match assigns-jump
    [`((,x . (:= . ,y)) . ,a-j) (int-assigns-jump (env<-assign env x y) label-env a-j)]
    [`,jump                     (int-jump env label-env (if (list? (car jump)) (car jump) jump))]
    [else                       (error "int-assigns-jump: error assign or lose jump")]))

;(trace int-assigns-jump)

(define (int-jump env label-env jump)
  (match jump
    [`(goto ,lbl)                (get-next-body env label-env lbl)]
    [`(if ,cond goto ,t goto ,f) (get-next-body env label-env (if (int-expr env cond) t f))]
    [`(return . ,expr)           (int-expr env (unqu expr))]
    [else                        (error "int-jump: error jump")]))

;(trace int-jump)

(define (int-expr env expr)
  (match expr
    [`(if ,cond ,t ,f) (int-expr env (if (int-expr env cond) t f))]
    [`(,name)          (if (dict-has-key? env name) (dict-ref env name) (list name))]
    [`(quote . ,args)  (unqu args)]
    [`(,op . ,args)    (let
                           ([red-args (map (lambda (x) (qu (int-expr env x))) args)])
                           (ns-eval (cons op red-args)))]
    [`,name            (if (dict-has-key? env name) (dict-ref env name) name)]))

;(trace int-expr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;--------------------------------- TuringMachine Interpreter -----------------------------------;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tm-int
  '((read Q Right)
    (init (Qtail := Q)
          (Left := '())
          (goto loop))
    (loop (if (null? Qtail) goto stop goto cont))
    (cont (Instruction := car Qtail)
          (Qtail := cdr Qtail)
          (Operator := car Instruction)
          (if (equal? Operator 'right) goto do-right goto cont1))
    (cont1 (if (equal? Operator 'left) goto do-left goto cont2))
    (cont2 (if (equal? Operator 'write) goto do-write goto cont3))
    (cont3 (if (equal? Operator 'goto) goto do-goto goto cont4))
    (cont4 (if (equal? Operator 'if) goto do-if goto error))
    (do-right (Left := cons (car Right) Left)
              (Right := cdr Right)
              (goto loop))
    (do-left (Right := cons (car Left) Right)
             (Left := cdr Left)
             (goto loop))
    (do-write (Symbol := cadr Instruction)
              (Right := cons Symbol (cdr Right))
              (goto loop))
    (do-goto (NextLabel := cadr Instruction)
             (Qtail := list-tail Q NextLabel)
             (goto loop))
    (do-if (Symbol := cadr Instruction)
           (NextLabel := cadddr Instruction)
           (if (equal? Symbol (car Right)) goto jump goto loop))
    (jump (Qtail := list-tail Q NextLabel)
          (goto loop))
    (error (return ('syntaxerror: Instruction)))
    (stop (return Right))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;------------------------------------------ execute --------------------------------------------;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;(displayln "INT-CHECK")
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;(displayln "`find-name`:")
;(equal? (int find-name '(y (x y z) (1 2 3))) 2)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;(displayln "`tm-example`:")
;(equal? (int tm-int `(,tm-example ,'(1 1 0 1 0 1))) '(1 1 0 1))
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;mix-aux;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (env-label pp vs)
  `(,pp . ,vs))

(define (is-static expr static)
  (match expr
    [`(quote . ,_) #t]
    [`(,_ . ,args) (andmap (lambda (e) (is-static e static)) args)]
    [`,x           (if (not (member x static)) #f #t)])
  )

(define (update-vs vs key val)
  (hash-set vs key (int-expr (hash->list vs) (unqu val))))

(define (reduce expr vs)
  (match expr 
    [`(,x) (list (car (reduce-with-flag x vs)))]
    [_     (car (reduce-with-flag expr vs))]))

(define (reduce-with-flag expr vs)
  (match expr
    [`(quote . ,args) (cons expr #t)]
    [`(,op . ,args)   (let*
                        ([red-args (map (lambda (e) (reduce-with-flag e vs)) args)]
                         [cond     (andmap cdr red-args)]
                         [new-args (map car red-args)])
                        (if cond
                          (cons (qu-if vs `(,op . ,new-args)) #t)
                          (cons `(,op . ,new-args) #f)))]
    [`,x              (if (hash-has-key? vs x)
                       (cons (qu (hash-ref vs x)) #t)
                       (cons x #f))]))

(define (qu-if vs expr) 
  (match expr
    [`(quote . _) expr]
    [_            (qu (int-expr (hash->list vs) expr))]))

(define (update-pending pending vs marked t f)
  (set-union
     pending
     (set-subtract (list->set (list (cons t vs) (cons f vs))) marked)))

(define tm-div
  (cons '(Q Qtail Instruction Operator Symbol NextLabel) '(Right Left)))

(define (tm-vs source)
  (list (cons 'Q source)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;--------------------------------------- Pretty Printer ----------------------------------------;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (pretty-block raw-labels block)
  (let
    ([label (index-of raw-labels (car block))]
     [rev-instrs (cdr (reverse (cdr block)))]
     [upd-last (map (lambda (x)
                      (if (index-of raw-labels x) (index-of raw-labels x) x)) (last block))])
    (cons label (reverse (cons upd-last rev-instrs)))))

(define (pretty program)
  (let
    ([raw-labels (map car (cdr program))])
    (cons (car program) (map (lambda (arg) (pretty-block raw-labels arg)) (cdr program)))))

;(trace pretty)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;------------------------------------------- Mixer ---------------------------------------------;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;task1;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define mix
  '((read program div vs0)
    (init (Pending := list->set (list (cons (caadr program) (make-immutable-hash vs0))))
          (static := car div)
          (dynamic := cdr div)
          (Marked := list->set ())
          (Res := (list (cons 'read (set-subtract (cdar program) static))))
          (program := make-immutable-hash (cdr program))
          (goto loop))

    (loop (if (set-empty? Pending) goto stop goto loop-begin))
    (loop-begin (PPVS := set-first Pending)
                (PP := car PPVS)
                (VS := cdr PPVS)
                (Pending := set-rest Pending)
                (Marked := set-add Marked PPVS)
                (BB := hash-ref program PP ())
                (Code := list (env-label PP VS))
                (goto inner-loop))
    
    (inner-loop (if (empty? BB)
                    goto loop-end
                    goto inner-loop-body))
    (inner-loop-body (Command := car BB)
                     (BB := cdr BB)
                     (goto is-assign))
    (is-assign (if (equal? (second Command) ':=)  goto do-assign goto is-goto))
    (is-goto   (if (equal? (car Command) 'goto)   goto do-goto   goto is-if))
    (is-if     (if (equal? (car Command) 'if)     goto do-if     goto is-return))
    (is-return (if (equal? (car Command) 'return) goto do-return goto do-fail))
    
    (do-assign (if (is-static (first Command) static)
                   goto do-static-assign
                   goto do-dynamic-assign))
    (do-static-assign (VS := update-vs VS (first Command) (cddr Command))
                      (goto inner-loop))
    (do-dynamic-assign (Code := append Code
                             (list (append (list (first Command) ':=) (reduce (cddr Command) VS))))
                       (goto inner-loop))

    (do-goto (BB := hash-ref program (second Command) ())
             (goto inner-loop))

    (do-if (if (is-static (second Command) static)
               goto do-static-if
               goto do-dynamic-if))

    (do-static-if (if (int-expr (hash->list VS) (second Command))
                      goto do-static-if-true
                      goto do-static-if-false))
    (do-static-if-true (BB := hash-ref program (fourth Command) ())
                       (goto inner-loop))
    (do-static-if-false (BB := hash-ref program (sixth Command) ())
                        (goto inner-loop))
    (do-dynamic-if (Pending := update-pending Pending VS Marked (fourth Command) (sixth Command))
                   (Code := append Code
                         (list (append (append (list 'if)
                           (list (reduce (second Command) VS)))
                           (list 'goto (env-label (fourth Command) VS)
                                 'goto (env-label (sixth Command) VS)))))
                   (goto inner-loop))
    
    (do-return (Code := append Code
                     (list (append (list 'return) (reduce (cdr Command) VS))))
               (goto inner-loop))
    
    (loop-end (Res := append Res (list Code))
              (goto loop))
    (do-fail (return error "MIX: error instruction"))
    (stop (return Res))))

;(trace mix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;------------------------------------------ execute --------------------------------------------;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(displayln "MIX-CHECK")

(define mix-div
  (cons '(program div static dynamic) '(Pending Marked Res PPVS PP VS vs0 BB Code Command)))

(define (mix-vs source-int source-div)
  `(,(cons 'program source-int) ,(cons 'div source-div)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;(displayln "`find-name`:")
;
;(define find-name-apply-2-args
;  (int mix `(,find-name ,(cons '(name namelist) '(valuelist)) ,(list (cons 'name 'y) (cons 'namelist '(x y z))))))
;
;(equal? (int find-name-apply-2-args `(,'(1 2 3))) 2)
;find-name-apply-2-args
;
;;task2;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;(displayln "`I proj`:")
;
;(define (proj1 source)
;  (pretty
;    (int mix `(,tm-int ,tm-div ,(tm-vs source)))))
;
;(displayln "`source`:")
;tm-example
;
;(displayln "`target-1`:")
;(define target-1
;  (proj1 tm-example))
;target-1
;
;(displayln "check `target-1`:")
;(equal? (int target-1 `(,'(1 1 0 1 0 1))) '(1 1 0 1))
;
;;task3;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;(displayln "`II proj`:")
;
;(define proj2
;  (pretty
;    (int mix `(,mix ,mix-div ,(mix-vs tm-int tm-div)))))
;
;(displayln "`proj2/comp`:")
;;proj2
;
;(displayln "`source`:")
;tm-example
;
;(displayln "`target-2`:")
;(define target-2
;  (pretty
;     (int proj2 `(,(tm-vs tm-example)))))
;target-2
;
;(displayln "check `target-2`:")
;(equal? (int target-2 `(,'(1 1 0 1 0 1))) '(1 1 0 1))
;
;task6-1;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(displayln "`III proj`:")

(define proj3
  (pretty
    (int mix `(,mix ,mix-div ,(mix-vs mix mix-div)))))

(displayln "`proj3/cogen`:")
proj3

(define comp
  (pretty
    (int proj3 `(,(mix-vs tm-int tm-div)))))

(displayln "`comp`:")
comp

(displayln "`source`:")
tm-example

(define target-3
  (pretty
    (int comp `(,(tm-vs tm-example)))))

(displayln "`target-3`:")
target-3

(displayln "check `target-3`:")
(equal? (int target-3 `(,'(1 1 0 1 0 1))) '(1 1 0 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;trick-mix-aux;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (update-pending-with-lbl pending vs marked t f)
  (set-union
     pending
     (set-subtract (list->set (list (env-label t vs) (env-label f vs))) marked)))

(define (get-dynamic-labels static program)
  (let
    ([dynamic-labels 
      (foldl (lambda (arg acc) (let ([l-arg (last arg)])
                                    (if
                                      (and (equal? (first l-arg) 'if) (not (is-static (second l-arg) static)))
                                      (append (list (fourth l-arg) (sixth l-arg)) acc)
                                      acc))) '() (hash->list program))])
    (map (lambda (arg) (cons arg (hash-ref program arg))) dynamic-labels)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;------------------------------------- The Trick Mix -------------------------------------------;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;task4;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define trick-mix
  '((read program div vs0)
    (init (Pending := list->set '())
          (static := car div)
          (dynamic := cdr div)
          (Res := (list (cons 'read (set-subtract (cdar program) static))))
          (PPVS := env-label (caadr program) (make-immutable-hash vs0))
          (PP := car PPVS)
          (VS := cdr PPVS)
          (BB := hash-ref (make-immutable-hash (cdr program)) (caadr program))
          (program := make-immutable-hash (cdr program))
          (Marked := set-add (list->set ()) PPVS)
          (dynamic-labels := get-dynamic-labels static program)
          (dynamic-labels-tmp := dynamic-labels)
          (Code := list (env-label PP VS))
          (goto inner-loop))
    
    (loop (if (set-empty? Pending)
              goto stop
              goto loop-begin))
    (loop-begin (PPVS := set-first Pending)
                (Pending := set-rest Pending)
                (PP := car PPVS)
                (VS := cdr PPVS)
                (Marked := set-add Marked PPVS)
                (Code := list (env-label PP VS))
                (goto exist-dyn))

    (exist-dyn (if (empty? dynamic-labels-tmp)
                   goto dyn-lbl-fail
                   goto do-dyn-lbl))
    (do-dyn-lbl (pp-tmp := caar dynamic-labels-tmp)
                (BB := cdar dynamic-labels-tmp)
                (dynamic-labels-tmp := cdr dynamic-labels-tmp)
                (if (equal? PP pp-tmp)
                    goto next
                    goto exist-dyn))
    (next (dynamic-labels-tmp := dynamic-labels)
          (goto inner-loop))
    
    (inner-loop (if (empty? BB)
                    goto loop-end
                    goto inner-loop-body))
    (inner-loop-body (Command := car BB)
                     (BB := cdr BB)
                     (goto is-assign))
    (is-assign (if (equal? (second Command) ':=)  goto do-assign goto is-goto))
    (is-goto   (if (equal? (car Command) 'goto)   goto do-goto   goto is-if))
    (is-if     (if (equal? (car Command) 'if)     goto do-if     goto is-return))
    (is-return (if (equal? (car Command) 'return) goto do-return goto do-fail))

    (do-assign (if (is-static (first Command) static)
                   goto do-static-assign
                   goto do-dynamic-assign))
    (do-static-assign (VS := update-vs VS (first Command) (cddr Command))
                      (goto inner-loop))
    (do-dynamic-assign (Code := append Code
                             (list (append (list (first Command) ':=) (reduce (cddr Command) VS))))
                       (goto inner-loop))
    
    (do-goto (BB := hash-ref program (cadr Command) ())
             (goto inner-loop))
    
    (do-if (if (is-static (second Command) static)
               goto do-static-if
               goto do-dynamic-if))
    
    (do-static-if (if (int-expr (hash->list VS) (second Command))
                      goto do-static-if-true
                      goto do-static-if-false))
    (do-static-if-true (BB := hash-ref program (fourth Command) ())
                       (goto inner-loop))
    (do-static-if-false (BB := hash-ref program (sixth Command) ())
                        (goto inner-loop))
    (do-dynamic-if (Pending := update-pending-with-lbl Pending VS Marked
                            (fourth Command)
                            (sixth Command))
                   (Code := append Code
                         (list (append (append (list 'if)
                           (list (reduce (second Command) VS)))
                           (list goto (env-label (fourth Command) VS)
                                 goto (env-label (sixth Command) VS)))))
                   (goto inner-loop))

    (do-return (Code := append Code
                     (list (append (list 'return) (reduce (cdr Command) VS))))
               (goto inner-loop))

    (loop-end (Res := append Res (list Code))
              (goto loop))
    
    (do-fail (return error "TRICK-MIX: error instruction"))
    (stop (return Res))
    (dyn-lbl-fail (return 'no-dyn-lbl))))

;(trace trick-mix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;------------------------------------------ execute --------------------------------------------;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(displayln "TRICK-MIX-CHECK")

(define trick-mix-div
  (cons '(program div static dynamic dynamic-labels dynamic-labels-tmp BB Command pp-tmp)
        '(Pending Marked Res PPVS PP VS vs0 BB Code)))

(define (trick-mix-vs source-int source-div)
  `(,(cons 'program source-int) ,(cons 'div source-div)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(displayln "`find-name`:")
;
;(define trick-find-name-apply-2-args
;  (int trick-mix `(,find-name ,(cons '(name namelist) '(valuelist)) ,(list (cons 'name 'y) (cons 'namelist '(x y z))))))
;
;(equal? (int trick-find-name-apply-2-args `(,'(1 2 3))) 2)
;trick-find-name-apply-2-args
;
;;task4-2;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;(displayln "`The Trick I proj`:")
;
;(define (trick-proj1 source)
;  (pretty
;    (int trick-mix `(,tm-int ,tm-div ,(tm-vs source)))))
;
;(displayln "`source`:")
;tm-example
;
;(displayln "`trick-target-1`:")
;(define trick-target-1
;  (trick-proj1 tm-example))
;trick-target-1
;
;(displayln "check `trick-target-1`:")
;(equal? (int trick-target-1 `(,'(1 1 0 1 0 1))) '(1 1 0 1))
;
;;task4-3;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;(displayln "`The Trick II proj`:")
;
;(define trick-proj2
;  (pretty
;    (int trick-mix `(,trick-mix ,trick-mix-div ,(trick-mix-vs tm-int tm-div)))))
;
;(displayln "`trick-proj2/comp`:")
;trick-proj2
;
;(displayln "`source`:")
;tm-example
;
;(displayln "`trick-target-2`:")
;(define trick-target-2
;  (pretty
;    (int trick-proj2 `(,(tm-vs tm-example)))))
;trick-target-2
;
;(displayln "check `trick-target-2`:")
;(equal? (int trick-target-2 `(,'(1 1 0 1 0 1))) '(1 1 0 1))
;
;;task6-2;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;(displayln "`The Trick III proj`:")
;
;(define trick-proj3
;  (pretty
;    (int trick-mix `(,trick-mix ,trick-mix-div ,(trick-mix-vs trick-mix trick-mix-div)))))
;
;(displayln "`trick-proj3/cogen`:")
;trick-proj3
;
;(define trick-comp
;  (pretty
;    (int trick-proj3 `(,(trick-mix-vs tm-int tm-div)))))
;
;(displayln "`trick-comp`:")
;trick-comp
;
;(displayln "`source`:")
;tm-example
;
;(define  trick-target-3
;  (pretty
;    (int trick-comp `(,(tm-vs tm-example)))))
;
;(displayln "` trick-target-3`:")
; trick-target-3
;
;(displayln "check `trick-target-3`:")
;(equal? (int trick-target-3 `(,'(1 1 0 1 0 1))) '(1 1 0 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;--------------------------- FlowChart Interpreter in FlowChart --------------------------------;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;task5-1;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(displayln "HERE!")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;------------------------------------------ execute --------------------------------------------;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(displayln "FC-INT-CHECK")

;task5-2;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(displayln "`FC I proj`:")

;task5-3;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(displayln "`FC II proj`:")

;task6-3;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(displayln "`FC III proj`:")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;