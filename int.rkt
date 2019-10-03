#lang racket

(provide (all-defined-out))
(require "aux.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;------------------------------- FlowChart Interpreter ----------------------------------;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (subst env name)
  (if
    (list? name)
    (map (lambda (x) (subst env x)) name)
    (if (dict-has-key? env name) (dict-ref env name) name)))

(define (int-expr env expr)
  (ns-eval (subst env expr)))

(define (env<-assign env assign)
  (dict-set env (second assign) (qu (int-expr env (third assign)))))

(define (int-jump env label-env jump)
  (match jump
    [`(goto ,lbl)      (int-block env label-env (dict-ref label-env lbl))]
    [`(if ,cond ,t ,f) (int-block env label-env
                                  (dict-ref label-env (if (int-expr env cond) t f)))]
    [`(return ,expr)   (int-expr env expr)]
    [else              (displayln "int-jump: error jump")]))

(define (int-block env label-env assigns-jump)
  (match assigns-jump
    [`(,jump)              (int-jump env label-env jump)]
    [`(,asgn . ,asgns-jmp) (int-block (env<-assign env asgn) label-env asgns-jmp)]
    [else                  (displayln "int-block: error assign or lose jump")]))

(define (init-env env blocks)
  (foldr (lambda (a e) (dict-set e (car a) (cdr a))) env blocks))

(define (int program data)
  (let
    ([env (init-env #hash() (map cons (cdar program) (map qu data)))]
     [label-env (init-env #hash() (cdr program))])
    (int-block env label-env (cdadr program))))

;find-name;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define find-name
  '((read name namelist valuelist)
   (search (if (equal? name (car namelist)) found cont))
   (cont (:= valuelist (cdr valuelist))
         (:= namelist (cdr namelist))
         (goto search))
   (found (return (car valuelist)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;----------------------------- TuringMachine Interpreter --------------------------------;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tm-int
  '((read Q Right)
   (init (:= Qtail Q)
         (:= Left '())
         (goto loop))
   (loop (if (empty? Qtail) fin cont))
   (cont (:= Instr (car Qtail))
         (:= Qtail (cdr Qtail))
         (:= Action (cadr Instr))
         (goto is-left))
   (is-left  (if (equal? Action 'left)  do-left  is-right))
   (is-right (if (equal? Action 'right) do-right is-write))
   (is-write (if (equal? Action 'write) do-write is-goto))
   (is-goto  (if (equal? Action 'goto)  do-goto  is-if))
   (is-if    (if (equal? Action 'if)    do-if    disp-fail))
   (do-left  (:= Right (cons (car Left) Right))
             (:= Left (cdr Left))
             (goto loop))
   (do-right (:= Left (cons (car Right) Left))
             (:= Right (cdr Right))
             (goto loop))
   (do-write (:= Right (cons (caddr Instr) (cdr Right)))
             (goto loop))
   (do-goto  (:= Qtail (list-tail Q (caddr Instr)))
             (goto loop))
   (do-if    (:= Cond (caddr Instr))
             (:= Instr (cddr Instr))
             (if (equal? Cond (car Right)) do-goto loop))        
   (fin (return Right))
   (disp-fail (return (display ("TM: error instruction"))))))

;tm-example;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tm-example
  '((0 if 0 goto 3)
    (1 right)
    (2 goto 0)
    (3 write 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
