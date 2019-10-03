#lang racket

(provide (all-defined-out))
(require racket/trace)


(define-namespace-anchor roguelike)
(define ns (namespace-anchor->namespace roguelike))
(define (ns-eval expr)
  (eval expr ns))

(define (qu var)
  (cons 'quote (list var)))

(define (zip-with-acc acc left right)
  (if
   (equal? (length left) (length right))
   (append acc (map list left right))
   acc))

(define (env-label pp vs)
  (list pp ':: vs))

(define (is-static-by-div exp div)
  (match exp
    [`(quote ,_)   #t]
    [`(,_ . ,args) (andmap (lambda (e) (is-static-by-div e div)) args)]
    [`,x           (if (member x div) #t #f)]))

(define (update-vs vs x val)
  (match vs
    [`()        (list (list x val))]
    [`(,h . ,t) (if
                  (equal? x (car h))
                  (cons (list x val) t)
                  (cons h (update-vs t x val)))]))

(define (reduce-with-flag exp vs)
  (match exp
    [`(quote ,c)    (cons (qu c) #t)]
    [`(,op . ,args) (let*
                      ([redArgs (map (lambda (e) (reduce-with-flag e vs)) args)]
                       [cond    (andmap cdr redArgs)]
                       [newArgs (map car redArgs)])
                      (if cond
                        (cons (qu (ns-eval `(,op . ,newArgs))) #t)
                        (cons `(,op . ,newArgs) #f)))]
    [`,x            (if
                      (dict-has-key? vs x)
                      (cons (car (dict-ref vs x)) #t)
                      (cons x #f))]))

(define (reduce exp vs)
  (car (reduce-with-flag exp vs)))

(define (ret-if-not-marked pp vs marked)
  (set-subtract (list (list (cadr pp) vs) (list (car pp) vs)) marked))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;----------------------------------- Pretty Printer -------------------------------------;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (add-block block acc)
  (match acc
    [`(,num ,used ,blocks)
        (if
          (member (car block) used)
          `(,num ,used ,(append blocks (list (cons (index-of used (car block)) (cdr block)))))
          `(,(+ num 1) ,(append used (list (car block))) ,(append blocks (list (cons num (cdr block))))))]))

(define (update-instr instr ids)
  (match (car instr)
    ['goto (list 'goto (index-of ids (cadr instr)))]
    ['if (list 'if (cadr instr) (index-of ids (caddr instr)) (index-of ids (cadddr instr)))]
    [_ instr]))

(define (pretty program)
  (let
    ([folded (foldl add-block `(0 () ()) (cdr program))])
    (cons (car program)
      (map (lambda (b) (cons (car b) (map (lambda (i) (update-instr i (cadr folded))) (cdr b))  ))
           (caddr folded)))))
