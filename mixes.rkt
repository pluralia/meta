#lang racket

(require "aux.rkt")
(require "int.rkt")
(require racket/trace)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;-------------------------------------- Mixer-1 -----------------------------------------;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define mix-1
  '((read program div vs0)
   (init (:= Pending (list (list (caadr program) (zip-with-acc '() div vs0))))
         (:= Marked '())                                
         (:= Res (list (cons 'read (set-subtract (cdar program) div))))
         (goto loop))
   
   (loop (if (empty? Pending) fin loop-begin))
   (loop-begin (:= PP (caar Pending))
               (:= VS (cadar Pending))
               (:= Pending (cdr Pending))
               (:= Marked (cons (list PP VS) Marked))
               (:= BB (dict-ref program PP))     
               (:= Code (list (env-label PP VS)))
               (goto inner-loop))
   
   (inner-loop (if (empty? BB) loop-end inner-loop-body))
   (inner-loop-body (:= Command (car BB))
                    (:= BB (cdr BB))
                    (goto is-assign))
   (is-assign (if (equal? (car Command) ':=)     do-assign is-goto))
   (is-goto   (if (equal? (car Command) 'goto)   do-goto   is-if))
   (is-if     (if (equal? (car Command) 'if)     do-if     is-return))
   (is-return (if (equal? 'return (car Command)) do-return disp-fail))
   
   (do-assign (if (is-static-by-div (second Command) div)
                  do-static-assign do-dynamic-assign))
   (do-static-assign (:= VS (update-vs VS (second Command) (reduce (third Command) VS)))
                     (goto inner-loop))
   (do-dynamic-assign (:= Code (append Code
                          (list (list ':= (second Command) (reduce (third Command) VS)))))
                      (goto inner-loop))

   (do-goto (:= BB (dict-ref program (second Command)))
            (goto inner-loop))

   (do-if (if (is-static-by-div (second Command) div)
              do-static-if do-dynamic-if))
   (do-static-if (if (ns-eval (reduce (second Command) VS))
                     do-static-if-true do-static-if-false))
   (do-static-if-true (:= BB (dict-ref program (third Command)))
                   (goto inner-loop))
   (do-static-if-false (:= BB (dict-ref program (fourth Command)))
                   (goto inner-loop))
   (do-dynamic-if (:= Pending (append Pending (ret-if-not-marked (cddr Command) VS Marked)))
                  (:= Code (append Code (list (list 'if
                      (reduce (second Command) VS)
                      (env-label (third Command) VS)
                      (env-label (fourth Command) VS)))))
                  (goto inner-loop))

   (do-return (:= Code (append Code
                  (list (list 'return (reduce (second Command) VS)))))
              (goto inner-loop))

   (loop-end (:= Res (append Res (list Code)))
             (goto loop))
   (disp-fail (return (displayln "MIX: error instruction")))
   (fin (return Res))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;------------------------------------- Mixer-2 ------------------------------------------;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define mix-2
  '((read program2 div2 vs02)
   (init2 (:= Pending2 (list (list (caadr program2) (zip-with-acc '() div2 vs02))))
         (:= Marked2 '())                                
         (:= Res2 (list (cons 'read (set-subtract (cdar program2) div2))))
         (goto loop2))
   
   (loop2 (if (empty? Pending2) fin2 loop-begin2))
   (loop-begin2 (:= PP2 (caar Pending2))
               (:= VS2 (cadar Pending2))
               (:= Pending2 (cdr Pending2))
               (:= Marked2 (cons (list PP2 VS2) Marked2))
               (:= BB2 (dict-ref program2 PP2))     
               (:= Code2 (list (env-label PP2 VS2)))
               (goto inner-loop2))
   
   (inner-loop2 (if (empty? BB2) loop-end2 inner-loop-body2))
   (inner-loop-body2 (:= Command2 (car BB2))
                    (:= BB2 (cdr BB2))
                    (goto is-assign2))
   (is-assign2 (if (equal? (car Command2) ':=)     do-assign2 is-goto2))
   (is-goto2   (if (equal? (car Command2) 'goto)   do-goto2   is-if2))
   (is-if2     (if (equal? (car Command2) 'if)     do-if2     is-return2))
   (is-return2 (if (equal? 'return (car Command2)) do-return2 disp-fail2))
   
   (do-assign2 (if (is-static-by-div (second Command2) div2)
                  do-static-assign2 do-dynamic-assign2))
   (do-static-assign2 (:= VS2 (update-vs VS2 (second Command2) (reduce (third Command2) VS2)))
                      (goto inner-loop2))
   (do-dynamic-assign2 (:= Code2 (append Code2
                          (list (list ':= (second Command2) (reduce (third Command2) VS2)))))
                      (goto inner-loop2))

   (do-goto2 (:= BB2 (dict-ref program2 (second Command2)))
            (goto inner-loop2))

   (do-if2 (if (is-static-by-div (second Command2) div2)
              do-static-if2 do-dynamic-if2))
   (do-static-if2 (if (ns-eval (reduce (second Command2) VS2))
                     do-static-if-true2 do-static-if-false2))
   (do-static-if-true2 (:= BB2 (dict-ref program2 (third Command2)))
                   (goto inner-loop2))
   (do-static-if-false2 (:= BB2 (dict-ref program2 (fourth Command2)))
                   (goto inner-loop2))
   (do-dynamic-if2 (:= Pending2 (append Pending2 (ret-if-not-marked (cddr Command2) VS2 Marked2)))
                  (:= Code2 (append Code2 (list (list 'if
                      (reduce (second Command2) VS2)
                      (env-label (third Command2) VS2)
                      (env-label (fourth Command2) VS2)))))
                  (goto inner-loop2))

   (do-return2 (:= Code2 (append Code2
                  (list (list 'return (reduce (second Command2) VS2)))))
              (goto inner-loop2))

   (loop-end2 (:= Res2 (append Res2 (list Code2)))
             (goto loop2))
   (disp-fail2 (return (displayln "MIX-2: error instruction")))
   (fin2 (return Res2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;------------------------------------ execute-1 -----------------------------------------;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define int-div '(Q Qtail Instr Action Cond))
(define int-vs0 (map qu (list tm-example '() '() '() '())))

(define apply-tm-in
  (lambda program
    (int (car program) `(,tm-in))))

(define proj1 (pretty-int mix-1 `(,tm-int ,int-div ,int-vs0)))
(displayln "proj1 (target1): ")
proj1
(display "check `target1`: ")
(equal? (apply-tm-in proj1) `,tm-out)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;------------------------------------- execute-2 ----------------------------------------;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define mix-div '(program div)) ; PP BB Command))
; program div ; clearly static
; VS vs0 ; clearly dynamic
; Pending Marked Code Res PP BB Command ; dynamic by
(define mix-vs0 (map qu (list tm-int int-div)))

(define apply-tm-example
  (lambda program
    (pretty-int (car program) `(,tm-example))))

(define proj2 (pretty-int mix-2 `(,mix-1 ,mix-div ,mix-vs0)))
(displayln "proj2 (comp): ")
proj2
(displayln "target2: ")
(apply-tm-example proj2 `(,tm-example))
(display "check `target2`: ")
(equal? (apply-tm-in (apply-tm-example proj2)) `,tm-out)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;