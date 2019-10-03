#lang racket

(require "aux.rkt")
(require "int.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;--------------------------------------- Mixer ------------------------------------------;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define mix
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
   (disp-fail (return (display ("MIX: error instruction"))))
   (fin (return Res))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;-------------------------------------- execute -----------------------------------------;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(equal? (int find-name '(y (x y z) (1 2 3))) 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (pretty-int program data)
  (pretty (int program data)))

(define tm-data (list 1 1 0 1 0 1))
(define tm-out (list 1 1 0 1))

(equal? (int tm-int `(,tm-example ,tm-data)) tm-out)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define int-div '(Q Qtail Instr Action Cond))
(define int-init (map qu (list tm-example '() '() '() '())))

(define exec1
  (lambda program
    (int (car program) `(,tm-data))))

(define proj1 (pretty-int mix `(,tm-int ,int-div ,int-init)))
(equal? (exec1 proj1) `,tm-out)
proj1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define mix-div '(program)) ;, div, vs0, Pending, Marked, BB, Code, VS))
(define mix-init (map qu (list tm-int '())))

;(define exec2
;  (lambda program
;    (int (car program) `(,tm-data))))
;
;(define proj2 (pretty-int mix `(,mix ,mix-div ,mix-init)))
;(equal? (exec2 proj2) `,tm-out)
