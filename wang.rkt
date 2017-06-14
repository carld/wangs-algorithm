#lang racket

(define output
  (let [(line 0)
        (width 50)]
    (lambda (sequent rule . notes)
      (set! line (+ line 1))
      (let [(seqstr (format "~a" sequent))]
        (printf "~a\t~a ~a ~a ~a~%" line seqstr (make-string (- width (string-length seqstr)) #\space) rule notes)))))

(define wang-eval
  (case-lambda
    [(sequent)
     (wang-eval sequent 'start)]
    [(sequent rule)
     (output sequent rule)
     (match-let ([`(,lhs ... => ,rhs ...) sequent])
       (cond
         ; rule 6: proved if formula occurs on both left and right sides aka an axiom
         [(not (null? (set-intersect lhs rhs)))    #t]
         [else
          (match sequent
            ; rule 1: not on the left; not on the right
            [`(,p ... (not ,q) ,r ... => ,s ...)                   (wang-eval `(,@p ,@r => ,@s ,q) 'R1)]
            [`(,p ... => ,prop (not ,q))                           (wang-eval `(,@p ,q => ,prop) 'R1)]
            ; rule 2: and on the left; or on the right
            [`(,p ... (,x and ,y) ,s ... => ,prop ...)             (wang-eval `(,@p ,x ,y ,@s => ,@prop) 'R2)]
            [`(,p ... => ,prop ... (,x or ,y))                     (wang-eval `(,@p => ,@prop ,x ,y)  'R2)]
            ; rule 3: splitting on the left AKA or on the left
            [`(,p ... (,x or ,y) ,s ... => ,prop ...)              (let [(s1 (wang-eval `(,@p ,x ,@s => ,@prop)  'R3))
                                                                         (s2 (wang-eval `(,@p ,y ,@s => ,@prop)  'R3))]
                                                                     (and s1 s2))]
            ; rule 4: splitting on the right AKA and on the right
            [`(,p ... => ,prop ... (,x and ,y) ,r ...)             (let [(s1 (wang-eval `(,@p => ,@prop ,x ,@r)  'R4))
                                                                         (s2 (wang-eval `(,@p => ,@prop ,y ,@r)  'R4))]
                                                                     (and s1 s2))]
            ; rule 5: eliminate implication
            [`(,s ... (,p -> ,q) ,t ... => ,prop ...)              (wang-eval `(,@s ((not ,p) or ,q) ,@t => ,@prop)  'R5)]
            ; rule 7: invalid if no connectives and no symbol occurs on both sides
            [_ #f])]))]))

(wang-eval `((not p) => q))
(wang-eval `((not p) r => q))
(wang-eval `(p => q))
(wang-eval `(p => p))
(wang-eval `((p and q) => s))
(wang-eval `(p => (s or t)))
(wang-eval `((p or q) => r))
(wang-eval `((p -> q) => r))
(wang-eval `(Q R => (not P) R))
(wang-eval `(Q => (not P) R Q))
(wang-eval `((P -> Q) (Q -> R) (not R) => (not P)))