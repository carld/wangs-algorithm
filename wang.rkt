#lang racket

(define (sequent->string sequent)
  (let* [(seq-as-str (format "~a" sequent))
         (sequent-max-width 50)
         (padding    (make-string (- sequent-max-width (string-length seq-as-str)) #\space))]
    (format "~a~a" sequent padding)))

(define wang-eval0
  (case-lambda
    [(sequent rule line)
     (printf "~a\t~a ~a applied to ~a~%" line (sequent->string sequent) rule (- line 1))
     (wang-eval (+ 1 line) sequent)]
    [(sequent line)
     (printf "~a\t~a~%" line (sequent->string sequent))
     (wang-eval (+ 1 line) sequent)]))

(define wang-eval
  (case-lambda
    [(sequent)
     (wang-eval0 sequent 1)]
    [(line sequent)
     (match-let ([`(,lhs ... => ,rhs ...) sequent])
       (cond
         ; rule 6: proved if formula occurs on both left and right sides aka an axiom
         [(not (null? (set-intersect lhs rhs)))    #t]
         [else
          (match sequent
            ; rule 1: NOT on the left; NOT on the right
            [`(,p ... (not ,q) ,r ... => ,rhs ...)                          (wang-eval0 `(,@p ,@r   => ,@rhs ,q) 'R1 line)]
            [`(,lhs ...               => ,p ... (not ,q) ,r ...)            (wang-eval0 `(,@lhs ,q  => ,@p ,@r) 'R1 line)]
            ; rule 2: AND on the left; OR on the right
            [`(,p ... (,q and ,r) ,s ... => ,rhs ...)                       (wang-eval0 `(,@p ,q ,r ,@s => ,@rhs) 'R2 line)]
            [`(,lhs ...                  => ,p ... (,q or ,r) ,s ...)       (wang-eval0 `(,@lhs => ,@p ,q ,r ,@s)  'R2 line)]
            ; rule 3: splitting on the left, OR on the left
            [`(,p ... (,q or ,r) ,s ...  => ,rhs ...)                       (let [(s1 (wang-eval0 `(,@p ,q ,@s => ,@rhs)  'R3 line))
                                                                                  (s2 (wang-eval0 `(,@p ,r ,@s => ,@rhs)  'R3 line))]
                                                                              (and s1 s2))]
            ; rule 4: splitting on the right, AND on the right
            [`(,lhs ...          => ,p ... (,q and ,r) ,s ...)              (let [(s1 (wang-eval0 `(,@lhs => ,@p ,q ,@s)  'R4 line))
                                                                                  (s2 (wang-eval0 `(,@lhs => ,@p ,r ,@s)  'R4 line))]
                                                                              (and s1 s2))]
            ; rule 5: eliminate implication
            [`(,p ... (,q -> ,r) ,s ...  => ,rhs ...)                         (wang-eval0 `(,@p ((not ,q) or ,r) ,@s => ,@rhs)  'R5 line)]
            [`(,lhs ...                  => ,p ... (,q -> ,r) ,s ...)         (wang-eval0 `(,@lhs => ,@p ((not ,q) or ,r) ,@s)  'R5 line)]
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
(wang-eval `(((P -> Q) -> R) (not Q) => (not R)))