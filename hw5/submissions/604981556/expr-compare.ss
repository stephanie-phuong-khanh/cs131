#lang racket
(provide expr-compare)

(define (lambda? x)
    (and (list? x) (equal? (length x) 3) (member (car x) '(lambda λ)))
)

(define (if? x)
    (and (list? x) (equal? (length x) 4) (member (car x) '(if)))
)

(define (shadow-check replacement expr)
    (let ([orig (car replacement)] [third (list-ref expr 2)])
        (cond
            [(or (equal? orig (list-ref expr 1))
                (member orig (list-ref expr 1)))
             expr]
            [(not (list? third))
            (cons (car expr) (list (cadr expr) (rename-args replacement third)))]
            [else (cons (car expr) (rename-args replacement (cdr expr)))]
        )
    )
)

(define (rename-args replacement expr)
    (let ([orig (car replacement)] [new (cadr replacement)])
        (cond
            [(not (list? expr)) (if (equal? orig expr) new expr)]
            [(null? expr) expr]
            [(equal? (car expr) 'quote) expr]
            [else
                (if (list? (car expr))
                (cond
                    [(lambda? (car expr))
                        (cons (shadow-check replacement (car expr)) (rename-args replacement (cdr expr)))]
                    [else (cons (rename-args replacement (car expr)) (rename-args replacement (cdr expr)))]
                )
                (cond
                    [(equal? (car expr) orig) (cons new (rename-args replacement (cdr expr)))]
                    [else (cons (car expr) (rename-args replacement (cdr expr)))])
            )]  
        )
    )
)     

(define (new-formals x y)
    (cond
        [(or (null? x) (null? y)) x]
        [(equal? (car x) (car y)) (cons (car x) (new-formals (cdr x) (cdr y)))]
        [else (let ([new-name (string->symbol (string-append (symbol->string (car x)) "!" (symbol->string(car y))))])
            (cons new-name (new-formals (cdr x) (cdr y))))]
    )
)

(define (list-replacements x y)
    (cond
        [(or (null? x) (null? y)) x]
        [(equal? (car x) (car y)) (list-replacements (cdr x) (cdr y))]
        [else (let ([new-name (string->symbol (string-append (symbol->string (car x)) "!" (symbol->string(car y))))])
            (cons (cons (car x) (list (car y) new-name)) (list-replacements (cdr x) (cdr y))))]
    )
)

(define (replace-x replace-list x)
    (cond
        [(null? replace-list) x]
        [else (let ([current (car replace-list)])
            (replace-x (cdr replace-list)
            (rename-args (list (car current) (list-ref current 2)) x))
        )]
    )
)

(define (replace-y replace-list y)
    (cond
        [(null? replace-list) y]
        [else (let ([current (car replace-list)])
            (replace-y (cdr replace-list) (rename-args (list (cadr current) (list-ref current 2)) y))
        )]
    )
)

(define (choose-lambda x y) (if (equal? x y) x 'λ))

(define (lambda-expr x y)
    (let ([formals-x (cadr x)] [formals-y (cadr y)])
        (cond
            [(not (equal? (length formals-x) (length formals-y)))
                (list 'if '% x y)]
            [(equal? formals-x formals-y) 
                (cons (choose-lambda (car x) (car y)) (cons (cadr x) (list (expr-compare (list-ref x 2) (list-ref y 2)))))]
            [else (let ([formals-new (new-formals formals-x formals-y)] [replacement-list (list-replacements formals-x formals-y)])
                    (cons (choose-lambda (car x) (car y))
                        (list formals-new
                            (expr-compare (replace-x replacement-list (list-ref x 2)) (replace-y replacement-list (list-ref y 2))))))]
        )
    )
)

(define (expr-compare x y)
    (cond     
        [(equal? x y) x]
        [(and (boolean? x) (boolean? y)) (if x '% '(not %))] 
        [(or
                (or (not (list? x)) (not (list? y)))
                (not (equal? (length x) (length y))) )
            (list 'if '% x y)
        ] ; everything beyond here are lists of equal length

        [(or (null? x) (null? y)) '()]

        [(or (or (equal? (car x) 'quote) (equal? (car y) 'quote))
            (and (or (if? x) (if? y)) (not (equal? (car x) (car y)))))
            (list 'if '% x y)]

        [(and (lambda? x) (lambda? y)) (lambda-expr x y)]

        [(equal? (car x) (car y))
            (cons (car x) (expr-compare (cdr x) (cdr y)))]

        [(not (equal? (car x) (car y)))
            (cond
                [(or (lambda? x) (lambda? y)) (list 'if '% x y)]
                [else (cons (expr-compare (car x) (car y))
                            (expr-compare (cdr x) (cdr y)))]
            )
        ]
    )
)

(define (test-expr-compare x y) 
  (and (equal? (eval x)
               (eval `(let ((% #t)) ,(expr-compare x y))))
       (equal? (eval y)
               (eval `(let ((% #f)) ,(expr-compare x y))))))

(define test-expr-x
    `(cons ((lambda (a) (* a 2)) 8) '(if (equal? b (f lambda)) 0 10)
))

(define test-expr-y
    `(cons ((lambda (c) (- c 4)) 9) '(if (equal? b (f λ)) 92 1)
))