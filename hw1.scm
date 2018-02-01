(define (is_rule_one exp)
    ; (+ c1 c2)
    (if (and (list? exp) (= (length exp) 3) )
        (and (equal? (car exp) '+)
             (not (list? (cadr exp)))
             (not (list? (cadr exp)))
        )
        #f
    )
)


