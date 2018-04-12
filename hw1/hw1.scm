(define (is_rule_1 exp)
    ; (+ c1 c2)
    (if (and (list? exp) (= (length exp) 3) )
        (let
            (
                (op (car exp))
                (c1 (cadr exp))
                (c2 (caddr exp))
            )
            (and
                (equal? op '+)
                (number? c1)
                (number? c2)
            )
        )
        #f
    )
)

(define (perform_rule_1 exp)
	; c1 + c2
	(display "rule 1") (newline)
	(+ (cadr exp) (caddr exp))
)

(define (is_rule_2 exp)
    ; (+ t c)
	(if (and (list? exp) (= (length exp) 3)
		(not (is_rule_9 exp)))
        (let
            (
                (op (car exp))
                (t (cadr exp))
                (c (caddr exp))
            )
            (and
                (equal? op '+)
                (not (number? t))
                (number? c)
            )
        )
        #f
    )
)

(define (perform_rule_2 exp)
	; (+ c t)
	(display "rule 2") (newline)
    (simplify (list '+ (caddr exp) (simplify (cadr exp))))
)

(define (is_rule_3 exp)
    ; (* c1 c2)
    (if (and (list? exp) (= (length exp) 3) )
        (let
            (
                (op (car exp))
                (c1 (cadr exp))
                (c2 (caddr exp))
            )
            (and
                (equal? op '*)
                (number? c1)
                (number? c2)
            )
        )
        #f
    )
)

(define (perform_rule_3 exp)
	; c1 * c2
	(display "rule 3") (newline)
    (* (cadr exp) (caddr exp))
)

(define (is_rule_4 exp)
	; (* t c)
	(if (and (list? exp) (= (length exp) 3)
		(not (is_rule_7 exp)) (not (is_rule_10 exp))
		(not (is_rule_11 exp)) (not (is_rule_13 exp))
		(not (is_rule_15 exp)))
        (let
            (
                (op (car exp))
                (t (cadr exp))
                (c (caddr exp))
            )
            (and
                (equal? op '*)
                (not (number? t))
                (number? c)
            )
        )
        #f
    )
)

(define (perform_rule_4 exp)
	; (* c t)
	(display "rule 4") (newline)
    (simplify (list '* (caddr exp) (simplify (cadr exp))))
)

(define (is_rule_5 exp)
	; (- c1 c2)
    (if (and (list? exp) (= (length exp) 3) )
        (let
            (
                (op (car exp))
                (c1 (cadr exp))
                (c2 (caddr exp))
            )
            (and
                (equal? op '-)
                (number? c1)
                (number? c2)
            )
        )
        #f
    )
)

(define (perform_rule_5 exp)
	; c1 - c2
	(display "rule 5") (newline)
    (- (cadr exp) (caddr exp))
)

(define (is_rule_6 exp)
    ; (- t c)
    (if (and (list? exp) (= (length exp) 3) )
        (let
            (
                (op (car exp))
                (t (cadr exp))
                (c (caddr exp))
            )
            (and
                (equal? op '-)
                (not (number? t))
                (number? c)
            )
        )
        #f
    )
)

(define (perform_rule_6 exp)
	; (- c t)
	(display "rule 6") (newline)
    (simplify (list '- (caddr exp) (simplify (cadr exp))))
)

(define (is_rule_7 exp)
    ; (+ t1 (+ t2 t3))
    (if (and (list? exp) (= (length exp) 3)
             (list? (caddr exp)) (= (length (caddr exp)) 3))
        (let
            (
                (op (car exp))
                (t1 (cadr exp))
                (op2 (car (caddr exp)))
                (t2 (cadr (caddr exp)))
                (t3 (caddr (caddr exp)))
            )
            (and
                (equal? op '+)
                (not (number? t1))
                (equal? op2 '+)
                (not (number? t2))
                (not (number? t3))
            )
        )
        #f
    )
)

(define (perform_rule_7 exp)
    ; (+ t1 (+ t2 t3))
    ; (+ (+ t1 t2) t3)
    (display "rule 7") (newline)
    (define t1 (simplify (cadr exp)))
    (define l1 (caddr exp))
    (define t2 (simplify (cadr l1)))
    (define t3 (simplify (caddr l1)))

    (define l2 (simplify (list '+ t1 t2)))
    (simplify (list '+ l2 t3))
)

(define (is_rule_8 exp)
    ; (* t1 (* t2 t3))
    (if (and (list? exp) (= (length exp) 3)
             (list? (caddr exp)) (= (length (caddr exp)) 3))
        (let
            (
                (op (car exp))
                (t1 (cadr exp))
                (op2 (car (caddr exp)))
                (t2 (cadr (caddr exp)))
                (t3 (caddr (caddr exp)))
            )
            (and
                (equal? op '*)
                (not (number? t1))
                (equal? op2 '*)
                (not (number? t2))
                (not (number? t3))
            )
        )
        #f
    )
)

(define (perform_rule_8 exp)
    ; (* t1 (* t2 t3))
	; (* (* t1 t2) t3)
	(display "rule 8") (newline)
    (define t1 (simplify (cadr exp)))
    (define l1 (caddr exp))
    (define t2 (simplify (cadr l1)))
    (define t3 (simplify (caddr l1)))

    (define l2 (simplify (list '* t1 t2)))
    (simplify (list '* l2 t3))
)

(define (is_rule_9 exp)
    ; (+ (+ c1 t) c2)
    (if (and (list? exp) (= (length exp) 3)
             (list? (cadr exp)) (= (length (cadr exp)) 3))
		(let
			(
				(op (car exp))
				(op2 (car (cadr exp)))
				(c1 (cadr (cadr exp)))
				(t (caddr (cadr exp)))
				(c2 (caddr exp))
			)
			(and
				(equal? op '+)
				(equal? op2 '+)
				(number? c1)
                (not (number? t))
                (number? c2)
			)
		)
        #f
    )
)

(define (perform_rule_9 exp)
    ; (+ (+ c1 t) c2)
	; (+ (+ c1 c2) t)
	(display "rule 9") (newline)
    (define l1 (cadr exp))
    (define c1 (cadr l1))
    (define t (simplify (caddr l1)))
    (define c2 (caddr exp))

    (define l2 (simplify (list '+ c1 c2)))
    (simplify (list '+ l2 t))
)

(define (is_rule_10 exp)
    ; (* (* c1 t) c2)
    (if (and (list? exp) (= (length exp) 3)
             (list? (cadr exp)) (= (length (cadr exp)) 3))
		(let
			(
				(op (car exp))
				(op2 (car (cadr exp)))
				(c1 (cadr (cadr exp)))
				(t (caddr (cadr exp)))
				(c2 (caddr exp))
			)
			(and
				(equal? op '*)
				(equal? op2 '*)
				(number? c1)
                (not (number? t))
                (number? c2)
			)
		)
        #f
    )
)

(define (perform_rule_10 exp)
    ; (+ (+ c1 t) c2)
	; (+ (+ c1 c2) t)
	(display "rule 10") (newline)
    (define l1 (cadr exp))
    (define c1 (cadr l1))
    (define t (simplify (caddr l1)))
    (define c2 (caddr exp))

    (define l2 (simplify (list '* c1 c2)))
    (simplify (list '* l2 t))
)

(define (is_rule_11 exp)
    ; (* (+ c1 t) c2)
    (if (and (list? exp) (= (length exp) 3)
             (list? (cadr exp)) (= (length (cadr exp)) 3))
		(let
			(
				(op (car exp))
				(op2 (car (cadr exp)))
				(c1 (cadr (cadr exp)))
				(t (caddr (cadr exp)))
				(c2 (caddr exp))
			)
			(and
				(equal? op '*)
				(equal? op2 '+)
				(number? c1)
                (not (number? t))
                (number? c2)
			)
		)
        #f
    )
)

(define (perform_rule_11 exp)
	; (* (+ c1 t) c2)
	; (+ (* c1 c2) (* c2 t))
	(display "rule 11") (newline)
    (define l1 (cadr exp))
    (define c1 (cadr l1))
    (define t (simplify (caddr l1)))
    (define c2 (caddr exp))

    (define l2 (simplify (list '* c1 c2)))
	(define l3 (simplify (list '* c2 t)))
	(simplify (list '+ l2 l3))
)

(define (is_rule_12 exp)
	; (* c1 (+ c2 t))
    (if (and (list? exp) (= (length exp) 3)
             (list? (caddr exp)) (= (length (caddr exp)) 3))
		(let
			(
				(op (car exp))
				(c1 (cadr exp))
				(op2 (car (caddr exp)))
				(c2 (cadr (caddr exp)))
				(t (caddr (caddr exp)))
			)
			(and
				(equal? op '*)
				(number? c1)
				(equal? op2 '+)
				(number? c2)
				(not (number? t))
			)
		)
        #f
    )
)

(define (perform_rule_12 exp)
	; (* c1 (+ c2 t))
	; (+ (* c1 c2) (* c1 t))
	(display "rule 12") (newline)
	(define c1 (cadr exp))
	(define l1 (caddr exp))
	(define c2 (cadr l1))
	(define t (simplify (caddr l1)))

	(define l2 (simplify (list '* c1 c2)))
	(define l3 (simplify (list '* c1 t)))
	(simplify (list '+ l2 l3))
)

(define (is_rule_13 exp)
	; (* (+ t1 t2) c)
    (if (and (list? exp) (= (length exp) 3)
             (list? (cadr exp)) (= (length (cadr exp)) 3))
		(let
			(
				(op (car exp))
				(op2 (car (cadr exp)))
				(t1 (cadr (cadr exp)))
				(t2 (caddr (cadr exp)))
				(c (caddr exp))
			)
			(and
				(equal? op '*)
				(equal? op2 '+)
				(not (number? t1))
				(not (number? t2))
				(number? c)
			)
		)
        #f
    )
)

(define (perform_rule_13 exp)
	; (* (+ t1 t2) c)
	; (+ (* c t1) (* c t2))
	(display "rule 13") (newline)
	(define l1 (cadr exp))
	(define t1 (simplify (cadr l1)))
	(define t2 (simplify (caddr l1)))
	(define c (caddr exp))

	(define l2 (simplify (list '* c t1)))
	(define l3 (simplify (list '* c t2)))
	(simplify (list '+ l2 l3))
)

(define (is_rule_14 exp)
	; (* c (+ t1 t2))
    (if (and (list? exp) (= (length exp) 3)
			 (list? (caddr exp)) (= (length (caddr exp)) 3))
		(let
			(
				(op (car exp))
				(c (cadr exp))
				(op2 (car (caddr exp)))
				(t1 (cadr (caddr exp)))
				(t2 (caddr (caddr exp)))
			)
			(and
				(equal? op '*)
				(number? c)
				(equal? op2 '+)
				(not (number? t1))
				(not (number? t2))
			)
		)
        #f
    )
)

(define (perform_rule_14 exp)
	; (* c (+ t1 t2))
	; (+ (* c t1) (* c t2))
	(display "rule 14") (newline)
	(define c (cadr exp))
	(define l1 (caddr exp))
	(define t1 (simplify (cadr l1)))
	(define t2 (simplify (caddr l1)))

	(define l2 (simplify (list '* c t1)))
	(define l3 (simplify (list '* c t2)))
	(simplify (list '+ l2 l3))
)

(define (is_rule_15 exp)
	; (* (- t1 t2) c)
    (if (and (list? exp) (= (length exp) 3)
             (list? (cadr exp)) (= (length (cadr exp)) 3))
		(let
			(
				(op (car exp))
				(op2 (car (cadr exp)))
				(t1 (cadr (cadr exp)))
				(t2 (caddr (cadr exp)))
				(c (caddr exp))
			)
			(and
				(equal? op '*)
				(equal? op2 '-)
				(not (number? t1))
				(not (number? t2))
				(number? c)
			)
		)
        #f
    )
)

(define (perform_rule_15 exp)
	; (* (- t1 t2) c)
	; (- (* c t1) (* c t2))
	(display "rule 15") (newline)
	(define l1 (cadr exp))
	(define t1 (simplify (cadr l1)))
	(define t2 (simplify (caddr l1)))
	(define c (caddr exp))

	(define l2 (simplify (list '* c t1)))
	(define l3 (simplify (list '* c t2)))
	(simplify (list '- l2 l3))
)

(define (is_rule_16 exp)
	; (* c (- t1 t2))
    (if (and (list? exp) (= (length exp) 3)
             (list? (caddr exp)) (= (length (caddr exp)) 3))
		(let
			(
				(op (car exp))
				(c (cadr exp))
				(op2 (car (caddr exp)))
				(t1 (cadr (caddr exp)))
				(t2 (caddr (caddr exp)))
			)
			(and
				(equal? op '*)
				(number? c)
				(equal? op2 '-)
				(not (number? t1))
				(not (number? t2))
			)
		)
        #f
    )
)

(define (perform_rule_16 exp)
	; (* c (- t1 t2))
	; (- (* c t1) (* c t2))
	(display "rule 16") (newline)
	(define c (cadr exp))
	(define l1 (caddr exp))
	(define t1 (simplify (cadr l1)))
	(define t2 (simplify (caddr l1)))

	(define l2 (simplify (list '* c t1)))
	(define l3 (simplify (list '* c t2)))
	(simplify (list '- l2 l3))
)

(define (is_rule_17 exp)
	; (* (+ t1 t2) t3)
    (if (and (list? exp) (= (length exp) 3)
             (list? (cadr exp)) (= (length (cadr exp)) 3))
		(let
			(
				(op (car exp))
				(op2 (car (cadr exp)))
				(t1 (cadr (cadr exp)))
				(t2 (caddr (cadr exp)))
				(t3 (caddr exp))
			)
			(and
				(equal? op '*)
				(equal? op2 '+)
				(not (number? t1))
				(not (number? t2))
				(not (number? t3))
			)
		)
        #f
    )
)

(define (perform_rule_17 exp)
	; (* (+ t1 t2) t3)
	; (+ (* t1 t3) (* t2 t3))
	(display "rule 17") (newline)
	(define l1 (cadr exp))
	(define t1 (simplify (cadr l1)))
	(define t2 (simplify (caddr l1)))
	(define t3 (simplify (caddr exp)))

	(define l2 (simplify (list '* t1 t3)))
	(define l3 (simplify (list '* t2 t3)))
	(simplify (list '+ l2 l3))
)

(define (is_rule_18 exp)
	; (* t1 (+ t2 t3))
    (if (and (list? exp) (= (length exp) 3)
             (list? (caddr exp)) (= (length (caddr exp)) 3))
		(let
			(
				(op (car exp))
				(t1 (cadr exp))
				(op2 (car (caddr exp)))
				(t2 (cadr (caddr exp)))
				(t3 (caddr (caddr exp)))
			)
			(and
				(equal? op '*)
				(not (number? t1))
				(equal? op2 '+)
				(not (number? t2))
				(not (number? t3))
			)
		)
        #f
    )
)

(define (perform_rule_18 exp)
	; (* t1 (+ t2 t3))
	; (+ (* t1 t2) (* t1 t3))
	(display "rule 18") (newline)
	(define t1 (simplify (cadr exp)))
	(define l1 (caddr exp))
	(define t2 (simplify (cadr l1)))
	(define t3 (simplify (caddr l1)))

	(define l2 (simplify (list '* t1 t2)))
	(define l3 (simplify (list '* t1 t3)))
	(simplify (list '+ l2 l3))
)

(define (is_rule_19 exp)
	; (* (- t1 t2) t3)
    (if (and (list? exp) (= (length exp) 3)
             (list? (cadr exp)) (= (length (cadr exp)) 3))
		(let
			(
				(op (car exp))
				(op2 (car (cadr exp)))
				(t1 (cadr (cadr exp)))
				(t2 (caddr (cadr exp)))
				(t3 (caddr exp))
			)
			(and
				(equal? op '*)
				(equal? op2 '-)
				(not (number? t1))
				(not (number? t2))
				(not (number? t3))
			)
		)
        #f
    )
)

(define (perform_rule_19 exp)
	; (* (- t1 t2) t3)
	; (- (* t1 t3) (* t2 t3))
	(display "rule 19") (newline)
	(define l1 (cadr exp))
	(define t1 (simplify (cadr l1)))
	(define t2 (simplify (caddr l1)))
	(define t3 (simplify (caddr exp)))

	(define l2 (simplify (list '* t1 t3)))
	(define l3 (simplify (list '* t2 t3)))
	(simplify (list '- l2 l3))
)

(define (is_rule_20 exp)
	; (* t1 (- t2 t3))
    (if (and (list? exp) (= (length exp) 3)
             (list? (caddr exp)) (= (length (caddr exp)) 3))
		(let
			(
				(op (car exp))
				(t1 (cadr exp))
				(op2 (car (caddr exp)))
				(t2 (cadr (caddr exp)))
				(t3 (caddr (caddr exp)))
			)
			(and
				(equal? op '*)
				(not (number? t1))
				(equal? op2 '-)
				(not (number? t2))
				(not (number? t3))
			)
		)
        #f
    )
)

(define (perform_rule_20 exp)
	; (* t1 (- t2 t3))
	; (- (* t1 t2) (* t1 t3))
	(display "rule 20") (newline)
	(define t1 (simplify (cadr exp)))
	(define l1 (caddr exp))
	(define t2 (simplify (cadr l1)))
	(define t3 (simplify (caddr l1)))

	(define l2 (simplify (list '* t1 t2)))
	(define l3 (simplify (list '* t1 t3)))
	(simplify (list '- l2 l3))
)

(define (simplify exp)
    (display exp) (newline)
    (cond
        ((is_rule_1 exp) (perform_rule_1 exp))
        ((is_rule_2 exp) (perform_rule_2 exp))
        ((is_rule_3 exp) (perform_rule_3 exp))
        ((is_rule_4 exp) (perform_rule_4 exp))
        ((is_rule_5 exp) (perform_rule_5 exp))
        ((is_rule_6 exp) (perform_rule_6 exp))
        ((is_rule_7 exp) (perform_rule_7 exp))
        ((is_rule_8 exp) (perform_rule_8 exp))
		((is_rule_9 exp) (perform_rule_9 exp))
		((is_rule_10 exp) (perform_rule_10 exp))
		((is_rule_11 exp) (perform_rule_11 exp))
		((is_rule_12 exp) (perform_rule_12 exp))
		((is_rule_13 exp) (perform_rule_13 exp))
		((is_rule_14 exp) (perform_rule_14 exp))
		((is_rule_15 exp) (perform_rule_15 exp))
		((is_rule_16 exp) (perform_rule_16 exp))
		((is_rule_17 exp) (perform_rule_17 exp))
		((is_rule_18 exp) (perform_rule_18 exp))
		((is_rule_19 exp) (perform_rule_19 exp))
		((is_rule_20 exp) (perform_rule_20 exp))
        (else exp)
    )
)



(newline)
;(define result (simplify '(+ 3 9))) ;1
;(define result (simplify '(+ (+ w 2) 3))) ;2
;(define result (simplify '(* 6 4))) ;3
;(define result (simplify '(* e 10))) ;4
;(define result (simplify '(- 7 5))) ;5
;(define result (simplify '(- w 24))) ;6
;(define result (simplify '(+ a (+ b c)))) ;7
;(define result (simplify '(* x (* y z)))) ;8
;(define result (simplify '(+ (+ 4 a) 5))) ;9
;(define result (simplify '(* (* 4 a) 5))) ;10
;(define result (simplify '(* (+ 4 a) 5))) ;11
;(define result (simplify '(* 3 (+ 5 a)))) ;12
;(define result (simplify '(* (+ a b) 3))) ;13
;(define result (simplify '(* 2 (+ a b)))) ;14
;(define result (simplify '(* (- a b) 6))) ;15
;(define result (simplify '(* 3 (- a b)))) ;16
;(define result (simplify '(* (+ a b) c))) ;17
;(define result (simplify '(* a (+ b c)))) ;18
;(define result (simplify '(* (- a v) c))) ;19
;(define result (simplify '(* a (- c b)))) ;20
(display result)
(quit)
