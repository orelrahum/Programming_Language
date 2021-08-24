;; The Flang interpreter

#lang pl


#|
Orel Rahum - 316423615
Netnanel Ben-Isahar - 204478150
|#



#|
  The grammar:
    <FLANG> ::= <num>
              | { + <FLANG> <FLANG> }
              | { - <FLANG> <FLANG> }
              | { * <FLANG> <FLANG> }
              | { / <FLANG> <FLANG> }
              | { with { <id> <FLANG> } <FLANG> }
              | <id>
              | { fun { <id> } <FLANG> }
              | { call <FLANG> <FLANG> }
              | True                          ;; add rule for True ;; Rule 10
              | False                         ;; Rule 11
              | { = <FLANG> <FLANG> }         ;; add rule for = ;; Rule 12
              | { < <FLANG> <FLANG> }         ;; Rule 13
              | { > <FLANG> <FLANG> } ;; Rule 14
              | { not <FLANG> } ;; Rule 15
              |{ if <FLANG> { then-do <FLANG>} {else-do <FLANG>} } ;; add rule 16 for (the above) if expressions

  Evaluation rules:

    subst:
 subst:
 N[v/x] = N
 {+ E1 E2}[v/x] = {+ E1[v/x] E2[v/x]}
 {- E1 E2}[v/x] = {- E1[v/x] E2[v/x]}
 {* E1 E2}[v/x] = {* E1[v/x] E2[v/x]}
 {/ E1 E2}[v/x] = {/ E1[v/x] E2[v/x]}
 y[v/x] = y
 x[v/x] = v
 {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]} ; if y =/= x
 {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
 {call E1 E2}[v/x] = {call E1[v/x] E2[v/x]}
 {fun {y} E}[v/x] = {fun {y} E[v/x]} ; if y =/= x
 {fun {x} E}[v/x] = {fun {x} E}
 B[v/x] = B ;; B is Boolean
 {= E1 E2}[v/x] = {= E1[v/x] E2[v/x]}
 {> E1 E2}[v/x] = {> E1[v/x] E2[v/x]}
 {< E1 E2}[v/x] = {< E1[v/x] E2[v/x]}
 { not E}[v/x] = {not E[v/x]}
 {if Econd {then-do Edo} {else-do Eelse}}[v/x]
 = {if Econd[v/x] {then-do Edo[v/x]} {else-do
Eelse[v/x]}}

    eval:
      eval(N)            = N
      eval({+ E1 E2})    = eval(E1) + eval(E2)  \ if both E1 and E2
      eval({- E1 E2})    = eval(E1) - eval(E2)   \ evaluate to numbers
      eval({* E1 E2})    = eval(E1) * eval(E2)   / otherwise error!
      eval({/ E1 E2})    = eval(E1) / eval(E2)  /
      eval(id)           = error!
      eval({with {x E1} E2}) = eval(E2[eval(E1)/x])
      eval(FUN)          = FUN ; assuming FUN is a function expression
      eval({call E1 E2}) = eval(Ef[eval(E2)/x]) if eval(E1)={fun {x}Ef}
                         = error!               otherwise
  |#

(define-type FLANG
  [Num  Number]
  [Add  FLANG FLANG]
  [Sub  FLANG FLANG]
  [Mul  FLANG FLANG]
  [Div  FLANG FLANG]
  [Id   Symbol]
  [With Symbol FLANG FLANG]
  [Fun  Symbol FLANG]
  [Call FLANG FLANG]
  [Bool Boolean ]
  [Bigger  FLANG FLANG]
  [Smaller FLANG FLANG]
  [Equal   FLANG FLANG]
  [Not     FLANG]
  [If      FLANG FLANG FLANG]
  )

(: parse-sexpr : Sexpr -> FLANG)
;; to convert s-expressions into FLANGs
(define (parse-sexpr sexpr)
  (match sexpr
    [(number: n)    (Num n)]
    ['True (Bool true)]
    ['False (Bool false)]
    [(symbol: name) (Id name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (With name (parse-sexpr named) (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
    [(cons 'fun more)
     (match sexpr
       [(list 'fun (list (symbol: name)) body)
        (Fun name (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])]
    [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list 'call fun arg) (Call (parse-sexpr fun) (parse-sexpr arg))]
    [(list '= lhs rhs) (Equal (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '> lhs rhs) (Bigger (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '< lhs rhs) (Smaller (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list 'not exp) (Not (parse-sexpr exp))]
    [(cons 'if rest)
     (match sexpr 
       [(list 'if lhs (list 'then-do mhs) (list 'else-do rhs)) (If (parse-sexpr lhs) (parse-sexpr mhs) (parse-sexpr rhs))]
       [else (error 'parse-sexpr "bad `if' syntax in ~s" sexpr)])]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> FLANG)
;; parses a string containing a FLANG expression to a FLANG AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))


(: subst : FLANG Symbol FLANG -> FLANG)
;; substitutes the second argument with the third argument in the
;; first argument, as per the rules of substitution; the resulting
;; expression contains no free instances of the second argument
(define (subst expr from to)
  (cases expr
    [(Num n) expr]
    [(Add l r) (Add (subst l from to) (subst r from to))]
    [(Sub l r) (Sub (subst l from to) (subst r from to))]
    [(Mul l r) (Mul (subst l from to) (subst r from to))]
    [(Div l r) (Div (subst l from to) (subst r from to))]
    [(Id name) (if (eq? name from) to expr)]
    [(With bound-id named-expr bound-body)
     (With bound-id
           (subst named-expr from to)
           (if (eq? bound-id from)
               bound-body
               (subst bound-body from to)))]
    [(Call l r) (Call (subst l from to) (subst r from to))]
    [(Fun bound-id bound-body)
     (if (eq? bound-id from)
         expr
         (Fun bound-id (subst bound-body from to)))]
    [(Bool b) expr]
    [(Equal l r) (Equal (subst l from to) (subst r from to))]
    [(Bigger l r) (Bigger (subst l from to) (subst r from to))]
    [(Smaller l r) (Smaller (subst l from to) (subst r from to))]
    [(Not arg) (Not(subst arg from to))]
    [(If l m r) (If(subst l from to)(subst m from to)(subst r from to))]
    ))



(: arith-op : (Number Number -> Number) FLANG FLANG -> FLANG)
;; gets a Racket numeric binary operator, and uses it within a FLANG
;; `Num' wrapper
(define (arith-op op expr1 expr2)
  (Num (op (Num->number expr1) (Num->number expr2))))

(: Num->number : FLANG -> Number)
(define (Num->number e)
  (cases e
    [(Num n) n]
    [else (error 'Num->number "expected a number, got: ~s" e)]))

(: logic-op : (Number Number -> Boolean) FLANG FLANG -> FLANG)
;; gets a Racket Boolean binary operator (on numbers), and applies it
;; to two `Num' wrapped FLANGs
(define (logic-op op expr1 expr2)
  (Bool (op ( Num->number expr1) ( Num->number expr2))))

(: flang->bool : FLANG -> Boolean)
;; gets a Flang E (of any kind) and returns a its appropiate
;; Boolean value -- which is true if and only if E does not
;; represent false
;; Remark: the `flang->bool` function will also be top-level
;; since it's used in more than one place.
(define (flang->bool e)
  (cases e
    [(Bool b) b]
    [else #t]))


(: eval : FLANG -> FLANG)
;; evaluates FLANG expressions by reducing them to *expressions*
(define (eval expr)
  (cases expr
    [(Num n) expr]
    [(Add l r) (arith-op + (eval l) (eval r))]
    [(Sub l r) (arith-op - (eval l) (eval r))]
    [(Mul l r) (arith-op * (eval l) (eval r))]
    [(Div l r) (arith-op / (eval l) (eval r))]
    [(With bound-id named-expr bound-body)
     (eval (subst bound-body
                  bound-id
                  (eval named-expr)))]
    [(Id name) (error 'eval "free identifier: ~s" name)]
    [(Fun bound-id bound-body) expr]
    [(Call fun-expr arg-expr)
     (let([fval (eval fun-expr)])
       (cases fval
         [(Fun bound-id bound-body)
          (eval (subst bound-body
                       bound-id
                       (eval arg-expr)))]
         [else (error 'eval "`call' expects a function, got: ~s"
                      fval)]))]

    [(Bool b) expr]
    [(Equal l r) (logic-op = (eval l) (eval r))]
    [(Bigger l r) (logic-op > (eval l) (eval r))]
    [(Smaller l r) (logic-op < (eval l) (eval r))]
    [(If l m r)
     (let ([res (eval l)])
       (if (flang->bool res) (eval m) (eval r)))]
    [(Not exp) (Bool(not (flang->bool(eval exp))))])) 
 
(: run : String -> (U Boolean Number FLANG))
;; evaluate a FLANG program contained in a string
(define (run str)
  (let ([result (eval (parse str))])
    (cases result
      [(Num n) n]
      [(Bool b) b]
      [else result])))



;; tests
(test (run "{call {fun {x} {+ x 1}} 4}")
      => 5)
(test (run "{with {add3 {fun {x} {+ x 3}}}
                {call add3 1}}")
      => 4)
(test (run "{with {add3 {fun {x} {+ x 3}}}
                {with {add1 {fun {x} {+ x 1}}}
                  {with {x 3}
                    {call add1 {call add3 x}}}}}")
      => 7)
 


(test (run "True") => true)
(test (run "False") => false)
(test (run "{= 1 2}") => false)
(test (run "{= 1 1}") => true)
(test (run "{< 1 2}") => true)
(test (run "{< 2 1}") => false)
(test (run "{> 2 1}") => true)
(test (run "{> 1 2}") => false)
(test (run "{not True}") => false)
(test (run "{not False}") => true)

(test (run "{with {foo {fun {x} {if {= x x} {then-do {/ x x}} {else-do {- 0 x}}}}} foo}") => (Fun 'x (If (Equal (Id 'x) (Id 'x)) (Div (Id 'x) (Id 'x)) (Sub (Num 0) (Id 'x)))))

(test (run "{if 1 {then-do 2} {else-do 3}}") => 2)
(test (run "{with {x {* 2 2}} {if {> x 0} {then-do {/ 2 x}} {else-do x}}}") => 1/2)
(test (run "{with {x {/ 4 2}} {if {< x 0} {then-do {/ 2 x}} {else-do x}}}") => 2)
(test (run "{with {x {- 1 2}} {if {< x 0} {then-do {- 2 x}} {else-do x}}}") => 3)
(test (run "{call {fun {x} {+ x 1}} 4}") => 5)
(test (run "{call {fun {x} {/ x 1}} 4}") => 4)
(test (run "{with {x 0} {if {> x 1} {then-do x} {else-do {* 6 x}}}}") => 0)

(test (run "{with {x 1} {if {< x 2} {then-do True} {else-do x}}}") => #t)
(test (run "{with {x 1} {if {not {= x 2}} {then-do True} {else-do x}}}") => #t)

(test (run "{if {= x x} {- x x} x}") =error> "parse-sexpr: bad `if' syntax in (if (= x x) (- x x) x)")
(test (run "{with y {with {{x x} x} y}}") =error> "bad `with' syntax in")
(test (run "{call {with {x 1} {with {x x} x}} 1}") =error> "eval: `call' expects a function, got: #(struct:Num 1)")
(test (run "{with {x 1} {if {x} {then-do {/ x x}} {else-do {* x x}}}}") =error> "parse-sexpr: bad syntax in (x)")
(test (run "{call {fun {0} {+ x x}} 1}") =error> "parse-sexpr: bad `fun' syntax in")
(test (run "{= True 5}") =error> "Num->number: expected a number, got: #(struct:Bool #t)")
(test (run "true") =error> "eval: free identifier: true")