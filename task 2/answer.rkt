#lang pl
#|

very nice quation.
take for me 4 year from my life
tomy and oskar they are very good friend
its quation to see if we know make  ROL BNF and Parsing code:


 1) {reg-len=2 {and{1 1} {1 1}}}:                          <ROL>
                                                             |    
                                                 {reg-len = <num> <RegE>}
                                                              |       |
                                                              2    {and<RegE><RegE>}
                                                                         |      |
                                                                    Reg<Bits> Reg<Bits>
                                                                         |         |
                                                                  <Bit><Bits>  <Bit><Bits>
                                                                    |     |      |     |
                                                                    1   <Bit>    1   <Bit>
                                                                          |            |
                                                                          1            1


2) {reg-len=3 {shl{1 1 0}}}:                          <ROL>
                                                          |    
                                                 {reg-len = <num> <RegE>}
                                                              |       |
                                                              2    {shl<RegE>}
                                                                       |
                                                                      Reg<Bits>
                                                                             |         
                                                                         <Bit><Bits> 
                                                                             |   |      
                                                                             1  <Bit><Bits>     
                                                                                  |     |
                                                                                  1   <Bit>       
                                                                                        |        
                                                                                        0



3){reg-len=2 {or{and{shl{0 1}}{0 0}}{1 1}}}:                               <ROL>
                                                                             |
                                                                  {reg-len = <num> <RegE> }
                                                                                |     |
                                                                                2    {or <RegE><RegE>}
                                                                                          /        \
                                                                             {and <regE><regE>}   <Bit><Bits>
                                                                                   /      |         |     |   
                                                                        {shl<regE>}  <Bit><Bits>    1   <Bit>
                                                                               |       |      |           |
                                                                       <Bit><Bits>     0    <Bit>         1
                                                                         |     |              |
                                                                         0    <bit>           0
                                                                                |
                                                                                1


|#



; The ROL BNF and Parsing code:
;; Defining two new types
;; used with efrat cohen the prinsses!!!!
(define-type BIT = (U 0 1))
(define-type Bit-List = (Listof BIT))     
 
;; The actual interpreter
#| BNF for the RegE language:
<ROL>  ::= {reg-len = <num> <RegE> }
<RegE> ::= <Bits>              (1)       
	  |{and <RegE> <RegE>} (2)       
	  |{or <RegE> <RegE>}  (3)     
	  |{shl <RegE>}        (4)
 
<Bits> ::= <Bit> | <Bit> <Bits>
<Bit> ::= 1 | 0
|#

;; RegE abstract syntax trees
(define-type RegE
  [Reg Bit-List]
  [And RegE RegE]
  [Or RegE RegE]
  [Shl RegE])
 
;; Next is a technical function that converts (casts)
;; (any) list into a bit-list. We use it in parse-sexpr.
(: list->bit-list : (Listof Any) -> Bit-List)
;; to cast a list of bits as a bit-list
(define (list->bit-list lst)
  (cond [(null? lst) null]
        [(eq? (first lst) 1)(cons 1 (list->bit-list (rest lst)))]
        [else (cons 0 (list->bit-list (rest lst)))])) 

(: parse-sexpr : Sexpr -> RegE)
;; to convert the main s-expression into ROL
(define (parse-sexpr sexpr)
  (match sexpr
    [(list 'reg-len = (number: n) args)
     (if (> n 0) (parse-sexpr-RegL args n)
         (error 'parse-sexpr "Register length must be at least 1 ~s" sexpr))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)])) 

(: parse-sexpr-RegL : Sexpr Number -> RegE)
;; to convert s-expressions into RegEs 
(define (parse-sexpr-RegL sexpr reg-len)
  (match sexpr
    [(list (and a (or 1 0)) ... )(if (= reg-len (length a)) (Reg (list->bit-list a)) (error 'parse-sexpr "wrong number of bits in ~s" a))]
    [(list 'and lst1 lst2) (And (parse-sexpr-RegL lst1 reg-len) (parse-sexpr-RegL lst2 reg-len))]
    [(list 'or lst1 lst2) (Or (parse-sexpr-RegL lst1 reg-len) (parse-sexpr-RegL lst2 reg-len))]
    [(list 'shl lst) (Shl(parse-sexpr-RegL lst reg-len))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)])) 
 
(: parse : String -> RegE)
;; parses a string containing a RegE expression to a RegE AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))


 ;; tests for quation 1
 (test (parse "{ reg-len = 4 {1 0 0 0}}") => (Reg '(1 0 0 0)))
 (test (parse "{ reg-len = 4 {shl {1 0 0 0}}}") => (Shl (Reg '(1 0 0 0))))
 (test (parse "{ reg-len = 4 {and {shl {1 0 1 0}} {shl {1 0 1 0}}}}") => (And (Shl (Reg'(1 0 1 0))) (Shl (Reg '(1 0 1 0)))))
 (test (parse "{ reg-len = 4 { or {and {shl {1 0 1 0}} {shl {1 0 0 1}}} {1 0 1 0}}}") =>(Or (And (Shl (Reg '(1 0 1 0))) (Shl (Reg '(1 0 0 1)))) (Reg '(1 0 1 0))))
 (test (parse "{ reg-len = 2 { or {and {shl {1 0}} {1 0}} {1 0}}}") => (Or (And (Shl(Reg '(1 0))) (Reg '(1 0))) (Reg '(1 0))))
 (test (parse "{ reg-len = 4 {or {1 1 1 1} {0 1 1}}}") =error> "wrong number of bits in")



 (test (parse "{ reg-len = 0 {shl{}}}") =error> "Register length must be at least 1")
 (test (parse "{ reg-len = 4 {or {0 1 0 0} {0 1 0}}}") =error> "wrong number of bits in")
 (test (parse "{ reg = 4 {- {1 1 1 1} {0 1 0 1}}}" ) =error> "bad syntax in")
 (test (parse "{ reg-len = 3 {and {2 2 1} {2 0 2}}}") =error> "bad syntax in")
 (test (parse "{ reg-len = 0 {and {2 2} {1}}}") =error> "Register length must be at least 1")
 (test (parse "{ reg-len = 4 {or {0 1 0 0} {0 1 0}}}") =error> "wrong number of bits in")


;;Questions 2
#|
a)

The AE language we’ve seen is limited to simple calculations.
Some calculators use a ‘memory’ cell to provide more computing “power”.
Here, we will try to enhance the AE language with a memory functionality.
A naive attempt at extending the AE syntax to support this language (call it ‘MAE’) is to add a set operator that sets the current memory value to some expression result,
and a get operator to retrieve the current memory-stored value:
Here, the intended meaning for a {set E} is to evaluate E,
store the result in the memory cell, and return it. There are, however, some problems with this approach.
|#

#|
PROBLWM: IF WE DO get and set itswe need to chosse what first.
solution : choowe need to choose by myself if to start from right to left or from left to right
m.s.l


|#

#|
{* {set {+ 2 {/ 18 12}}} get};;now the get can use the memory and get the variable from that 3*3=9
;;get-> 8

<AE> ::= <num>
  | { + <AE> <AE> }
  | { - <AE> <AE> }
  | { * <AE> <AE> }
  | { / <AE> <AE> }

;;adding two seq options- seq with set and seq with computation
 <MAE>::= | {seq {set <AE>} <DEA>}
  | {seq <AE>}


<DEA>::={set<GEA>}<DEA>
        | <GEA>

<GEA>::=<num>
  | get
  | { + <GEA> <GEA> }
  | { - <GEA> <GEA> }
  | { * <GEA> <GEA> }
  | { / <GEA> <GEA> }



<MAE>
{ seq <AE> }
{ seq { *     <AE>    <num> } }
{ seq { * <num> <num> <num> } }
{ seq { * 316    423    615 } }
{ seq { * 316    423    615 } }
|#


;;Questions 3
#|
define a sum-of-squares function
which takes a list of numbers as input, and produces a number which is the sum of the squares of all of the numbers in the list.
very nice quations pls do it on test !!!
tomy and oskar they are good friend

take for me few +-30 min , I want chocolate .
Tomyyyyyyyyyyyyyyy

(map proc lst ...+) → list?
proc : procedure?
lst : list?
Applies proc to the elements of the lsts from the first elements to the last.
The proc argument must accept the same number of arguments as the number of supplied lsts, and all lsts must have the same number of elements.
he result is a list containing each result of proc in order.

proc init lst ...+) → any/c
proc : procedure?
init : any/c
lst : list?
Like map,
foldl applies a procedure to the elements of one or more lists. Whereas map combines the return values into a list,
foldl combines the return values in an arbitrary way that is determined by proc.
|#

(: sum-of-squares : (Listof Number) -> Number)
(define (sum-of-squares lst)
  (foldl + 0 (map square lst)))

(: square : Number -> Number) 
(define (square num)
  (* num num))


;;tests for questions 3 here
(test (square 0) => 0)
(test (square 2) => 4)
(test (square -2) => 4)
(test (square -1) => 1)
(test (sum-of-squares '()) => 0)
(test (sum-of-squares '(-2)) => 4)
(test (sum-of-squares '(-1)) => 1)
(test (sum-of-squares '(0)) => 0)
(test (sum-of-squares '(1)) => 1)
(test (sum-of-squares '(2)) => 4)
(test (sum-of-squares '(3)) => 9)
(test (sum-of-squares '(1 2 )) => 5)
(test (sum-of-squares '(0 2 )) => 4)
(test (sum-of-squares '(0 -2 )) => 4)
(test (sum-of-squares '(1 2 3)) => 14)
(test (sum-of-squares '(2 2 2)) => 12)


;;Questions 4
#| here I will write all my note on Quetions 4
this questions is to write binary tree, very nice one.
stammmmmmmmmmmmmmmmmmmm
I want to cry, I like to cry.
now 2:13 am in Thursday and bc covid-19 I cant go drink a beer so I need finish this task
take for me 3 days
used with efrat cohen the prinsses
|#

;;1 (a)
(define-type BINTREE
  [Leaf Number]  
  [Node BINTREE BINTREE])


;;2 (b)
;; we will get tree and fuction and return a tree after using f for num
(: tree-map : (Number -> Number) BINTREE -> BINTREE)
(define (tree-map f tree)
  (cases tree
    [(Leaf num) (Leaf (f num))]
    [(Node L-leaf R-leaf) (Node (tree-map f L-leaf)  (tree-map f R-leaf))]))

;;3 (c)
;;who is Messing its mikooo!
;; what want the warm?
;; Looking for wait (fishing rod)

;4 (d)
;;in d we ger combiner function , leaf fuction and binTREE and return combine bin tree

(: tree-fold : (All (A) (A A -> A) (Number -> A) BINTREE -> A))
(define (tree-fold f leaf_f tree)
  (cases tree
    [(Leaf x) (leaf_f x)]
    [(Node L-child R-child) (f (tree-fold f leaf_f L-child)  (tree-fold f leaf_f R-child))]))


  

;;5 (e)
(: tree-flatten : BINTREE -> (Listof Number))
;; recieves a bintree
;;return list of values from yamin to small (small like izi is smallanit)

(define (tree-flatten tree)
  (tree-fold (inst append Number) (inst list Number) tree))

;;6 (f)






;;7 (g)
(: switch-nodes : BINTREE BINTREE -> BINTREE)
  (define (switch-nodes L-child R-child)
      (Node  R-child L-child ))
;;the input is a tree, and it returns a tree that is its mirror image
(: tree-reverse : BINTREE -> BINTREE)
(define (tree-reverse nodeTree)
  (tree-fold switch-nodes Leaf nodeTree))








;; test for quation 4!
(test (tree-map add1 (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))) => (Node (Leaf 2) (Node (Leaf 3) (Leaf 4))))


(test (tree-map add1 (Node (Leaf 0) (Leaf 1))) =>(Node (Leaf 1) (Leaf 2)))
(test (tree-map add1 (Node (Leaf 1.1) (Leaf 1.2))) =>(Node (Leaf 2.1) (Leaf 2.2)))
(test (tree-map add1 (Node (Leaf 1) (Node (Leaf 1) (Leaf 1))))=> (Node (Leaf 2) (Node (Leaf 2) (Leaf 2))))



(test (tree-fold + add1(Node (Leaf 1) (Leaf 1))) => 4)
(test (tree-fold + add1(Node (Leaf 5) (Leaf 5))) => 12)
(test (tree-fold + add1(Node (Leaf 5) (Leaf -2))) => 5)
(test (tree-fold + add1(Node (Leaf 5) (Leaf -1))) => 6)
(test (tree-fold + add1(Node (Leaf 5) (Leaf 7))) => 14)
(test (tree-fold + add1(Node (Leaf 5) (Leaf 6))) => 13)



(test (tree-flatten (Node(Leaf 5)(Leaf 5))) => (list 5 5))
(test (tree-flatten (Node (Leaf 2)(Node(Leaf 2)(Leaf 2)))) => (list 2 2 2))


(test (tree-reverse (Node (Leaf 5) (Leaf 6))) => (Node (Leaf 6) (Leaf 5)))
(test (tree-reverse (Node (Node (Leaf 9) (Leaf 5)) (Leaf 4))) => (Node (Leaf 4) (Node (Leaf 5) (Leaf 9))))
(test (equal? (reverse (tree-flatten (tree-reverse (Node (Node (Leaf 4) (Leaf 5)) (Leaf 6)))))(tree-flatten (tree-reverse (tree-reverse (Node (Node (Leaf 4) (Leaf 5)) (Leaf 6)))))))
(test (equal? (reverse (tree-flatten (Node (Leaf 1) (Leaf 0))))(tree-flatten (tree-reverse (Node (Leaf 1) (Leaf 0))))))
(test (equal? (reverse (tree-flatten (Node (Leaf 4.2) (Leaf 5))))(tree-flatten (tree-reverse (Node (Leaf 4.2) (Leaf 5))))))

















