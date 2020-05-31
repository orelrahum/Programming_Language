#lang pl
;;Questions 1
#| that consumes a list of strings and returns the first string that contains the string "pl" as a suffix – if one such exists, and returns #f otherwise
it was very fun for me, this Question take for me 2 hour.
|#
(: plSuffixContained : (Listof String) -> (U Boolean String))
(define (plSuffixContained list )
  (cond
   [(null? list) #f]
   [(> 2 (string-length (first list))) (plSuffixContained (rest list))]
   [(equal? "pl" (substring (first list) ( -(string-length (first list)) 2))) (first list)]
    [else(plSuffixContained (rest list))]))
(test (plSuffixContained '("pllp" "plyy" "ppp" "lpTT" "lol")) => false)
(test (plSuffixContained '("yyyt" "TplT" "plTT" "PlPl" "plplpl")) => "plplpl")
(test (plSuffixContained '("a")) => false)
(test (plSuffixContained '("")) => false)
(test (plSuffixContained '("pl" "pllp" "plyy" "ppp" "lpTT" "lol" )) => "pl")
(test (plSuffixContained '("pl")) => "pl")
(test (plSuffixContained '("orel" "pl")) => "pl")


;;Questions 2.1
#|
write-poly function that consumes a list of coefficients (numbers) a1,a2,...,an and returns the polynomial (in a reversed order of coefficients) "a1xn+a2xn-1+⋯+an".
I used with tail-recursion
this Question take for me 2 week and its cost for me 10 year from my life
this very Problematic question.
If you getting bored bc COVID-19 , try this Question!!
|#

;;[(and (= 1 (length (rest list))) (< 0 (first list)) (< 0 (second list))) (string-append shimon "+" (number->string (first list))"x+"  (number->string (second list)))]


(: write-poly : (Listof Number) -> String)
(define (write-poly  list  )

(cond
   [(null? list)""]
   [(= 0 (length (rest list))) (number->string(first list))]
   [(and (= 1 (length (rest list))) (= 0 (first list))) (number->string (second list))]
   [(and (= 1 (length (rest list))) (= 0 (second list))) (string-append (number->string (first list))"x" )]
   [(and (= 1 (length (rest list))) (> 0 (second list))) (string-append (number->string (first list)) "x"  (number->string (second list)))]
   [(= 1 (length (rest list))) (string-append (number->string (first list)) "x+"  (number->string (second list)))]
   [(= 0 (first list)) (write-poly   (rest list))]
    [else(write-poly-help (string-append  (number->string (first list)) "x^" (number->string (length (rest list)))) (rest list))]))




(: write-poly-help : String (Listof Number) -> String)
(define (write-poly-help shimon list )
(cond
   [(and (= 1 (length (rest list))) (= 0 (first list)) (= 0 (second list)))    shimon ]
   [(and (= 1 (length (rest list))) (= 0 (first list)) (< 0 (second list))) (string-append  shimon "+" (number->string (second list)))]
   [(and (= 1 (length (rest list))) (= 0 (first list)) (> 0 (second list))) (string-append  shimon    (number->string (second list)))]
   [(and (= 1 (length (rest list))) (> 0 (first list)) (= 0 (second list))) (string-append shimon     (number->string (first list)) "x")]
   [(and (= 1 (length (rest list))) (> 0 (first list)) (< 0 (second list))) (string-append shimon     (number->string (first list)) "x+"  (number->string (second list)))]
   [(and (= 1 (length (rest list))) (> 0 (first list)) (> 0 (second list))) (string-append shimon     (number->string (first list)) "x"  (number->string (second list)))]
   [(and (= 1 (length (rest list))) (< 0 (first list)) (= 0 (second list))) (string-append shimon "+" (number->string (first list)) "x" )]
   [(and (= 1 (length (rest list))) (< 0 (first list)) (< 0 (second list))) (string-append shimon "+" (number->string (first list)) "x+"  (number->string (second list)))]
   [(and (= 1 (length (rest list))) (< 0 (first list)) (> 0 (second list))) (string-append shimon "+" (number->string (first list)) "x"  (number->string (second list)))]
   [(and (< 1 (length (rest list))) (> 0 (first list)))  (write-poly-help (string-append shimon  (number->string (first list)) "x^"  (number->string (length (rest list)))) (rest list))]
    [else(write-poly-help (string-append shimon "+" (number->string (first list)) "x^"  (number->string (length (rest list)))) (rest list))]))  

;;tests
(test (write-poly '()) => "")
(test (write-poly '(3 2 -6)) => "3x^2+2x-6")
(test (write-poly '(7 8 9 10)) => "7x^3+8x^2+9x+10")
(test (write-poly '(1 0 2)) => "1x^2+2")
(test (write-poly '(-1)) => "-1")
(test (write-poly '(5)) => "5")
(test (write-poly '(0)) => "0")
(test (write-poly '(-1 3)) => "-1x+3")
(test (write-poly '(-1 -3)) => "-1x-3")
(test (write-poly '(3 0)) => "3x")
(test (write-poly '(4 0)) => "4x")
(test (write-poly '(1 3 0)) => "1x^2+3x")
(test (write-poly '(0 3 0)) => "3x")
(test (write-poly '(0 0 3)) => "3")
(test (write-poly '(4 -1 0 3)) => "4x^3-1x^2+3")
(test (write-poly '(-4 -1 0 3)) => "-4x^3-1x^2+3")
(test (write-poly '(0 0 0)) => "0")
(test (write-poly '(1 0 -4)) => "1x^2-4")
(test (write-poly '(1 -1 0)) => "1x^2-1x")
(test (write-poly '(1 -1 1)) => "1x^2-1x+1")
(test (write-poly '(1 -1 -1)) => "1x^2-1x-1")
(test (write-poly '(3 0 0)) => "3x^2")
(test (write-poly '(0 3)) => "3")
(test (write-poly '(1 0 1 0)) => "1x^3+1x")







;;Questions 2.2
#|
compute-poly function  consumes a number x and a list of coefficients (numbers) a1,a2,...,an and returns the result of the polynomial
I used with tail-recursion
this Question take for me 2 hours
|#



(: compute-poly : Number (Listof Number) -> Number)
(define (compute-poly  value list )
  (cond
    [(null? list) 0]
    [(= 0 value) 0]
    [else (compute-poly-help 0 value list )]))



(: compute-poly-help : Number Number (Listof Number) -> Number)
(define (compute-poly-help Answer value list   )
  (cond
  [(= 0 (length (rest list)))  (+ Answer (first list))]
  [else (compute-poly-help (+ Answer (* (first list) (expt  value (length(rest list))))) value (rest list))]))
 
;;(expt  value (length(rest list)))



(test (compute-poly 2 '()) => 0)
(test (compute-poly 2 '(6)) => 6)
(test (compute-poly 2 '(3 2 6)) => 22)
(test (compute-poly 2 '(4 1)) => 9)
(test (compute-poly 2 '(1 2 1)) => 9)
(test (compute-poly 3 '(4 3 -2 0)) => 129)
(test (compute-poly 0 '(2 4 3 )) => 0)
(test (compute-poly 4 '(3 2 10)) => 66)


;;Questions 3
#|
In this data structure you will need to define a new type called KeyStack
very nice quations
take for me few hours.
|#
 
#|
EmptyKS – this should be a variant of the data type (constructor).
Push – this too should be a variant of the data type.
The push operation should take as input a symbol (key),
a string (value), and an existing keyed-stack and return an extended key-stack in the natural way
take me 30 min.
very nice quation
|#

(define-type KeyStack
  [EmptyKS]
  [Push Symbol String KeyStack])



#|
search-stack – the search operation should take as input a symbol (key)
and a keyed-stack and return the first (LIFO, last in first out) value that is keyed accordingly
If the key does not appear in the original stack, it should return a #f value
take for me 1 hour.
dont like this quetion
|#

(: search-stack : Symbol KeyStack -> (U String #f))
( define (search-stack symbol ks)
   (cases ks
     [(EmptyKS) #f]
     [(Push key val ks2) (if (equal? key symbol) val
                   (search-stack symbol ks2))]))

#|
pop-stack – the pop operation should take as input a keyed-stack and return the keyed-stack without its first (keyed) value
If the original stack was empty, it should return a #f value
its take me too much time, and I dont like it
|#
(: pop-stack : KeyStack -> (U KeyStack #f))
( define (pop-stack ks)
   (cases ks
     [(EmptyKS) #f]
     [(Push key val ks) ks]))



;;all my tests here

(test (EmptyKS) => (EmptyKS))
(test (Push 'b "B" (Push 'a "A" (EmptyKS))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))) => (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))))
(test (search-stack 'a (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => "AAA")
(test (search-stack 'c (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => #f)
(test (pop-stack (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (pop-stack (EmptyKS)) => #f)



;;Questions 1
#| here I will write all my note on Quetions 4
we get code and we need to add only note.
I suugest all our task question on futere task will In this style
take for me 10 min
|#
(: is-odd? : Natural -> Boolean)
;;this function get natural number and output a boolean.
;; this fuction check if its odd number
;; first check is if this number is zero, if not , send it to another fuction if its even minus 1
(define (is-odd? x)
  (if (zero? x)
      false
      (is-even? (- x 1))))


(: is-even? : Natural -> Boolean)
;;this function get natural number and output a boolean.
;; this fuction check if its even  number
;; first check is if this number is zero, if not , send it to another fuction if its odd minus 1
(define (is-even? x)
  (if (zero? x)
      true
      (is-odd? (- x 1))))


;; tests --- is-odd?/is-even?
(test (not (is-odd? 12)))
(test (is-even? 12))
(test (not (is-odd? 0)))
(test (is-even? 0))
(test (is-odd? 1))
(test (not (is-even? 1)))


(: every? : (All (A) (A -> Boolean) (Listof A) -> Boolean))
;; this fuction get listOf A and return true or false for each item on the list
;; this fuction  Going over the list and invoking A on the items until having false or done when its  true.

(define (every? pred lst)
  (or (null? lst)
      (and (pred (first lst))
           (every? pred (rest lst)))))


;; An example for the usefulness of this polymorphic function
(: all-even? : (Listof Natural) -> Boolean)
;; input : list
;; output : boolean
;; this fuction check if all number on list its even
(define (all-even? lst)
  (every? is-even? lst))


;; tests
(test (all-even? null))
(test (all-even? (list 0)))
(test (all-even? (list 2 4 6 8)))
(test (not (all-even? (list 1 3 5 7))))
(test (not (all-even? (list 1))))
(test (not (all-even? (list 2 4 1 6))))


(: every2? : (All (A B) (A -> Boolean) (B -> Boolean) (Listof A) (Listof B) -> Boolean))
;; This functon get 2 preds (pred1 and pred2) and 2 Lists (lst1 and lst2)
;; and return if all element in lst1 (or lst2) satisfied pred1 (or pred2)
(define (every2? pred1 pred2 lst1 lst2)
  (or (null? lst1) ;; both lists assumed to be of same length
      (and (pred1 (first lst1))
           (pred2 (first lst2))
           (every2? pred1 pred2 (rest lst1) (rest lst2)))))


