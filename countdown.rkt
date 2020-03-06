;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname countdown) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***********************************************
;;   Ke Xin (Cindy) Liu (20834557)
;;   CS 135 Fall 2019
;;   Assignment 10, Problem 2 a)
;; ***********************************************
;;

;; An Operator (Op) is (anyof '+ '- '* '/)

;; A Binary Expression Tree (BET) is one of:
;; * Nat
;; * (list Op BET BET)

;; An Operator Tree (OPT) is one of:
;; * empty
;; * (list Op OPT OPT)

;; A Tree is one of:
;; * empty
;; (list Nat Tree Tree)

;;
;; ***********************************************
;;   Part i
;; ***********************************************
;;

;; (swap i j lst) produces lst with the elements at positions
;;   i and j swapped
;; swap: Nat Nat (listof X) -> (listof X)
;;   requires: i, j < (length lst)
;; Examples:
(check-expect (swap 0 3 '(0 1 2 3 4 5)) '(3 1 2 0 4 5))

(define (swap i j lst)
  (local
    [(define my-i (list-ref lst i))
     (define my-j (list-ref lst j))
     (define (swap-loc i j lst)
       (cond
         [(empty? lst) empty]
         [(= 0 i) (cons my-j (swap-loc (sub1 i) (sub1 j) (rest lst)))]
         [(= 0 j) (cons my-i (swap-loc (sub1 i) (sub1 j) (rest lst)))]
         [else (cons (first lst)
                     (swap-loc (sub1 i) (sub1 j) (rest lst)))]))]
    
    (swap-loc i j lst)))

;; Tests:
(check-expect (swap 0 1 '(1 2)) (list 2 1))
(check-expect (swap 0 2 '(1 2 3)) (list 3 2 1))
(check-expect (swap 1 2 '(1 2 3)) (list 1 3 2))


;;
;; ***********************************************
;;   Part ii
;; ***********************************************
;;

;; (generate-permutations lst) produces all possible permutations
;;   of lst.
;; generate-permutations: (listof X) -> (listof (listof X))
;; Examples:
(check-expect (generate-permutations '(2 4 8))
              (list
               (list 8 4 2)
               (list 8 2 4)
               (list 4 8 2)
               (list 4 2 8)
               (list 2 8 4)
               (list 2 4 8)))

(define (generate-permutations lst)
  (cond
    [(empty? lst) empty]
    [(empty? (rest lst)) (list lst)]
    [else
     (local
       [;; (gen-per1 len lst) produces all possible permutations
        ;;   of lst.
        ;; gen-per1: Num (listof X) -> (listof (listof X))
        ;; requires: len is integer
        (define (gen-per1 len lst)
          (cond
            [(> 0 len) empty]
            [else (append (gen-per2 len lst) (gen-per1 (sub1 len) lst))]))

        ;; (gen-per2 n lst) produces all the possible permutations of lst with
        ;;   the nth element of the lst fixed.
        ;; gen-per2: Nat (listof X) -> (listof (listof X))
        (define (gen-per2 n lst)
          (my-append (list-ref lst n)
                     (generate-permutations (my-remove n lst))))

        ;; (my-remove n lst) produces a list with the nth element removed from
        ;;   the given list, lst 
        ;; my-remove: Nat (listof X) -> (listof X)
        ;; requires: n < (length lst)
        (define (my-remove n lst)
          (cond
            [(= 0 n) (rest lst)]
            [else (cons (first lst) (my-remove (sub1 n) (rest lst)))]))

        ;; (my-append elem lst) appends the given element, elem, to all
        ;;   elements of the given list, lst
        ;; my-append: X (listof (listof X)) -> (listof (listof X))
        (define (my-append elem lst)
          (cond
            [(empty? lst) empty]
            [else (cons (append (list elem) (first lst))
                        (my-append elem (rest lst)))]))]

       (gen-per1 (- (length lst) 1) lst))]))

;; Tests:
(check-expect (generate-permutations '(1)) (list (list 1)))
(check-expect (generate-permutations '()) empty)
(check-expect (generate-permutations '(1 2)) (list (list 2 1) (list 1 2)))
(check-expect (generate-permutations '(1 2 3 4))
              (list
               (list 4 3 2 1)
               (list 4 3 1 2)
               (list 4 2 3 1)
               (list 4 2 1 3)
               (list 4 1 3 2)
               (list 4 1 2 3)
               (list 3 4 2 1)
               (list 3 4 1 2)
               (list 3 2 4 1)
               (list 3 2 1 4)
               (list 3 1 4 2)
               (list 3 1 2 4)
               (list 2 4 3 1)
               (list 2 4 1 3)
               (list 2 3 4 1)
               (list 2 3 1 4)
               (list 2 1 4 3)
               (list 2 1 3 4)
               (list 1 4 3 2)
               (list 1 4 2 3)
               (list 1 3 4 2)
               (list 1 3 2 4)
               (list 1 2 4 3)
               (list 1 2 3 4)))
(check-expect (generate-permutations '(1 2 3 4 5))
              (list
               (list 5 4 3 2 1)
               (list 5 4 3 1 2)
               (list 5 4 2 3 1)
               (list 5 4 2 1 3)
               (list 5 4 1 3 2)
               (list 5 4 1 2 3)
               (list 5 3 4 2 1)
               (list 5 3 4 1 2)
               (list 5 3 2 4 1)
               (list 5 3 2 1 4)
               (list 5 3 1 4 2)
               (list 5 3 1 2 4)
               (list 5 2 4 3 1)
               (list 5 2 4 1 3)
               (list 5 2 3 4 1)
               (list 5 2 3 1 4)
               (list 5 2 1 4 3)
               (list 5 2 1 3 4)
               (list 5 1 4 3 2)
               (list 5 1 4 2 3)
               (list 5 1 3 4 2)
               (list 5 1 3 2 4)
               (list 5 1 2 4 3)
               (list 5 1 2 3 4)
               (list 4 5 3 2 1)
               (list 4 5 3 1 2)
               (list 4 5 2 3 1)
               (list 4 5 2 1 3)
               (list 4 5 1 3 2)
               (list 4 5 1 2 3)
               (list 4 3 5 2 1)
               (list 4 3 5 1 2)
               (list 4 3 2 5 1)
               (list 4 3 2 1 5)
               (list 4 3 1 5 2)
               (list 4 3 1 2 5)
               (list 4 2 5 3 1)
               (list 4 2 5 1 3)
               (list 4 2 3 5 1)
               (list 4 2 3 1 5)
               (list 4 2 1 5 3)
               (list 4 2 1 3 5)
               (list 4 1 5 3 2)
               (list 4 1 5 2 3)
               (list 4 1 3 5 2)
               (list 4 1 3 2 5)
               (list 4 1 2 5 3)
               (list 4 1 2 3 5)
               (list 3 5 4 2 1)
               (list 3 5 4 1 2)
               (list 3 5 2 4 1)
               (list 3 5 2 1 4)
               (list 3 5 1 4 2)
               (list 3 5 1 2 4)
               (list 3 4 5 2 1)
               (list 3 4 5 1 2)
               (list 3 4 2 5 1)
               (list 3 4 2 1 5)
               (list 3 4 1 5 2)
               (list 3 4 1 2 5)
               (list 3 2 5 4 1)
               (list 3 2 5 1 4)
               (list 3 2 4 5 1)
               (list 3 2 4 1 5)
               (list 3 2 1 5 4)
               (list 3 2 1 4 5)
               (list 3 1 5 4 2)
               (list 3 1 5 2 4)
               (list 3 1 4 5 2)
               (list 3 1 4 2 5)
               (list 3 1 2 5 4)
               (list 3 1 2 4 5)
               (list 2 5 4 3 1)
               (list 2 5 4 1 3)
               (list 2 5 3 4 1)
               (list 2 5 3 1 4)
               (list 2 5 1 4 3)
               (list 2 5 1 3 4)
               (list 2 4 5 3 1)
               (list 2 4 5 1 3)
               (list 2 4 3 5 1)
               (list 2 4 3 1 5)
               (list 2 4 1 5 3)
               (list 2 4 1 3 5)
               (list 2 3 5 4 1)
               (list 2 3 5 1 4)
               (list 2 3 4 5 1)
               (list 2 3 4 1 5)
               (list 2 3 1 5 4)
               (list 2 3 1 4 5)
               (list 2 1 5 4 3)
               (list 2 1 5 3 4)
               (list 2 1 4 5 3)
               (list 2 1 4 3 5)
               (list 2 1 3 5 4)
               (list 2 1 3 4 5)
               (list 1 5 4 3 2)
               (list 1 5 4 2 3)
               (list 1 5 3 4 2)
               (list 1 5 3 2 4)
               (list 1 5 2 4 3)
               (list 1 5 2 3 4)
               (list 1 4 5 3 2)
               (list 1 4 5 2 3)
               (list 1 4 3 5 2)
               (list 1 4 3 2 5)
               (list 1 4 2 5 3)
               (list 1 4 2 3 5)
               (list 1 3 5 4 2)
               (list 1 3 5 2 4)
               (list 1 3 4 5 2)
               (list 1 3 4 2 5)
               (list 1 3 2 5 4)
               (list 1 3 2 4 5)
               (list 1 2 5 4 3)
               (list 1 2 5 3 4)
               (list 1 2 4 5 3)
               (list 1 2 4 3 5)
               (list 1 2 3 5 4)
               (list 1 2 3 4 5)))


;;
;; ***********************************************
;;   Part iii
;; ***********************************************
;;

;; (generate-tuples lst n) produces all tuples of length n of
;;   elements in lst.
;; generate-tuples: (listof X) Nat -> (listof (listof X))
;; Examples:
(check-expect
 (generate-tuples '(+ -) 3)
 '((+ + +) (+ + -) (+ - +) (+ - -)
           (- + +) (- + -) (- - +) (- - -)))
(check-expect
 (generate-tuples '(+ -) 0)
 (list empty))

(define (generate-tuples lst n)
  (cond
    [(empty? lst) empty]
    [(= n 0) (list empty)]
    [else
     (local
       [;; (my-append1 lst1 lst2) produces a list with each of the elements in
        ;;   lst1 append to all the elements in lst2
        ;; my-append1: (listof X) (listof (listof X)) -> (listof (listof X))
        (define (my-append1 lst1 lst2)
          (cond
            [(empty? lst1) empty]
            [else (append (my-append2 (first lst1) lst2)
                          (my-append1 (rest lst1) lst2))]))

        ;; (my-append elem lst) appends the given element, elem, to all
        ;;   elements of the given list, lst
        ;; my-append2: X (listof (listof X))-> (listof (listof X))
        (define (my-append2 elem lst)
          (cond
            [(empty? lst) empty]
            [else (cons (append (list elem) (first lst))
                        (my-append2 elem (rest lst)))]))]
       
       (my-append1 lst (generate-tuples lst (sub1 n))))]))

;; Tests:
(check-expect (generate-tuples '() 2) empty)
(check-expect (generate-tuples '(+) 2)
              (list (list '+ '+)))
(check-expect (generate-tuples '(+ - * /) 1)
              (list (list '+) (list '-) (list '*) (list '/)))
(check-expect (generate-tuples '(+ - * /) 2)
              (list
               (list '+ '+)
               (list '+ '-)
               (list '+ '*)
               (list '+ '/)
               (list '- '+)
               (list '- '-)
               (list '- '*)
               (list '- '/)
               (list '* '+)
               (list '* '-)
               (list '* '*)
               (list '* '/)
               (list '/ '+)
               (list '/ '-)
               (list '/ '*)
               (list '/ '/)))
(check-expect(generate-tuples '(+ - * /) 3)
             (list
              (list '+ '+ '+)
              (list '+ '+ '-)
              (list '+ '+ '*)
              (list '+ '+ '/)
              (list '+ '- '+)
              (list '+ '- '-)
              (list '+ '- '*)
              (list '+ '- '/)
              (list '+ '* '+)
              (list '+ '* '-)
              (list '+ '* '*)
              (list '+ '* '/)
              (list '+ '/ '+)
              (list '+ '/ '-)
              (list '+ '/ '*)
              (list '+ '/ '/)
              (list '- '+ '+)
              (list '- '+ '-)
              (list '- '+ '*)
              (list '- '+ '/)
              (list '- '- '+)
              (list '- '- '-)
              (list '- '- '*)
              (list '- '- '/)
              (list '- '* '+)
              (list '- '* '-)
              (list '- '* '*)
              (list '- '* '/)
              (list '- '/ '+)
              (list '- '/ '-)
              (list '- '/ '*)
              (list '- '/ '/)
              (list '* '+ '+)
              (list '* '+ '-)
              (list '* '+ '*)
              (list '* '+ '/)
              (list '* '- '+)
              (list '* '- '-)
              (list '* '- '*)
              (list '* '- '/)
              (list '* '* '+)
              (list '* '* '-)
              (list '* '* '*)
              (list '* '* '/)
              (list '* '/ '+)
              (list '* '/ '-)
              (list '* '/ '*)
              (list '* '/ '/)
              (list '/ '+ '+)
              (list '/ '+ '-)
              (list '/ '+ '*)
              (list '/ '+ '/)
              (list '/ '- '+)
              (list '/ '- '-)
              (list '/ '- '*)
              (list '/ '- '/)
              (list '/ '* '+)
              (list '/ '* '-)
              (list '/ '* '*)
              (list '/ '* '/)
              (list '/ '/ '+)
              (list '/ '/ '-)
              (list '/ '/ '*)
              (list '/ '/ '/)))
(check-expect (generate-tuples '(+ - * /) 4)
              (list
               (list '+ '+ '+ '+)
               (list '+ '+ '+ '-)
               (list '+ '+ '+ '*)
               (list '+ '+ '+ '/)
               (list '+ '+ '- '+)
               (list '+ '+ '- '-)
               (list '+ '+ '- '*)
               (list '+ '+ '- '/)
               (list '+ '+ '* '+)
               (list '+ '+ '* '-)
               (list '+ '+ '* '*)
               (list '+ '+ '* '/)
               (list '+ '+ '/ '+)
               (list '+ '+ '/ '-)
               (list '+ '+ '/ '*)
               (list '+ '+ '/ '/)
               (list '+ '- '+ '+)
               (list '+ '- '+ '-)
               (list '+ '- '+ '*)
               (list '+ '- '+ '/)
               (list '+ '- '- '+)
               (list '+ '- '- '-)
               (list '+ '- '- '*)
               (list '+ '- '- '/)
               (list '+ '- '* '+)
               (list '+ '- '* '-)
               (list '+ '- '* '*)
               (list '+ '- '* '/)
               (list '+ '- '/ '+)
               (list '+ '- '/ '-)
               (list '+ '- '/ '*)
               (list '+ '- '/ '/)
               (list '+ '* '+ '+)
               (list '+ '* '+ '-)
               (list '+ '* '+ '*)
               (list '+ '* '+ '/)
               (list '+ '* '- '+)
               (list '+ '* '- '-)
               (list '+ '* '- '*)
               (list '+ '* '- '/)
               (list '+ '* '* '+)
               (list '+ '* '* '-)
               (list '+ '* '* '*)
               (list '+ '* '* '/)
               (list '+ '* '/ '+)
               (list '+ '* '/ '-)
               (list '+ '* '/ '*)
               (list '+ '* '/ '/)
               (list '+ '/ '+ '+)
               (list '+ '/ '+ '-)
               (list '+ '/ '+ '*)
               (list '+ '/ '+ '/)
               (list '+ '/ '- '+)
               (list '+ '/ '- '-)
               (list '+ '/ '- '*)
               (list '+ '/ '- '/)
               (list '+ '/ '* '+)
               (list '+ '/ '* '-)
               (list '+ '/ '* '*)
               (list '+ '/ '* '/)
               (list '+ '/ '/ '+)
               (list '+ '/ '/ '-)
               (list '+ '/ '/ '*)
               (list '+ '/ '/ '/)
               (list '- '+ '+ '+)
               (list '- '+ '+ '-)
               (list '- '+ '+ '*)
               (list '- '+ '+ '/)
               (list '- '+ '- '+)
               (list '- '+ '- '-)
               (list '- '+ '- '*)
               (list '- '+ '- '/)
               (list '- '+ '* '+)
               (list '- '+ '* '-)
               (list '- '+ '* '*)
               (list '- '+ '* '/)
               (list '- '+ '/ '+)
               (list '- '+ '/ '-)
               (list '- '+ '/ '*)
               (list '- '+ '/ '/)
               (list '- '- '+ '+)
               (list '- '- '+ '-)
               (list '- '- '+ '*)
               (list '- '- '+ '/)
               (list '- '- '- '+)
               (list '- '- '- '-)
               (list '- '- '- '*)
               (list '- '- '- '/)
               (list '- '- '* '+)
               (list '- '- '* '-)
               (list '- '- '* '*)
               (list '- '- '* '/)
               (list '- '- '/ '+)
               (list '- '- '/ '-)
               (list '- '- '/ '*)
               (list '- '- '/ '/)
               (list '- '* '+ '+)
               (list '- '* '+ '-)
               (list '- '* '+ '*)
               (list '- '* '+ '/)
               (list '- '* '- '+)
               (list '- '* '- '-)
               (list '- '* '- '*)
               (list '- '* '- '/)
               (list '- '* '* '+)
               (list '- '* '* '-)
               (list '- '* '* '*)
               (list '- '* '* '/)
               (list '- '* '/ '+)
               (list '- '* '/ '-)
               (list '- '* '/ '*)
               (list '- '* '/ '/)
               (list '- '/ '+ '+)
               (list '- '/ '+ '-)
               (list '- '/ '+ '*)
               (list '- '/ '+ '/)
               (list '- '/ '- '+)
               (list '- '/ '- '-)
               (list '- '/ '- '*)
               (list '- '/ '- '/)
               (list '- '/ '* '+)
               (list '- '/ '* '-)
               (list '- '/ '* '*)
               (list '- '/ '* '/)
               (list '- '/ '/ '+)
               (list '- '/ '/ '-)
               (list '- '/ '/ '*)
               (list '- '/ '/ '/)
               (list '* '+ '+ '+)
               (list '* '+ '+ '-)
               (list '* '+ '+ '*)
               (list '* '+ '+ '/)
               (list '* '+ '- '+)
               (list '* '+ '- '-)
               (list '* '+ '- '*)
               (list '* '+ '- '/)
               (list '* '+ '* '+)
               (list '* '+ '* '-)
               (list '* '+ '* '*)
               (list '* '+ '* '/)
               (list '* '+ '/ '+)
               (list '* '+ '/ '-)
               (list '* '+ '/ '*)
               (list '* '+ '/ '/)
               (list '* '- '+ '+)
               (list '* '- '+ '-)
               (list '* '- '+ '*)
               (list '* '- '+ '/)
               (list '* '- '- '+)
               (list '* '- '- '-)
               (list '* '- '- '*)
               (list '* '- '- '/)
               (list '* '- '* '+)
               (list '* '- '* '-)
               (list '* '- '* '*)
               (list '* '- '* '/)
               (list '* '- '/ '+)
               (list '* '- '/ '-)
               (list '* '- '/ '*)
               (list '* '- '/ '/)
               (list '* '* '+ '+)
               (list '* '* '+ '-)
               (list '* '* '+ '*)
               (list '* '* '+ '/)
               (list '* '* '- '+)
               (list '* '* '- '-)
               (list '* '* '- '*)
               (list '* '* '- '/)
               (list '* '* '* '+)
               (list '* '* '* '-)
               (list '* '* '* '*)
               (list '* '* '* '/)
               (list '* '* '/ '+)
               (list '* '* '/ '-)
               (list '* '* '/ '*)
               (list '* '* '/ '/)
               (list '* '/ '+ '+)
               (list '* '/ '+ '-)
               (list '* '/ '+ '*)
               (list '* '/ '+ '/)
               (list '* '/ '- '+)
               (list '* '/ '- '-)
               (list '* '/ '- '*)
               (list '* '/ '- '/)
               (list '* '/ '* '+)
               (list '* '/ '* '-)
               (list '* '/ '* '*)
               (list '* '/ '* '/)
               (list '* '/ '/ '+)
               (list '* '/ '/ '-)
               (list '* '/ '/ '*)
               (list '* '/ '/ '/)
               (list '/ '+ '+ '+)
               (list '/ '+ '+ '-)
               (list '/ '+ '+ '*)
               (list '/ '+ '+ '/)
               (list '/ '+ '- '+)
               (list '/ '+ '- '-)
               (list '/ '+ '- '*)
               (list '/ '+ '- '/)
               (list '/ '+ '* '+)
               (list '/ '+ '* '-)
               (list '/ '+ '* '*)
               (list '/ '+ '* '/)
               (list '/ '+ '/ '+)
               (list '/ '+ '/ '-)
               (list '/ '+ '/ '*)
               (list '/ '+ '/ '/)
               (list '/ '- '+ '+)
               (list '/ '- '+ '-)
               (list '/ '- '+ '*)
               (list '/ '- '+ '/)
               (list '/ '- '- '+)
               (list '/ '- '- '-)
               (list '/ '- '- '*)
               (list '/ '- '- '/)
               (list '/ '- '* '+)
               (list '/ '- '* '-)
               (list '/ '- '* '*)
               (list '/ '- '* '/)
               (list '/ '- '/ '+)
               (list '/ '- '/ '-)
               (list '/ '- '/ '*)
               (list '/ '- '/ '/)
               (list '/ '* '+ '+)
               (list '/ '* '+ '-)
               (list '/ '* '+ '*)
               (list '/ '* '+ '/)
               (list '/ '* '- '+)
               (list '/ '* '- '-)
               (list '/ '* '- '*)
               (list '/ '* '- '/)
               (list '/ '* '* '+)
               (list '/ '* '* '-)
               (list '/ '* '* '*)
               (list '/ '* '* '/)
               (list '/ '* '/ '+)
               (list '/ '* '/ '-)
               (list '/ '* '/ '*)
               (list '/ '* '/ '/)
               (list '/ '/ '+ '+)
               (list '/ '/ '+ '-)
               (list '/ '/ '+ '*)
               (list '/ '/ '+ '/)
               (list '/ '/ '- '+)
               (list '/ '/ '- '-)
               (list '/ '/ '- '*)
               (list '/ '/ '- '/)
               (list '/ '/ '* '+)
               (list '/ '/ '* '-)
               (list '/ '/ '* '*)
               (list '/ '/ '* '/)
               (list '/ '/ '/ '+)
               (list '/ '/ '/ '-)
               (list '/ '/ '/ '*)
               (list '/ '/ '/ '/)))


;;
;; ***********************************************
;;   Part iv
;; ***********************************************
;;

;; (create-bets nlon nloop) produces a list of all possible BET
;;   based off of nlon and nloop.
;; create-bets: (listof (listof Num)) (listof (listof Op)) ->
;;                                                         (listof BET)
;; requires: (length nlon) - (length nloop) = 1
;; Examples:
(check-expect (create-bets
               '((8 6 4 2))
               '((/ + -)))
              (list
               (list '/ (list '+ (list '- 8 6) 4) 2)
               (list '/ (list '+ 8 (list '- 6 4)) 2)
               (list '/ (list '+ 8 6) (list '- 4 2))
               (list '/ 8 (list '+ (list '- 6 4) 2))
               (list '/ 8 (list '+ 6 (list '- 4 2)))))

(define (create-bets nlon nloop)
  (cond
    [(empty? nlon) empty]
    [(or (empty? nloop) (empty? (first nloop)))
     (append (list (first (first nlon)))
             (create-bets (rest nlon) nloop))]
    [else
     (local
       [;; (create-bets/fix-lon lon nloop lot) produces a list of all possible
        ;;   BET based with the given list of number, lon, nloop and the given
        ;;   list of tree structures, lot
        ;; create-bets/fix-lon: (listof Num) (listof (listof Op)) (listof Tree)
        ;;                       -> (listof BET)
        (define (create-bets/fix-lon lon nloop lot)
          (cond
            [(empty? nloop) empty]
            [else (append (create-bets/fix-loop lon (first nloop) lot)
                          (create-bets/fix-lon lon (rest nloop) lot))]))

        ;; (create-bets/fix-loop lon loop lot) produces a list of all possible
        ;;   BET based with the given list of number, lon, given list of
        ;;   operators, loop, and the given list of tree structures, lot
        ;; create-bets/fix-loop: (listof Num) (listof Op) (listof Tree) ->
        ;;                                                        (listof BET)
        (define (create-bets/fix-loop lon loop lot)
          (cond
            [(empty? lot) empty]
            [else (cons (make-bet lon (make-opt loop (first lot)))
                        (create-bets/fix-loop lon loop (rest lot)))]))

        ;; (make-opt loop tree) inserts the given list of operators, loop,
        ;;   into the given tree structure, tree
        ;; make-opt: (listof Op) Tree -> OPT
        (define (make-opt loop tree)
          (cond
            [(empty? tree) empty]
            [else
             (local
               [;; (count-nodes tree) produces the number of nodes in the given
                ;;    tree, tree
                ;; count-nodes: Tree -> Nat
                (define (count-nodes tree)
                  (cond
                    [(empty? tree) 0] 
                    [else (+ 1 (count-nodes (second tree))
                             (count-nodes (third tree)))]))

                ;; (my-first loop n) returns the first n elements of the given
                ;;   list, loop
                ;; my-first: (listof X) Nat -> (listof X)
                (define (my-first loop n)
                  (cond
                    [(= 0 n) empty]
                    [else (cons (first loop)
                                (my-first (rest loop) (sub1 n)))]))]
       
               (cons (first loop)
                     (cons (make-opt (my-first (rest loop)
                                               (count-nodes (second tree)))
                                     (second tree))
                           (cons (make-opt
                                  (reverse
                                   (my-first (reverse (rest loop))
                                             (count-nodes (third tree))))
                                  (third tree)) empty))))]))

        ;; (make-bet lon opt) creats a BET with the given list of Num and the
        ;;   given OPT, opt
        ;; make-bet: (listof Num) OPT -> BET
        (define (make-bet lon opt)
          (cond
            [(empty? opt) (first lon)]
            [else
             (local
               [;; (my-first loop n) returns the first n elements of the given
                ;;   list, loop
                ;; my-first: (listof X) Nat -> (listof X)
                (define (my-first lon n)
                  (cond
                    [(= 0 n) empty]
                    [else (cons (first lon) (my-first (rest lon) (sub1 n)))]))

                ;; (count-leafs tree) produces the number of leafs in the given
                ;;   tree, tree
                ;; count-nodes: OPT -> Nat
                (define (count-leafs tree)
                  (cond
                    [(empty? tree) 1] 
                    [else (+ 0 (count-leafs (second tree))
                             (count-leafs (third tree)))]))]
       
               (cons (first opt)
                     (cons (make-bet (my-first lon (count-leafs (second opt)))
                                     (second opt))
                           (cons (make-bet
                                  (reverse
                                   (my-first (reverse lon)
                                             (count-leafs (third opt))))
                                  (third opt)) empty))))]))

        ;; (create-left-right-pairs node-cnt) produces a list of number-pairs
        ;;   indicating the size of the left and right subtrees for a given
        ;;   overall node-count on a tree, node-cnt
        ;; create-left-right-pairs: Nat -> (listof (list Nat Nat))
        (define (create-left-right-pairs node-cnt)
          (local
            [(define o-node-cnt (- node-cnt 1))
             (define (create/acc cnt)
               (cond
                 [(> cnt o-node-cnt) empty]
                 [else (cons (list (- o-node-cnt cnt) cnt)
                             (create/acc (add1 cnt)))]))]

            (create/acc 0)))

        ;; (create-tree-structure n) produces a list of Tree given the number
        ;;   of nodes in the tree, n
        ;; create-tree-structure: Nat -> (listof Tree)
        (define (create-tree-structure n)
          (cond
            [(zero? n) empty]
            [else (create-tree-struct/helper n (create-left-right-pairs n))]))

        ;; (create-tree-struct/helper n lst) produces a list of Tree given the
        ;;   Nat of begining node and the different combinations of the number
        ;;   of left nodes and right nodes
        ;; create-tree-struct/helper: Nat (listof (list Nat Nat)) ->
        ;;                                                       (listof Tree)
        (define (create-tree-struct/helper n lst)
          (cond
            [(empty? lst) empty]
            [else
             (append (create-tree n (first (first lst)) (second (first lst)))
                     (create-tree-struct/helper n (rest lst)))]))

        ;; (create-tree n left-ncount right-ncount) produces all possible trees
        ;;   given the Nat of the trees, n, and the number of left nodes,
        ;;   left-ncount, and the number of right nodes, right-ncount
        ;; create-tree: Nat Nat Nat -> (listof Tree)
        (define (create-tree n left-ncount right-ncount)
          (cond
            [(and (zero? left-ncount) (zero? right-ncount))
             (list (list n empty empty))]
            [(zero? left-ncount)
             (local
               [;; (integrate-right lot n) produces a list of tree by using n
                ;;   as the Nat and lot as the right tree
                ;; integrate-right: (listof Tree) Nat -> (listof Tree) 
                (define (integrate-right lot n)
                  (cond
                    [(empty? lot) empty]
                    [else (cons (cons n (cons empty (cons (first lot) empty)))
                                (integrate-right (rest lot) n))]))]
       
               (integrate-right (create-tree-structure right-ncount) n))]
            [(zero? right-ncount)
             (local
               [;; (integrate-left lot n) produces a list of tree by using n as
                ;;   the Nat and lot as the left tree
                ;; integrate-left: (listof Tree) Nat -> (listof Tree) 
                (define (integrate-left lot n)
                  (cond
                    [(empty? lot) empty]
                    [else (cons (cons n (cons (first lot) (cons empty empty)))
                                (integrate-left (rest lot) n))]))]
       
               (integrate-left (create-tree-structure left-ncount) n))]
            [else (integrate n (create-tree-structure left-ncount)
                             (create-tree-structure right-ncount))]))

        ;; (integrate n lot1 lot2) produces all the different possible trees
        ;;   with n as the Nat and an element of lot1 as the right node and an
        ;;   element of lot2 as the left node 
        ;; integrate: Nat (listof Tree) (listof Tree) -> (listof Tree)
        (define (integrate n lot1 lot2)
          (cond
            [(empty? lot1) empty]
            [else (append (integrate/helper n (first lot1) lot2)
                          (integrate n (rest lot1) lot2))]))

        ;; (integrate/helper n tree lst) creates a list of Trees that have n as
        ;;   the Nat, tree as the left node and each element of lst as the
        ;;   left node 
        ;; integrate/helper: Nat Tree (listof Tree) -> (listof Tree)
        (define (integrate/helper n tree lst)
          (cond
            [(empty? lst) empty]
            [else (cons (cons n (cons tree (cons (first lst) empty)))
                        (integrate/helper n tree (rest lst)))]))]
       
       (append
        (create-bets/fix-lon (first nlon) nloop
                             (create-tree-structure (length (first nloop))))
        (create-bets (rest nlon) nloop)))]))

;; Tests:
(check-expect (create-bets '((2) (3) (4)) '(())) (list 2 3 4))
(check-expect (create-bets '((2) (3) (4)) empty) (list 2 3 4))
(check-expect (create-bets '((2) (3) (4)) '(() ())) (list 2 3 4))
(check-expect (create-bets '((8 6)) '((+) (-)))
              (list (list '+ 8 6) (list '- 8 6)))
(check-expect (create-bets '((6 8)) '((+) (-)))
              (list (list '+ 6 8) (list '- 6 8)))
(check-expect (create-bets '((8 6 4 2)) '((/ + -) (* + -)))
              (list
               (list '/ (list '+ (list '- 8 6) 4) 2)
               (list '/ (list '+ 8 (list '- 6 4)) 2)
               (list '/ (list '+ 8 6) (list '- 4 2))
               (list '/ 8 (list '+ (list '- 6 4) 2))
               (list '/ 8 (list '+ 6 (list '- 4 2)))
               (list '* (list '+ (list '- 8 6) 4) 2)
               (list '* (list '+ 8 (list '- 6 4)) 2)
               (list '* (list '+ 8 6) (list '- 4 2))
               (list '* 8 (list '+ (list '- 6 4) 2))
               (list '* 8 (list '+ 6 (list '- 4 2)))))
(check-expect (create-bets '((8 6 4 2 1)) '((/ + - *)))
              (list
               (list '/ (list '+ (list '- (list '* 8 6) 4) 2) 1)
               (list '/ (list '+ (list '- 8 (list '* 6 4)) 2) 1)
               (list '/ (list '+ (list '- 8 6) (list '* 4 2)) 1)
               (list '/ (list '+ 8 (list '- (list '* 6 4) 2)) 1)
               (list '/ (list '+ 8 (list '- 6 (list '* 4 2))) 1)
               (list '/ (list '+ (list '- 8 6) 4) (list '* 2 1))
               (list '/ (list '+ 8 (list '- 6 4)) (list '* 2 1))
               (list '/ (list '+ 8 6) (list '- (list '* 4 2) 1))
               (list '/ (list '+ 8 6) (list '- 4 (list '* 2 1)))
               (list '/ 8 (list '+ (list '- (list '* 6 4) 2) 1))
               (list '/ 8 (list '+ (list '- 6 (list '* 4 2)) 1))
               (list '/ 8 (list '+ (list '- 6 4) (list '* 2 1)))
               (list '/ 8 (list '+ 6 (list '- (list '* 4 2) 1)))
               (list '/ 8 (list '+ 6 (list '- 4 (list '* 2 1))))))
(check-expect (create-bets '((8 6 4 2 1) (1 1 1 1 1)) '((/ + - *)))
              (list
               (list '/ (list '+ (list '- (list '* 8 6) 4) 2) 1)
               (list '/ (list '+ (list '- 8 (list '* 6 4)) 2) 1)
               (list '/ (list '+ (list '- 8 6) (list '* 4 2)) 1)
               (list '/ (list '+ 8 (list '- (list '* 6 4) 2)) 1)
               (list '/ (list '+ 8 (list '- 6 (list '* 4 2))) 1)
               (list '/ (list '+ (list '- 8 6) 4) (list '* 2 1))
               (list '/ (list '+ 8 (list '- 6 4)) (list '* 2 1))
               (list '/ (list '+ 8 6) (list '- (list '* 4 2) 1))
               (list '/ (list '+ 8 6) (list '- 4 (list '* 2 1)))
               (list '/ 8 (list '+ (list '- (list '* 6 4) 2) 1))
               (list '/ 8 (list '+ (list '- 6 (list '* 4 2)) 1))
               (list '/ 8 (list '+ (list '- 6 4) (list '* 2 1)))
               (list '/ 8 (list '+ 6 (list '- (list '* 4 2) 1)))
               (list '/ 8 (list '+ 6 (list '- 4 (list '* 2 1))))
               (list '/ (list '+ (list '- (list '* 1 1) 1) 1) 1)
               (list '/ (list '+ (list '- 1 (list '* 1 1)) 1) 1)
               (list '/ (list '+ (list '- 1 1) (list '* 1 1)) 1)
               (list '/ (list '+ 1 (list '- (list '* 1 1) 1)) 1)
               (list '/ (list '+ 1 (list '- 1 (list '* 1 1))) 1)
               (list '/ (list '+ (list '- 1 1) 1) (list '* 1 1))
               (list '/ (list '+ 1 (list '- 1 1)) (list '* 1 1))
               (list '/ (list '+ 1 1) (list '- (list '* 1 1) 1))
               (list '/ (list '+ 1 1) (list '- 1 (list '* 1 1)))
               (list '/ 1 (list '+ (list '- (list '* 1 1) 1) 1))
               (list '/ 1 (list '+ (list '- 1 (list '* 1 1)) 1))
               (list '/ 1 (list '+ (list '- 1 1) (list '* 1 1)))
               (list '/ 1 (list '+ 1 (list '- (list '* 1 1) 1)))
               (list '/ 1 (list '+ 1 (list '- 1 (list '* 1 1))))))

              
;;
;; ***********************************************
;;   Part v
;; ***********************************************
;;

;; Useful constants 
(define operator-lst (list (list '+ +)
                           (list '- -)
                           (list '* *)
                           (list '/ /)))

;; (evaluate-bets lobet target) produces a list of all BET from
;;   lobet that evaluate to the target value.
;; evaluate-bets: (listof BET) Nat -> (listof BET)
;; Examples:
(check-expect (evaluate-bets
               (create-bets
                (generate-permutations '(2 4 8))
                (generate-tuples '(+ - *) 2)) 2)
              (list
               (list '- 8 (list '+ 4 2))
               (list '- (list '- 8 4) 2)
               (list '- 8 (list '+ 2 4))
               (list '- (list '- 8 2) 4)))

(define (evaluate-bets lobet target)
  (cond
    [(empty? lobet) empty]
    [else
     (local
       [;; (evaluate bet) evaluates the given BET, bet, if B
        ;; evaluate: BET -> (anyof Nat false)
        (define (evaluate bet)
          (cond
            [(integer? bet) bet]
            [(or (false? (evaluate (second bet)))
                 (false? (evaluate (third bet)))) false]
            [(symbol=? (first bet) '/)
             (cond
               [(zero? (evaluate (third bet))) false]
               [(not (zero? (remainder (evaluate (second bet))
                                       (evaluate (third bet)))))
                false]
               [else ((find-op (first bet))
                      (evaluate (second bet)) (evaluate (third bet)))])]
            [(symbol=? (first bet) '-)
             (cond
               [(> (evaluate (second bet)) (evaluate (third bet)))
                ((find-op (first bet))
                 (evaluate (second bet)) (evaluate (third bet)))]
               [else false])]
            [else ((find-op (first bet))
                   (evaluate (second bet)) (evaluate (third bet)))]))

        ;; (find-op s) returns the operator that matches the given symbol, s
        ;; (find-op s): (anyof '+ '- '* '/) -> (anyof + - * /)
        (define (find-op s)
          (local
            [;; (find-op/helper s lst) returns the operator in the given list,
             ;;   lst, that matches the given symbol, s
             ;; find-op/helper: (anyof '+ '- '* '/)
             ;;  (listof (anyof (list '+ +) (list '- -) (list '* *)
             ;;  (list '/ /))) -> (anyof + - * /)
             (define (find-op/helper s lst)
               (cond
                 [(symbol=? s (first (first lst))) (second (first lst))]
                 [else (find-op/helper s (rest lst))]))]

            (find-op/helper s operator-lst)))]
       
       (cond
         [(false? (evaluate (first lobet)))
          (evaluate-bets (rest lobet) target)]
         [(= (evaluate (first lobet)) target)
          (cons (first lobet) (evaluate-bets (rest lobet) target))]
         [else (evaluate-bets (rest lobet) target)]))]))

;; Tests:
(check-expect (evaluate-bets
               (create-bets
                (generate-permutations '(2))
                (generate-tuples '(+ - *) 0)) 2)
              (list 2))
(check-expect (evaluate-bets
               (create-bets
                (generate-permutations '(2))
                (generate-tuples '(+ - *) 0)) 3)
              empty)
(check-expect (evaluate-bets
               (create-bets
                (generate-permutations '(2 1))
                (generate-tuples '(+ - *) 1)) 3)
              (list (list '+ 1 2) (list '+ 2 1)))
(check-expect (evaluate-bets
               (create-bets
                (generate-permutations '(2 1 1))
                (generate-tuples '(+ - *) 2)) 3)
              (list
               (list '+ (list '* 1 1) 2)
               (list '+ 1 (list '* 1 2))
               (list '* 1 (list '+ 1 2))
               (list '+ (list '* 1 2) 1)
               (list '+ 1 (list '* 2 1))
               (list '* (list '+ 1 2) 1)
               (list '* 1 (list '+ 2 1))
               (list '+ (list '* 1 1) 2)
               (list '+ 1 (list '* 1 2))
               (list '* 1 (list '+ 1 2))
               (list '+ (list '* 1 2) 1)
               (list '+ 1 (list '* 2 1))
               (list '* (list '+ 1 2) 1)
               (list '* 1 (list '+ 2 1))
               (list '+ (list '* 2 1) 1)
               (list '+ 2 (list '* 1 1))
               (list '* (list '+ 2 1) 1)
               (list '+ (list '* 2 1) 1)
               (list '+ 2 (list '* 1 1))
               (list '* (list '+ 2 1) 1)))
(check-expect
 (evaluate-bets (create-bets (generate-permutations '(1 5 7 10 25))
                             (generate-tuples '(+ - * /) 4)) 175)
 (list
  (list '* 25 (list '+ (list '- 10 7) (list '- 5 1)))
  (list '* 25 (list '- (list '+ (list '- 10 7) 5) 1))
  (list '* 25 (list '- 10 (list '+ (list '- 7 5) 1)))
  (list '* 25 (list '- (list '- 10 (list '- 7 5)) 1))
  (list '* 25 (list '- 10 (list '- 7 (list '- 5 1))))
  (list '* 25 (list '+ (list '- 10 (list '+ 7 1)) 5))
  (list '* 25 (list '+ (list '- (list '- 10 7) 1) 5))
  (list '* 25 (list '- 10 (list '- (list '+ 7 1) 5)))
  (list '* 25 (list '- (list '+ 10 5) (list '+ 7 1)))
  (list '* 25 (list '- (list '- (list '+ 10 5) 7) 1))
  (list '* 25 (list '- (list '+ 10 5) (list '+ 1 7)))
  (list '* 25 (list '- (list '+ 10 (list '- 5 1)) 7))
  (list '* 25 (list '- (list '- (list '+ 10 5) 1) 7))
  (list '* (list '* 25 (list '- (list '/ 10 5) 1)) 7)
  (list '* 25 (list '* (list '- (list '/ 10 5) 1) 7))
  (list '* 25 (list '/ (list '- (list '* 10 5) 1) 7))
  (list '* (list '/ 25 (list '- (list '/ 10 5) 1)) 7)
  (list '/ (list '* 25 (list '- (list '* 10 5) 1)) 7)
  (list '* 25 (list '+ (list '- 10 (list '+ 1 7)) 5))
  (list '* 25 (list '+ (list '- (list '- 10 1) 7) 5))
  (list '* 25 (list '- 10 (list '+ 1 (list '- 7 5))))
  (list '* 25 (list '- 10 (list '- (list '+ 1 7) 5)))
  (list '* 25 (list '- (list '- 10 1) (list '- 7 5)))
  (list '* 25 (list '- (list '+ (list '- 10 1) 5) 7))
  (list '- (list '* (list '- 25 7) 10) (list '* 5 1))
  (list '- (list '* (list '- 25 7) 10) (list '/ 5 1))
  (list '* (list '- (list '* (list '- 25 7) 10) 5) 1)
  (list '* (list '* 25 7) (list '- (list '/ 10 5) 1))
  (list '* 25 (list '* 7 (list '- (list '/ 10 5) 1)))
  (list '* 25 (list '/ 7 (list '- (list '/ 10 5) 1)))
  (list '/ (list '- (list '* (list '- 25 7) 10) 5) 1)
  (list '/ (list '* 25 7) (list '- (list '/ 10 5) 1))
  (list '- (list '* (list '- 25 7) (list '* 10 1)) 5)
  (list '- (list '* (list '- 25 7) 10) (list '* 1 5))
  (list '- (list '* (list '- 25 7) (list '/ 10 1)) 5)
  (list '- (list '* (list '* (list '- 25 7) 10) 1) 5)
  (list '- (list '/ (list '* (list '- 25 7) 10) 1) 5)
  (list '+ (list '* (list '- 25 (list '+ 7 1)) 10) 5)
  (list '+ (list '* (list '- (list '- 25 7) 1) 10) 5)
  (list '- (list '* (list '- 25 (list '* 7 1)) 10) 5)
  (list '- (list '* (list '- 25 7) (list '* 1 10)) 5)
  (list '- (list '* (list '- 25 (list '/ 7 1)) 10) 5)
  (list '- (list '* (list '* (list '- 25 7) 1) 10) 5)
  (list '- (list '* (list '/ (list '- 25 7) 1) 10) 5)
  (list '+ (list '* (list '+ (list '+ 25 7) 1) 5) 10)
  (list '+ (list '* (list '+ 25 (list '+ 7 1)) 5) 10)
  (list '* 25 (list '+ 5 (list '- 10 (list '+ 7 1))))
  (list '* 25 (list '+ 5 (list '- (list '- 10 7) 1)))
  (list '* 25 (list '- (list '+ 5 10) (list '+ 7 1)))
  (list '* 25 (list '- (list '+ 5 (list '- 10 7)) 1))
  (list '* 25 (list '- (list '- (list '+ 5 10) 7) 1))
  (list '* 25 (list '+ 5 (list '- 10 (list '+ 1 7))))
  (list '* 25 (list '+ 5 (list '- (list '- 10 1) 7)))
  (list '* 25 (list '- (list '+ 5 10) (list '+ 1 7)))
  (list '* 25 (list '- (list '+ 5 (list '- 10 1)) 7))
  (list '* 25 (list '- (list '- (list '+ 5 10) 1) 7))
  (list '* 25 (list '/ (list '- (list '* 5 10) 1) 7))
  (list '/ (list '* 25 (list '- (list '* 5 10) 1)) 7)
  (list '* 25 (list '+ (list '- 5 1) (list '- 10 7)))
  (list '* 25 (list '- (list '+ (list '- 5 1) 10) 7))
  (list '+ (list '* (list '- 25 (list '+ 1 7)) 10) 5)
  (list '+ (list '* (list '- (list '- 25 1) 7) 10) 5)
  (list '- (list '* (list '- (list '* 25 1) 7) 10) 5)
  (list '- (list '* (list '- 25 (list '* 1 7)) 10) 5)
  (list '- (list '* (list '- (list '/ 25 1) 7) 10) 5)
  (list '+ (list '* (list '+ (list '+ 25 1) 7) 5) 10)
  (list '+ (list '* (list '+ 25 (list '+ 1 7)) 5) 10)
  (list '- (list '* 10 (list '- 25 7)) (list '* 5 1))
  (list '- (list '* 10 (list '- 25 7)) (list '/ 5 1))
  (list '* (list '- (list '* 10 (list '- 25 7)) 5) 1)
  (list '/ (list '- (list '* 10 (list '- 25 7)) 5) 1)
  (list '+ 10 (list '* (list '+ (list '+ 25 7) 1) 5))
  (list '+ 10 (list '* (list '+ 25 (list '+ 7 1)) 5))
  (list '+ (list '* 10 (list '- 25 (list '+ 7 1))) 5)
  (list '+ (list '* 10 (list '- (list '- 25 7) 1)) 5)
  (list '- (list '* 10 (list '- 25 (list '* 7 1))) 5)
  (list '- (list '* 10 (list '- 25 7)) (list '* 1 5))
  (list '- (list '* 10 (list '- 25 (list '/ 7 1))) 5)
  (list '- (list '* (list '* 10 (list '- 25 7)) 1) 5)
  (list '- (list '* 10 (list '* (list '- 25 7) 1)) 5)
  (list '- (list '* 10 (list '/ (list '- 25 7) 1)) 5)
  (list '- (list '/ (list '* 10 (list '- 25 7)) 1) 5)
  (list '+ 10 (list '* (list '+ (list '+ 25 1) 7) 5))
  (list '+ 10 (list '* (list '+ 25 (list '+ 1 7)) 5))
  (list '+ (list '* 10 (list '- 25 (list '+ 1 7))) 5)
  (list '+ (list '* 10 (list '- (list '- 25 1) 7)) 5)
  (list '- (list '* 10 (list '- (list '* 25 1) 7)) 5)
  (list '- (list '* 10 (list '- 25 (list '* 1 7))) 5)
  (list '- (list '* 10 (list '- (list '/ 25 1) 7)) 5)
  (list '+ 10 (list '* (list '+ (list '+ 7 25) 1) 5))
  (list '+ 10 (list '* (list '+ 7 (list '+ 25 1)) 5))
  (list '* (list '+ (list '- 10 7) (list '- 5 1)) 25)
  (list '* (list '- (list '+ (list '- 10 7) 5) 1) 25)
  (list '* (list '- 10 (list '+ (list '- 7 5) 1)) 25)
  (list '* (list '- (list '- 10 (list '- 7 5)) 1) 25)
  (list '* (list '- 10 (list '- 7 (list '- 5 1))) 25)
  (list '+ 10 (list '* (list '+ (list '+ 7 1) 25) 5))
  (list '+ 10 (list '* (list '+ 7 (list '+ 1 25)) 5))
  (list '* (list '- (list '* 10 (list '- 7 1)) 25) 5)
  (list '* (list '+ (list '- 10 (list '+ 7 1)) 5) 25)
  (list '* (list '+ (list '- (list '- 10 7) 1) 5) 25)
  (list '* (list '- 10 (list '- (list '+ 7 1) 5)) 25)
  (list '+ 10 (list '* 5 (list '+ (list '+ 25 7) 1)))
  (list '+ 10 (list '* 5 (list '+ 25 (list '+ 7 1))))
  (list '* (list '- (list '* 10 5) 25) (list '* 7 1))
  (list '* (list '- (list '* 10 5) 25) (list '/ 7 1))
  (list '* (list '* (list '- (list '* 10 5) 25) 7) 1)
  (list '/ (list '* (list '- (list '* 10 5) 25) 7) 1)
  (list '+ 10 (list '* 5 (list '+ (list '+ 25 1) 7)))
  (list '+ 10 (list '* 5 (list '+ 25 (list '+ 1 7))))
  (list '* (list '- (list '* 10 5) (list '* 25 1)) 7)
  (list '* (list '- (list '* 10 5) 25) (list '* 1 7))
  (list '* (list '- (list '* 10 5) (list '/ 25 1)) 7)
  (list '* (list '* (list '- (list '* 10 5) 25) 1) 7)
  (list '* (list '/ (list '- (list '* 10 5) 25) 1) 7)
  (list '+ 10 (list '* 5 (list '+ (list '+ 7 25) 1)))
  (list '+ 10 (list '* 5 (list '+ 7 (list '+ 25 1))))
  (list '+ 10 (list '* 5 (list '+ (list '+ 7 1) 25)))
  (list '+ 10 (list '* 5 (list '+ 7 (list '+ 1 25))))
  (list '* (list '- (list '+ 10 5) (list '+ 7 1)) 25)
  (list '* (list '- (list '- (list '+ 10 5) 7) 1) 25)
  (list '+ 10 (list '* 5 (list '+ (list '+ 1 25) 7)))
  (list '+ 10 (list '* 5 (list '+ 1 (list '+ 25 7))))
  (list '* (list '- (list '* (list '* 10 5) 1) 25) 7)
  (list '* (list '- (list '* 10 (list '* 5 1)) 25) 7)
  (list '* (list '- (list '* 10 5) (list '* 1 25)) 7)
  (list '* (list '- (list '* 10 (list '/ 5 1)) 25) 7)
  (list '* (list '- (list '/ (list '* 10 5) 1) 25) 7)
  (list '* (list '- (list '/ 10 5) 1) (list '* 25 7))
  (list '* (list '* (list '- (list '/ 10 5) 1) 25) 7)
  (list '/ (list '* (list '- (list '* 10 5) 1) 25) 7)
  (list '+ 10 (list '* 5 (list '+ (list '+ 1 7) 25)))
  (list '+ 10 (list '* 5 (list '+ 1 (list '+ 7 25))))
  (list '* (list '- (list '+ 10 5) (list '+ 1 7)) 25)
  (list '* (list '- (list '+ 10 (list '- 5 1)) 7) 25)
  (list '* (list '- (list '- (list '+ 10 5) 1) 7) 25)
  (list '* (list '- (list '/ 10 5) 1) (list '* 7 25))
  (list '* (list '* (list '- (list '/ 10 5) 1) 7) 25)
  (list '* (list '/ (list '- (list '* 10 5) 1) 7) 25)
  (list '+ 10 (list '* (list '+ (list '+ 1 25) 7) 5))
  (list '+ 10 (list '* (list '+ 1 (list '+ 25 7)) 5))
  (list '- (list '* 10 (list '- (list '* 1 25) 7)) 5)
  (list '- (list '* (list '* 10 1) (list '- 25 7)) 5)
  (list '- (list '* 10 (list '* 1 (list '- 25 7))) 5)
  (list '- (list '* (list '/ 10 1) (list '- 25 7)) 5)
  (list '+ 10 (list '* (list '+ (list '+ 1 7) 25) 5))
  (list '+ 10 (list '* (list '+ 1 (list '+ 7 25)) 5))
  (list '* (list '+ (list '- 10 (list '+ 1 7)) 5) 25)
  (list '* (list '+ (list '- (list '- 10 1) 7) 5) 25)
  (list '* (list '- 10 (list '+ 1 (list '- 7 5))) 25)
  (list '* (list '- 10 (list '- (list '+ 1 7) 5)) 25)
  (list '* (list '- (list '- 10 1) (list '- 7 5)) 25)
  (list '* (list '- (list '* (list '* 10 1) 5) 25) 7)
  (list '* (list '- (list '* 10 (list '* 1 5)) 25) 7)
  (list '* (list '- (list '* (list '/ 10 1) 5) 25) 7)
  (list '* (list '- (list '+ (list '- 10 1) 5) 7) 25)
  (list '* (list '* 7 25) (list '- (list '/ 10 5) 1))
  (list '* 7 (list '* 25 (list '- (list '/ 10 5) 1)))
  (list '* 7 (list '/ 25 (list '- (list '/ 10 5) 1)))
  (list '/ (list '* 7 25) (list '- (list '/ 10 5) 1))
  (list '+ (list '* (list '+ (list '+ 7 25) 1) 5) 10)
  (list '+ (list '* (list '+ 7 (list '+ 25 1)) 5) 10)
  (list '* 7 (list '- (list '* 10 5) (list '* 25 1)))
  (list '* 7 (list '- (list '* 10 5) (list '/ 25 1)))
  (list '* (list '* 7 (list '- (list '* 10 5) 25)) 1)
  (list '* 7 (list '* (list '- (list '* 10 5) 25) 1))
  (list '* 7 (list '/ (list '- (list '* 10 5) 25) 1))
  (list '/ (list '* 7 (list '- (list '* 10 5) 25)) 1)
  (list '* 7 (list '- (list '* (list '* 10 5) 1) 25))
  (list '* 7 (list '- (list '* 10 (list '* 5 1)) 25))
  (list '* 7 (list '- (list '* 10 5) (list '* 1 25)))
  (list '* 7 (list '- (list '* 10 (list '/ 5 1)) 25))
  (list '* 7 (list '- (list '/ (list '* 10 5) 1) 25))
  (list '* (list '* 7 (list '- (list '/ 10 5) 1)) 25)
  (list '* 7 (list '* (list '- (list '/ 10 5) 1) 25))
  (list '* (list '/ 7 (list '- (list '/ 10 5) 1)) 25)
  (list '* 7 (list '- (list '* (list '* 10 1) 5) 25))
  (list '* 7 (list '- (list '* 10 (list '* 1 5)) 25))
  (list '* 7 (list '- (list '* (list '/ 10 1) 5) 25))
  (list '* 7 (list '- (list '* 5 10) (list '* 25 1)))
  (list '* 7 (list '- (list '* 5 10) (list '/ 25 1)))
  (list '* (list '* 7 (list '- (list '* 5 10) 25)) 1)
  (list '* 7 (list '* (list '- (list '* 5 10) 25) 1))
  (list '* 7 (list '/ (list '- (list '* 5 10) 25) 1))
  (list '/ (list '* 7 (list '- (list '* 5 10) 25)) 1)
  (list '* 7 (list '- (list '* (list '* 5 10) 1) 25))
  (list '* 7 (list '- (list '* 5 (list '* 10 1)) 25))
  (list '* 7 (list '- (list '* 5 10) (list '* 1 25)))
  (list '* 7 (list '- (list '* 5 (list '/ 10 1)) 25))
  (list '* 7 (list '- (list '/ (list '* 5 10) 1) 25))
  (list '* 7 (list '- (list '* (list '* 5 1) 10) 25))
  (list '* 7 (list '- (list '* 5 (list '* 1 10)) 25))
  (list '* 7 (list '- (list '* (list '/ 5 1) 10) 25))
  (list '+ (list '* (list '+ (list '+ 7 1) 25) 5) 10)
  (list '+ (list '* (list '+ 7 (list '+ 1 25)) 5) 10)
  (list '* (list '- (list '* (list '- 7 1) 10) 25) 5)
  (list '* 7 (list '- (list '* (list '* 1 10) 5) 25))
  (list '* 7 (list '- (list '* 1 (list '* 10 5)) 25))
  (list '* (list '* 7 1) (list '- (list '* 10 5) 25))
  (list '* 7 (list '* 1 (list '- (list '* 10 5) 25)))
  (list '* (list '/ 7 1) (list '- (list '* 10 5) 25))
  (list '* 7 (list '- (list '* (list '* 1 5) 10) 25))
  (list '* 7 (list '- (list '* 1 (list '* 5 10)) 25))
  (list '* (list '* 7 1) (list '- (list '* 5 10) 25))
  (list '* 7 (list '* 1 (list '- (list '* 5 10) 25)))
  (list '* (list '/ 7 1) (list '- (list '* 5 10) 25))
  (list '+ (list '* 5 (list '+ (list '+ 25 7) 1)) 10)
  (list '+ (list '* 5 (list '+ 25 (list '+ 7 1))) 10)
  (list '+ 5 (list '* (list '- 25 (list '+ 7 1)) 10))
  (list '+ 5 (list '* (list '- (list '- 25 7) 1) 10))
  (list '+ (list '* 5 (list '+ (list '+ 25 1) 7)) 10)
  (list '+ (list '* 5 (list '+ 25 (list '+ 1 7))) 10)
  (list '+ 5 (list '* (list '- 25 (list '+ 1 7)) 10))
  (list '+ 5 (list '* (list '- (list '- 25 1) 7) 10))
  (list '+ 5 (list '* 10 (list '- 25 (list '+ 7 1))))
  (list '+ 5 (list '* 10 (list '- (list '- 25 7) 1)))
  (list '* (list '- (list '* 5 10) 25) (list '* 7 1))
  (list '* (list '- (list '* 5 10) 25) (list '/ 7 1))
  (list '* (list '* (list '- (list '* 5 10) 25) 7) 1)
  (list '/ (list '* (list '- (list '* 5 10) 25) 7) 1)
  (list '+ 5 (list '* 10 (list '- 25 (list '+ 1 7))))
  (list '+ 5 (list '* 10 (list '- (list '- 25 1) 7)))
  (list '* (list '- (list '* 5 10) (list '* 25 1)) 7)
  (list '* (list '- (list '* 5 10) 25) (list '* 1 7))
  (list '* (list '- (list '* 5 10) (list '/ 25 1)) 7)
  (list '* (list '* (list '- (list '* 5 10) 25) 1) 7)
  (list '* (list '/ (list '- (list '* 5 10) 25) 1) 7)
  (list '* (list '+ 5 (list '- 10 (list '+ 7 1))) 25)
  (list '* (list '+ 5 (list '- (list '- 10 7) 1)) 25)
  (list '* (list '- (list '+ 5 10) (list '+ 7 1)) 25)
  (list '* (list '- (list '+ 5 (list '- 10 7)) 1) 25)
  (list '* (list '- (list '- (list '+ 5 10) 7) 1) 25)
  (list '* 5 (list '- (list '* 10 (list '- 7 1)) 25))
  (list '* (list '- (list '* (list '* 5 10) 1) 25) 7)
  (list '* (list '- (list '* 5 (list '* 10 1)) 25) 7)
  (list '* (list '- (list '* 5 10) (list '* 1 25)) 7)
  (list '* (list '- (list '* 5 (list '/ 10 1)) 25) 7)
  (list '* (list '- (list '/ (list '* 5 10) 1) 25) 7)
  (list '/ (list '* (list '- (list '* 5 10) 1) 25) 7)
  (list '* (list '+ 5 (list '- 10 (list '+ 1 7))) 25)
  (list '* (list '+ 5 (list '- (list '- 10 1) 7)) 25)
  (list '* (list '- (list '+ 5 10) (list '+ 1 7)) 25)
  (list '* (list '- (list '+ 5 (list '- 10 1)) 7) 25)
  (list '* (list '- (list '- (list '+ 5 10) 1) 7) 25)
  (list '* (list '/ (list '- (list '* 5 10) 1) 7) 25)
  (list '+ (list '* 5 (list '+ (list '+ 7 25) 1)) 10)
  (list '+ (list '* 5 (list '+ 7 (list '+ 25 1))) 10)
  (list '+ (list '* 5 (list '+ (list '+ 7 1) 25)) 10)
  (list '+ (list '* 5 (list '+ 7 (list '+ 1 25))) 10)
  (list '* 5 (list '- (list '* (list '- 7 1) 10) 25))
  (list '+ (list '* 5 (list '+ (list '+ 1 25) 7)) 10)
  (list '+ (list '* 5 (list '+ 1 (list '+ 25 7))) 10)
  (list '* (list '- (list '* (list '* 5 1) 10) 25) 7)
  (list '* (list '- (list '* 5 (list '* 1 10)) 25) 7)
  (list '* (list '- (list '* (list '/ 5 1) 10) 25) 7)
  (list '* (list '+ (list '- 5 1) (list '- 10 7)) 25)
  (list '* (list '- (list '+ (list '- 5 1) 10) 7) 25)
  (list '+ (list '* 5 (list '+ (list '+ 1 7) 25)) 10)
  (list '+ (list '* 5 (list '+ 1 (list '+ 7 25))) 10)
  (list '- (list '* (list '- (list '* 1 25) 7) 10) 5)
  (list '- (list '* (list '* 1 (list '- 25 7)) 10) 5)
  (list '- (list '* 1 (list '* (list '- 25 7) 10)) 5)
  (list '* 1 (list '- (list '* (list '- 25 7) 10) 5))
  (list '+ (list '* (list '+ (list '+ 1 25) 7) 5) 10)
  (list '+ (list '* (list '+ 1 (list '+ 25 7)) 5) 10)
  (list '- (list '* (list '* 1 10) (list '- 25 7)) 5)
  (list '- (list '* 1 (list '* 10 (list '- 25 7))) 5)
  (list '* 1 (list '- (list '* 10 (list '- 25 7)) 5))
  (list '* (list '- (list '* (list '* 1 10) 5) 25) 7)
  (list '* (list '- (list '* 1 (list '* 10 5)) 25) 7)
  (list '* (list '* 1 (list '- (list '* 10 5) 25)) 7)
  (list '* 1 (list '* (list '- (list '* 10 5) 25) 7))
  (list '+ (list '* (list '+ (list '+ 1 7) 25) 5) 10)
  (list '+ (list '* (list '+ 1 (list '+ 7 25)) 5) 10)
  (list '* (list '* 1 7) (list '- (list '* 10 5) 25))
  (list '* 1 (list '* 7 (list '- (list '* 10 5) 25)))
  (list '* (list '* 1 7) (list '- (list '* 5 10) 25))
  (list '* 1 (list '* 7 (list '- (list '* 5 10) 25)))
  (list '* (list '- (list '* (list '* 1 5) 10) 25) 7)
  (list '* (list '- (list '* 1 (list '* 5 10)) 25) 7)
  (list '* (list '* 1 (list '- (list '* 5 10) 25)) 7)
  (list '* 1 (list '* (list '- (list '* 5 10) 25) 7))))


;;
;; ***********************************************
;;   Part vi
;; ***********************************************
;;

;; (countdown-numbers lon target) produces a BET using the numbers in lon
;; that evaluates to target, or false if no such BET exists.
;; countdown-numbers: (listof Nat) Nat -> (anyof BET false)
;; Examples
(check-expect (countdown-numbers '(2 4 8) 2) (list '- 8 (list '+ 4 2)))

(define (countdown-numbers lon target)
  (cond
    [(empty? lon) false]
    [else
     (local
       [(define loop '(+ - * /))
        (define my-countdown-numbers
          (evaluate-bets
           (create-bets (generate-permutations lon)
                        (generate-tuples loop (- (length lon) 1))) target))]
           
       (cond [(empty? my-countdown-numbers) false]
             [else (first my-countdown-numbers)]))]))

;; Tests:
(check-expect (countdown-numbers '(2 1) 100) false)
(check-expect (countdown-numbers '(2) 2) 2)
(check-expect (countdown-numbers '() 2) false)
(check-expect (countdown-numbers '(1 2) 3) (list '+ 2 1))
(check-expect (countdown-numbers '(1 2 3) 6)
              (list '+ (list '+ 3 2) 1))
(check-expect (countdown-numbers '(1 2 3 5) 6)
              (list '+ 5 (list '- 3 (list '* 2 1))))
(check-expect (countdown-numbers '(1 1 2 3 5) 6)
              (list '+ (list '+ (list '+ (list '- 5 3) 2) 1) 1))
(check-expect (countdown-numbers '(1 1 2 3 5) 6000) false)
(check-expect (countdown-numbers '(1 5 7 10 25) 175)
              (list '* 25 (list '+ (list '- 10 7) (list '- 5 1))))