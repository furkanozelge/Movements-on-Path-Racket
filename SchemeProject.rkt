#lang scheme

(require test-engine/racket-tests)


(define (buildPath rows)
  (cond
    [(null? rows) null ]
    [else (cons (buildPath (cdr rows))
                (car rows))]))
(define (equal list1 list2)
 (cond
 ((not (list? list1)) (eq? list1 list2))
 ((not (list? list2)) #F)
 ((empty? list1) (empty? list2))
 ((empty? list2) #F)
 ((equal (car list1) (car list2))
 (equal (cdr list1) (cdr list2)))
 (else #F)
))

(define sample-path (buildPath
                     '(("S" "-" "-" "-" "-")
                       ("E" "-" "-" "-" "-")
                       ("E" "E" "E" "E" "E")
                       ("-" "-" "-" "-" "E")
                       ("-" "-" "-" "-" "F")))) 

(define (getWidth mypath)
  (lenOflist (cdr mypath))
  )

(define (getHeight mypath)
  (if (eq? '() mypath) 0
      (+ 1 (getHeight (car mypath)))
  ))

(define (third a_list)
 ((compose car (compose cdr cdr)) a_list))


(define (lenOflist list)
  (if (empty? list) 0
      (+ 1 (lenOflist (cdr list)))
  ))

(define (map fun a_list)
 (cond
 ((empty? a_list) '())
 (else (cons (fun (car a_list)) (map fun (cdr a_list))))
))

(define (compose f g) (lambda (x)(f (g x))))

(define (getLetter linked dirRow dirCol)

  (define (letter linked)
  (getCol (getRow linked
                  dirRow) dirCol))

  (if
   (or (negative? dirRow)
          (>= dirRow
              (getHeight linked))
          (negative? dirCol)
          (>= dirCol
              (getWidth linked)))
      "-"
      (letter linked )))
 (define (append list1 list2)
 (cond
 ((empty? list1) list2)
 (else (cons (car list1) (append (cdr list1) list2)))
))

(define (getRow mypath dirRow)
  (if (> dirRow 0)
      (getRow (car mypath)
              (- dirRow 1))
       (cdr mypath))
  )

(define (getCol mypath dirCol)
  (if (> dirCol 0)
      (getCol (cdr mypath)
              (- dirCol 1))
      (car mypath))
  )


(define (adder a_list)
 (cond
 ((empty? a_list) 0)
 (else (+ (car a_list)
          (adder (cdr a_list))))
))


(define (foot mypath dirRow dirColumn output a1for)
  (define
    (way rowp colq direction)
    (and (not
          (equal direction a1for))
         (or (string=?
              (getLetter mypath rowp colq) "F")
             (string=?
              (getLetter mypath rowp colq) "E"))))
  (cond
    [(string=?
      (getLetter mypath dirRow dirColumn) "F")
     output ]

    [(way (- dirRow 1) dirColumn 'U)
     (foot mypath (- dirRow 1)
           dirColumn
           (append output '(U)) 'D) ]

    [(way (+ dirRow 1) dirColumn 'D)
     (foot mypath (+ dirRow 1)
           dirColumn
           (append output '(D))
           'U) ]
    [(way dirRow (+ dirColumn 1) 'R)
     (foot mypath dirRow
           (+ dirColumn 1)
           (append output '(R))
           'L) ]

    [(way dirRow (- dirColumn 1) 'L)
     (foot mypath dirRow
           (- dirColumn 1)
           (append output '(L)) 'R) ]

    ))
(define (member atm a_list)
 (cond
 ((empty? a_list) #F)
 ((eq? atm (car a_list)) #T)
 (else
  (member atm
               (cdr a_list)))
))

(define (solvePath mypath)
  (foot mypath 0 0 (list) '-))



(check-expect (getHeight sample-path) '5)
(check-expect (getWidth sample-path) '5)
(check-expect (getLetter sample-path 0 0) '"S")
(check-expect (getLetter sample-path 1 0)  '"E")
(check-expect (getLetter sample-path 2 2) '"E")
(check-expect (getLetter sample-path 4 3) '"-")
(check-expect (getLetter sample-path 5 0) '"-")
(check-expect (getLetter sample-path 2 0) '"E")
(check-expect (getLetter sample-path 1 2) '"-")
(check-expect (getLetter sample-path 1 1) '"-")
(check-expect (getLetter sample-path 1 2) '"-")
(check-expect (getLetter sample-path 1 5) '"-")
(check-expect (getLetter sample-path 3 5) '"-")
(check-expect (getLetter sample-path 2 5) '"-")
(check-expect (getLetter sample-path 5 3) '"-")
(check-expect (getLetter sample-path 3 2) '"-")
(check-expect (getLetter sample-path 1 4) '"-")
(check-expect (getLetter sample-path 1 5) '"-")
(check-expect (getLetter sample-path 4 1) '"-")
(check-expect (getLetter sample-path 3 3) '"-")
(check-expect (getLetter sample-path 5 5) '"-")
(check-expect (getLetter sample-path 4 5) '"-")
(check-expect (getLetter sample-path 5 1) '"-")
(check-expect (getLetter sample-path 4 4) '"F")
(check-expect (solvePath sample-path) '(D D R R R R D D))

(test)