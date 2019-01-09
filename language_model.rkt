#lang racket

#| Lab 3 - Sabirah Shuaybi |#

;NOTE - I have separated all functions (on the top) and the function calls (on bottom) for better readability

;-----------------------------------------------------------------------------------
; Helper functions for Lab 3
;-----------------------------------------------------------------------------------

; Function to split a string into a list of substrings on a given separator string

(define (split str c)
  (letrec ((chars (map (lambda (x) (string x)) (string->list str)))) ; get a list of characters and convert to strings
  (reverse (foldl (lambda (x res)                                    ; use foldl to build up a result list, then reverse
         (if (equal? c x)                                            ; check if current string is equal to separator
             (cons "" (cons (first res) (rest res)))                 ; if so, break and start a new string
             (cons (string-append (first res) x) (rest res))))       ; otherwise, append current string to result
       (list "")
       chars))))

;-----------------------------------------------------------------------------------

;Function that creates a hash table of bigrams and their probabilities

(define (store-probabilities unqs probs)
  (make-immutable-hash (zip unqs probs)))      ; zip together bigrams and their probabilities and create hash table

;Example usage:
;(define look-up (store-probabilities unique-bigrams bigram-probs))

;-----------------------------------------------------------------------------------

;Function for generating the most likely sentence given a starting word

(define (generate-sentence prev lst table max)
  (letrec ((helper (lambda (prev sent)
                    (let ((next (get-likely-next-word prev lst table)))     ; get most likely next word
                      (if (or (= (first next) 0)                                 ; there is no likely next word
                              (>= (length sent) max))                            ; maximum length
                          (foldr (lambda (x y) (string-append x " " y)) "" sent) ; join the words together and return
                          (helper (list-ref (first (rest next)) 1)               ; recursive call
                                  (append sent (list (list-ref (first (rest next)) 1)))))))))
    (helper prev (list prev))))

;Example usage:
;(generate-sentence "hobbit" unique-bigrams look-up 20)

;-----------------------------------------------------------------------------------
; Functions to be defined in Part 2:
;-----------------------------------------------------------------------------------

#| Goes through the str input and replaces any occurences of before with after |#
(define (replace str before after) ;str is the whole text
  (letrec ((helper (lambda (s b a n res)
                     (cond ((= (string-length s) n) res) ;check whether we are at the end of str, then return acc result
                           ((equal? (string (string-ref s n)) b) ;check is current section of string is equal to b
                            (helper s b a (+ n 1)(string-append res a)))
                            (else (helper s b a (+ n 1) (string-append res (string (string-ref s n)))))))))
          (helper str before after 0 "")))


#| Replaces newlines, commas and colons with empty string |#
(define (strip-punctuation str)
  (let ((str1 (replace str "\n" "")))
  (let ((str2 (replace str1 "," "")))
  (replace str2 ":" ""))))

#| Inserts an end-of-sentence boundry tag <s> after end of sentences |#
(define (add-tags str)
  (let ((str-1 (replace str "." ". <s>")))
  (let ((str-2 (replace str-1 "!" "! <s>")))
  (replace str-2 "?" "? <s>"))))

#| Takes a str and splits it into a list of individual words |#
(define (get-words str)
  (split str " "))

;-----------------------------------------------------------------------------------
; Functions to be defined in Part 3:
;-----------------------------------------------------------------------------------

#| Takes two lists and returns a list of lists where each nested list at index i contains
 the ith item of list 1 and the ith item of list 2 |#
(define (zip l1 l2)
  (cond ((or (empty? l1) (empty? l2)) '()) ;if either list is empty, return empty list
        (else (cons (list (first l1) (first l2)) ;otherwise, cons a list with the first elements of both lists
                    (zip (rest l1) (rest l2)))))) 

#| Takes a list of words and returns a list of bigrams |#
(define (make-bigrams lst) 
  (zip (cons "<s>" lst) lst))

;-----------------------------------------------------------------------------------
; Functions to be defined in Part 4:
;-----------------------------------------------------------------------------------

#| Calls remove-duplicates to get a list of unique bigrams |#
(define (get-unique-bigrams lst)
  (remove-duplicates lst))

#| Returns a list containing the first word of each bigram |#
(define (get-matching-unigrams lst)
  (map (lambda (x) (first x)) lst))

#| Returns a n-gram count for a list of unique n-grams |#
(define (get-ngram-counts unqs all)
  (map (lambda (word)
         (let((count-lst(get-a-count word all)))
           (length count-lst))) ; the length of this filtered list is the count
       unqs))

#| Helper Function for get-ngram-counts that returns a list
where each element is equal to the word argument passed in |#
(define (get-a-count word all)
  (filter (lambda (x) (equal? word x)) all))

#| Calculates bigram probabilities using the maximum likelihood estimate formula |#
(define (calc-bigram-prob bicount unicount)
  (map (lambda (x y) (/ x y)) bicount unicount))

;-----------------------------------------------------------------------------------
; Functions to be defined in Part 5:
;-----------------------------------------------------------------------------------

#| Takes a word and a list of unique bigrams and
returns a list of all bigrams that begin with that word |#
(define (get-relevant-bigrams word lst)
  (filter (lambda (x) (equal? word (first x))) lst))

#| Returns the most likely next word and its probability, given a preceding word |#
(define (get-likely-next-word word unqs table)
  (let ((rel-bigrams (get-relevant-bigrams word unqs)))
    (foldl (lambda (x y)
             (if (> (first (hash-ref table x)) (first y)) ;comparing probabilities
                    (list (first (hash-ref table x)) x)
                    y)) 
             (list 0 "") ;initial value is a list of 2 elements
             rel-bigrams))) ;iterating through the relevant bigrams for the word arg

;-------------------------------------------------------------------------------
#| HOBBIT LANGUAGE MODELING CODE |#

; Reading in the hobbit data
(define input (open-input-file "hobbit.txt"))
(define hobbit (read-string 10000 input))

; Pre-processing the data
(define hobbit-2 (strip-punctuation hobbit))
(define hobbit-w-tags (add-tags hobbit-2))
(define hobbit-word-lst (get-words hobbit-w-tags))

; Creating bigrams for the Hobbit data
(define hobbit-bigram-lst (make-bigrams hobbit-word-lst))
(define hobbit-unique-bigrams (get-unique-bigrams hobbit-bigram-lst))
(define hobbit-matching-unigrams (get-matching-unigrams hobbit-unique-bigrams))

; Calculating bigram probabilities 
(define hobbit-unicount(get-ngram-counts hobbit-matching-unigrams hobbit-word-lst))
(define hobbit-bicount(get-ngram-counts hobbit-unique-bigrams hobbit-bigram-lst))
(define hobbit-bigram-probs (calc-bigram-prob hobbit-bicount hobbit-unicount))

; Generating likely sentences 
(define hobbit-bigram-table (store-probabilities hobbit-unique-bigrams hobbit-bigram-probs))
hobbit-bigram-table

(hash-ref hobbit-bigram-table '("hobbit" "was"))
(get-relevant-bigrams "hobbit" hobbit-unique-bigrams)
(get-likely-next-word "tunnel" hobbit-unique-bigrams hobbit-bigram-table)

(generate-sentence "hobbit" hobbit-unique-bigrams hobbit-bigram-table 20)

#| The Most Likely Sentence Generated from the Hobbit data when the first word is "Hobbit" is:
"hobbit was a very comfortable tunnel a very comfortable tunnel a very comfortable tunnel a very comfortable tunnel a very " |#


;-------------------------------------------------------------------------------

#| ALICE IN WONDERLAND LANGUAGE MODELING CODE |#

; Reading in the Alice data
(define input2 (open-input-file "alice.txt"))
(define alice (read-string 10000 input2))

; Pre-processing the data
(define alice-2 (strip-punctuation alice))
(define alice-w-tags (add-tags alice-2))
(define alice-word-lst (get-words alice-w-tags))

; Creating bigrams for the Alice data
(define alice-bigram-lst (make-bigrams alice-word-lst))
(define alice-unique-bigrams (get-unique-bigrams alice-bigram-lst))
alice-unique-bigrams

(define alice-matching-unigrams (get-matching-unigrams alice-unique-bigrams))
alice-matching-unigrams

; Calculating bigram probabilities 
(define alice-unicount(get-ngram-counts alice-matching-unigrams alice-word-lst))
(define alice-bicount(get-ngram-counts alice-unique-bigrams alice-bigram-lst))

(define alice-bigram-probs (calc-bigram-prob alice-bicount alice-unicount))
alice-bigram-probs

; Generating likely sentences 
(define alice-bigram-table (store-probabilities alice-unique-bigrams alice-bigram-probs))
alice-bigram-table

(generate-sentence "<s>" alice-unique-bigrams alice-bigram-table 15)

#| The Most Likely Sentence Generated from the Alice in Wonderland data when the first word is "<s" is:
"<s> I shall be worth the little door she was not a little door she " |#
