#lang racket
(provide prefix-tree%
	     gnode)

(struct gnode (alphabet frequency childlist) #:transparent)

(define prefix-tree%
  (class object%

    (super-new)

    ;; a field to get the initial words to set the tree
    (init-field initial-word-list)

    ;; private variable to define the trie
    (define main-trie (gnode #\space 0 '()))

    ;; temp for using map without displaying
    (define te 1)

    ;; add all the initial words to the trie
    (set! te (map (lambda (x) (add-word x 1)) initial-word-list))

    ;; public function to add a word
    (define/public (add-word word num)
      (define (add-word-helper word-to-list num trie)
        (match word-to-list
          [(cons x '())
           #:when (equal? x (gnode-alphabet trie))
           (gnode (gnode-alphabet trie)
                  (+ (gnode-frequency trie) num)
                  (gnode-childlist trie))]
          [(cons x rest)
           #:when (equal? x (gnode-alphabet trie))
           (let ([insert-point (ormap (lambda (x) (equal? (car rest) (gnode-alphabet x)))
                                      (gnode-childlist trie))])
             (if (equal? #f insert-point)
                 (gnode (gnode-alphabet trie)
                        (gnode-frequency trie)
                        (cons (add-word-helper rest num
                                               (gnode (car rest) 0 '()))
                              (gnode-childlist trie)))
                 (gnode (gnode-alphabet trie)
                        (gnode-frequency trie)
                        (map (lambda (x) (add-word-helper rest num x))
                             (gnode-childlist trie)))))]
          [_ trie]))
        (when word (set! main-trie (add-word-helper (cons #\space (string->list (string-downcase word))) num main-trie))))


    
    ;; private function that gets all suffixes from this node

    (define/private (all-suffixes trie)
      (match trie
        [(gnode alp freq chl)
         #:when (> freq 0)
         (cons (cons (list alp) freq)
               (map (lambda (y) (cons (cons alp (car y)) (cdr y)))
                               (append-map (lambda (z) (all-suffixes z)) chl)))]
        [(gnode alp freq chl)
         (map (lambda (y) (cons (cons alp (car y)) (cdr y)))
                               (append-map (lambda (z) (all-suffixes z)) chl))]
        [_ (error "Suffix not a gnode")]))

    

    ;; public function to display the trie
    (define/public (show)
       (map (lambda (z) (cons (list->string (cdr (car z))) (cdr z))) (all-suffixes main-trie)))

    (define/public (show-trie)
      (displayln main-trie))


    ;; public function that finds all completions for a given prefix
    (define/public (get-all-completions prefix)
      (define (get-all-completions-helper prefix build-prefix trie)
        (match prefix
          ['()
           (map (lambda (z) (cons (list->string (append build-prefix (car z))) (cdr z)))
                (all-suffixes trie))]
          [(cons x rest)
           #:when (equal? x (gnode-alphabet trie))
           (append-map (lambda (z) (get-all-completions-helper rest
                                                        build-prefix
                                                        z))
                       (gnode-childlist trie))]
          [_ '()]))
      (if (not prefix) '() (map car (sort (get-all-completions-helper (cons #\space (string->list prefix))
                                  (string->list prefix)
                                  main-trie) #:key cdr >))))
    


    ;; public function to delete the frequency of a word from the trie

    (define/public (delete-word-freq word freq)
      (define (dwf-helper word freq trie)
        (match word
          [(cons x '())
           (match trie
             [(gnode alp fr chl)
              #:when (equal? alp x)
              (gnode alp (max 0 (- fr freq)) chl)]
             [_ trie])]
          [(cons x rest)
           (match trie
             [(gnode alp fr chl)
              #:when (equal? alp x)
              (gnode alp fr (map (lambda (x) (dwf-helper rest freq x)) chl))]
             [_ trie])]))
      (when word (set! main-trie (dwf-helper (cons #\space (string->list word))
                                  freq
                                  main-trie))))
    
    (define/public (prune trie)
      (match trie
        [(gnode alp fr '())
         #:when (equal? fr 0)
         '()]
        [(gnode alp fr chl)
         #:when (equal? fr 0)
         (let* ([res (map (lambda (x) (prune x)) chl)])
           (cond [(equal? '(()) (remove-duplicates res))
                  '()]
                 [else (gnode alp fr (remove* (list '()) res))]))]
        [_ trie]))

    (define/public (delete-word word freq)
      (begin (delete-word-freq word freq)
             (set! main-trie (prune main-trie))))

    ))

(define (read-hist-word-list file-path #:pick? [choice 'word])
  (call-with-input-file file-path
    (lambda (fin)
      (for/list ([word-count (in-lines fin)])
        (let ([wc-split (string-split word-count #:trim? #t)])
          (match choice
            ['word   (car wc-split)]
            ['counts (string->number (cadr wc-split))]
            ['both   (cons (car wc-split)
                           (string->number (cadr wc-split)))]))))))

; (define dictionary
;   (take (read-hist-word-list "../google-books-common-words.txt") 10))

; ;(define dictionary
; ;  (list "apple" "ant" "aloha" "always" "almight"))

; (define my-trie (new prefix-tree%
;                      [initial-word-list dictionary]))
