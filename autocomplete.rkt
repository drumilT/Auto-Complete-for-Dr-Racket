#lang racket/base

(require racket/class racket/match)
(provide autocompletion-cursor<%> autocompletion-cursor%)

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
        (set! main-trie (add-word-helper (cons #\space (string->list (string-downcase word))) num main-trie)))


    
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
      (map car (sort (get-all-completions-helper (cons #\space (string->list prefix))
                                  (string->list prefix)
                                  main-trie) #:key cdr >)))
    


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
      (set! main-trie (dwf-helper (cons #\space (string->list word))
                                  freq
                                  main-trie)))
    
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


(define autocompletion-cursor<%>
  (interface ()
    get-completions  ;      -> (listof string) 
    get-length       ;      -> int
    empty?           ;      -> boolean
    narrow           ; char -> autocompletion-cursor<%>
    widen))          ; char -> autocompletion-cursor<%>
    

(define editor-trie (new prefix-trie%
                         [initial-word-list '()]))

;; ============================================================
;; autocompletion-cursor<%> implementation

(define autocompletion-cursor%
  (class* object% (autocompletion-cursor<%>)
    
    (init-field word all-words)        
    
    (define-values (rnk max-count) (rank word))
    ;; this determines the fuzziness
    ;; if we set mx to +inf.0, we get just the prefix matches
    ;; if we set mx to 1000, we get just the matches somewhere in the word
    ;; this definition is fuzzier the more parts there are in the word
    (define mx (cond
                 [(<= max-count 2) max-count]
                 [(<= max-count 4) (- max-count 1)]
                 [else (- max-count 2)]))
    
    ;; all the possible completions for `word', in ranked order
    (define all-completions 
      (map car (sort
                ;; we don't use `rnk' as the key to avoid
                ;; constructing a huge list
                (for*/list ([w (in-list all-words)]
                            [r (in-value (rnk w))]
                            #:when (>= r mx))
                  (cons w r))
                (match-lambda** [((cons w r) (cons w* r*))
                                 (or (> r r*)
                                     ;; prefer shorter matches
                                     (< (string-length w) (string-length w*)))]))))
    
    (define all-completions-length (length all-completions))
    
    (define/public (narrow c)
      (new autocompletion-cursor%
           [word (string-append word (list->string (list c)))]
           [all-words all-words]))
    
    (define/public (widen)
      (let ([strlen (string-length word)])
        (cond
          [(< strlen 2) #f]
          [else 
           (new autocompletion-cursor%
                [word (substring word 0 (- (string-length word) 1))]
                [all-words all-words])])))
    
    (define/public (get-completions) all-completions)
    (define/public (get-length) all-completions-length)
    (define/public (empty?) (eq? (get-length) 0))
    
    (super-new)))
