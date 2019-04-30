#lang racket
(define/public (get-all-completions-2-distance prefix dist)
  (define (get-all-completions-2-helper prefix build-prefix trie dist)
    ( if (dist > 0)
         (match prefix
      ['()
       (match trie
         [(gnode _ _ _)
          (map (lambda (z) (cons (list->string (append (reverse build-prefix) (car z))) (cdr z)))
               (all-suffixes trie))]
         [_ (begin (displayln trie)
                   trie)])]
      [(cons x rest)
       (match trie
         [(gnode alp fre chl)
          #:when (equal? x alp)
          (cond [(equal? x #\space)
                 (append-map (lambda (z) (get-all-completions-2-helper rest build-prefix z dist))
                             chl)]
                [(and (equal? rest '()) (equal? chl '()) (> fre 0))
                 (list (cons (list->string (reverse (cons x build-prefix))) fre))]
                [else
                 (append-map (lambda (z) (get-all-completions-2-helper rest (cons x build-prefix) z dist))
                             chl)])
          ]
         [(gnode alp _ chl)
          (append-map (lambda (z) (get-all-completions-2-helper prefix (cons alp build-prefix) z (- dist 1)))
                      chl)]
         [_ '()])]
      [_ '()]) '())
  (map car (sort (get-all-completions-2-helper (cons #\space (string->list prefix))
                                             '()
                                             main-trie dist) #:key cdr >)));;
