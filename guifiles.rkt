#lang racket
(require racket/gui)
(require "prefix-tree.rkt")
(define mytree (new prefix-tree% [initial-word-list '()])) 
(define f (new frame% [label "Autcomplete with racket"]
                      [width 400]
                      [height 400]))
(define c (new editor-canvas% [parent f]))
(define mytext%
  (class text%
  (inherit/super find-wordbreak)
   (inherit/super get-text)
    (define start 0)
     (inherit/super get-end-position)
    (inherit/super get-start-position)
    (super-new)
    (define/override (on-char key-event)
      (let ((code (send key-event get-key-code)))
        (cond [(equal? code #\space) (begin (send mytree add-word (super get-text start (get-end-position)) 1) (set! start (+ (get-end-position) 1)) (super on-char key-event))]
              [(equal? code #\backspace) (begin (send mytree delete  (super get-text start (get-end-position)))
              [else (super on-char key-event)]
              )))))
(define t (new mytext%))
(send c set-editor t)
(send f show #t)
(define mb (new menu-bar% [parent f]))
(define m-edit (new menu% [label "Edit"] [parent mb]))
(define m-font (new menu% [label "Font"] [parent mb]))
(append-editor-operation-menu-items m-edit #f)
(append-editor-font-menu-items m-font)
(send t set-max-undo-history 100)
