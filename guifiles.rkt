#lang racket
(require racket/gui)
(define start 0)
(define f (new frame% [label "My Racket"]
                      [width 400]
                      [height 400]))
(define c (new editor-canvas% [parent f]))
(define mytext%
  (class text%
  (inherit/super find-wordbreak)
      (inherit/super get-text)
     (inherit/super get-end-position)
    (inherit/super get-start-position)
    (super-new)
    (define/override (on-char key-event)
      (let ((code (send key-event get-key-code)))
        (super on-char key-event)
        (cond [(equal? code #\space) (begin (displayln (super get-text start (get-end-position))) (set! start (get-end-position)))]
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
