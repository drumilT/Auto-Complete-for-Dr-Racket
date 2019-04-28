#lang racket
(require racket/string)
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
<<<<<<< HEAD
    (inherit/super get-start-position get-character)
=======
    (inherit/super get-start-position)
    (inherit/super get-character)
>>>>>>> 526c48fdd53c6694240e753efb6ea44b2c365867
    (super-new)
    
    (define/public (get-word-around-cursor)
      (let ([start-pos (map (lambda (x) (box (get-end-position))) (list 1 2 3 4 5 6 7 8 9))]
            [end-pos (map (lambda (x) (box (get-end-position))) (list 1 2 3 4 5 6 7 8 9))])
        (begin (displayln "@@@@@@@@@@@@@@@@@@@@@@@@@@@")
;               (super find-wordbreak (list-ref start-pos 0) #f 'caret)
;               (displayln "find-wordbreak start-pos #f 'caret")
;               (display ">>")
;               (display (super get-text (unbox (list-ref start-pos 0)) (unbox (list-ref end-pos 0))))
;               (displayln "<<")
;               (super find-wordbreak #f (list-ref end-pos 1) 'caret)
;               (displayln "find-wordbreak #f end-pos 'caret")
;               (display ">>")
;               (display (super get-text (unbox (list-ref start-pos 1)) (unbox (list-ref end-pos 1))))
;               (displayln "<<")
               (super find-wordbreak (list-ref start-pos 2) (list-ref end-pos 2) 'caret)
               (displayln "find-wordbreak start-pos end-pos 'caret")
               (display ">>")
               (display (super get-text (unbox (list-ref start-pos 2)) (unbox (list-ref end-pos 2))))
               (displayln "<<")
;               (super find-wordbreak (list-ref start-pos 3) #f 'selection)
;               (displayln "find-wordbreak start-pos #f 'selection")
;               (display ">>")
;               (display (super get-text (unbox (list-ref start-pos 3)) (unbox (list-ref end-pos 3))))
;               (displayln "<<")
;               (super find-wordbreak #f (list-ref end-pos 4) 'selection)
;               (displayln "find-wordbreak #f end-pos 'selection")
;               (display ">>")
;               (display (super get-text (unbox (list-ref start-pos 4)) (unbox (list-ref end-pos 4))))
;               (displayln "<<")
               (super find-wordbreak (list-ref start-pos 5) (list-ref end-pos 5) 'selection)
               (displayln "find-wordbreak start-pos end-pos 'selection")
               (display ">>")
               (display (super get-text (unbox (list-ref start-pos 5)) (unbox (list-ref end-pos 5))))
               (displayln "<<")
               (displayln (equal? "\n" (super get-text (unbox (list-ref start-pos 5)) (unbox (list-ref end-pos 5)))))
;               (super find-wordbreak (list-ref start-pos 6) #f 'line)
;               (displayln "find-wordbreak start-pos #f 'line")
;               (display ">>")
;               (display (super get-text (unbox (list-ref start-pos 6)) (unbox (list-ref end-pos 6))))
;               (displayln "<<")
;               (super find-wordbreak #f (list-ref end-pos 7) 'line)
;               (displayln "find-wordbreak #f end-pos 'line")
;               (display ">>")
;               (display (super get-text (unbox (list-ref start-pos 7)) (unbox (list-ref end-pos 7))))
;               (displayln "<<")
               (super find-wordbreak (list-ref start-pos 8) (list-ref end-pos 8) 'line)
               (displayln "find-wordbreak start-pos end-pos 'line")
               (display ">>")
               (display (super get-text (unbox (list-ref start-pos 8)) (unbox (list-ref end-pos 8))))
               (displayln "<<")
               (displayln (super get-character (- (get-end-position) 1)))
               (displayln (super get-character (get-end-position)))
               (displayln "@@@@@@@@@@@@@@@@@@@@@@@@@@")
               (displayln "<<")
               (super find-wordbreak (list-ref start-pos 8) (list-ref end-pos 8) 'line)
               (displayln "find-wordbreak start-pos end-pos 'line")
               (display ">>")
               )
        ))
    (define (new-word-getter curpos)
    (define (flter word)
      (if (null? (string-split word)) #f
          (car (string-split word))))
      (let*
          ((curpos1 (box curpos))
           (curpos2 (box curpos))
            (prevchar (if (zero? curpos) #f (get-character (- curpos 1)))))
      (cond
        [(and prevchar (char-whitespace? prevchar)) (if (char-whitespace? (get-character curpos)) #f
         (begin (super find-wordbreak #f curpos2 'caret)
                                        (flter (get-text curpos (unbox curpos2) 'caret))))]

        [(char-whitespace? (get-character curpos))
         (begin (super find-wordbreak curpos1 #f 'caret)
                                        (flter (get-text (unbox curpos1) curpos 'caret)))]
        [else
             (begin (super find-wordbreak curpos1 curpos2 'caret)
                    (flter (get-text (unbox curpos1) (unbox curpos2))))]
           )))
    (define/override (on-char key-event)
      (displayln (new-word-getter (get-end-position)))
      (let ((code (send key-event get-key-code)))
        (cond ;[(equal? code #\space) (begin ;(send mytree add-word (super get-text start (get-end-position)) 1) (set! start (+ (get-end-position) 1))
;                                            (super on-char key-event))]
;              [(equal? code #\backspace) (begin ;(send mytree delete-word (super get-text start (get-end-position)) 1)
;                                                (super on-char key-event))]
              [else (begin (super on-char key-event)
                           ;(get-word-around-cursor)
                           )]
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
(define wmap (send t get-wordbreak-map))
;; word ke andar both work fine
;; if enter is there then without pos - 1, caret is going wrong

;; use 'selection start-pos end-pos
;; checks needed for whitespaces and ([]) as separators