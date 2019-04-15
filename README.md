# CS154-Project #

# Instructions for loading the files to drracket #

* Replace the text.rkt and autocomplete.rkt in racket/share/pkgs/gui-lib/framework/private/
* Now run on command line :
    * $ raco setup framework
    * $ raco setup drracket
* Then run drracket to see the changes.

# To Dos : #

 - [ ] The trie must be made in text.rkt and must be
      - [ ] Able to add word on pressing space
      - [ ] Able to delete word on backspacing out a word completely
      - [ ] Above two tasks amount to the tracking of the current word
 - [ ] Probably the autocompletion-cursor implementation will also be done
      in text.rkt only (Reason: sending whole trie back and forth maybe?)
 - [ ] Try implementing bulk delete and bulk adding (if user uses mouse)
 - [ ] Implementation of edit distance in finding nearest matching prefixes
