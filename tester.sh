#! /bin/bash

cp text.rkt ../racket/share/pkgs/gui-lib/framework/private/
cp autocomplete.rkt ../racket/share/pkgs/gui-lib/framework/private/
../racket/bin/raco setup framework
../racket/bin/raco setup drracket
../racket/bin/drracket