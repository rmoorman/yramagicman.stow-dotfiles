#lang racket
(require "mkcolor_alacritty.rkt")
(require "mkcolor_globals.rkt")
(require "mkcolor_dwm.rkt")

(define (empty-xresources)
  (let ( [ xresources (file->string (string-join
                                     (list config-dir "/config/X11/Xresources" ) "" )) ])
    (let ([split-resources (string-split xresources "\n") ])
      split-resources
      (filter (lambda line
                (cond [(string-prefix? (first line ) "!") #f]
                      [(string-prefix? (first line ) "*.color") #f]
                      [(string-prefix? (first line ) "*.foreground") #f]
                      [(string-prefix? (first line ) "*.background") #f]
                      [(string-prefix? (first line ) "*.cursorColor") #f]
                      [(string=? (first line ) "") #f]
                      [else #t]))
              split-resources))))

(define (rebuild-colors selection)
  (let ( [ no-colors (empty-xresources) ])
    (flatten (list no-colors (get-new-colors selection)))))

(define (write-tmp-xresources selection)
  (define out-file (open-output-file "/tmp/xresources"))

  (for-each (lambda line
              (display (first line) out-file)
              (newline out-file))
            (rebuild-colors selection)))

(define (list-options)
  (let ([options (list->set
                  (flatten (map
                            (lambda item
                              (first  (string-split (first item ) "." )))
                            (map path->string (directory-list colors-dir)))))])
    (display "Must provide one of the following options:")
    (newline)
    (set-for-each options (lambda item
                            ( display (string-append "\t" (first item ) ) )
                            (newline)))))


(cond
  [(= 1 (length (vector->list (current-command-line-arguments))))
   (write-alacritty (first (vector->list (current-command-line-arguments))))
   (write-tmp-xresources (first (vector->list (current-command-line-arguments))))
   (write-config.h-tmp (first (vector->list (current-command-line-arguments))))]
  [else (list-options) ])
