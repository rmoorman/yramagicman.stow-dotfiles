#lang racket
(require "mkcolor_globals.rkt")

(define dwmconf (file->string (string-join (list dwm-dir "/" "config.h") "")))
(define dwmconf-split (string-split dwmconf "\n"))

(define (replace-color haystack new-needle)
  (let ([needle haystack])
    (if (regexp-match #px"#[0-9a-f]{6}" haystack)
        (string-replace needle (first (regexp-match #px"#[0-9a-f]{6}" haystack)) new-needle)
        #t)))

(define (replace-from-xresources replace selection color)
  (let ([replacement (filter (lambda str
                               (string-prefix? (first str) (string-append "*." color)))
                             (get-new-colors selection )) ])
    (replace-color replace (second (string-split (first replacement ))))))

(define (mod-dwm selection)
  (filter (lambda str
            (cond [(string-contains? (first str) " foreground[]")
                   (replace-from-xresources  (first str) selection foreground)]
                  [(string-contains? (first str) " sel_foreground[]")
                   (replace-from-xresources  (first str) selection sel_foreground)]
                  [(string-contains? (first str) " background[]")
                   (replace-from-xresources  (first str) selection background)]
                  [(string-contains? (first str) " sel_background[]")
                   (replace-from-xresources  (first str) selection sel_background)]
                  [(string-contains? (first str) " border[]")
                   (replace-from-xresources  (first str) selection border)]
                  [(string-contains? (first str) " sel_border[]")
                   (replace-from-xresources  (first str) selection sel_border)]
                  [else #t])) dwmconf-split))

(define (write-config.h-tmp selection)
  (define out-file (open-output-file "/tmp/dwm.config.h"))
  (for-each (lambda line
              (display (first line) out-file)
              (newline out-file))
            (mod-dwm selection)))
(provide
 write-config.h-tmp)
