#lang racket

(define home (string-join (take (string-split
                                  (path->string (find-system-path 'home-dir)) "/" ) 2) "/"
                          #:before-first "/"))

;; (define st-dir (string-join (list home "/Documents/st" ) ""))
(define config-dir (string-join (list home "/Documents/dots" ) ""))
(define colors-dir (string-join (list config-dir "/config/colors" ) ""))

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

(define (get-new-colors selection)
  (let ( [new-colors (string-split
                       (file->string
                         (string-join
                           (list colors-dir "/" selection ".xresources") "" )) "\n")])
    (filter (lambda line
              (if (string=? "" (first line ))
                #f
                #t)) new-colors)))

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
  (let ( [options (list->set
                    (flatten (map
                               (lambda item
                                 (first  (string-split (first item ) "." )))
                               (map path->string (directory-list colors-dir))))) ] )
    (display "Must provide one of the following options:")
    (newline)
    (set-for-each options (lambda item
                            ( display (string-append "\t" (first item ) ) )
                            (newline)))))

(define dwm-dir (string-join (list home "/Documents/dwm" ) ""))
(define foreground  "foreground" )
(define background  "background" )
(define border  "color0" )
(define sel_foreground  "background" )
(define sel_background  "foreground" )
(define sel_border  "foreground" )
(define dwmconf (file->string (string-join (list dwm-dir "/" "config.h") "")))
(define dwmconf-split (string-split dwmconf "\n"))

(define ( replace-color haystack new-needle)
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
  (define out-file (open-output-file "/tmp/config.h"))

  (for-each (lambda line
              (display (first line) out-file)
              (newline out-file))
            (mod-dwm selection)))

(cond
 [(= 1 (length (vector->list (current-command-line-arguments))))

  (write-tmp-xresources (first (vector->list (current-command-line-arguments))))
  (write-config.h-tmp (first (vector->list (current-command-line-arguments))))]
 [else (println "argument required") ])
