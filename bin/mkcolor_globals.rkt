#lang racket
(define home (string-join (take (string-split
                                 (path->string (find-system-path 'home-dir)) "/" ) 2) "/"
                          #:before-first "/"))

(define config-dir (string-join (list home "/Documents/dots" ) ""))
(define colors-dir (string-join (list config-dir "/config/colors" ) ""))
(define alacritty-dir (string-join (list config-dir "/config/alacritty" ) ""))
(define alacritty-yml (string-join (list alacritty-dir "/alacritty.yml") ""))
;; (define st-dir (string-join (list home "/Documents/st" ) ""))
(define dwm-dir (string-join (list home "/Documents/dwm" ) ""))
(define foreground  "foreground" )
(define background  "background" )
(define border  "color0" )
(define sel_foreground  "background" )
(define sel_background  "foreground" )
(define sel_border  "foreground" )

(define (get-new-colors selection)
  (string-split
   (file->string
    (string-join
     (list colors-dir "/" selection ".xresources") "" )) "\n" #:repeat? #t))

(provide
 home
 alacritty-yml
 config-dir
 colors-dir
 get-new-colors
 dwm-dir
 foreground
 background
 border
 sel_foreground
 sel_background
 sel_border
 )
