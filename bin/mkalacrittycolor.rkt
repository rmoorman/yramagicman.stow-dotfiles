#lang racket
(require yaml)
(define home (string-join (take (string-split
                                  (path->string (find-system-path 'home-dir)) "/" ) 2) "/"
                          #:before-first "/"))

;; (define st-dir (string-join (list home "/Documents/st" ) ""))
(define config-dir (string-join (list home "/Documents/dots" ) ""))
(define colors-dir (string-join (list config-dir "/config/colors" ) ""))
(define alacritty-dir (string-join (list config-dir "/config/alacritty" ) ""))
(define alacritty-yml (string-join (list alacritty-dir "/alacritty.yml") ""))
(define alacritty (file->yaml alacritty-yml))

(define bright-colors
  (hash-ref
    (hash-ref alacritty "colors")
    "bright"))

(define normal-colors
  (hash-ref
    (hash-ref alacritty "colors")
    "normal"))

(define (get-new-colors selection)
  (let ( [new-colors (string-split
                       (file->string
                         (string-join
                           (list colors-dir "/" selection ".xresources") "" )) "\n")])
    (filter (lambda line
              (if (string=? "" (first line ))
                #f
                #t)) new-colors)))

(get-new-colors "ashes_dark")
(define bright-map (make-hash))
(hash-set! bright-map "black"   "color8")
(hash-set! bright-map "red"     "color9")
(hash-set! bright-map "green"   "color10")
(hash-set! bright-map "yellow"  "color11")
(hash-set! bright-map "blue"    "color12")
(hash-set! bright-map "magenta" "color13")
(hash-set! bright-map "cyan"    "color14")
(hash-set! bright-map "white"   "color15")

(define normal-map (make-hash))
(hash-set! normal-map "black"   "color0")
(hash-set! normal-map "red"     "color1")
(hash-set! normal-map "green"   "color2")
(hash-set! normal-map "yellow"  "color3")
(hash-set! normal-map "blue"    "color4")
(hash-set! normal-map "magenta" "color5")
(hash-set! normal-map "cyan"    "color6")
(hash-set! normal-map "white"   "color7")
