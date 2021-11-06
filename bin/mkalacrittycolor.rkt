#lang racket
(require racket/trace)
(require yaml)
(define home (string-join (take (string-split
                                  (path->string (find-system-path 'home-dir)) "/" ) 2) "/"
                          #:before-first "/"))

;; (define st-dir (string-join (list home "/Documents/st" ) ""))
(define config-dir (string-join (list home "/Documents/dots" ) ""))
(define colors-dir (string-join (list config-dir "/config/colors" ) ""))
(define alacritty-dir (string-join (list config-dir "/config/alacritty" ) ""))
(define alacritty-yml (string-join (list alacritty-dir "/alacritty.yml") ""))
(filter
 (lambda (key)
   (hash-iterate-key (file->yaml alacritty-yml) key ))
 (range 0 9 ))
