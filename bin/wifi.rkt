#lang racket

(require racket/system)

(define iw-output (with-output-to-string
                    (lambda ()
                      (system "iwctl station wlan0 get-networks"))))

(define lines (let ([out (string-split iw-output "\n")])
                (take (reverse out) (- (length out) 4))))
(define split-lines
  (filter (lambda (line)
            (not ( empty? line)))
          (map (lambda (line) ( string-split line "  " #:trim? #t))
               lines)))

(define not-empty (map (lambda (lines)
                         (filter (lambda (line)
                                   (not (string=? "" line) )) lines)) split-lines) )

(define network-pairs
  (reverse (map (lambda (item)
                  (cond [(string-prefix? (first item) "\e")
                         (list (substring (first item) 13) (second item) "connected")]
                        [else (list (first item) (second item) "") ]))
                not-empty)))

(define (security-type pair)
  (string-trim (second  pair) ))

(define (ssid pair)
  (string-trim (first  pair) ))

(for-each (lambda (item)
            (display (string-join
                      (list ( security-type item  ) " "
                            (ssid item) " " (third item) "\n" ) ))) network-pairs)
(define (asknet)
  (display "Which network? ")
  (define network (read-line))
  (define matches? (filter (lambda (item)
                             (string=? network (first item)))
                           network-pairs))
  (if (empty? matches?)
      (asknet)
      (first matches?)))

(define (askpass)
  (display "Pass Phrase? ")
  (read-line))

(define (do-connect net pass)
 (string-join (list "iwctl --passphrase" pass "station wlan0 connect"  net)))

(define desired-net (first (asknet) ))

(if (string=? (second desired-net) "psk" )
    (do-connect desired-net (askpass) )
    (do-connect desired-net ""))
