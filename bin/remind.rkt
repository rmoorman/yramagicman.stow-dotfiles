#lang racket

(require racket/trace)
(require racket/date)

(define home (string-join (take (string-split
                                  (path->string (find-system-path 'home-dir)) "/" ) 2) "/"
                          #:before-first "/"))

(define remind-file (string-join (list home "/.local/tasks/remind/reminders.txt" ) ""))

(define (make-partial-path path offset)
  (let ([list-path (string-split path "/")])
    (string-join (take list-path offset) "/" #:before-first "/")))

(define (inc a) (+ 1 a))

(define (m-now) (seconds->date (current-seconds)))

(define (check-path path offset)
  (let ([split-path (string-split path "/")])
    (cond [(= (- (length split-path) 0 ) offset) #t]
          [(directory-exists? (make-partial-path path offset))
           (check-path path (inc offset))]
          [else (make-directory (make-partial-path path offset))])))

(define (m-ensure-file)
  """Ensure that a the reminder file exists, if not write newline to file"""
  (if (file-exists? remind-file)
    #t
    ((lambda ()
       (check-path remind-file 0)
       (let ([ out-file (open-output-file remind-file) ] )
         (display (newline) out-file))
       ))))

(define (m-remind-content)
  """ Read reminder file to string """
  (m-ensure-file)
  (file->string remind-file))

;; Create list from reminder file, splitting on newlines
(define remind-list (string-split (m-remind-content) "\n"))

(define (write-reminders remind-list)
  """ write list of reminders to file """
  (define out-file (open-output-file remind-file #:exists 'truncate ))
  (for-each (lambda line
              (display (first line) out-file)
              (newline out-file)) remind-list))

(define (parse-date date-string)
  """ return date struct for date time string in dd/mm/yyy HH:mm format"
  (define date-time-parts  (string-split date-string " "))
  (let ([ date-parts (string-split (first date-time-parts) "/")]
        [ time-parts (string-split (second date-time-parts) ":") ])
    (seconds->date (find-seconds 00                                   ;; seconds
                                 (string->number (second time-parts)) ;; minutes
                                 (string->number (first time-parts))  ;; hours
                                 (string->number (first date-parts))  ;; day
                                 (string->number (second date-parts)) ;; month
                                 (string->number (third date-parts))  ;; year
                                 ))))

(define-struct reminder (time interval content))

(define (get-interval-times rtime interval)
  """ Get time from listed reminder intervals """
  (map (lambda (t)
         (let ([parsed-time (parse-date rtime) ])
           (if (< (current-seconds) (date->seconds parsed-time ))
             (seconds->date (- (date->seconds parsed-time ) (minutes (string->number t))))
             0))) interval))

(define (interval-times reminder )
  """ get interval times for a specific reminder """
  (get-interval-times (reminder-time reminder ) (reminder-interval reminder)))

(define (parse-reminder reminder)
  """
  Format: dd/mm/yy HH:mm notification,intervals content.
  Takes a reminder of the above format and returns a reminder struct.
  """
  (define spaces (string-split reminder " "))
  (let ([content  (string-join (list-tail spaces 3) " " )  ]
        [time (string-join (take spaces 2 )) ]
        [intervals (string-split (third (take spaces 3) ) ",")])
    (make-reminder time intervals content)))

(define (minutes t)
  """ covert minutes to seconds"""
  (* t 60 ))

;; Parse all reminders in the files
(define parsed-reminders (map parse-reminder remind-list))

(define (get-minute d)
  (date-minute d))

(define (get-hour d)
  (date-hour d))

(define (check-time t)
  (if (equal? 0 t)
    #f
    (let ([current-minute (get-minute ( m-now ))]
          [current-hour (get-hour ( m-now ))]
          [date-struct t])
      (and (equal? current-minute (get-minute date-struct))
           (= current-hour (get-hour date-struct))))))

(define (m-notify-me-on-time)
  (for-each (lambda (n)
              (cond
                [(check-time (parse-date (reminder-time n ) ))
                 (system (string-append "notify-send 'on time " (reminder-content n) "'"))]
                [else #f]))
            parsed-reminders))


(define (m-notify-me-ahead-of-time)
  (for-each (lambda (r)
              (for-each (lambda (n)
                          (cond
                            [(check-time n)
                             ((lambda ()
                                (system (string-append "notify-send ' ahead" (reminder-content r) "'"))))]
                            [else #f]))
                        (interval-times r)))
            parsed-reminders))

(define (m-remind)
 (m-notify-me-on-time)
 (m-notify-me-ahead-of-time))

(define valid-args (list "-h" "--help" "--list" "-l"))
(define (m-get-args)
  (vector->list (current-command-line-arguments)))

(define ( m-should-be-reminder )
  (empty?
    (filter (lambda (a)
              (member a (get-args))) valid-args )))

(if (and (m-should-be-reminder) (>= (length (get-args)) 3))
  (parse-reminder (string-join (get-args)))
  (dispatch-args (get-args)))

;; (cond
;;   [(= 1 (length (vector->list (current-command-line-arguments))))
;;    (write-alacritty (first (vector->list (current-command-line-arguments))))
;;    (write-tmp-xresources (first (vector->list (current-command-line-arguments))))
;;    (write-config.h-tmp (first (vector->list (current-command-line-arguments))))]
;;   [else (list-options) ])
