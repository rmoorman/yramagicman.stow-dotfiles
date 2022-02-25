#lang racket

(require racket/trace)
(require racket/date)

(define home (string-join (take (string-split
                                  (path->string (find-system-path 'home-dir)) "/" ) 2) "/"
                          #:before-first "/"))

(define exec-notify "/run/current-system/sw/bin/notify-send")

(define remind-file (string-join (list home "/.local/share/remind/reminders.txt" ) ""))


(define (inc a) (+ 1 a))

(define (m-now) (seconds->date (current-seconds)))

(define (make-partial-path path offset)
  (let ([list-path (string-split path "/")])
    (string-join (take list-path offset) "/" #:before-first "/")))

(define (check-path path offset)
  (let ([split-path (string-split path "/")])
    (cond [(equal? (- (length split-path) 0 ) offset) #t]
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
         (displayln "write a reminder here" out-file))
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
              (displayln (first line) out-file)) remind-list))

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
  (get-interval-times (reminder-time reminder) (reminder-interval reminder)))

(define (parse-reminder reminder)
  """
  Format: dd/mm/yy HH:mm notification,intervals content.
  Takes a reminder of the above format and returns a reminder struct.
  TODO: Enable repition with daily,weekly,montly,day-of-week
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

(define (get-day d)
  (date-day d))

(define (check-time t)
  (if (equal? 0 t)
    #f
    (let ([current-minute (get-minute ( m-now ))]
          [current-hour (get-hour ( m-now ))]
          [current-day (get-day ( m-now ))]
          [date-struct t])
      (and (equal? current-minute (get-minute date-struct))
           (equal? current-hour (get-hour date-struct))
           (equal? current-day (get-day date-struct))))))

(define (m-notify-me-on-time)
  (for-each (lambda (n)
              (cond
                [(check-time (parse-date (reminder-time n ) ))
                 (system (string-append exec-notify " '" (reminder-content n) "'"))]
                [else #f]))
            parsed-reminders))

(define (m-notify-me-on-time-stdout)
  (for-each (lambda (n)
              (cond
                [(check-time (parse-date (reminder-time n ) ))
                 (displayln (reminder-content n))]
                [else #f]))
            parsed-reminders))

(define (m-notify-me-ahead-of-time)
  (for-each (lambda (r)
              (for-each (lambda (n)
                          (cond
                            [(check-time n)
                             ((lambda ()
                                (system (string-append exec-notify " '" (reminder-content r) "'"))))]
                            [else #f]))
                        (interval-times r)))
            parsed-reminders))

(define (m-notify-me-ahead-of-time-stdout)
  (for-each (lambda (r)
              (for-each (lambda (n)
                          (cond
                            [(check-time n)
                             ((lambda ()
                                (displayln (reminder-content r))))]
                            [else #f]))
                        (interval-times r)))
            parsed-reminders))

(define (m-remind)
  (m-notify-me-on-time)
  (m-notify-me-ahead-of-time))

(define (m-remind-stdout)
  (m-notify-me-on-time-stdout)
  (m-notify-me-ahead-of-time-stdout))

(define (m-get-args)
  (vector->list (current-command-line-arguments)))

(define (m-send-reminder reminder-list)
  ;; TODO Parse out reminder times to figure out which one is
  ;; actually next, instead of just displaying the last reminder
  ;; in the file
  (reminder-content (parse-reminder (first (reverse reminder-list)))))


(define help-strings
  (list
    "Basic usage: remind dd/mm/yyy HH:mm int,int... some text"
    ""
    "Flags:"
    "remind: Shows reminder for current minute on stdout, intended to be used as cron job"
    "or systemd timer"
    "remind --notify: use notify-send to display reminders"
    "remind -n, --next use notify-send to display reminders"
    "remind -h, --help: Show this message."
    "remind -l, --list: List reminders."
    "remind -n, --next: Display notifcation with next reminder, regardless of schedule."
    "remind -e, --edit: Open reminder list in $EDITOR."
    "remind -t, --test: Test notifications. Sends notification on every call to remind"
    "remind -s, --stdout Test notifications on stdout. Sends notification on every call to remind"
    ""
    "Further Instruction:"
    "Add a reminder with `remind dd/mm/yyyy HH:mm int,int some text`."
    "Where dd/mm/yyyy is the current date in day/month/year format, zero padded,"
    "HH:mm is the time of the event in 24 hour time,"
    "and `int,int` is a comma separated list of any length of minutes prior to"
    "an event at which you want to be notified"
    ""
    (string-append "Reminders are stored in " remind-file)))

(define (call-and-exit proc)
  (proc) (exit))

(define (dispatch-args args)
  (for-each (lambda (arg)
         (cond
           [(or (equal? arg "-h") (equal? arg "--help"))
            (call-and-exit (lambda ()
                             (map displayln help-strings)))]
           [(or (equal? arg "-l") (equal? arg "--list"))
            (call-and-exit (lambda ()
                             (map displayln remind-list)))]
           [(or (equal? arg "-n") (equal? arg "--next"))
            (call-and-exit (lambda ()
                             (system (string-append exec-notify " '" (m-send-reminder remind-list) "'"))))]
           [(or (equal? arg "-e") (equal? arg "--edit"))
            (call-and-exit (lambda ()
                             (system (string-append "$EDITOR " remind-file))))]
           [(or (equal? arg "-t") (equal? arg "--test"))
            (call-and-exit (lambda () (system  (string-append exec-notify " 'test notification'" ))))]
           [(or (equal? arg "-s") (equal? arg "--stdout"))
            (call-and-exit (lambda () (displayln "test notification")))]
           [(or (equal? arg "--notify"))
            (call-and-exit (lambda ()
                             (m-remind)))]
           [(not (equal? 0 (length (m-get-args))))
            (call-and-exit (lambda ()
                             (write-reminders
                               (flatten (list remind-list (list (string-join (m-get-args))))))))]
           [else  (call-and-exit (displayln "arg parsing failed") )]
           )) args )
         (call-and-exit (lambda () (m-remind))))

(dispatch-args (m-get-args))
