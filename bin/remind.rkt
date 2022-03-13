#lang racket

(require racket/trace)
(require racket/date)
(require rackunit)

(define home (string-join (take (string-split
                                  (path->string (find-system-path 'home-dir)) "/" ) 2) "/"
                          #:before-first "/"))

(define exec-notify "/run/current-system/sw/bin/notify-send")

(define days (hash
               "sun" 0
               "mon" 1
               "tue" 2
               "wed" 3
               "thu" 4
               "fri" 5
               "sat" 6 ))

(test-case "maps week days to integers"
           (check-equal? (hash-ref days "sun") 0)
           (check-equal? (hash-ref days "mon") 1)
           (check-equal? (hash-ref days "tue") 2)
           (check-equal? (hash-ref days "wed") 3)
           (check-equal? (hash-ref days "thu") 4)
           (check-equal? (hash-ref days "fri") 5)
           (check-equal? (hash-ref days "sat") 6))

(define (get-minute d)
  (date-minute d))

(define (get-hour d)
  (date-hour d))

(define (get-day d)
  (date-day d))

(define (get-month d)
  (date-month d))

(define (get-year d)
  (date-year d))

(define remind-file (string-join (list home "/.local/share/remind/reminders.txt" ) ""))

(define (m-now) (seconds->date (current-seconds)))

(define (make-partial-path path offset)
  (let ([list-path (string-split path "/")])
    (string-join (take list-path offset) "/" #:before-first "/")))

(test-case "returns expected partial path"
           (check-equal? (make-partial-path "/home/user/documents/file" 1) "/home"))

(define (check-path path offset)
  (let ([split-path (string-split path "/")])
    (cond [(equal? (- (length split-path) 0 ) offset) #t]
          [(directory-exists? (make-partial-path path offset))
           (check-path path (+ 1 offset))]
          [else (make-directory (make-partial-path path offset))])))

(define (m-ensure-file)
  """Ensure that a the reminder file exists, if not write newline to file"""
  (if (file-exists? remind-file)
    #t
    ((lambda ()
       (check-path remind-file 0)
       (let ([ out-file (open-output-file remind-file) ] )
         (displayln "01/01/1991 01:01 0,0 write a reminder here" out-file))
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

(define (diff-days word #:day [day (current-seconds)])
  (define day-seconds (* 60 60 24))
  (define current-dow (date-week-day (seconds->date day)))
  (cond
    [(string=? word "sun") (+ day
                              (* (hash-ref days word) day-seconds))]
    [(> 0 (- current-dow (hash-ref days word)))
     (+ day (* (- (hash-ref days word) current-dow)
                            day-seconds))]
    [(< 0 (- current-dow (hash-ref days word)))
     (+ day (* (+ 7 (- (hash-ref days word) current-dow))
                            day-seconds))]
    [else day]))

  (test-case "maps word to date sunday"
             (check-equal? (date-week-day (seconds->date (diff-days "mon" #:day 1647208240))) (hash-ref days "mon"))
             (check-equal? (date-week-day (seconds->date (diff-days "tue" #:day 1647208240))) (hash-ref days "tue"))
             (check-equal? (date-week-day (seconds->date (diff-days "wed" #:day 1647208240))) (hash-ref days "wed"))
             (check-equal? (date-week-day (seconds->date (diff-days "thu" #:day 1647208240))) (hash-ref days "thu"))
             (check-equal? (date-week-day (seconds->date (diff-days "fri" #:day 1647208240))) (hash-ref days "fri"))
             (check-equal? (date-week-day (seconds->date (diff-days "sat" #:day 1647208240))) (hash-ref days "sat"))
             (check-equal? (date-week-day (seconds->date (diff-days "sun" #:day 1647208240))) 0))

  (test-case "maps word to date monday"
             (check-equal? (date-week-day (seconds->date (diff-days "mon" #:day 1647294956))) (hash-ref days "mon"))
             (check-equal? (date-week-day (seconds->date (diff-days "tue" #:day 1647294956))) (hash-ref days "tue"))
             (check-equal? (date-week-day (seconds->date (diff-days "wed" #:day 1647294956))) (hash-ref days "wed"))
             (check-equal? (date-week-day (seconds->date (diff-days "thu" #:day 1647294956))) (hash-ref days "thu"))
             (check-equal? (date-week-day (seconds->date (diff-days "fri" #:day 1647294956))) (hash-ref days "fri"))
             (check-equal? (date-week-day (seconds->date (diff-days "sat" #:day 1647294956))) (hash-ref days "sat"))
             (check-equal? (date-week-day (seconds->date (diff-days "sun" #:day 1647294956))) 0)
             )

  (define (parse-date date-string)
    """ return date struct for date time string in dd/mm/yyy HH:mm format"
    (define date-time-parts (string-split date-string " "))
    (let ([ date-parts (string-split (first date-time-parts) "/")]
          [ time-parts (string-split (second date-time-parts) ":") ])
      (seconds->date (find-seconds 00 ;; seconds
                                   (string->number (second time-parts)) ;; minutes
                                   (string->number (first time-parts)) ;; hours
                                   (string->number (first date-parts)) ;; day
                                   (string->number (second date-parts)) ;; month
                                   (string->number (third date-parts)) ;; year
                                   ))))

  (test-case "parses date correctly"
             (check-equal? (parse-date "01/01/1991 13:05" )
                           (date* 0 5 13 1 1 1991 2 0 #f -18000 0 "EST")))

  (define-struct reminder (time interval content))

  (define (get-interval-times rtime interval)
    """ Get time from listed reminder intervals """
    (define (minutes t)
      """ covert minutes to seconds"""
      (* t 60 ))
    (map (lambda (t)
           (let ([parsed-time (parse-date rtime) ])
             (if (< (current-seconds) (date->seconds parsed-time ))
               (seconds->date (- (date->seconds parsed-time ) (minutes (string->number t))))
               0))) interval))

  (define (interval-times reminder )
    """ get interval times for a specific reminder """
    (get-interval-times (reminder-time reminder) (reminder-interval reminder)))

  (test-case "returns interval times"
             (check-equal?
               (interval-times (make-reminder "01/01/2031 13:05" (list "5" "10") "test"))
               (list
                 (date* 0 0 13 1 1 2031 3 0 #f -18000 0 "EST")
                 (date* 0 55 12 1 1 2031 3 0 #f -18000 0 "EST")))

             (check-equal?
               (interval-times (make-reminder "01/01/1991 13:05" (list "5" "10") "test"))
               (list 0 0)))

  (define (parse-reminder reminder)
    """
    Format: dd/mm/yy HH:mm notification,intervals content.
    Takes a reminder of the above format and returns a reminder struct.
    TODO: Enable repition with daily,weekly,montly,day-of-week
    """
    (define spaces (string-split reminder " "))
    (let ([content (string-join (list-tail spaces 3) " " ) ]
          [time (string-join (take spaces 2 )) ]
          [intervals (string-split (third (take spaces 3) ) ",")])
      (make-reminder time intervals content)))

  (test-case "parses past reminders"
             (check-equal?
               (reminder-time (parse-reminder "01/01/1991 13:05 5,10 test"))
               (reminder-time (make-reminder "01/01/1991 13:05" (list "5" "10") "test")))
             (check-equal?
               (reminder-interval (parse-reminder "01/01/1991 13:05 5,10 test"))
               (reminder-interval (make-reminder "01/01/1991 13:05" (list "5" "10") "test")))
             (check-equal?
               (reminder-content (parse-reminder "01/01/1991 13:05 5,10 test"))
               (reminder-content (make-reminder "01/01/1991 13:05" (list "5" "10") "test")))
             )

  (test-case "parses future reminders"
             (check-equal?
               (reminder-time (parse-reminder "01/01/2035 13:05 5,10 test"))
               (reminder-time (make-reminder "01/01/2035 13:05" (list "5" "10") "test")))
             (check-equal?
               (reminder-interval (parse-reminder "01/01/2035 13:05 5,10 test"))
               (reminder-interval (make-reminder "01/01/2035 13:05" (list "5" "10") "test")))
             (check-equal?
               (reminder-content (parse-reminder "01/01/2035 13:05 5,10 test"))
               (reminder-content (make-reminder "01/01/2035 13:05" (list "5" "10") "test"))))

  ;; Parse all reminders in the files
  (define parsed-reminders (map parse-reminder remind-list))


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

  (test-case "parses time reminders"
             (check-equal? (check-time 0) #f)
             (check-equal? (check-time (m-now)) #t))

  (test-case "parses time reminders"
             (check-equal? (check-time 0) #f)
             (check-equal? (check-time (m-now)) #t))

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
                   (call-and-exit (lambda () (system (string-append exec-notify " 'test notification'" ))))]
                  [(or (equal? arg "-s") (equal? arg "--stdout"))
                   (call-and-exit (lambda () (displayln "test notification")))]
                  [(or (equal? arg "--notify"))
                   (call-and-exit (lambda ()
                                    (m-remind)))]
                  [(not (equal? 0 (length (m-get-args))))
                   (call-and-exit (lambda ()
                                    (write-reminders
                                      (flatten (list remind-list (list (string-join (m-get-args))))))))]
                  [else (call-and-exit (displayln "arg parsing failed") )]
                  )) args )
    (call-and-exit (lambda () (m-remind))))

  (dispatch-args (m-get-args))
