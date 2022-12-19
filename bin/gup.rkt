#lang racket
;(provide main)
(require racket/system)

(define (execute-command proc-name)
  (define proc (find-executable-path proc-name))
  (lambda (args)
    (with-output-to-string
      (thunk (apply system* proc args)))))

(define home (string-append (getenv "HOME") "/" ))
(define repo-file (string-split (file->string (string-append home ".config/gits")) "\n"))
(define repos (filter
               (lambda x
                 (not (string-prefix? (string-join x) "#" ) ) ) repo-file))

(define paths (map string->path repos))

(define (changes)
  (define ch (string-split ((execute-command "git") (list "status")) "\n" #:trim? #t))
  (define not-empty (filter
                     (lambda (x)
                       (and (not (string=? "" x)) (> (string-length x) (string-length "modified"))))
                     ch))
  (define return-value (filter
                        (lambda (y)
                          (string=? "modified" (substring y 1 (string-length "\tmodified"))))
                        not-empty))
  (map (lambda (z)
         (substring z 1)) return-value))


(define (show-changes repo)
  (string-append (path->string repo) "\n" (string-join (changes) "\n") (make-string 50 #\space)))

(define (check-status)
  (define commands
    '(("checkout" "-q" "master")
      ("rev-parse" "@")
      ("rev-parse" "@{u}")
      ("merge-base" "@" "@{u}")))
  (define result (map (execute-command "git") commands))
  (let ([local (second result)]
        [remote (third result)]
        [base (fourth result)])
    (cond
      ((string=? local remote) 0)
      ((string=? base remote) 1)
      ((string=? base local) -1)
      (else 2))))

(define (pull-push pull push repo)
  ((execute-command "git") (list "fetch" "--all"))
  (define stat (check-status))
  (cond
    ((not (= stat 0))
     (cond
       ((= stat -1) (pull))
       ((= stat 1) (push))
       (else ((execute-command "git" )  (list "log" "-n" "5" ) ))
       ))
    (else (displayln (show-changes repo) ))))

(define (pull)
  (let ([git (list "git" "pull" "--rebase")])
  ((execute-command (first git)) (rest git))))

(define (push)
  (let ([git (list "git" "push")])
    ((execute-command (first git)) (rest git))))

(define (run-git-processes repo)
  (if (directory-exists? repo)
      ((lambda ()
         (current-directory repo)
         (define cur-dir (path->string (current-directory)))
         (displayln (string-append cur-dir "..."))
         (pull-push pull push repo)
       ))
      #f
      ))

(for-each run-git-processes paths)
