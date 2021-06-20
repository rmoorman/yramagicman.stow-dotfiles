;;; package -- Summary
;;; Commentary:
;;; Code:
(global-set-key (kbd "C-c e") 'mu4e)
;; Mu4e
(if (file-exists-p "/usr/share/emacs/site-lisp/mu4e/")
    (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e/")
  (message "mu4e not found in /usr/share/emacs/site-lisp/mu4e/"))

(if (file-exists-p "/usr/local/Cellar/mu/1.4.5/share/emacs/site-lisp/mu/mu4e" )
    (add-to-list 'load-path "/usr/local/Cellar/mu/1.4.5/share/emacs/site-lisp/mu/mu4e" )
  (message "mu4e not found in /usr/local/Cellar/mu/1.4.5/share/emacs/site-lisp/mu/mu4e"))

;; (defvar mu4e-drafts-folder "/Drafts")
;; (defvar mu4e-sent-folder   "/Sent")
;; (defvar mu4e-trash-folder  "/Trash")
;; (defvar mu4e-get-mail-command (expand-file-name "~/.local/bin/getallmail") )
;; (defvar sendmail-program "/usr/bin/msmtp" )
(add-hook 'after-init-hook
          (lambda nil ""
            (require 'mu4e)
            (defvar mu4e-maildir (expand-file-name "~/.config/mail/"))

            ;; (add-hook mu4e-compose-mode-hook 'visual-line-mode)
            (defvar mu4e-mu-binary (executable-find "mu"))
            (setq mu4e-get-mail-command (expand-file-name "~/.local/bin/getallmail")
                  user-mail-address "jonathandavis@gilsons.org"
                  user-full-name "Jonathan"
                  message-send-mail-function 'message-send-mail-with-sendmail
                  sendmail-program "/usr/bin/msmtp")

            (setq mu4e-contexts
                  `( ,(make-mu4e-context
                       :name "Private"
                       :enter-func (lambda () (mu4e-message "Entering Private context"))
                       :leave-func (lambda () (mu4e-message "Leaving Private context"))
                       ;; we match based on the contact-fields of the message
                       :match-func (lambda (msg)
                                     (when msg
                                       (mu4e-message-contact-field-matches msg
                                                                           :to "jonathandavis@gilsons.org")))
                       :vars '( ( user-mail-address      . "jonathandavis@gilsons.org"  )
                                ( mu4e-drafts-folder . "/gilsons/Drafts")
                                (mu4e-sent-folder   .  "/gilsons/Sent")
                                (mu4e-trash-folder  .  "/gilsons/Trash")
                                ( user-full-name     . "Jonathan" )
                                ( mu4e-compose-signature . "Jonathan")))
                     ,(make-mu4e-context
                       :name "Work"
                       :enter-func (lambda () (mu4e-message "Entering Work context"))
                       :leave-func (lambda () (mu4e-message "Leaving Work context"))
                       ;; we match based on the contact-fields of the message
                       :match-func (lambda (msg)
                                     (when msg
                                       (mu4e-message-contact-field-matches msg
                                                                           :to "jgilson@omnispear.com")))
                       :vars '( ( user-mail-address      . "jgilson@omnispear.com"  )
                                ( mu4e-drafts-folder . "/work/[Gmail]/Drafts")
                                ( mu4e-sent-folder   .  "/work/[Gmail]/Sent Mail")
                                ( mu4e-trash-folder  .  "/work/[Gmail]/Trash")
                                ( user-full-name     . "Jonathan" )
                                ( mu4e-compose-signature . "Jonathan")))
                     ))

            (global-set-key (kbd "C-c m r") 'mu4e-compose-reply)
            (global-set-key (kbd "C-c m c") 'mu4e-compose)
            (global-set-key (kbd "C-c m e") 'mu4e-compose-edit)


            ))

(provide 'my_mu4e)
;;; mu4e.el ends here
