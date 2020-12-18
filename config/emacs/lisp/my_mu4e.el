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

(defvar mu4e-drafts-folder "/Drafts")
(defvar mu4e-sent-folder   "/Sent")
(defvar mu4e-trash-folder  "/Trash")
(defvar mu4e-get-mail-command (expand-file-name "~/.local/bin/getallmail") )
(defvar sendmail-program (expand-file-name "~/.local/bin/getallmail") )
(add-hook 'after-init-hook
          (lambda nil ""
            (require 'mu4e)
            (defvar mu4e-maildir (expand-file-name "~/.config/mail/"))

            ;; (add-hook mu4e-compose-mode-hook 'visual-line-mode)
            (defvar mu4e-mu-binary (executable-find "mu"))
            (setq mu4e-drafts-folder "/Drafts"
                  mu4e-sent-folder   "/Sent"
                  mu4e-trash-folder  "/Trash"
                  mu4e-get-mail-command (expand-file-name "~/.local/bin/getallmail")
                  user-mail-address "jonathandavis@gilsons.org"
                  user-full-name "Jonathan"
                  message-send-mail-function 'message-send-mail-with-sendmail
                  sendmail-program "/bin/msmtp")

            (global-set-key (kbd "C-c m r") 'mu4e-compose-reply)
            (global-set-key (kbd "C-c m c") 'mu4e-compose)
            (global-set-key (kbd "C-c m e") 'mu4e-compose-edit)


            ))

(provide 'my_mu4e)
;;; mu4e.el ends here
