;;; package -- Summary
;;; Commentary:
;;; Code:

;; hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook (lambda nil "" (untabify (point-min) (point-max))))

(add-hook 'kill-buffer-hook (lambda nil ""
                              (if (file-directory-p "~/intelephense")
                                  (delete-directory "~/intelephense")
                                nil)))

(add-hook 'find-file-hook 'ttymode)
(add-hook 'tty-setup-hook 'ttymode)
(add-hook 'minibuffer-exit-hook (lambda ()
                                  (ttymode)
                                  (save-some-buffers 1)))
;; (dolist (mode '(
;;                 ansi-term-mode
;;                 apropos-mode-hook
;;                 dired-mode-hook
;;                 eshell-mode-hook
;;                 markdown-mode-hook
;;                 org-mode-hook
;;                 shell-mode-hook
;;                 term-mode-hook
;;                 ))
;;   (add-hook mode (lambda ()
;;                    (display-line-numbers-mode 0))))
;; (global-display-line-numbers-mode)
;; (add-hook message-mode-hook 'visual-line-mode)

(defvar my/default-font-size 110)
(defvar my/default-variable-font-size 110)
(defun my/set-font-faces ()
  (message "Setting faces!")
  (set-face-attribute 'default nil :font "Fira Mono" :height my/default-font-size)

  ;; Set the fixed pitch face
  (set-face-attribute 'fixed-pitch nil :font "Fira Mono" :height my/default-font-size)

  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil :font "Cantarell" :height my/default-variable-font-size :weight 'regular))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                ;; (setq doom-modeline-icon t)
                (with-selected-frame frame
                  (my/set-font-faces))))
    (my/set-font-faces))

(defun focus-test ()
  (if (not (frame-focus-state))
      (save-writable)
    nil))
(add-function :after after-focus-change-function #'focus-test)

(provide 'my_hooks)
;;; my_hooks.el ends here
