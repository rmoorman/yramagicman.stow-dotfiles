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

(defun set-faces ()
  (when (frame-focus-state)
    (set-face-attribute-from-resource 'fringe :background "background" "background" nil)
    (set-face-attribute-from-resource 'mode-line-inactive :background "background" "background" nil)
    (set-face-attribute-from-resource 'mode-line-inactive :foreground "foreground" "foreground" nil)
    (set-face-attribute-from-resource 'mode-line :background "background" "color8" nil)
    (set-face-attribute-from-resource 'mode-line :foreground "foreground" "color7" nil)
    (set-face-attribute `mode-line-inactive nil :box nil )
    (set-face-attribute `mode-line nil :box nil )
    (set-face-attribute-from-resource 'hl-line :background "background" "color8" nil)))

(if (daemonp)
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (with-selected-frame frame
                                   (my/set-font-faces)))))

(defun save-on-focus-lost ()
  (if (not (frame-focus-state))
    (save-writable)
    nil))
(add-function :after after-focus-change-function #'save-on-focus-lost)

(add-function :after after-focus-change-function #'set-faces)
(provide 'my_hooks)
;;; my_hooks.el ends here
