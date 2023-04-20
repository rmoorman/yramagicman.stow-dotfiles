
;;; package -- Summary
;;; Commentary:
;;; Code:

(use-package auto-package-update
  :custom
  (auto-package-update-interval 30)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

(defun ttymode nil
  "Set tty things."
  (interactive)
  (when (not ( display-graphic-p ) )
    ;; (display-line-numbers-mode 0)
    ;; (linum-relative-mode 0)
    (set-background-color "black")))

(defun ansiterm-vert nil
  "Set tty things."
  (interactive)
  (evil-window-vsplit)
  (ansi-term (executable-find "zsh")))

(defun ansiterm nil
  "Set tty things."
  (interactive)
  (ansi-term (executable-find "zsh")))

(provide 'my_functions)
;;; functions.el ends here
;;; functions.el ends here
