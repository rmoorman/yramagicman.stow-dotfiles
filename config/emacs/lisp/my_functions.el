
;;; package -- Summary
;;; Commentary:
;;; Code:

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
