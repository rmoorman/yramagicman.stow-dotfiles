
;;; package -- Summary
;;; Commentary:
;;; Code:

(defun ttymode nil
  "Set tty things."
  (interactive)
  (when (not ( display-graphic-p ) )
    (disable-theme 'base16-gruvbox-dark-pale)

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

(defun save-writable ()
  "Save buffer only if writable."
  (if (and buffer-file-name
           (file-writable-p buffer-file-name))
      (save-buffer)
    nil))
(provide 'my_functions)
;;; functions.el ends here
