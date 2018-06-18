;;; package --- Summary
;;; user hooks for various modes

;;; Code

(add-hook 'before-save-hook
          (lambda nil
            ; (evil-normal-state)
            (set-buffer-file-coding-system 'undecided-unix)
            (delete-trailing-whitespace)
            (untabify)))

; hopefully save buffer if writable
(add-hook 'evil-normal-state-entry-hook 'save-file)


(add-hook 'evil-mode-hook 'evil-commentary-mode)

(add-hook 'emacs-lisp-mode-hook
      (lambda nil
        ;; (add-hook 'after-save-hook 'eval-buffer)
        (eldoc-mode)))

(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'flyspell-mode)

(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'message-mode-hook 'flyspell-mode)

(add-hook 'dired-load-hook
          (lambda ()
            (require 'dired-x)))
;; (add-hook 'dired-mode-hook
;;           (lambda ()
;;             (setq dired-omit-files "\*pyc")
;;             (setq dired-omit-files-p t)
;;             (dired-omit-mode)))

(add-to-list 'auto-mode-alist '("/\\..*zsh\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.*php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.*.blade.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.*html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.*vue$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.*scss$" . scss-mode))
(add-to-list 'auto-mode-alist '(".zshrc" . shell-script-mode))
(add-to-list 'auto-mode-alist '(".zpreztorc" . shell-script-mode))
(add-to-list 'auto-mode-alist '(".zprofile" . shell-script-mode))
(add-to-list 'auto-mode-alist '("mutt-tardis*" . message-mode))
(add-to-list 'auto-mode-alist '("mutt-k-nine*" . message-mode))
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG" . diff-mode))



(provide 'user_hooks)
;;; user_hooks.el ends here
