;;; package -- Summary
;;; Commentary:
;;; Code:

(use-package general
  :config
  (general-create-definer my-space-def
    ;; :prefix my-space
    :prefix "SPC")

  (general-create-definer my-leader-def
    ;; :prefix my-leader
    :prefix ",")

  (my-space-def
    :states '( normal visual )
    :keymaps 'override
    " " 'save-buffer)

  (my-leader-def
    :states '( normal visual )
    :keymaps 'override
    "d" 'dired
    "," 'evil-switch-to-windows-last-buffer
    "b" 'ivy-switch-buffer
    "e" 'eval-defun
    "t" 'ansiterm-vert
    "f" 'projectile-find-file
    "F" 'find-file
    )

  (general-define-key
   :states '(normal visual)
   :keymaps 'override

   "C-a" 'evil-numbers/inc-at-pt

   "C-x" 'evil-numbers/dec-at-pt

   ))

;; keybind
(global-set-key (kbd "C-c b") 'buffer-menu)
(global-set-key (kbd "C-c t") 'ttymode)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(provide 'my_keys)
;;; keys.el ends here
