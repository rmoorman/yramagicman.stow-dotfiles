;;; package -- Summary
;;; Commentary:
;;; Code:

(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  (setq evil-vsplit-window-right 1
        evil-split-window-below 1)
  (add-hook 'evil-normal-state-entry-hook `save-writable))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))


(use-package evil-escape
  :after evil
  :init
  (setq-default evil-escape-key-sequence "  "
                evil-escape-delay 0.4)
  :config
  (evil-escape-mode 1))

(use-package evil-numbers)

(use-package evil-visualstar
  :config
  (global-evil-visualstar-mode t))

(use-package evil-commentary
  :config
  (evil-commentary-mode 1))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-matchit
  :config
  (setq evilmi-shortcut "%")
  (global-evil-matchit-mode 1))

(provide 'my_evil)
;;; evil.el ends here
