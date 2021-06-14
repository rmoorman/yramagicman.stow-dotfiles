;;; package -- Summary
;;; Commentary:
;;; Code:


(use-package projectile
  :config
  (setq projectile-completion-system 'ivy
        projectile-enable-caching nil
        projectile-file-exists-local-cache-expire (* 5 60)
        projectile-indexing-method 'alien)
  (projectile-mode +1))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package ivy
  :config
  (ivy-mode 1))

;; (use-package flycheck
;;   :config
;;   (setq flycheck-typescript-tslint-executable "~/.local/bin/tslint")
;;   (global-flycheck-mode))

;; (use-package flycheck-pkg-config
;;   :after flycheck)

;; misc. packages
(use-package magit)
(use-package disable-mouse)
;; (use-package vterm)
(show-paren-mode 1)

(use-package base16-theme)

(provide 'my_util)
;;; modeline.el ends here
