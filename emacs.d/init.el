;;; package --- .emacs
;;; Commentary:
;;; a comment
;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'package)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(add-to-list 'load-path
	     (expand-file-name "use-package" user-emacs-directory))

(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)
(use-package evil
  :config
  (evil-mode 1))
(use-package evil-escape
  :init
  (setq-default evil-escape-key-sequence "  ")
  (setq-default evil-escape-delay 0.2)
  :config
  (evil-escape-mode 1))
(use-package evil-leader
  :config
  (evil-leader-mode 1))
(use-package auto-complete
  :config
  (auto-complete-mode 1))
(use-package php-mode
  :mode "\\.php\\'"
  :interpreter "php")
(use-package markdown-mode
  :mode "\\.md\\'")
(ido-mode 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(electric-pair-mode t)
 '(package-selected-packages
   (quote
    (markdown-mode evil-leader php-mode auto-complete evil-escape undo-tree evil)))
 '(tab-always-indent nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
