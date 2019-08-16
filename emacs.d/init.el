;;; package --- .emacs
;;; Commentary:
;;; a comment
;;; Code:

;; Hide mouse interface quickly
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Package initialization
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'package)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

;; use-package bootstrap and init
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Evil config
(use-package evil
  :config
  (evil-mode 1)
  (add-hook 'evil-normal-state-entry-hook (lambda ()
					    (if buffer-file-name
						(when
						    (file-writable-p buffer-file-name)
						  (save-buffer)))
					    (lambda() nil))))

(use-package evil-escape
  :init
  (setq-default evil-escape-key-sequence "  ")
  (setq-default evil-escape-delay 0.2)
  :config
  (evil-escape-mode 1))

(use-package evil-leader
  :config
  (evil-leader/set-leader ",")
  (evil-leader/set-key
    "," 'evil-switch-to-windows-last-buffer
    "s" 'magit-status
    "c" 'magit-commit-create
    "f" 'find-file
    "SPC" 'save-buffer)
  (global-evil-leader-mode 1))

;; Completion settings
(use-package auto-complete
  :config
  (global-auto-complete-mode 1))

(use-package php-mode
  :mode "\\.php\\'"
  :interpreter "php"
  :config
  (ac-php-mode))
(use-package ac-php)

;; folding
(use-package vimish-fold)
(use-package evil-vimish-fold
  :config
  (evil-vimish-fold-mode 1))

;; keybinds
(global-set-key (kbd "C-c b") 'buffer-menu)
(global-set-key (kbd "C-c t") 'vterm)
;; misc. packages
(use-package magit)
(use-package vterm)
(use-package markdown-mode
  :mode "\\.md\\'")
(use-package linum-relative
  :config
  (linum-relative-global-mode))
(use-package web-mode
  :mode "\\.blade.php|.vue|.html\\'")
(ido-mode 1)
(show-paren-mode 1)
(message "loading mu4e")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e/")
(require 'mu4e)
;; default
(setq mu4e-maildir (expand-file-name "~/.config/mail/"))

(setq mu4e-drafts-folder "/Drafts")
(setq mu4e-sent-folder   "/Sent")
(setq mu4e-trash-folder  "/trash")
(setq mu4e-sent-messages-behavior 'delete)
;; setup some handy shortcuts
(setq mu4e-maildir-shortcuts
      '(("/Inbox"             . ?i)
	("/Sent" . ?s)
	("/trash"     . ?t)))
(setq mu4e-get-mail-command "fetchmail")
;; hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Added by Customize menu
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(electric-pair-mode t)
 '(package-selected-packages
   (quote
    (web-mode evil-vimish-fold vimish-fold magit ac-php vterm linum-relative shell-script-mode use-package markdown-mode evil-leader php-mode auto-complete evil-escape undo-tree evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
