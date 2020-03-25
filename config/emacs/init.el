;;; package --- .emacs
;;; Commentary:
;;; a comment
;;; Code:

;; Hide mouse interface quickly
(if (and (fboundp 'menu-bar-mode)
         (not (string= "darwin" system-type)))
    (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq ring-bell-function 'ignore)
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

(use-package evil-commentary
  :config
  (evil-commentary-mode 1))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

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


(use-package ivy
  :config
  (ivy-mode 1))
;; keybinds
(global-set-key (kbd "C-c b") 'buffer-menu)
(global-set-key (kbd "C-c t") 'vterm)
(global-set-key (kbd "C-c e") 'mu4e)

;; Modes
(use-package markdown-mode
  :mode "\\.md\\'")
(use-package racket-mode
  :mode "\\.rkt\\'")
(use-package linum-relative
  :config
  (linum-relative-global-mode))
(use-package web-mode
  :mode "\\.blade.php|.component.html|.vue|.html\\'")

;; typescript
(use-package typescript-mode
  :mode "\\.ts\\'")
;; (use-package tss
;;   :mode "\\.ts")

(use-package nroff-mode
  :mode "\\.mom\\'")

(use-package flycheck
  :config
  (custom-set-variables
   '(flycheck-typescript-tslint-executable "~/.local/bin/tslint"))
  (global-flycheck-mode))


;; misc. packages
(use-package magit)
(use-package vterm)
(show-paren-mode 1)

(use-package base16-theme)
;; Mu4e
(if (file-exists-p "/usr/share/emacs/site-lisp/mu4e/")
    (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e/")
  (message "mu4e not found in /usr/share/emacs/site-lisp/mu4e/"))

(if (file-exists-p "/usr/local/Cellar/mu/1.2.0_1/share/emacs/site-lisp/mu/mu4e" )
    (add-to-list 'load-path "/usr/local/Cellar/mu/1.2.0_1/share/emacs/site-lisp/mu/mu4e" )
  (message "mu4e not found in /usr/local/Cellar/mu/1.2.0_1/share/emacs/site-lisp/mu/mu4e"))

(require 'mu4e)
(setq mu4e-maildir (expand-file-name "~/.config/mail/"))

(setq mu4e-mu-binary (executable-find "mu"))
(setq mu4e-drafts-folder "/Drafts")
(setq mu4e-sent-folder   "/Sent")
(setq mu4e-trash-folder  "/trash")
(setq mu4e-sent-messages-behavior 'delete)
(setq mu4e-get-mail-command "getmail -n && getmail -r gmail -n -d ")
(setq user-mail-address "jonathandavis@gilsons.org"
      user-full-name "Jonathan")
(setq message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "/bin/msmtp")

;; hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook (lambda nil "" (untabify (point-min) (point-max) )))

;; settings
(setq vc-follow-symlinks t)
(setq make-backup-files nil)

;; Added by Customize menu
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(base16-ocean))
 '(custom-safe-themes
   '("78c1c89192e172436dbf892bd90562bc89e2cc3811b5f9506226e735a953a9c6" default))
 '(electric-pair-mode t)
 '(exec-path
   '("/usr/local/bin" "/usr/bin" "/bin" "/usr/sbin" "/sbin" "/Applications/Emacs.app/Contents/MacOS/bin-x86_64-10_14" "/Applications/Emacs.app/Contents/MacOS/libexec-x86_64-10_14" "/Applications/Emacs.app/Contents/MacOS/libexec" "/Applications/Emacs.app/Contents/MacOS/bin"))
 '(flycheck-typescript-tslint-executable "~/.local/bin/tslint")
 '(package-selected-packages
   '(tss flycheck ivy typescript-mode evil-surround evil-commentary web-mode evil-vimish-fold vimish-fold magit ac-php vterm linum-relative shell-script-mode use-package markdown-mode evil-leader php-mode auto-complete evil-escape undo-tree evil))
 '(tab-width 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
