;; package --- init.el
;;; Commentary:
;;; a comment
;;; Code:

;; Hide mouse interface quickly
(if (and (fboundp 'menu-bar-mode)
         (not (string= "darwin" system-type)))
    (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq custom-file "~/Documents/dots/config/emacs/custom.el")
(load custom-file)

(setq ring-bell-function 'ignore)
;; Package initialization
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'package)
(defvar gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; use-package bootstrap and init
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(defun ttymode nil
  "Set tty things."
  (interactive)
  (when (not ( display-graphic-p ) )
    (disable-theme 'base16-gruvbox-dark-pale)

    (display-line-numbers-mode 0)
    (linum-relative-mode 0)
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

(eval-when-compile
  (require 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package projectile
  :config
  (setq projectile-completion-system 'ivy)
  (setq projectile-enable-caching t)
  (projectile-mode +1))

;; Evil config
;; evil-collection is picky and doesn't like this being set
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  (setq evil-vsplit-window-right 1)
  (setq evil-split-window-below 1)
  (add-hook 'evil-normal-state-entry-hook (lambda ()
                                            (if buffer-file-name
                                                (when
                                                    (file-writable-p buffer-file-name)
                                                  (save-buffer)))
                                            (lambda() nil))))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))


(use-package evil-escape
  :init
  (setq-default evil-escape-key-sequence "  ")
  (setq-default evil-escape-delay 0.2)
  :config
  (evil-escape-mode 1))

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
    "f" 'projectile-find-file))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package evil-commentary
  :config
  (evil-commentary-mode 1))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  ;; :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  )

(use-package lsp-mode
  :after (lsp-mode ivy))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package php-mode
  :mode "\\.php\\'"
  :interpreter "php"
  :hook (php-mode . lsp-deferred))


(use-package lua-mode
  :mode "\\.lua\\'")

(use-package ivy
  :config
  (ivy-mode 1))

;; keybind
(global-set-key (kbd "C-c b") 'buffer-menu)
(global-set-key (kbd "C-c t") 'ttymode)
(global-set-key (kbd "C-c e") 'mu4e)

;; Modes
(use-package markdown-mode
  :mode "\\.md\\'"
  :hook (markdown-mode . (lambda nil
                           ( visual-line-mode )
                           (linum-relative-mode 0)
                           ( display-line-numbers-mode 0))))


(use-package racket-mode
  :mode "\\.rkt\\'")

(use-package linum-relative
  :config
  (linum-relative-global-mode))

(use-package web-mode
  :hook (web-mode . lsp-deferred)
  :mode "\\.\\(vue\\|html\\|blade.php\\)\\'")

(use-package haskell-mode
  :mode "\\.hs\\'")

;; typescript
(use-package typescript-mode
  :hook (typescript-mode . lsp-deferred)
  :mode "\\.\\(ts\\|js\\)\\'")

(use-package nroff-mode
  :mode "\\.mom\\'")

(use-package flycheck
  :config
  (setq flycheck-typescript-tslint-executable "~/.local/bin/tslint")
  (global-flycheck-mode))

;; misc. packages
(use-package magit)
;; (use-package vterm)
(show-paren-mode 1)

(use-package base16-theme)

;; Mu4e
(if (file-exists-p "/usr/share/emacs/site-lisp/mu4e/")
    (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e/")
  (message "mu4e not found in /usr/share/emacs/site-lisp/mu4e/"))

(if (file-exists-p "/usr/local/Cellar/mu/1.4.5/share/emacs/site-lisp/mu/mu4e" )
    (add-to-list 'load-path "/usr/local/Cellar/mu/1.4.5/share/emacs/site-lisp/mu/mu4e" )
  (message "mu4e not found in /usr/local/Cellar/mu/1.4.5/share/emacs/site-lisp/mu/mu4e"))

(defvar mu4e-drafts-folder "/Drafts")
(defvar mu4e-sent-folder   "/Sent")
(defvar mu4e-trash-folder  "/Trash")
(defvar mu4e-get-mail-command (expand-file-name "~/.local/bin/getallmail") )
(defvar sendmail-program (expand-file-name "~/.local/bin/getallmail") )
(add-hook 'after-init-hook
          (lambda nil ""
            (require 'mu4e)
            (defvar mu4e-maildir (expand-file-name "~/.config/mail/"))

            (defvar mu4e-mu-binary (executable-find "mu"))
            (setq mu4e-drafts-folder "/Drafts")
            (setq mu4e-sent-folder   "/Sent")
            (setq mu4e-trash-folder  "/Trash")
            (setq mu4e-get-mail-command (expand-file-name "~/.local/bin/getallmail") )
            (setq user-mail-address "jonathandavis@gilsons.org"
                  user-full-name "Jonathan")
            (setq message-send-mail-function 'message-send-mail-with-sendmail
                  sendmail-program "/bin/msmtp")))
;; hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook (lambda nil "" (untabify (point-min) (point-max))))

(add-hook 'kill-buffer-hook (lambda nil ""
                              (if (file-directory-p "~/intelephense")
                                  (delete-directory "~/intelephense")
                                nil)))
;; quit-window-hook add remove file dir command
(dolist (mode '(org-mode-hook
                term-mode-hook
                ansi-term-mode
                apropos-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda ()
                   (linum-relative-mode 0)
                   (display-line-numbers-mode 0))))

(setq vc-follow-symlinks t)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(global-hl-line-mode t)

(server-mode)

;; Save all tempfiles in $TMPDIR/emacs$UID/
(defconst emacs-tmp-dir (expand-file-name (format "emacs%d" (user-uid)) temporary-file-directory))
(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
      emacs-tmp-dir)

;; (add-hook 'focus-in-hook
;;           (lambda nil
;;             (when ( display-graphic-p )
;;               (enable-theme 'base16-gruvbox-dark-pale))))



(add-hook 'find-file-hook 'ttymode)
(add-hook 'tty-setup-hook 'ttymode)

(provide 'init.el)
;;; init.el ends here
