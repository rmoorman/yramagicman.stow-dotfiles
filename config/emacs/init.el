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

(setq inhibit-startup-screen t
      ;; ring-bell-function 'ignore
      visible-bell 1)

(eval-when-compile
  (require 'use-package))

(require 'use-package-ensure)

(setq use-package-always-ensure t)
(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "lisp"))
(setq custom-file (concat user-emacs-directory "custom.el"))

(load custom-file)

(use-package no-littering)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(setq-default indent-tabs-mode nil
              c-default-style "k&r"
              tab-width 4
              c-basic-offset 4)

(set-default 'truncate-lines t)

(setq-default ispell-program-name (executable-find "hunspell"))

(setq create-lockfiles nil)
(setq vc-follow-symlinks t)

(use-package projectile
  :config
  (setq projectile-completion-system 'ivy
        projectile-enable-caching nil
        projectile-file-exists-local-cache-expire (* 5 60)
        projectile-indexing-method 'alien)
  (projectile-mode +1))

;; misc. packages
(use-package magit)
(use-package disable-mouse)
;; (use-package vterm)
(show-paren-mode 1)
(global-hl-line-mode t)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)
(use-package base16-theme)

(electric-indent-mode nil)

(setq-default electric-indent-inhibit t)
(setq backward-delete-char-untabify-method 'hungry)

(add-hook 'after-init-hook
          (lambda nil
            (load "server")
            (unless (server-running-p) (server-start))))

(global-auto-revert-mode t)

(use-package auto-package-update
  :custom
  (auto-package-update-interval 30)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

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

;; ***** modeline *****
(setq-default mode-line-format (list
                                " "
                                mode-line-client
                                '(:eval
                                  (propertize
                                   (cond ((eq 'emacs evil-state) " E " )
                                         ((eq 'normal evil-state) " N ")
                                         ((eq 'visual evil-state) " V ")
                                         ((eq 'insert evil-state) " I ")
                                         (t " U " )
                                         )))
                                " %*%* "
                                vc-mode
                                " %m "
                                '(:eval
                                  (propertize
                                   (if (buffer-file-name)
                                       (mapconcat 'identity (nthcdr 4 (split-string (buffer-file-name) "/")) "/")
                                     " ")))
                                " "
                                mode-line-percent-position
                                " "
                                " L:%0l "
                                mode-line-modes
                                ))
(force-mode-line-update)
;; (load "my_mu4e.el")

(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  (setq evil-vsplit-window-right 1
        evil-split-window-below 1)
  (add-hook 'evil-normal-state-entry-hook (lambda () (save-some-buffers 1))))

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

(use-package general
  :config
  (general-create-definer my-space-def
    ;; :prefix my-space
    :prefix "SPC")

  ;; (general-create-definer my-leader-def
  ;;   ;; :prefix my-leader
  ;;   :prefix ",")

  (my-space-def
    :states '( normal )
    :keymaps 'override
    "s"  'save-buffer
    "d"  'dired
    "D"  'dired-other-frame
    ","  'evil-switch-to-windows-last-buffer
    "b"  'ivy-switch-buffer
    "o"  'ido-switch-buffer-other-frame
    "B"  'ibuffer
    "e"  'eval-defun
    "t"  'ansiterm-vert
    "f"  'projectile-find-file
    "p"  'projectile-switch-project
    "n"  'flymake-goto-next-error
    "gs" 'magit-status
    "gc" 'magit-checkout
    "jr" 'lsp-find-references
    "jd" 'lsp-find-definition
    "w"  'visual-line-mode
    "F"  'find-file)

  ;; (my-leader-def
  ;;   :states '( normal visual )
  ;;   :keymaps 'override
  ;;   "d" 'dired
  ;;   "," 'evil-switch-to-windows-last-buffer
  ;;   "b" 'ivy-switch-buffer
  ;;   "B" 'ibuffer
  ;;   "e" 'eval-defun
  ;;   "t" 'ansiterm-vert
  ;;   "f" 'projectile-find-file
  ;;   "F" 'find-file
  ;;   )

  (general-define-key
   :states '(normal visual)
   :keymaps 'override
   "C-a" 'evil-numbers/inc-at-pt
   "C-x" 'evil-numbers/dec-at-pt
   "C-c w" 'toggle-truncate-lines
   "C-+" 'text-scale-increase
   "C--" 'text-scale-decrease
   "C-0" 'text-scale-adjust
   "C-w C-f" 'toggle-frame-fullscreen
   "C-w t" 'ansiterm-vert))

;; keybind
(global-set-key (kbd "C-c b") 'buffer-menu)
(global-set-key (kbd "C-c t") 'ttymode)
(global-set-key (kbd "C-c f") 'set-x-faces)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("<tab>" . company-complete-selection))
  :custom
  (company-minimum-prefix-length 3)
  (company-idle-delay 0.0))

;; (use-package company-box
;;   :hook (company-mode . company-box-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  ;; :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-file-watch-threshold nil)
  (setq lsp-headerline-breadcrumb-enable nil))

;; (use-package lsp-ui
;;   :hook (lsp-mode . lsp-ui-mode)
;;   :custom
;;   (lsp-ui-doc-position 'bottom))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package ivy
  :config
  (ivy-mode 1))

;; (use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))
;; (setenv "JAVA_HOME"  "path_to_java_folder/Contents/Home/")
;; (setq lsp-java-java-path "path_to_java_folder/Contents/Home/")

(use-package org)
                                        ; :hook
                                        ; (org-mode . (lambda nil
                                        ;               (visual-line-mode))))

(use-package lua-mode
  :mode "\\.lua\\'")

;; Modes
(use-package markdown-mode
  :mode "\\.md\\'"
  :hook (markdown-mode . (lambda nil
                           ( visual-line-mode )
                           ;; (linum-relative-mode 0)
                           )))

(use-package php-mode
  :mode "\\.\\(php\\|inc\\|tpl\\)\\'"
  :interpreter "php"
  :hook (php-mode . lsp-deferred))

(use-package racket-mode
  :mode "\\.rkt\\'"
  :interpreter "racket"
  :hook (racket-mode . lsp-deferred))

(use-package web-mode
  :hook (web-mode . lsp-deferred)
  :mode "\\.\\(vue\\|html\\|blade.php\\)\\'"
  :init
  (setq web-mode-style-padding 0)
  (setq web-mode-script-padding 0))

(use-package haskell-mode
  :mode "\\.hs\\'")

(use-package typescript-mode
  :hook (typescript-mode . lsp-deferred)
  :mode "\\.\\(ts\\|js\\)\\'")

(use-package nroff-mode
  :mode "\\.mom\\'")

(use-package rust-mode
  :mode "\\.rs\\'")

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package dockerfile-mode
  :mode "\\Dockerfile|dockerfile\\'")

(use-package highlight-indent-guides
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-character "|")
  )

;; hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook (lambda nil "" (untabify (point-min) (point-max))))

(add-hook 'kill-buffer-hook (lambda nil ""
                              (if (file-directory-p "~/intelephense")
                                  (delete-directory "~/intelephense")
                                nil)))

(add-hook 'find-file-hook 'ttymode)
(add-hook 'tty-setup-hook 'ttymode)
(add-hook 'minibuffer-exit-hook (lambda ()
                                  (ttymode)
                                  (save-some-buffers 1)))
;; (dolist (mode '(
;;                 ansi-term-mode
;;                 apropos-mode-hook
;;                 dired-mode-hook
;;                 eshell-mode-hook
;;                 markdown-mode-hook
;;                 org-mode-hook
;;                 shell-mode-hook
;;                 term-mode-hook
;;                 ))
;;   (add-hook mode (lambda ()
;;                    (display-line-numbers-mode 0))))
;; (global-display-line-numbers-mode)
;; (add-hook message-mode-hook 'visual-line-mode)

(defvar my/default-font-size 110)
(defvar my/default-variable-font-size 110)
(defun my/set-font-faces ()
  (interactive)
  (set-face-attribute 'default nil :font "Fira Code" :height my/default-font-size)

  ;; Set the fixed pitch face
  (set-face-attribute 'fixed-pitch nil :font "Fira Code" :height my/default-font-size)

  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil :font "Cantarell" :height my/default-variable-font-size :weight 'regular))

(defun fix-colors ()
  (when (and (frame-focus-state) (display-graphic-p))
    (set-face-attribute-from-resource 'fringe :background "background" "background" nil)
    (set-face-attribute-from-resource 'mode-line-inactive :background "background" "background" nil)
    (set-face-attribute-from-resource 'mode-line-inactive :foreground "foreground" "foreground" nil)
    (set-face-attribute-from-resource 'mode-line :background "background" "color8" nil)
    (set-face-attribute-from-resource 'mode-line :foreground "foreground" "color7" nil)
    (set-face-attribute `mode-line-inactive nil :box nil )
    (set-face-attribute `mode-line nil :box nil )))
                                        ; (set-face-attribute-from-resource 'hl-line :background "background" "color8" nil)))

(defun set-x-faces ()
  (interactive)
  (my/set-font-faces)
  (fix-colors))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (my/set-font-faces)))))

(defun save-on-focus-lost ()
  (if (not (frame-focus-state))
      (save-some-buffers 1)
    nil))
(add-function :after after-focus-change-function #'save-on-focus-lost)

(add-function :after after-focus-change-function #'set-x-faces)
(set-x-faces)

(load-theme 'base16-atelier-cave)
(setq initial-buffer-choice "~/.config/emacs/start.org")
(provide 'init.el)
;; init.el ends here
