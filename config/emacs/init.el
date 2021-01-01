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
      visible-bell 1
      indent-line-function 'insert-tab)

(eval-when-compile
  (require 'use-package))

(require 'use-package-ensure)

(setq use-package-always-ensure t)
(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(setq custom-file(concat user-emacs-directory "custom.el"))
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


(setq-default ispell-program-name (executable-find "hunspell"))

(setq create-lockfiles nil)
(setq vc-follow-symlinks t)

(load "my_functions.el")
(load "my_modeline.el")
(load "my_mu4e.el")
(load "my_evil.el")
(load "my_keys.el")
(load "my_completion.el")
(load "my_util.el")
(load "my_filetypes.el")
(load "my_hooks.el")

(global-hl-line-mode t)

(server-mode)

(global-auto-revert-mode t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:extend t :background "#3a3a3a")))))

(provide 'init.el)
;; init.el ends here
