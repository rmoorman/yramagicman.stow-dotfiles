;;; package --- custom.el
;;; Commentary:
;;; a comment
;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-idle-delay 0.0)
 '(company-minimum-prefix-length 1)
 '(custom-file "~/.config/emacs/custom.el")
 '(custom-safe-themes
   '("50d07ab55e2b5322b2a8b13bc15ddf76d7f5985268833762c500a90e2a09e7aa" default))
 '(flycheck-typescript-tslint-executable "~/.local/bin/tslint")
 '(inhibit-startup-screen t)
 '(lsp-ui-doc-position 'bottom)
 '(package-selected-packages
   '(company which-key lsp-ui company-box haskell-mode lsp-mode base16-theme magit flycheck typescript-mode web-mode linum-relative racket-mode markdown-mode ivy lua-mode ac-php auto-complete evil-surround evil-commentary general evil-escape evil-collection evil projectile use-package))
 '(projectile-enable-caching t)
 '(tab-width 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:extend t :background "#3a3a3a"))))
 '(mode-line ((t (:background "black" :foreground "gainsboro" :box (:line-width -1 :style released-button)))))
 '(mode-line-inactive ((t (:inherit mode-line :background "black" :foreground "dim gray" :box (:line-width -1 :color "grey40") :weight light)))))
(provide 'custom.el)
;;; custom.el ends here
