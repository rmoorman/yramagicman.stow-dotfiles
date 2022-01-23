;; package --- init.el
;;; Commentary:
;;; a comment
;;; Code:
(use-package org
  :hook
  (org-mode . (lambda nil
                (visual-line-mode))))

(use-package lua-mode
  :mode "\\.lua\\'")


;; Modes
(use-package markdown-mode
  :mode "\\.md\\'"
  :hook (markdown-mode . (lambda nil
                           ( visual-line-mode )
                           ;; (linum-relative-mode 0)
                           )))

(use-package racket-mode
  :mode "\\.rkt\\'")

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

(provide 'my_filetypes.el)
;; my_filetypes.el ends here
