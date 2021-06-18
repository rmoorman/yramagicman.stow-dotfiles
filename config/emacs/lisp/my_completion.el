;;; package -- Summary
;;; Commentary:
;;; Code:


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

;; (use-package company-box
;;   :hook (company-mode . company-box-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  ;; :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-headerline-breadcrumb-enable nil))

;; (use-package lsp-ui
;;   :hook (lsp-mode . lsp-ui-mode)
;;   :custom
;;   (lsp-ui-doc-position 'bottom))

(use-package php-mode
  :mode "\\.php\\'"
  :interpreter "php"
  :hook (php-mode . lsp-deferred))

;; (use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))
;; (setenv "JAVA_HOME"  "path_to_java_folder/Contents/Home/")
;; (setq lsp-java-java-path "path_to_java_folder/Contents/Hom
(provide 'my_completion)
;;; modeline.el ends here
