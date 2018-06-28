(evil-mode 1)
(require 'evil-surround)
(global-evil-surround-mode 1)
(global-evil-visualstar-mode)
(require 'evil-commentary)
(evil-commentary-mode)
(defun evil-map-key (key-str fn-quoted)
    "Map key for both insert and normal modes KEY-STR FN-QUOTED."
    (define-key evil-insert-state-map  (kbd key-str) fn-quoted)
    (define-key evil-normal-state-map  (kbd key-str) fn-quoted)
    (define-key evil-replace-state-map (kbd key-str) fn-quoted)
    (define-key evil-visual-state-map  (kbd key-str) fn-quoted))

(define-key evil-visual-state-map  (kbd "TAB") 'evilmi-jump-items)
(define-key evil-normal-state-map  (kbd "TAB") 'evilmi-jump-items)
(define-key evil-normal-state-map (kbd "Q") 'fill-paragraph)
(define-key evil-normal-state-map (kbd ", ,") 'evil-switch-to-windows-last-buffer)
(define-key evil-normal-state-map  (kbd "SPC SPC") 'save-file)
(define-key evil-normal-state-map (kbd "C-u") (lambda ()
                    (interactive)
                    (evil-scroll-up nil)))
(define-key evil-normal-state-map (kbd "C-d") (lambda ()
                        (interactive)
                        (evil-scroll-down nil)))

(defun evil-font-increase nil
  (interactive)
  (text-scale-adjust 0.5))

(defun evil-font-decrease nil
  (interactive)
  (text-scale-adjust -0.5))

(defun evil-font-reset nil
  (interactive)
  (text-scale-set 0))

(require 'evil-numbers)
(define-key evil-normal-state-map ( kbd "C-a" ) 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map ( kbd "C-x" ) 'evil-numbers/dec-at-pt)
(global-set-key (kbd "C-x") 'evil-numbers/dec-at-pt)
(define-key evil-normal-state-map ( kbd "C-+" ) 'evil-font-increase)
(define-key evil-normal-state-map ( kbd "C-=" ) 'evil-font-increase)
(define-key evil-normal-state-map ( kbd "C--" ) 'evil-font-decrease)
(define-key evil-normal-state-map ( kbd "C-0" ) 'evil-font-reset)

(require 'evil-magit)
(evil-magit-init)

(require 'evil-escape)
(setq-default evil-escape-key-sequence "  ")
(setq evil-escape-unordered-key-sequence t)
(setq-default evil-escape-delay 0.4)
(evil-escape-mode)

(provide 'evil-settings)
;;; evil-settings.el ends here
