(evil-mode 1)
(require 'evil-surround)
(global-evil-surround-mode 1)
(global-evil-visualstar-mode)
(defun evil-map-key (key-str fn-quoted)
    "Map key for both insert and normal modes KEY-STR FN-QUOTED."
    (define-key evil-insert-state-map  (kbd key-str) fn-quoted)
    (define-key evil-normal-state-map  (kbd key-str) fn-quoted)
    (define-key evil-replace-state-map (kbd key-str) fn-quoted)
    (define-key evil-visual-state-map  (kbd key-str) fn-quoted))

(define-key evil-visual-state-map  (kbd "TAB") 'evilmi-jump-items)
(define-key evil-normal-state-map  (kbd "TAB") 'evilmi-jump-items)
(define-key evil-normal-state-map  (kbd "SPC SPC") 'save-file)
(evil-map-key "C-S-z" 'evil-exit-emacs-state)
(evil-map-key "C-M-z" 'evil-emacs-state)
(evil-map-key "M-c" 'evil-force-normal-state)
(evil-map-key "M-s" 'save-buffer)
(evil-map-key "C-q" 'kill-emacs)
(evil-map-key "M-q" 'kill-emacs)
(evil-map-key "C-s" 'save-buffer)
(evil-map-key "C-n" 'evil-normal-state)
(evil-map-key "M-n" 'evil-normal-state)
(evil-map-key "C-S-e" 'eval-buffer)
(evil-map-key "C-h" 'evil-window-left)
(evil-map-key "C-j" 'evil-window-down)
(evil-map-key "C-k" 'evil-window-up)
(evil-map-key "C-l" 'evil-window-right)
(define-key evil-normal-state-map (kbd "Q") 'fill-paragraph)

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
(evil-map-key "C-a" 'evil-numbers/inc-at-pt)
(evil-map-key "C-x" 'evil-numbers/dec-at-pt)
(evil-map-key "C-+" 'evil-font-increase)
(evil-map-key "C-=" 'evil-font-increase)
(evil-map-key "C--" 'evil-font-decrease)
(evil-map-key "C-0" 'evil-font-reset)

(require 'general)



(require 'evil-escape)
(setq-default evil-escape-key-sequence "  ")
(setq evil-escape-unordered-key-sequence t)
(evil-escape-mode)
(setq-default evil-escape-delay 0.4)




(provide 'evil-settings)
;;; evil-settings.el ends here
