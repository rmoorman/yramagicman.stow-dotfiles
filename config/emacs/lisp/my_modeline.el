;;; package -- Summary
;;; Commentary:
;;; Code:


(setq-default mode-line-format (list
                                " "
                                mode-line-client
                                '(:eval
                                  (propertize
                                   (cond ((eq 'emacs evil-state) " EMACS " )
                                         ((eq 'normal evil-state) " NORMAL ")
                                         ((eq 'visual evil-state) " VISUAL ")
                                         ((eq 'insert evil-state) " INSERT ")
                                         (t " U " )
                                         )))
                                " %& "
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
                                " L:%l "

                                ))

(force-mode-line-update)
(provide 'my_modeline)
;;; modeline.el ends here
