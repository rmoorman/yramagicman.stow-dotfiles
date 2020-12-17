;;; package -- Summary
;;; Commentary:
;;; Code:


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
                                " L:%l C:%C "

                                ))

(force-mode-line-update)
(provide 'modeline)
;;; modeline.el ends here
