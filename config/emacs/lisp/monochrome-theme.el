;;; monochrome-theme.el --- Custom face theme for Emacs

;; Copyright (C) 2011-2021 Free Software Foundation, Inc.

;; Author: Kristoffer Grönlund <krig@koru.se>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(deftheme monochrome
  "Medium-contrast faces with a dark gray background.
Adapted, with permission, from a Vim color scheme by Lars H. Nielsen.
Basic, Font Lock, Isearch, Gnus, Message, and Ansi-Color faces
are included.")

(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   'monochrome
   `(default ((,class (:background "#000000" :foreground "#adadad"))))
   `(cursor ((,class (:background "#656565" :foreground "#ffffff"))))
   ;; Highlighting faces
   `(fringe ((,class (:background "#000000"))))
   `(highlight ((,class (:background "#222222" :foreground "#ffffff"
                                     :underline t))))
   `(region ((,class (:background "#222222" :foreground "#ffffff"))))
   `(secondary-selection ((,class (:background "#212121" :foreground "#ffffff"))))
   `(isearch ((,class (:background "#343434" :foreground "#ffffff"))))
   `(lazy-highlight ((,class (:background "#222222" :foreground "#ffffff"))))
   ;; Mode line faces
   `(mode-line ((,class (:background "#444444" :foreground "#eeeeee"))))
   `(mode-line-inactive ((,class (:background "#444444" :foreground "#857b6f"))))
   ;; Escape and prompt faces
   `(minibuffer-prompt ((,class (:foreground "#adadad"))))
   `(escape-glyph ((,class (:foreground "#adadad" :weight bold))))
   `(homoglyph ((,class (:foreground "#adadad" :weight bold))))
   ;; Font lock faces
   `(font-lock-builtin-face ((,class (:foreground "#adadad"))))
   `(font-lock-comment-face ((,class (:foreground "#555555"))))
   `(font-lock-constant-face ((,class (:foreground "#adadad"))))
   `(font-lock-function-name-face ((,class (:foreground "#adadad"))))
   `(font-lock-keyword-face ((,class (:foreground "#adadad"))))
   `(font-lock-string-face ((,class (:foreground "#adadad"))))
   `(font-lock-type-face ((,class (:foreground "#adadad" :weight bold))))
   `(font-lock-variable-name-face ((,class (:foreground "#adadad"))))
   `(font-lock-warning-face ((,class (:foreground "#adadad"))))
   ;; Button and link faces
   `(link ((,class (:foreground "#adadad" :underline t))))
   `(link-visited ((,class (:foreground "#adadad" :underline t))))
   `(button ((,class (:background "#333333" :foreground "#adadad"))))
   `(header-line ((,class (:background "#303030" :foreground "#adadad"))))
   ;; Gnus faces
   `(gnus-group-news-1 ((,class (:weight bold :foreground "#adadad"))))
   `(gnus-group-news-1-low ((,class (:foreground "#adadad"))))
   `(gnus-group-news-2 ((,class (:weight bold :foreground "#adadad"))))
   `(gnus-group-news-2-low ((,class (:foreground "#adadad"))))
   `(gnus-group-news-3 ((,class (:weight bold :foreground "#adadad"))))
   `(gnus-group-news-3-low ((,class (:foreground "#adadad"))))
   `(gnus-group-news-4 ((,class (:weight bold :foreground "#adadad"))))
   `(gnus-group-news-4-low ((,class (:foreground "#adadad"))))
   `(gnus-group-news-5 ((,class (:weight bold :foreground "#adadad"))))
   `(gnus-group-news-5-low ((,class (:foreground "#adadad"))))
   `(gnus-group-news-low ((,class (:foreground "#adadad"))))
   `(gnus-group-mail-1 ((,class (:weight bold :foreground "#adadad"))))
   `(gnus-group-mail-1-low ((,class (:foreground "#adadad"))))
   `(gnus-group-mail-2 ((,class (:weight bold :foreground "#adadad"))))
   `(gnus-group-mail-2-low ((,class (:foreground "#adadad"))))
   `(gnus-group-mail-3 ((,class (:weight bold :foreground "#adadad"))))
   `(gnus-group-mail-3-low ((,class (:foreground "#adadad"))))
   `(gnus-group-mail-low ((,class (:foreground "#adadad"))))
   `(gnus-header-content ((,class (:foreground "#adadad"))))
   `(gnus-header-from ((,class (:weight bold :foreground "#adadad"))))
   `(gnus-header-subject ((,class (:foreground "#adadad"))))
   `(gnus-header-name ((,class (:foreground "#adadad"))))
   `(gnus-header-newsgroups ((,class (:foreground "#adadad"))))
   ;; Message faces
   `(message-header-name ((,class (:foreground "#adadad" :weight bold))))
   `(message-header-cc ((,class (:foreground "#adadad"))))
   `(message-header-other ((,class (:foreground "#adadad"))))
   `(message-header-subject ((,class (:foreground "#adadad"))))
   `(message-header-to ((,class (:foreground "#adadad"))))
   `(message-cited-text ((,class (:foreground "#adadad"))))
   `(message-separator ((,class (:foreground "#adadad" :weight bold))))

   '(racket-keyword-argument-face ((t (:foreground "#adadad"))))
   '(racket-selfeval-face ((t (:foreground "#adadad"))))))

(custom-theme-set-variables
 'monochrome
 '(ansi-color-names-vector [
                            "#242424"
                            "#e5786d"
                            "#95e454"
                            "#cae682"
                            "#8ac6f2"
                            "#333366"
                            "#ccaa8f"
                            "#f6f3e8"
                            ]))

(provide-theme 'monochrome)

;;; monochrome-theme.el ends here
