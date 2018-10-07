;;; awesome-tray.el ---  Modular tray bar

;; Filename: awesome-tray.el
;; Description: Modular tray bar
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-10-07 07:30:16
;; Version: 0.2
;; Last-Updated: 2018-10-07 10:50:19
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/awesome-tray.el
;; Keywords:
;; Compatibility: GNU Emacs 27.0.50
;;
;; Features that might be required by this library:
;;
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Modular tray bar.
;;
;; I don't like mode-line, it's too high, affect me to read the code.
;; With Emacs, we only need to focus on very little information, such as time, current mode, git branch.
;; Excessive information can seriously interfere with our attention.
;;

;;; Installation:
;;
;; Put awesome-tray.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'awesome-tray)
;; (awesome-tray-mode 1)
;;
;; No need more.

;;; Customize:
;;
;; `awesome-tray-mode-line-active-color'
;; `awesome-tray-mode-line-inactive-color'
;; `awesome-tray-info-face'
;;
;; All of the above can customize by:
;;      M-x customize-group RET awesome-tray RET
;;

;;; Change log:
;;
;; 2018/10/07
;;      * First released.
;;      * Add row/column information.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require


;;; Code:
(defgroup awesome-tray nil
  "Modular tray bar."
  :group 'awesome-tray)

(defcustom awesome-tray-mode-line-active-color "DarkRed"
  "Active color."
  :type 'string
  :group 'awesome-tray)

(defcustom awesome-tray-mode-line-inactive-color "Gray10"
  "Inactive color."
  :type 'string
  :group 'awesome-tray)

(defface awesome-tray-info-face
  '((t (:foreground "gray30" :bold t)))
  "Face tray info."
  :group 'awesome-tray)

(define-minor-mode awesome-tray-mode
  "Modular tray bar."
  :require 'awesome-tray-mode
  :global t
  (if awesome-tray-mode
      (awesome-tray-enable)
    (awesome-tray-disable)))

(defvar awesome-tray-mode-line-colors)

(defvar awesome-tray-timer)

(defun awesome-tray-enable ()
  ;; Save mode-line colors.
  (setq awesome-tray-mode-line-colors
        (list (face-attribute 'mode-line :foreground)
              (face-attribute 'mode-line :background)
              (face-attribute 'mode-line :box)
              (face-attribute 'mode-line-inactive :foreground)
              (face-attribute 'mode-line-inactive :background)
              (face-attribute 'mode-line-inactive :box)
              ))
  ;; Disable mode line.
  (set-face-attribute 'mode-line nil
                      :foreground awesome-tray-mode-line-active-color
                      :background awesome-tray-mode-line-active-color
                      :height 0.1
                      :box nil)
  (set-face-attribute 'mode-line-inactive nil
                      :foreground awesome-tray-mode-line-inactive-color
                      :background awesome-tray-mode-line-inactive-color
                      :height 0.1
                      :box nil)
  ;; Add update timer.
  (setq awesome-tray-timer
        (run-with-timer 0 0.5 'awesome-tray-show-info))
  (add-hook 'focus-in-hook 'awesome-tray-show-info)
  ;; Notify user.
  (message "Enable awesome tray."))

(defun awesome-tray-disable ()
  ;; Restore mode-line colors.
  (set-face-attribute 'mode-line nil
                      :foreground (nth 0 awesome-tray-mode-line-colors)
                      :background (nth 1 awesome-tray-mode-line-colors)
                      :box (nth 2 awesome-tray-mode-line-colors)
                      :height 1)
  (set-face-attribute 'mode-line-inactive nil
                      :foreground (nth 3 awesome-tray-mode-line-colors)
                      :background (nth 4 awesome-tray-mode-line-colors)
                      :box (nth 5 awesome-tray-mode-line-colors)
                      :height 1)
  (setq awesome-tray-mode-line-colors nil)
  ;; Cancel timer.
  (cancel-timer awesome-tray-timer)
  (remove-hook 'focus-in-hook 'awesome-tray-timer)
  ;; Update mode-line.
  (force-mode-line-update)
  (redraw-display)
  (with-current-buffer " *Minibuf-0*"
    (erase-buffer))
  ;; Notify user.
  (message "Disable awesome tray."))

(defun awesome-tray-show-info ()
  (unless (current-message)
    (let* ((tray-info (awesom-tray-build-info))
           (tray-info-width (length tray-info))
           (tray-info-padding-right 2)
           (minibuffer-width (window-width)))
      (with-current-buffer " *Minibuf-0*"
        (erase-buffer)
        (put-text-property 0 (length tray-info) 'face 'awesome-tray-info-face tray-info)
        (insert (format "%s %s" (make-string (- minibuffer-width tray-info-width tray-info-padding-right) ?\ ) tray-info))
        ))))

(defun awesom-tray-build-info ()
  (let ((info ""))
    (mapcar '(lambda (i) (setq info (format " %s %s" info i)))
            (list
             ;; Git branch.
             (if (fboundp 'magit-get-current-branch)
                 (let ((branch (magit-get-current-branch)))
                   (if branch
                       (format "Git:%s" branch)
                     ""))
               "")
             ;; Current mode.
             major-mode
             ;; Location.
             (format "(%s:%s)" (line-number-at-pos) (current-column))
             ;; Date.
             (format-time-string "[%Y-%m-%d %H:%M]")))
    info))

(provide 'awesome-tray)

;;; awesome-tray.el ends here
