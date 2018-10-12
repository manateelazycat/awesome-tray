;;; awesome-tray.el ---  Modular tray bar

;; Filename: awesome-tray.el
;; Description: Modular tray bar
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-10-07 07:30:16
;; Version: 1.0
;; Last-Updated: 2018-10-13 07:30:13
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
;; `awesome-tray-active-modules'
;;
;; All of the above can customize by:
;;      M-x customize-group RET awesome-tray RET
;;

;;; Change log:
;;
;; 2018/10/13
;;	* Use `awesome-tray-process-exit-code-and-output' fetch git current branch for better error handling.
;; 
;; 2018/10/11
;;      * Reimplement `awesome-tray-module-git-info' don't depend on magit.
;;      * Add last-command module, handy for debug emacs.
;;
;; 2018/10/09
;;      * Add new option `awesome-tray-active-modules'.
;;
;; 2018/10/07
;;      * First released.
;;      * Add row/column information.
;;      * Add `awesome-tray-advice' make tray information visible always.
;;      * Use `frame-width' instead `window-width' to handle blank characters fill.
;;      * Don't fill blank if message string is wider than frame width.
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

(defcustom awesome-tray-active-modules
  '("location" "git" "mode-name" "date")
  "Default active modules."
  :type 'list
  :group 'awesome-tray)

(defface awesome-tray-module-git-face
  '((t (:foreground "SystemPinkColor" :bold t)))
  "Git face."
  :group 'awesome-tray)

(defface awesome-tray-module-mode-name-face
  '((t (:foreground "green3" :bold t)))
  "Mode name face."
  :group 'awesome-tray)

(defface awesome-tray-module-location-face
  '((t (:foreground "SystemOrangeColor" :bold t)))
  "Location face."
  :group 'awesome-tray)

(defface awesome-tray-module-date-face
  '((t (:foreground "SystemGrayColor" :bold t)))
  "Date face."
  :group 'awesome-tray)

(defface awesome-tray-module-last-command-face
  '((t (:foreground "SystemBlueColor" :bold t)))
  "Date face."
  :group 'awesome-tray)

(defface awesome-tray-module-buffer-name-face
  '((t (:foreground "SystemOrangecolor" :bold t)))
  "Buffer name face."
  :group 'awesome-tray)

(define-minor-mode awesome-tray-mode
  "Modular tray bar."
  :require 'awesome-tray-mode
  :global t
  (if awesome-tray-mode
      (awesome-tray-enable)
    (awesome-tray-disable)))

(defvar awesome-tray-info-padding-right 2)

(defvar awesome-tray-mode-line-colors nil)

(defvar awesome-tray-timer nil)

(defvar awesome-tray-active-p nil)

(defvar awesome-tray-all-modules
  '("last-command" "git" "buffer-name" "mode-name" "location" "date"))

(defun awesome-tray-enable ()
  ;; Save mode-line colors when first time.
  ;; Don't change `awesome-tray-mode-line-colors' anymore.
  (unless awesome-tray-mode-line-colors
    (setq awesome-tray-mode-line-colors
          (list (face-attribute 'mode-line :foreground)
                (face-attribute 'mode-line :background)
                (face-attribute 'mode-line :family)
                (face-attribute 'mode-line :box)
                (face-attribute 'mode-line-inactive :foreground)
                (face-attribute 'mode-line-inactive :background)
                (face-attribute 'mode-line-inactive :family)
                (face-attribute 'mode-line-inactive :box)
                )))
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
  (setq awesome-tray-active-p t)
  (message "Enable awesome tray."))

(defun awesome-tray-disable ()
  ;; Restore mode-line colors.
  (set-face-attribute 'mode-line nil
                      :foreground (nth 0 awesome-tray-mode-line-colors)
                      :background (nth 1 awesome-tray-mode-line-colors)
                      :family (nth 2 awesome-tray-mode-line-colors)
                      :box (nth 3 awesome-tray-mode-line-colors)
                      :height 1)
  (set-face-attribute 'mode-line-inactive nil
                      :foreground (nth 4 awesome-tray-mode-line-colors)
                      :background (nth 5 awesome-tray-mode-line-colors)
                      :family (nth 6 awesome-tray-mode-line-colors)
                      :box (nth 7 awesome-tray-mode-line-colors)
                      :height 1)
  ;; Cancel timer.
  (when (timerp awesome-tray-timer)
    (cancel-timer awesome-tray-timer))
  (remove-hook 'focus-in-hook 'awesome-tray-show-info)
  ;; Update mode-line.
  (force-mode-line-update)
  (redraw-display)
  (with-current-buffer " *Minibuf-0*"
    (erase-buffer))
  ;; Notify user.
  (setq awesome-tray-active-p nil)
  (message "Disable awesome tray."))

(defun awesome-tray-build-info ()
  (condition-case nil
      (mapconcat 'identity (mapcar 'awesome-tray-get-module-info awesome-tray-active-modules) " ")
    (format "Awesome Tray broken.")))

(defun awesome-tray-get-module-info (module-name)
  (cond ((string-equal module-name "git")
         (propertize (awesome-tray-module-git-info) 'face 'awesome-tray-module-git-face))
        ((string-equal module-name "mode-name")
         (propertize (awesome-tray-module-mode-name-info) 'face 'awesome-tray-module-mode-name-face))
        ((string-equal module-name "location")
         (propertize (awesome-tray-module-location-info) 'face 'awesome-tray-module-location-face))
        ((string-equal module-name "date")
         (propertize (awesome-tray-module-date-info) 'face 'awesome-tray-module-date-face))
        ((string-equal module-name "last-command")
         (propertize (awesome-tray-module-last-command-info) 'face 'awesome-tray-module-last-command-face))
        ((string-equal module-name "buffer-name")
         (propertize (awesome-tray-module-buffer-name-info) 'face 'awesome-tray-module-buffer-name-face))
        ))

(defun awesome-tray-module-git-info ()
  (if (executable-find "git")
      (let* ((git-info (awesome-tray-process-exit-code-and-output "git" "symbolic-ref" "--short" "HEAD"))
             (status (nth 0 git-info))
             (result (nth 1 git-info)))
        (if (equal status 0)
            (replace-regexp-in-string "\n" "" result)
          ""))
    ""))

(defun awesome-tray-module-mode-name-info ()
  (format "%s" major-mode))

(defun awesome-tray-module-location-info ()
  (format "(%s:%s)" (line-number-at-pos) (current-column)))

(defun awesome-tray-module-date-info ()
  (format-time-string "[%Y-%m-%d %H:%M]"))

(defun awesome-tray-module-last-command-info ()
  (format "%s" last-command))

(defun awesome-tray-module-buffer-name-info ()
  (format "%s" (buffer-name)))

(defun awesome-tray-show-info ()
  ;; Only flush tray info when current message is empty.
  (unless (current-message)
    (awesome-tray-flush-info)))

(defun awesome-tray-flush-info ()
  (let* ((tray-info (awesome-tray-build-info)))
    (with-current-buffer " *Minibuf-0*"
      (erase-buffer)
      (insert (concat (make-string (max 0 (- (frame-width) (length tray-info) awesome-tray-info-padding-right)) ?\ ) tray-info)))))

(defun awesome-tray-get-echo-format-string (message-string)
  (let* ((tray-info (awesome-tray-build-info))
         (blank-length (- (frame-width) (length tray-info) (length message-string) awesome-tray-info-padding-right)))
    (if (> blank-length 0)
        (concat
         message-string
         (make-string (max 0 (- (frame-width) (length message-string) (length tray-info) awesome-tray-info-padding-right)) ?\ )
         tray-info)
      ;; Don't fill blank if message string is wider than frame width.
      (concat message-string tray-info))))

(defun awesome-tray-process-exit-code-and-output (program &rest args)
  "Run PROGRAM with ARGS and return the exit code and output in a list."
  (with-temp-buffer
    (list (apply 'call-process program nil (current-buffer) nil args)
          (buffer-string))))

;; Wrap `message' make tray information visible always
;; even other plugins call `message' to flush minibufer.
(defadvice message (around awesome-tray-advice activate)
  (condition-case nil
      (if awesome-tray-active-p
          (cond
           ;; Just flush tray info if message string is empty.
           ((not (ad-get-arg 0))
            ad-do-it
            (awesome-tray-flush-info))
           ;; Otherwise, wrap message string with tray info.
           (t (let ((formatted-string (apply 'format (ad-get-args 0))))
                (ad-set-args 0 `(,(awesome-tray-get-echo-format-string formatted-string)))
                ad-do-it)))
        ad-do-it)
    ad-do-it))

(provide 'awesome-tray)

;;; awesome-tray.el ends here
