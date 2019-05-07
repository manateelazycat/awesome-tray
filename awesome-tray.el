;;; awesome-tray.el ---  Modular tray bar

;; Filename: awesome-tray.el
;; Description: Modular tray bar
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-10-07 07:30:16
;; Version: 2.2
;; Last-Updated: 2019-05-08 06:58:59
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/awesome-tray.el
;; Keywords:
;; Compatibility: GNU Emacs 27.0.50
;;
;; Features that might be required by this library:
;;
;; `cl'
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
;; `awesome-tray-git-update-duration'
;;
;; All of the above can customize by:
;;      M-x customize-group RET awesome-tray RET
;;

;;; Change log:
;;
;; 2019/05/08
;;      * Disable git modulde default, it have performance when we change buffer too fast.
;;
;; 2019/04/29
;;      * Fix position not update when execute command `beginning-of-buffer' or `end-of-buffer'.
;;
;; 2019/04/25
;;      * Add 'circe' module displaying circe tracking-buffer modeline info.
;;      * The circe module is not activated by default, it's added to `awesome-tray-all-modules'.
;;
;; 2018/11/25
;;      * Add `RVM' support.
;;      * The rvm module is not activated by default, I move it to `awesome-tray-all-modules'.
;;
;; 2018/11/18
;;      * Fix the problem of displaying duplicate information when the mouse is in the minibuffer window.
;;
;; 2018/11/12
;;      * Remove Mac color, use hex color instead.
;;
;; 2018/11/03
;;      * Add percent information in location module.
;;      * Fix error: Not enough arguments for format string.
;;
;; 2018/10/29
;;      * Use `unspecified' attribute fix black block of mode-line inactive status.
;;      * Add `awesome-tray-git-update-duration' option.
;;
;; 2018/10/21
;;      * Use `advice-add' re-implmenet `awesome-tray-message-advice'
;;      * Add parent-dir module.
;;      * Don't show parent-dir if current mode is `dired-mode'.
;;
;; 2018/10/13
;;      * Use `awesome-tray-process-exit-code-and-output' fetch git current branch for better error handling.
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
;;      * Add `awesome-tray-message-advice' make tray information visible always.
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
(require 'cl)

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
  '("location" "parent-dir" "mode-name" "date")
  "Default active modules."
  :type 'list
  :group 'awesome-tray)

(defcustom awesome-tray-git-update-duration 5
  "Update duration of git command, in seconds.

It's very slow start new process in Windows platform.
Maybe you need set this option with bigger value to speedup on Windows platform."
  :type 'integer
  :group 'awesome-tray)

(defface awesome-tray-module-git-face
  '((t (:foreground "#ff2d55" :bold t)))
  "Git face."
  :group 'awesome-tray)

(defface awesome-tray-module-rvm-face
  '((t (:foreground "#333fff" :bold t)))
  "RVM face."
  :group 'awesome-tray)

(defface awesome-tray-module-circe-face
  '((t (:foreground "#333fff" :bold t)))
  "Circe face."
  :group 'awesome-tray)

(defface awesome-tray-module-mode-name-face
  '((t (:foreground "green3" :bold t)))
  "Mode name face."
  :group 'awesome-tray)

(defface awesome-tray-module-location-face
  '((t (:foreground "#ff9500" :bold t)))
  "Location face."
  :group 'awesome-tray)

(defface awesome-tray-module-date-face
  '((t (:foreground "#8e8e93" :bold t)))
  "Date face."
  :group 'awesome-tray)

(defface awesome-tray-module-last-command-face
  '((t (:foreground "#007aff" :bold t)))
  "Date face."
  :group 'awesome-tray)

(defface awesome-tray-module-buffer-name-face
  '((t (:foreground "#ff9500" :bold t)))
  "Buffer name face."
  :group 'awesome-tray)

(defface awesome-tray-module-parent-dir-face
  '((t (:foreground "#9DED4D" :bold t)))
  "Parent dir face."
  :group 'awesome-tray)

(define-minor-mode awesome-tray-mode
  "Modular tray bar."
  :require 'awesome-tray-mode
  :global t
  (if awesome-tray-mode
      (awesome-tray-enable)
    (awesome-tray-disable)))

(defvar awesome-tray-info-padding-right 1)

(defvar awesome-tray-mode-line-colors nil)

(defvar awesome-tray-timer nil)

(defvar awesome-tray-active-p nil)

(defvar awesome-tray-all-modules
  '("last-command" "parent-dir" "git" "buffer-name" "mode-name" "location" "rvm" "date" "circe"))

(defvar awesome-tray-git-command-last-time 0)

(defvar awesome-tray-git-command-cache "")

(defvar awesome-tray-last-tray-info nil)

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
                      :box nil
                      :inherit 'unspecified)
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
      (mapconcat 'identity (remove-if #'(lambda (n) (equal (length n) 0))
                                      (mapcar 'awesome-tray-get-module-info awesome-tray-active-modules)) " ")
    (format "Awesome Tray broken.")))

(defun awesome-tray-get-module-info (module-name)
  (cond ((string-equal module-name "git")
         (propertize (awesome-tray-module-git-info) 'face 'awesome-tray-module-git-face))
        ((string-equal module-name "rvm")
         (propertize (awesome-tray-module-rvm-info) 'face 'awesome-tray-module-rvm-face))
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
        ((string-equal module-name "parent-dir")
         (propertize (awesome-tray-module-parent-dir-info) 'face 'awesome-tray-module-parent-dir-face)
         )
        ((string-equal module-name "circe")
         (propertize (awesome-tray-module-circe-info) 'face 'awesome-tray-module-circe-face)
         )))

(defun awesome-tray-module-git-info ()
  (if (executable-find "git")
      (let ((current-seconds (awesome-tray-current-seconds)))
        (if (> (- current-seconds awesome-tray-git-command-last-time) awesome-tray-git-update-duration)
            (progn
              (setq awesome-tray-git-command-last-time current-seconds)
              (awesome-tray-update-git-command-cache))
          awesome-tray-git-command-cache))
    ""))

(defun awesome-tray-module-circe-info ()
  "Display circe tracking buffers"
  (if (listp tracking-mode-line-buffers)
      (apply 'concat (cl-loop for entry in tracking-mode-line-buffers
                              collect (or (plist-get entry :propertize) "")))
    ""))

(defun awesome-tray-module-rvm-info ()
  (if (executable-find "rvm-prompt")
      (format "rvm:%s" (replace-regexp-in-string
                        "\n" ""
                        (nth 1 (awesome-tray-process-exit-code-and-output "rvm-prompt")))
              )
    ""))

(defun awesome-tray-module-mode-name-info ()
  (format "%s" major-mode))

(defun awesome-tray-module-location-info ()
  (let* ((bottom-line (line-number-at-pos (point-max)))
         (location-percent
          (cond ((equal (line-number-at-pos) 1)
                 "top")
                ((equal (line-number-at-pos) bottom-line)
                 "bottom")
                (t
                 (format "%.f%%" (* (/ (float (line-number-at-pos)) bottom-line) 100))))
          ))
    (format "(%s:%s %s)"
            (line-number-at-pos)
            (current-column)
            location-percent
            )))

(defun awesome-tray-module-date-info ()
  (format-time-string "[%Y-%m-%d %H:%M]"))

(defun awesome-tray-module-last-command-info ()
  (format "%s" last-command))

(defun awesome-tray-module-buffer-name-info ()
  (format "%s" (buffer-name)))

(defun awesome-tray-module-parent-dir-info ()
  (if (derived-mode-p 'dired-mode)
      ""
    (format "dir:%s" (file-name-nondirectory (directory-file-name default-directory)))))

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
         (blank-length (- (frame-width) (length tray-info) (length message-string) awesome-tray-info-padding-right))
         (empty-fill-string (make-string (max 0 (- (frame-width) (length tray-info) awesome-tray-info-padding-right)) ?\ ))
         (message-fill-string (make-string (max 0 (- (frame-width) (length message-string) (length tray-info) awesome-tray-info-padding-right)) ?\ ))
         )
    (prog1
        (if (> blank-length 0)
            ;; Fill message's end with whitespace to keep tray info at right of minibuffer.
            (concat message-string message-fill-string tray-info)
          (if (and awesome-tray-last-tray-info
                   message-string
                   (string-suffix-p awesome-tray-last-tray-info message-string))
              ;; Fill empty whitespace if new message contain duplicate tray-info (cause by move mouse on minibuffer window).
              (concat empty-fill-string tray-info)
            ;; Don't fill whitepsace at end of message if new message is very long.
            (concat message-string tray-info)))
      ;; Record last tray information.
      (setq awesome-tray-last-tray-info tray-info))))

(defun awesome-tray-process-exit-code-and-output (program &rest args)
  "Run PROGRAM with ARGS and return the exit code and output in a list."
  (with-temp-buffer
    (list (apply 'call-process program nil (current-buffer) nil args)
          (buffer-string))))

(defun awesome-tray-current-seconds ()
  (string-to-number (format-time-string "%s")))

(defun awesome-tray-update-git-command-cache ()
  (let* ((git-info (awesome-tray-process-exit-code-and-output "git" "symbolic-ref" "--short" "HEAD"))
         (status (nth 0 git-info))
         (result (format "git:%s" (nth 1 git-info))))
    (setq awesome-tray-git-command-cache
          (if (equal status 0)
              (replace-regexp-in-string "\n" "" result)
            ""))
    awesome-tray-git-command-cache))

;; Wrap `message' make tray information visible always
;; even other plugins call `message' to flush minibufer.
(defun awesome-tray-message-advice (old-message &rest arguments)
  (condition-case nil
      (if awesome-tray-active-p
          (cond
           ;; Just flush tray info if message string is empty.
           ((not (car arguments))
            (apply old-message arguments)
            (awesome-tray-flush-info))
           ;; Otherwise, wrap message string with tray info.
           (t
            (apply old-message "%s" (cons (awesome-tray-get-echo-format-string (apply 'format arguments)) '()))))
        (apply old-message arguments))
    (apply old-message arguments)
    ))

(advice-add #'message :around #'awesome-tray-message-advice)

(defun awesome-tray-end-of-buffer-advice (old-func &rest arguments)
  (apply old-func arguments)
  (message ""))

(advice-add #'end-of-buffer :around #'awesome-tray-end-of-buffer-advice)

(defun awesome-tray-beginning-of-buffer-advice (old-func &rest arguments)
  (apply old-func arguments)
  (message ""))

(advice-add #'beginning-of-buffer :around #'awesome-tray-beginning-of-buffer-advice)

(provide 'awesome-tray)

;;; awesome-tray.el ends here
