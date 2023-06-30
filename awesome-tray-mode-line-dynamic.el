;;; awesome-tray-mode-line-dynamic.el --- summary -*- lexical-binding: t -*-

;; Filename: awesome-tray-mode-line-dynamic.el
;; Description: Dynamic mode line tray bar
;; Author: Aboubacar TRAORE <traxyax@gmail.com>
;; Maintainer: Aboubacar TRAORE <traxyax@gmail.com>
;; Created: 2023-06-30T19:31:26+00:00
;; Last-Updated: 2023-06-30T20:00:01+00:00
;;           By: Aboubacar TRAORE
;; Compatibility: GNU Emacs 28.1


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Add support for dynamic mode line color based on certain conditions.

;;; Installation:
;;
;; Put awesome-tray.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'awesome-tray-mode-line-dynamic)
;; (awesome-tray-mode-line-dynamic 1)

;;; Requires:
(require 'awesome-tray)

;;; Code:

(defcustom awesome-tray-mode-line-dynamic-color ""
  "Dynamic color."
  :type 'string
  :group 'awesome-tray)

(defcustom awesome-tray-mode-line-dynamic-active-color awesome-tray-mode-line-active-color
  "Active color."
  :type 'string
  :group 'awesome-tray)

(defcustom awesome-tray-mode-line-modified-readonly-color "Green"
  "Modified + readonly color."
  :type 'string
  :group 'awesome-tray)

(defcustom awesome-tray-mode-line-readonly-color "DarkGreen"
  "Readonly color."
  :type 'string
  :group 'awesome-tray)

(defcustom awesome-tray-mode-line-modified-color "DarkOrange"
  "Modified color."
  :type 'string
  :group 'awesome-tray)

(defun awesome-tray-mode-line-dynamic-update ()
  "Update the color dynamically of the awesome-tray-mode-line."
  (setq awesome-tray-mode-line-dynamic-color
        (cond
         ((and (buffer-modified-p) buffer-read-only)
          awesome-tray-mode-line-modified-readonly-color)
         (buffer-read-only
          awesome-tray-mode-line-readonly-color)
         ((buffer-modified-p)
          awesome-tray-mode-line-modified-color)
         (t
          awesome-tray-mode-line-dynamic-active-color)))

  (when (not (string-equal awesome-tray-mode-line-dynamic-color
                           awesome-tray-mode-line-active-color))
    (setq awesome-tray-mode-line-active-color
          awesome-tray-mode-line-dynamic-color)
    (awesome-tray-enable)))

(defun awesome-tray-mode-line-dynamic-enable ()
  "Enable the dynamic behavior of the awesome-tray-mode-line.
This function adds the 'awesome-tray-mode-line-dynamic-update' function
to the 'post-command-hook', which triggers the dynamic update of the
awesome-tray-mode-line after each command execution.

Use this function to enable the dynamic behavior of the
awesome-tray-mode-line, allowing it to update dynamically based on
certain conditions."
  (add-hook 'post-command-hook 'awesome-tray-mode-line-dynamic-update))

(defun awesome-tray-mode-line-dynamic-disable ()
  "Disable the dynamic behavior of the awesome-tray-mode-line.
This function removes the 'post-command-hook' that triggers the
'dynamic-update' function responsible for dynamically updating the
awesome-tray-mode-line. Additionally, it sets the active color of the
mode-line to its default, and re-enables the
awesome-tray-mode.

Use this function to disable the dynamic behavior of the
awesome-tray-mode-line when it is no longer needed or desired."
  (remove-hook 'post-command-hook 'awesome-tray-mode-line-dynamic-update)
  (setq awesome-tray-mode-line-active-color
        awesome-tray-mode-line-dynamic-active-color)
  (awesome-tray-enable))

;;;###autoload
(define-minor-mode awesome-tray-mode-line-dynamic
  "Toggle the dynamic behavior of the awesome-tray-mode-line.
When enabled, the awesome-tray-mode-line dynamically updates its appearance
based on certain conditions. This minor mode is global, meaning it affects
all buffers.

Toggle this mode on or off to control the dynamic behavior of the
awesome-tray-mode-line."
  :global t
  (if awesome-tray-mode-line-dynamic
      (awesome-tray-mode-line-dynamic-enable)
    (awesome-tray-mode-line-dynamic-disable)))

(provide 'awesome-tray-mode-line-dynamic)

;;; awesome-tray-mode-line-dynamic.el ends here
