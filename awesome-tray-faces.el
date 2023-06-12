;;; awesome-tray-faces.el --- Faces for Awesome Tray -*-lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;;; Commentary:
;;
;; This module provides the faces for Awesome Tray.
;;
;;; Code:


;; Base Faces:

(defface awesome-tray-default-face '((t :inherit default :bold t))
  "Face for string constant ouside modules."
  :group 'awesome-tray)

(defface awesome-tray-grey-face
  '((((background light)) :foreground "dim grey" :bold t)
    (t :foreground "dark grey" :bold t))
  "Awesome tray grey."
  :group 'awesome-tray)

(defface awesome-tray-red-face
  '((((background light)) :foreground "#cc2444" :bold t)
    (t :foreground "#ff2d55" :bold t))
  "Awesome tray red."
  :group 'awesome-tray)

(defface awesome-tray-green-face
  '((((background light)) :foreground "#00a400" :bold t)
    (t :foreground "green3" :bold t))
  "Awesome tray green."
  :group 'awesome-tray)

(defface awesome-tray-green-path-face
  '((((background light)) :foreground "#5e8e2e" :bold t)
    (t :foreground "#9ded4d" :bold t))
  "Awesome green face for paths."
  :group 'awesome-tray)

(defface awesome-tray-blue-face
  '((((background light)) :foreground "#2832cc" :bold t)
    (t :foreground "#333fff" :bold t))
  "Awesome tray blue."
  :group 'awesome-tray)

(defface awesome-tray-blue-bright-face
  '((((background light)) :foreground "#0061cc" :bold t)
    (t :foreground "#007aff" :bold t))
  "Date face."
  :group 'awesome-tray)

(defface awesome-tray-orange-face
  '((((background light)) :foreground "#cc7700" :bold t)
    (t :foreground "#ff9500" :bold t))
  "Awesome tray orange."
  :group 'awesome-tray)

(defface awesome-tray-yellow-face
  '((((background light)) :foreground "gold" :bold t)
    (t :foreground "yellow" :bold t))
  "Awesome tray yellow."
  :group 'awesome-tray)

(defface awesome-tray-pink-face
  '((((background light)) :foreground "deep pink" :bold t)
    (t :foreground "hot pink" :bold t))
  "Awesome tab pink."
  :group 'awesome-tray)

(defface awesome-tray-magenta-face
  '((((background light)) :foreground "dark magenta" :bold t)
    (t :foreground "magenta" :bold t))
  "Awesome tray magenta."
  :group 'awesome-tray)

(defface awesome-tray-cyan-face
  '((((background light)) :foreground "#008080" :bold t)
    (t :foreground "#00ced1" :bold t))
  "Awesome tray cyan."
  :group 'awesome-tray)


;; Contextual Faces

(defface awesome-tray-module-git-face
  '((((background light)) :inherit awesome-tray-red-face)
    (t :inherit awesome-tray-red-face))
  "Git face."
  :group 'awesome-tray)

(defface awesome-tray-module-awesome-tab-face
  '((((background light)) :inherit awesome-tray-pink-face)
    (t :inherit awesome-tray-pink-face))
  "Awesome tab face."
  :group 'awesome-tray)

(defface awesome-tray-module-rvm-face
  '((((background light)) :inherit awesome-tray-blue-face)
    (t :inherit awesome-tray-blue-face))
  "RVM face."
  :group 'awesome-tray)

(defface awesome-tray-module-circe-face
  '((((background light)) :inherit awesome-tray-blue-face)
    (t :inherit awesome-tray-blue-face))
  "Circe face."
  :group 'awesome-tray)

(defface awesome-tray-module-mode-name-face
  '((((background light)) :inherit awesome-tray-green-face)
    (t :inherit awesome-tray-green-face))
  "Mode name face."
  :group 'awesome-tray)

(defface awesome-tray-module-location-face
  '((((background light)) :inherit awesome-tray-orange-face)
    (t :inherit awesome-tray-orange-face))
  "Location face."
  :group 'awesome-tray)

(defface awesome-tray-module-location-or-page-face
  '((((background light)) :inherit awesome-tray-orange-face)
    (t :inherit awesome-tray-orange-face))
  "Location or page face."
  :group 'awesome-tray)

(defface awesome-tray-module-word-count-face
  '((((background light)) :inherit awesome-tray-orange-face)
    (t :inherit awesome-tray-orange-face))
  "Word count face."
  :group 'awesome-tray)

(defface awesome-tray-module-anzu-face
  '((((background light)) :inherit awesome-tray-orange-face)
    (t :inherit awesome-tray-orange-face))
  "Anzu face."
  :group 'awesome-tray)

(defface awesome-tray-module-github-face
  '((((background light)) :inherit awesome-tray-cyan-face)
    (t :inherit awesome-tray-cyan-face))
  "Github face."
  :group 'awesome-tray)

(defface awesome-tray-module-hostname-face
  '((((background light)) :inherit awesome-tray-cyan-face)
    (t :inherit awesome-tray-cyan-face))
  "Hostname face."
  :group 'awesome-tray)

(defface awesome-tray-module-volume-face
  '((((background light)) :inherit awesome-tray-cyan-face)
    (t :inherit awesome-tray-cyan-face))
  "Volume face."
  :group 'awesome-tray)

(defface awesome-tray-module-mpd-face
  '((((background light)) :inherit awesome-tray-cyan-face)
    (t :inherit awesome-tray-cyan-face))
  "Mpd face."
  :group 'awesome-tray)

(defface awesome-tray-module-date-face
  '((((background light)) :inherit awesome-tray-grey-face)
    (t :inherit awesome-tray-grey-face))
  "Date face."
  :group 'awesome-tray)

(defface awesome-tray-module-celestial-face
  '((((background light)) :inherit awesome-tray-grey-face)
    (t :inherit awesome-tray-grey-face))
  "Celestial lunar phase and sunrise/set face."
  :group 'awesome-tray)

(defface awesome-tray-module-last-command-face
  '((((background light)) :inherit awesome-tray-blue-bright-face)
    (t :inherit awesome-tray-blue-bright-face))
  "Date face."
  :group 'awesome-tray)

(defface awesome-tray-module-buffer-name-face
  '((((background light)) :inherit awesome-tray-orange-face)
    (t :inherit awesome-tray-orange-face))
  "Buffer name face."
  :group 'awesome-tray)

(defface awesome-tray-module-file-path-face
  '((((background light)) :inherit awesome-tray-green-path-face)
    (t :inherit awesome-tray-green-path-face))
  "Parent dir face."
  :group 'awesome-tray)

(defface awesome-tray-module-parent-dir-face
  '((((background light)) :inherit awesome-tray-green-path-face)
    (t :inherit awesome-tray-green-path-face))
  "Parent dir face."
  :group 'awesome-tray)

(defface awesome-tray-module-awesome-tab-face
  '((((background light)) :inherit awesome-tray-pink-face)
    (t :inherit awesome-tray-pink-face))
  "Awesome tab face."
  :group 'awesome-tray)

(defface awesome-tray-module-evil-face
  '((((background light)) :inherit awesome-tray-cyan-face)
    (t :inherit awesome-tray-cyan-face))
  "Evil state face."
  :group 'awesome-tray)

(defface awesome-tray-module-meow-face
  '((((background light)) :inherit awesome-tray-cyan-face)
    (t :inherit awesome-tray-cyan-face))
  "Meow state face."
  :group 'awesome-tray)

(defface awesome-tray-module-battery-face
  '((((background light)) :inherit awesome-tray-cyan-face)
    (t :inherit awesome-tray-cyan-face))
  "Battery state face."
  :group 'awesome-tray)

(defface awesome-tray-module-buffer-read-only-face
  '((((background light)) :inherit awesome-tray-red-face)
    (t :inherit awesome-tray-red-face))
  "Buffer read only face."
  :group 'awesome-tray)

(defface awesome-tray-module-belong-face
  '((((background light)) :inherit awesome-tray-red-face)
    (t :inherit awesome-tray-red-face))
  "Buffer read only face."
  :group 'awesome-tray)

(defface awesome-tray-module-input-method-face
  '((((background light)) :inherit awesome-tray-cyan-face)
    (t :inherit awesome-tray-cyan-face))
  "Input method face."
  :group 'awesome-tray)

(defface awesome-tray-module-clock-face
  '((((background light)) :inherit awesome-tray-blue-bright-face)
    (t :inherit awesome-tray-blue-bright-face))
  "Org clock face."
  :group 'awesome-tray)

(defface awesome-tray-module-org-pomodoro-face
  '((((background light)) :inherit awesome-tray-magenta-face)
    (t :inherit awesome-tray-magenta-face))
  "Org-pomodoro face."
  :group 'awesome-tray)

(defface awesome-tray-module-pdf-view-page-face
  '((((background light)) :inherit awesome-tray-orange)
    (t :inherit awesome-tray-pink-face))
  "Pdf-view-page face."
  :group 'awesome-tray)

(defface awesome-tray-module-flymake-error
  '((t :inherit awesome-tray-red-face))
  "Flymake error face."
  :group 'awesome-tray)

(defface awesome-tray-module-flymake-warning
  '((t :inherit awesome-tray-yellow-face))
  "Flymake warning face."
  :group 'awesome-tray)

(defface awesome-tray-module-flymake-note
  '((t :inherit awesome-tray-blue-bright-face))
  "Flymake note face."
  :group 'awesome-tray)

(provide 'awesome-tray-faces)

;;; awesome-tray-faces.el ends here
