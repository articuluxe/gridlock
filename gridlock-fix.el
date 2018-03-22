;;; gridlock-fix.el --- gridlock for fix
;; Copyright (C) 2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Thursday, March 22, 2018
;; Version: 1.0
;; Modified Time-stamp: <2018-03-22 08:44:08 dharms>
;; Modified by: Dan Harms
;; Keywords: tools
;; URL: https://github.com/articuluxe/gridlock.git
;; Package-Requires: ((emacs "24.4"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Defines a minor mode for gridlock with fix messages.
;;

;;; Code:
(require 'gridlock)

(defcustom gridlock-fix-preferred-display-schemes
  '("popup" "pos-tip" "quick-peek" "echo")
  "Preferred display schemes for `gridlock-fix-mode'.
Display schemes will be loaded in this order.")

(defvar gridlock-fix-init-hook '()
  "Hooks run after setting up `gridlock-fix-mode' but before inspecting buffer.")

(defvar-local gridlock-fix-metadata nil
  "The metadata for this fix buffer.")

(defun gridlock-fix-get-title (field)
  "Return the title associated with FIELD."
  ;; (let* ((idx (gridlock-get-index field))
  ;;        (len (length gridlock-csv-metadata))
  ;;        (default (format "Heading %d" (1+ idx)))
  ;;        str)
  ;;   (if (>= idx len)
  ;;       default                         ;start ot 1 for user-visible names
  ;;     (setq str (aref gridlock-csv-metadata idx))
  ;;     (if (string-empty-p str) default str)))
  )

(defun gridlock-fix-gather-titles (fields)
  "Return a list of all field headings in field list FIELDS."
  ;; (mapcar 'identity gridlock-csv-metadata)
  )

(defvar gridlock-fix-mode-map
  (let ((map gridlock-mode-map))
    (define-key map [t] 'gridlock-fix-turn-off)
    map)
  "Keymap for `gridlock-fix-mode'.")

(defun gridlock-fix-reset()
  "Reset `gridlock-fix' state."
  (gridlock-reset)
  (gridlock-fix--reset-metadata)
  )

(defun gridlock-fix-turn-off ()
  "Turn off `gridlock-fix-mode'."
  (interactive)
  (gridlock-fix-mode -1))

(define-minor-mode gridlock-fix-mode
  "Navigate between and explicate FIX fields."
  :init-value nil
  :lighter " GRID"
  :keymap gridlock-fix-mode-map
  (if gridlock-fix-mode
      (progn
        (setq gridlock-anchor-regex "^")
        (setq gridlock-field-delimiter "\000")
        (setq gridlock-field-regex-begin "fix payload=")
        (setq gridlock-field-regex-end nil)
        (setq gridlock-metadata-get-title-func #'gridlock-fix-get-title)
        (setq gridlock-metadata-gather-titles-func #'gridlock-fix-gather-titles)
        (run-hooks 'gridlock-fix-init-hook)
        (gridlock-fix-reset)
        (gridlock-fix--get-buffer-metadata)
        (gridlock-activate-one-of-display-schemes
         gridlock-fix-preferred-display-schemes)
        (gridlock-show-title))
    (gridlock-fix-reset)))

(provide 'gridlock-fix)
;;; gridlock-fix.el ends here