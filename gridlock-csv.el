;;; gridlock-csv.el --- gridlock for csv-mode
;; Copyright (C) 2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Tuesday, February 20, 2018
;; Version: 1.0
;; Modified Time-stamp: <2018-03-21 17:38:36 dharms>
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
;; Defines a minor mode for gridlock with csv files.
;;

;;; Code:
(require 'gridlock)

(defcustom gridlock-csv-preferred-display-schemes
  '("popup" "pos-tip" "quick-peek" "echo")
  "Preferred display schemes for `gridlock-csv-mode'.
Display schemes will be loaded in this order.")

(defvar gridlock-csv-init-hook '()
  "Hooks run after setting up `gridlock-csv-mode' but before inspecting buffer.")

(defvar-local gridlock-csv-metadata nil
  "The metadata for this csv buffer.")

(defun gridlock-csv--get-buffer-metadata ()
  "Get the metadata for the current buffer."
  (let ((i 0)
        (gridlock-field-regex-begin "#")
        anchor fields len)
    (gridlock-csv--reset-metadata)
    (save-excursion
      (goto-char (point-min))
      (and (setq anchor (search-forward-regexp gridlock-anchor-regex (line-end-position) t))
           (setq fields (gridlock--check-anchor anchor))
           (setq len (length fields))
           (setq gridlock-csv-metadata
                 (make-vector len ""))
           (while (< i len)
             (aset gridlock-csv-metadata i
                   (string-trim (gridlock-get-str (aref fields i))))
             (setq i (1+ i)))))))

(defun gridlock-csv--reset-metadata ()
  "Reset metadata associated with current buffer."
  (setq gridlock-csv-metadata nil))

(defun gridlock-csv-get-title (field)
  "Return the title associated with FIELD."
  (let* ((idx (gridlock-get-index field))
         (len (length gridlock-csv-metadata))
         (default (format "Heading %d" (1+ idx)))
         str)
    (if (>= idx len)
        default                         ;start ot 1 for user-visible names
      (setq str (aref gridlock-csv-metadata idx))
      (if (string-empty-p str) default str))))

(defun gridlock-csv-gather-titles (fields)
  "Return a list of all field headings in field list FIELDS."
  (mapcar 'identity gridlock-csv-metadata))

(defvar gridlock-csv-mode-map
  (let ((map gridlock-mode-map))
    (define-key map [t] 'gridlock-csv-turn-off)
    map)
  "Keymap for `gridlock-csv-mode'.")

(defun gridlock-csv-reset()
  "Reset `gridlock-csv' state."
  (gridlock-reset)
  (gridlock-csv--reset-metadata)
  )

(defun gridlock-csv-turn-off ()
  "Turn off `gridlock-csv-mode'."
  (interactive)
  (gridlock-csv-mode -1))

(define-minor-mode gridlock-csv-mode
  "Navigate between and explicate CSV fields."
  :init-value nil
  :lighter " GRID"
  :keymap gridlock-csv-mode-map
  (if gridlock-csv-mode
      (progn
        (setq gridlock-anchor-regex "^")
        (setq gridlock-field-delimiter ",")
        (setq gridlock-field-regex-begin nil)
        (setq gridlock-field-regex-end nil)
        (setq gridlock-metadata-get-title-func #'gridlock-csv-get-title)
        (setq gridlock-metadata-gather-titles-func #'gridlock-csv-gather-titles)
        (run-hooks 'gridlock-csv-init-hook)
        (gridlock-csv-reset)
        (gridlock-csv--get-buffer-metadata)
        (gridlock-activate-one-of-display-schemes
         gridlock-csv-preferred-display-schemes)
        (gridlock-show-title))
    (gridlock-csv-reset)))

(provide 'gridlock-csv)
;;; gridlock-csv.el ends here
