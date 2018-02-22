;;; gridlock-csv.el --- gridlock for csv-mode
;; Copyright (C) 2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Tuesday, February 20, 2018
;; Version: 1.0
;; Modified Time-stamp: <2018-02-21 06:24:23 dharms>
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

(defvar gridlock-csv-mode-map
  (let ((map gridlock-mode-map))
    (define-key map [t] 'gridlock-csv-turn-off)
    map)
  "Keymap for `gridlock-csv-mode'.")

(defun gridlock-csv-reset()
  "Reset `gridlock-csv' state."
  ;; todo
  (gridlock-reset)
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
        (gridlock-csv-reset))
    t))

(provide 'gridlock-csv)
;;; gridlock-csv.el ends here
