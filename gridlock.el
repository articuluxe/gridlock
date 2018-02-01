;;; gridlock.el --- navigate among delimited fields
;; Copyright (C) 2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Friday, January 26, 2018
;; Version: 0.1
;; Modified Time-stamp: <2018-02-01 17:54:05 dharms>
;; Modified by: Dan Harms
;; Keywords: tools
;; URL: https://github.com/articuluxe/gridlock.git
;; Package-Requires: ((emacs "24.3"))

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
;; Navigate between delimited fields, and explicate them if possible.
;;

;;; Code:
(defgroup gridlock nil
  "Helper to navigate between fields, and explicate them."
  :group 'tools
  :prefix "gridlock")

(defcustom gridlock-prefix-key (kbd "M-p")
  "Prefix key for `gridlock-mode'."
  :type 'vector)

(defvar-local gridlock-points nil
  "List of gridlock points in the current buffer.")

(defvar gridlock-anchor-regexp nil
  "Regexp representing the anchor point that defines what lines
the user is interested in."

(defun gridlock--find-next ()
  "Find location of next anchor point."

  (search-forward-regexp gridlock-anchor-regexp nil t)



(defun gridlock-define-keys (map)
  "Define in keymap MAP bindings for `gridlock-mode'."
  )

(defun gridlock-mode-map
    (let ((map (make-sparse-keymap)))
      (gridlock-define-keys map)
      map)
  "Keymap for `gridlock-mode'.")

(defun gridlock-define-prefix (map)
  "Define a prefix kepmap for `gridlock-mode' inside keymap MAP."
  (define-key map gridlock-prefix-key gridlock-mode-map))

(defvar gridlock-prefix-map
  (let ((map (make-sparse-keymap)))
    (gridlock-define-prefix map)
    map)
  "Prefix keymap for `gridlock-mode'.")

(define-minor-mode gridlock-mode
  "Navigate between and explicate fields."
  :init-value nil
  :lighter " GRID"
  :keymap gridlock-prefix-map
  (if gridlock-mode
      (gridlock-define-prefix global-map)
    t))

(provide 'gridlock)
;;; gridlock.el ends here
