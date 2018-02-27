;;; gridlock-postip.el --- echo field info via pos-tip
;; Copyright (C) 2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Tuesday, February 27, 2018
;; Version: 1.0
;; Modified Time-stamp: <2018-02-27 17:39:51 dharms>
;; Modified by: Dan Harms
;; Keywords: tools gridlock
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
;; Provide a `pos-tip'-based display of the current gridlock field.
;;

;;; Code:
(when (require 'pos-tip nil t)
  (push (cons "pos-tip" (cons #'gridlock-postip-on #'gridlock-postip-off))
        gridlock-display-schemes))

(defun gridlock-postip-on (str)
  "Show STR (the gridlock cell's title) to the user via `pos-tip-show'."
  (pos-tip-show str))

(defun gridlock-postip-off ()
  "Stop showing field info to the user."
  nil)

(provide 'gridlock-postip)
;;; gridlock-postip.el ends here
