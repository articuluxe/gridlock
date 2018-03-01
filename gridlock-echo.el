;;; gridlock-echo.el --- echo field info to minibuffer
;; Copyright (C) 2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Monday, February 26, 2018
;; Version: 1.0
;; Modified Time-stamp: <2018-02-28 17:50:17 dharms>
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
;; Provide a minibuffer-based echo area for the current field.
;;

;;; Code:
(push (cons "echo" (cons #'gridlock-echo-on #'gridlock-echo-off))
      gridlock-display-schemes)

(defun gridlock-echo-on (str)
  "Show STR (the gridlock cell's title) to the user via the minibuffer."
  (message str))

(defun gridlock-echo-off (str)
  "Stop showing field info STR to the user."
  nil)

(provide 'gridlock-echo)
;;; gridlock-echo.el ends here
