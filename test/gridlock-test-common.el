;;; gridlock-test-common.el --- common test utilities for gridlock
;; Copyright (C) 2017-2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Wednesday, June 21, 2017
;; Version: 1.0
;; Modified Time-stamp: <2018-02-02 17:23:32 dharms>
;; Modified by: Dan Harms
;; Keywords: gridlock test

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

;;

;;; Code:
;; transfer dependencies from argv into load-path
(let ((lst (cdr argv))
      add elt)
  (setq argv nil)
  (while lst
    (setq elt (car lst))
    (if add
        (progn
          (push elt load-path)
          (setq add nil))
      (unless
          (setq add (string= elt "-L"))
        (push elt argv)))
    (setq lst (cdr lst))))
(push (concat (file-name-directory load-file-name) "/..") load-path)
(push (file-name-directory load-file-name) load-path)

(require 'ert)
(setq debug-on-error t)
;;; gridlock-test-common.el ends here
