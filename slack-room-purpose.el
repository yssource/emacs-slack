;;; slack-room-purpose.el ---                        -*- lexical-binding: t; -*-

;; Copyright (C) 2017

;; Author:  <yuya373@yuya373>
;; Keywords:

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

(require 'slack-room-topic)

(defclass slack-room-purpose (slack-room-topic) ())

(defun slack-room-purpose-create (payload)
  (slack-room-topic-create payload 'slack-room-purpose))

(defmethod slack-purpose-to-string ((purpose slack-room-purpose) team)
  (format "Purpose:\n\t%s\n\tby %s at %s"
          (oref purpose value)
          (slack-user-name (oref purpose creator) team)
          (slack-message-time-to-string (oref purpose last-set))))


(provide 'slack-room-purpose)
;;; slack-room-purpose.el ends here
