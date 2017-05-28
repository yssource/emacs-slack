;;; slack-room-topic.el ---                          -*- lexical-binding: t; -*-

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
(require 'eieio)

(defclass slack-room-topic ()
  ((value :initarg :value :initform "" :type string)
   (creator :initarg :creator :initform "" :type string)
   (last-set :initarg :last_set :initform "" :type string)))

(defun slack-room-topic-create (payload &optional class)
  (when payload
    (let ((value (plist-get payload :value))
          (creator (plist-get payload :creator)))
      (when (and value (< 0 (length value)) creator (< 0 (length creator)))
        (make-instance (or class 'slack-room-topic)
                       :value value
                       :creator creator
                       :last_set (number-to-string (plist-get payload :last_set)))))))

(defmethod slack-topic-to-string ((topic slack-room-topic) team)
  (format "%s\n\tby %s at %s"
          (oref topic value)
          (slack-user-name (oref topic creator) team)
          (slack-message-time-to-string (oref topic last-set))))


(provide 'slack-room-topic)
;;; slack-room-topic.el ends here
