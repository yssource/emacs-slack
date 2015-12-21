;;; slack-file.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2015  南優也

;; Author: 南優也 <yuyaminami@minamiyuunari-no-MacBook-Pro.local>
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

(defclass slack-file ()
  ((id :initarg :id)
   (created :initarg :created)
   (timestamp :initarg :timestamp)
   (name :initarg :name)
   (size :initarg :size)
   (public :initarg :public)
   (url :initarg :url)
   (url-download :initarg :url_download)
   (url-private :initarg :url_private)
   (channels :initarg :channels :type list)
   (groups :initarg :groups :type list)
   (ims :initarg :ims :type list)
   (reactions :initarg :reactions :type list)
   (username :initarg :username)
   (bot-id :initarg :bot_id)
   (mimetype :initarg :mimetype :type string)
   (filetype :initarg :filetype :type string)
   (user :initarg :user :type string)
   (ts :initarg :ts :type string)))

(defun slack-file-create (payload)
  (unless (boundp 'file-payload)
    (setq file-payload payload))
  (plist-put payload :channels (append (plist-get payload :channels) nil))
  (plist-put payload :groups (append (plist-get payload :groups) nil))
  (plist-put payload :ims (append (plist-get payload :ims) nil))
  (plist-put payload :reactions (append (plist-get payload :reactions) nil))
  (let ((params (slack-collect-slots 'slack-file payload)))
    (apply #'slack-file "file" params)))

(defmethod slack-file-to-string ((file slack-file))
  (with-slots (user name url url-download size filetype timestamp) file
    (let* ((username (slack-user-name user))
           (time-string (slack-message-time-to-string (number-to-string timestamp)))
           (header (format "%s\t%s" username time-string))
           (body (concat (format "url: %s\nurl-download: %s\n" url url-download)
                         (format "size: %s filetype: %s" size filetype))))
      (slack-message-put-header-property header)
      (slack-message-put-text-property body)
      (concat header "\n" body "\n" user "\n"))))

(provide 'slack-file)
;;; slack-file.el ends here
