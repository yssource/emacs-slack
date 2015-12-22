;;; slack-user.el ---slack user interface            -*- lexical-binding: t; -*-

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

(require 'slack-request)
(require 'slack-room)
(require 'slack-file)

(defvar slack-users)
(defvar slack-token)
(defvar slack-buffer-function)
(defconst slack-user-stars-list-url "https://slack.com/api/stars.list")

(defun slack-user-find (id)
  (cl-find-if (lambda (user)
                (string= id (plist-get user :id)))
              slack-users))

(defun slack-user-find-by-name (name)
  (cl-find-if (lambda (user)
                (string= name (plist-get user :name)))
              slack-users))

(defun slack-user-get-id (name)
  (let ((user (slack-user-find-by-name name)))
    (if user
        (plist-get user :id)
      nil)))

(defun slack-user-name (id)
  (let ((user (slack-user-find id)))
    (if user
        (plist-get user :name))))

(defun slack-user-names ()
  (mapcar (lambda (u) (cons (plist-get u :name) (plist-get u :id)))
          slack-users))

(defun slack-user-select-from-list ()
  (let* ((list (slack-user-names))
         (candidates (mapcar #'car list)))
    (slack-room-select-from-list
     (candidates "Select User: ")
     (cdr (cl-assoc selected list :test #'string=)))))

(defun slack-user-stars-list-request (&optional page user-id)
  (let ((target-user (or user-id (slack-user-select-from-list))))
    (cl-labels ((on-stars-list
                 (&key data &allow-other-keys)
                 (slack-request-handle-error
                  (data "slack-user-stars-list")
                  (slack-user-on-stars-list data target-user))))
      (slack-request
       slack-user-stars-list-url
       :params (list (cons "token" slack-token)
                     (cons "user" target-user)
                     (cons "page" (number-to-string (or page 1))))
       :success #'on-stars-list
       :sync nil))))

(defun slack-user-stars-list ()
  (interactive)
  (let* ((word (thing-at-point 'line))
         (target-page (ignore-errors (get-text-property 0 'target-page word)))
         (user-id (ignore-errors (get-text-property 0 'user-id word))))
    (slack-user-stars-list-request target-page user-id)))

(defvar slack-user-stars-list-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "RET") #'slack-user-stars-list)
    keymap))

(defun slack-user-on-stars-list (data user-id)
  (cl-labels ((create-object (item)
                             (cond
                              ((string= "message" (plist-get item :type))
                               (slack-message-create (plist-get item :message)))
                              ((string= "file" (plist-get item :type))
                               (slack-file-create (plist-get item :file)))))
              (create-messages (object)
                               (if object
                                   (cond
                                    ((object-of-class-p object 'slack-message)
                                     (slack-message-to-string object))
                                    ((object-of-class-p object 'slack-file)
                                     (slack-file-to-string object))))))
    (let* ((objects (mapcar #'create-object (append (plist-get data :items) nil)))
           (messages (cl-remove-if #'null (mapcar #'create-messages objects)))
           (paging (plist-get data :paging))
           (cur-page (plist-get paging :page))
           (prev-page (or (and (< 1 cur-page) (1- cur-page)) nil))
           (next-page (or (and (< cur-page (plist-get paging :pages)) (1+ cur-page)) nil)))
      (funcall slack-buffer-function
               (slack-buffer-create-info
                (concat "*Slack - Starred Items of " (slack-user-name user-id) "*")
                #'(lambda ()
                    (insert (concat (propertize "Starred Items"
                                                'face '(:underline t :weight bold))
                                    "\n\n"))
                    (mapc #'(lambda (m) (insert m) (insert "\n"))
                          (nreverse messages))
                    (insert (concat (format "total: %s page: %s/%s"
                                            (plist-get paging :total)
                                            (plist-get paging :page)
                                            (plist-get paging :pages))
                                    "\n\n"))
                    (when prev-page
                      (insert (propertize
                               "(prev-page)"
                               'face '(:underline t)
                               'target-page prev-page
                               'user-id user-id
                               'keymap slack-user-stars-list-keymap))
                      (insert "\n"))
                    (when next-page
                      (insert (propertize
                               "(next-page)"
                               'face '(:underline t)
                               'target-page next-page
                               'user-id user-id
                               'keymap slack-user-stars-list-keymap)))))))))

(provide 'slack-user)
;;; slack-user.el ends here
