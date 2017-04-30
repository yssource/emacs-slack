;;; slack-all-unread.el ---                          -*- lexical-binding: t; -*-

;; Copyright (C) 2017  南優也

;; Author: 南優也 <yuyaminami@minamiyuuya-no-MacBook.local>
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

(defcustom slack-all-unreads-only-subscribed-channels nil
  "If set t, display subscribed channels unreads messages.")

(define-derived-mode slack-all-unread-mode lui-mode "Slack All Unread")

(defun slack-all-unread-create-buffer (team)
  (let ((buf-name (format "%s - All Unreads" (slack-team-name team))))
    (or (get-buffer buf-name)
        (let ((buf (generate-new-buffer buf-name)))
          (with-current-buffer buf
            (slack-all-unread-mode)
            (add-hook 'lui-pre-output-hook 'slack-buffer-buttonize-link nil t))
          buf))))

(defface slack-all-unread-channel-line-face
  '((t (:weight bold :height 1.5 :underline t)))
  "Face used to All Unread Header."
  :group 'slack)

(defface slack-all-unread-show-more-message-line-face
  '((t (:underline t)))
  "Face used to \"show more message\" line."
  :group 'slack)

(defun slack-all-unread-channel-line (room team)
  (let ((str (format "# %s\t\t%s messages"
                     (slack-room-name room)
                     (oref room unread-count-display))))
    (propertize str 'face 'slack-all-unread-channel-line-face)))

(defun slack-all-unread-show-more-message-line (room team first-message)
  (let ((str "show more message"))
    (propertize str
                'face 'slack-all-unread-show-more-message-line-face
                'keymap (let ((map (make-sparse-keymap)))
                          (define-key map (kbd "RET")
                            #'(lambda ()
                                (interactive)
                                (slack-room-create-buffer
                                 room team
                                 :with-this-buffer
                                 #'(lambda () (slack-buffer-goto (oref first-message ts))))))
                          map))))

(defface slack-all-unread-make-all-messages-read-line-face
  '((t (:underline t)))
  "Face used to \"make all messages as read\" line."
  :group 'slack)

(defun slack-all-unread-make-all-read-line (room team message)
  (let ((str "make all messages as read"))
    (propertize str
                'face 'slack-all-unread-make-all-messages-read-line-face
                'keymap (let ((map (make-sparse-keymap)))
                          (define-key map (kbd "RET")
                            #'(lambda ()
                                (interactive)
                                (slack-room-update-mark room team message)))
                          map))))

(defun slack-all-unread-insert-messages (room team messages)
  (let ((lui-time-stamp-position nil))
    (lui-insert (format "%s\n" (slack-all-unread-channel-line room team)))
    (lui-insert (format "%s\n" (slack-all-unread-make-all-read-line room team (car (last messages))))))
  (let* ((max-display-count 5)
         (unread-count (oref room unread-count-display))
         (messages-display (cl-subseq messages
                                      0 (min unread-count max-display-count (length messages)))))
    (mapc #'(lambda (message) (slack-buffer-insert message team t)) messages-display)

    (when (< max-display-count unread-count)
      (let ((lui-time-stamp-position nil))
        (lui-insert (slack-all-unread-show-more-message-line room team (car messages-display)))))

    (lui-insert "\n")))

(defun slack-all-unread ()
  (interactive)
  (cl-labels
      ((unread-messages (room)
                        (slack-room-latest-messages room (slack-room-sorted-messages room)))
       (channels-display (team)
                         (let ((all (cl-remove-if #'slack-room-hiddenp
                                                  (append (oref team ims) (oref team channels) (oref team groups)))))
                           (cl-sort (if slack-all-unreads-only-subscribed-channels
                                        (cl-remove-if-not #'(lambda (e) (slack-room-subscribedp e team))
                                                          all)
                                      all)
                                    #'string>
                                    :key #'(lambda (e) (oref e last-read))))))
    (let* ((team (slack-team-select))
           (channels (channels-display team))
           (buf (slack-all-unread-create-buffer team))
           (inhibit-read-only t))
      (with-current-buffer buf
        (delete-region (point-min) (point-max))
        (mapc #'(lambda (room)
                  (let* ((unread-messages (unread-messages room))
                         (need-request (= 0 (length (oref room messages)))))
                    (when (< 0 (oref room unread-count-display))
                      (if need-request
                          (slack-room-history-request
                           room team
                           :async t
                           :after-success #'(lambda ()
                                              (slack-all-unread-insert-messages room team (unread-messages room))))
                        (slack-all-unread-insert-messages room team unread-messages)))))
              channels)
        (goto-char (point-min)))
      (funcall slack-buffer-function buf))))


(provide 'slack-all-unread)
;;; slack-all-unread.el ends here
