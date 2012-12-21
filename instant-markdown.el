;;; instant-markdown.el ---

;; Copyright (C) 2012 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL:
;; Version: 0.01

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

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'url)
(require 'url-http)

(defgroup instant-markdown nil
  "Emacs `instant-markdown' client"
  :group 'markdown)

(defcustom instant-markdown:port 8090
  "Port number of `instant-mark-d'"
  :type 'integer
  :group 'instant-markdown)

(defun instant-markdown:default-callback (status)
  (message "%s" status))

(defun instant-markdown:request-url ()
  (format "http://localhost:%d" instant-markdown:port))

(defun instant-markdown:request (method input &optional cb)
  (let ((url-request-method method)
        (url-request-data input)
        (url-request-extra-headers '(("Content-Type" . "text/x-markdown")))
        (callback (or cb #'instant-markdown:default-callback)))
    (url-retrieve "http://localhost:8090/" callback)))

;;;###autoload
(defun instant-markdown:refresh ()
  (interactive)
  (let ((markdown (buffer-substring-no-properties (point-min) (point-max))))
    (instant-markdown:request "PUT" markdown)))

(defvar instant-markdown:server-proc nil)

(defun instant-markdown:start-sentinel (process state)
  (message "@@ %s" state)
  (unless instant-markdown:server-proc
))

;;;###autoload
(defun instant-markdown:start ()
  (interactive)
  (unless instant-markdown:server-proc
    (let ((proc (start-process "instant-markdown-d" "*instant-markdown-d*"
                               "instant-markdown-d")))
      (unless proc
        (error "Failed exec `instant-markdown-d'"))
      (sit-for 0.5)
      (setq instant-markdown:server-proc proc))))

(defun instant-markdown:stop-callback ()
  (kill-process instant-markdown:server-proc)
  (setq instant-markdown:server-proc nil))

;;;###autoload
(defun instant-markdown:stop ()
  (interactive)
  (when instant-markdown:server-proc
    (error "`instant-markdown-d' does not started"))
  (instant-markdown:request "DELETE" nil #'instant-markdown:stop-callback))

(add-hook 'post-command-hook
          (lambda ()
            (when instant-markdown:server-proc
              (instant-markdown:refresh))))

(provide 'instant-markdown)

;;; instant-markdown.el ends here
