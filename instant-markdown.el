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

(defcustom instant-markdown:executable "instant-markdown-d"
  "The path of the instant-markdown-d executable."
  :type 'string
  :group 'instant-markdown)

(defcustom instnat-markdown:enable-auto-refresh t
  "Enable auto refresh after save."
  :type 'boolean
  :group 'instant-markdown)

(defcustom instant-markdown:port 8090
  "Port number of `instant-markdown'"
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
    (url-retrieve (instant-markdown:request-url) callback)))

(defvar instant-markdown:buffer-modified-tick-last nil)

(defun instant-markdown:refresh-if-buffer-modified ()
  (interactive)
  (let ((modified-tick (buffer-modified-tick)))
    (when (or (not (numberp instant-markdown:buffer-modified-tick-last))
              (/= instant-markdown:buffer-modified-tick-last modified-tick))
      (setq instant-markdown:buffer-modified-tick-last modified-tick)
      (instant-markdown:refresh))))

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
                               instant-markdown:executable)))
      (unless proc
        (error (format "Failed exec `%s'" instant-markdown:executable)))
      (sit-for 0.5)
      (setq instant-markdown:server-proc proc))
    (when instnat-markdown:enable-auto-refresh
      (instant-markdown:add-local-hook))))

(defun instant-markdown:stop-callback (status)
  (kill-process instant-markdown:server-proc)
  (setq instant-markdown:server-proc nil))

;;;###autoload
(defun instant-markdown:stop ()
  (interactive)
  (unless instant-markdown:server-proc
    (error (format "`%s' does not started" instant-markdown:executable)))
  (instant-markdown:request "DELETE" nil #'instant-markdown:stop-callback)
  (when instnat-markdown:enable-auto-refresh
    (instant-markdown:remove-local-hook)))

(defun instant-markdown:add-local-hook ()
  (add-hook 'after-save-hook 'instant-markdown:refresh nil t))

(defun instant-markdown:remove-local-hook ()
  (remove-hook 'after-save-hook 'instant-markdown:refresh t))

(provide 'instant-markdown)

;;; instant-markdown.el ends here
