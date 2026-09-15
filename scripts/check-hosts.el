;;; check-hosts.el --- Upload to every shipped host and read it back -*- lexical-binding: t; -*-
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;;; Commentary:

;; The suite stops at the transport, so it cannot tell a live host from a
;; dead one.  This puts the fixture on every host in
;; `clipimg-upload-services', fetches the URL that came back, and compares
;; the bytes.  A host that answers with a landing page instead of the
;; image fails here, which no amount of unit testing would show.
;;
;; It uploads for real, so it is never part of `make test' and never runs
;; in CI: hosts rate-limit, and a suite that spams them deserves the ban.
;; Run it by hand before adding an entry or trusting an old one.
;;
;; A host needing a credential is reported as skipped rather than failed
;; when auth-source has nothing for it.

;;; Code:

(require 'clipimg-upload)
(require 'url)

(defvar check-hosts-fixture
  (expand-file-name "test/fixture.png"
                    (file-name-directory
                     (directory-file-name
                      (file-name-directory (or load-file-name buffer-file-name)))))
  "Image sent to every host.")

(defun check-hosts--fetch (url)
  "Return the bytes URL serves, or nil."
  (let ((buffer (ignore-errors (url-retrieve-synchronously url t t 60))))
    (when buffer
      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-min))
            (when (re-search-forward "\r?\n\r?\n" nil t)
              (buffer-substring-no-properties (point) (point-max))))
        (kill-buffer buffer)))))

(defun check-hosts--check (service data clip)
  "Upload CLIP to SERVICE and return a verdict comparing what comes back with DATA."
  (condition-case error
      (let* ((url (clipimg-upload-send clip service))
             (served (check-hosts--fetch url)))
        (cond ((null served) (list :fail service url "nothing served"))
              ((equal served data) (list :pass service url "identical"))
              (t (list :fail service url
                       (format "served %d bytes, not the image" (length served))))))
    (user-error
     (let ((message (error-message-string error)))
       (if (string-match-p "No secret" message)
           (list :skip service nil "no credential in auth-source")
         (list :fail service nil message))))
    (error (list :fail service nil (error-message-string error)))))

(defun check-hosts ()
  "Upload to every shipped host, read it back, and exit non-zero on any failure."
  (let* ((data (clipimg--file-bytes check-hosts-fixture))
         (clip (clipimg-clip-create :data data :type 'png :time (current-time)))
         (verdicts (mapcar (lambda (service) (check-hosts--check service data clip))
                           (clipimg-upload-service-names)))
         (failed (seq-filter (lambda (verdict) (eq (car verdict) :fail)) verdicts)))
    (dolist (verdict verdicts)
      (message "%-6s %-10s %s%s"
               (upcase (substring (symbol-name (nth 0 verdict)) 1))
               (nth 1 verdict)
               (or (nth 2 verdict) "")
               (if (nth 2 verdict) (format "  (%s)" (nth 3 verdict))
                 (nth 3 verdict))))
    (message "%d host(s) checked, %d failed" (length verdicts) (length failed))
    (kill-emacs (if failed 1 0))))

(provide 'check-hosts)
;;; check-hosts.el ends here
