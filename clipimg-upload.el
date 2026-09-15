;;; clipimg-upload.el --- Send a clipboard image to an image host -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Created: September 15, 2026
;; Keywords: multimedia tools
;; URL: https://github.com/agzam/clipimg.el
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Uploading for `clipimg'.  Hosts live in `clipimg-upload-services', one
;; entry per host, so adding one is data rather than code.  Every host
;; here takes the same shape of request: a multipart POST carrying one
;; file field and a few constant fields, answering with either a bare URL
;; or JSON.
;;
;; The request is built and sent with `url.el', so the package needs no
;; program on PATH to upload.  Credentials come from auth-source, and are
;; looked for at the moment of upload, so opening the menu never asks for
;; a passphrase.
;;
;; An upload is public and most of these hosts cannot take it back, so
;; every command here asks first, naming the host and the size.

;;; Code:

(require 'auth-source)
(require 'subr-x)
(require 'url)
(require 'url-http)
(require 'clipimg)

;; url-http.el declares this one without a value, which marks it special
;; only inside that file.
(defvar url-http-response-status)

(defcustom clipimg-upload-services
  '((litterbox
     :label "Litterbox"
     :url "https://litterbox.catbox.moe/resources/internals/api.php"
     :file-field "fileToUpload"
     ;; 72h is the longest the host offers; the others are 1h, 12h and 24h.
     :fields (("reqtype" . "fileupload") ("time" . "72h"))
     :answer text
     :retention "72 hours")
    (catbox
     :label "Catbox"
     :url "https://catbox.moe/user/api.php"
     :file-field "fileToUpload"
     :fields (("reqtype" . "fileupload"))
     :credential (:host "catbox.moe" :field "userhash" :optional t)
     :answer text
     :retention "kept, with your address in their log")
    (0x0
     :label "0x0.st"
     :url "https://0x0.st"
     :file-field "file"
     :answer text
     :retention "30 days to a year, the larger the shorter")
    (uguu
     :label "Uguu"
     :url "https://uguu.se/upload?output=text"
     :file-field "files[]"
     :answer text
     :retention "3 hours")
    (tmpfiles
     :label "tmpfiles.org"
     :url "https://tmpfiles.org/api/v1/upload"
     :file-field "file"
     :answer (data url)
     :retention "1 hour")
    (imgbb
     :label "ImgBB"
     :url "https://api.imgbb.com/1/upload"
     :file-field "image"
     :credential (:host "api.imgbb.com" :field "key")
     :answer (data url)
     :retention "kept until you delete it"))
  "Image hosts an upload can go to.
Each entry maps a symbol to a plist.  `:label' names the host for the
menu and `:url' is where the POST goes.  `:file-field' names the form
field the bytes travel in, and `:fields' is an alist of form fields sent
along with them.  `:answer' says how to find the URL in the reply:
`text' for a bare URL, a list of symbols for a path into JSON, or a
function of the reply body.  `:retention' describes in words how long
the host says it will serve the image.  `:credential', when present, is
a plist of `:host' to look up in auth-source, either `:header' with an
optional `:format' or `:field' to carry the secret, `:default' for a key
the package ships when auth-source holds none, and `:optional' when an
upload works without one."
  :type '(alist :key-type symbol :value-type plist)
  :group 'clipimg)

(defcustom clipimg-upload-service 'litterbox
  "Image host `clipimg' uploads to.
The default needs no account and drops the image after three days, which
is the least an upload can commit you to."
  :type 'symbol
  :group 'clipimg)

(defcustom clipimg-upload-timeout 60
  "Seconds to wait for a host to answer an upload."
  :type 'natnum
  :group 'clipimg)

(defconst clipimg-upload--user-agent
  "clipimg.el (https://github.com/agzam/clipimg.el)"
  "What clipimg calls itself when uploading.
0x0.st turns away the generic agent strings a library sends.")


;;;; Services

(defun clipimg-upload--property (service key)
  "Return property KEY of SERVICE in `clipimg-upload-services'."
  (plist-get (alist-get service clipimg-upload-services) key))

(defun clipimg-upload-service-names ()
  "Return the symbol of every known service."
  (mapcar #'car clipimg-upload-services))

(defun clipimg-upload-label (service)
  "Return the name SERVICE is shown under."
  (or (clipimg-upload--property service :label) (symbol-name service)))

(defun clipimg-upload-retention (service)
  "Return how long an image stays on SERVICE, in words."
  (or (clipimg-upload--property service :retention) "for however long it likes"))

(defun clipimg-upload-problem (service)
  "Return why SERVICE cannot take an upload, or nil when it can.
A missing credential is not a problem here: auth-source is asked at the
moment of upload, so that drawing a menu never unlocks a keyring."
  (unless (clipimg-upload--property service :url)
    (format "%s is not a known upload service" service)))

(defun clipimg-upload--secret (service)
  "Return the secret SERVICE needs, from auth-source or from its entry.
A line in authinfo for the host of the entry wins over the `:default' the
entry carries, so a user with an account of their own never rides on the
one clipimg ships.  Return nil when SERVICE needs no secret, or when it
works without the one it asks for, and signal when a required secret is
nowhere to be found."
  (when-let* ((credential (clipimg-upload--property service :credential))
              (host (plist-get credential :host)))
    (let* ((user (plist-get credential :user))
           (found (car (apply #'auth-source-search :host host :max 1
                              (when user (list :user user)))))
           (secret (plist-get found :secret)))
      (cond ((functionp secret) (funcall secret))
            (secret secret)
            ((plist-get credential :default))
            ((plist-get credential :optional) nil)
            (t (user-error "No secret for %s: add a line for machine %s to your authinfo"
                           (clipimg-upload-label service) host))))))

(defun clipimg-upload--fields (service secret)
  "Return the form fields SERVICE takes, with SECRET among them when it belongs."
  (let ((credential (clipimg-upload--property service :credential))
        (fields (clipimg-upload--property service :fields)))
    (if-let* ((field (and secret (plist-get credential :field))))
        (append fields (list (cons field secret)))
      fields)))

(defun clipimg-upload--headers (service boundary secret)
  "Return the headers of an upload to SERVICE of a body under BOUNDARY.
SECRET rides along in a header when SERVICE asks for one there."
  (let ((credential (clipimg-upload--property service :credential)))
    (cons (cons "Content-Type"
                (concat "multipart/form-data; boundary=" boundary))
          (if-let* ((header (and secret (plist-get credential :header))))
              (list (cons header
                          (format (or (plist-get credential :format) "%s") secret)))
            nil))))


;;;; The request body

(defun clipimg-upload--boundary ()
  "Return a multipart boundary unlikely to turn up in an image."
  (concat "clipimg" (md5 (format "%s%s" (random) (float-time)))))

(defun clipimg-upload--field-part (boundary name value)
  "Return the part of BOUNDARY carrying form field NAME set to VALUE."
  (concat "--" boundary "\r\n"
          "Content-Disposition: form-data; name=\"" name "\"\r\n"
          "\r\n"
          value "\r\n"))

(defun clipimg-upload--body (boundary fields file-field filename type data)
  "Return a multipart body of FIELDS and DATA, divided by BOUNDARY.
FIELDS is an alist of form fields.  DATA travels in a field named
FILE-FIELD, under FILENAME and announced as media type TYPE.  The result
is unibyte, because the bytes of an image are not text."
  (let ((head (concat (mapconcat (lambda (field)
                                   (clipimg-upload--field-part
                                    boundary (car field) (cdr field)))
                                 fields "")
                      "--" boundary "\r\n"
                      "Content-Disposition: form-data"
                      "; name=\"" file-field "\""
                      "; filename=\"" filename "\"\r\n"
                      "Content-Type: " type "\r\n"
                      "\r\n"))
        (tail (concat "\r\n--" boundary "--\r\n")))
    (concat (encode-coding-string head 'utf-8)
            (encode-coding-string data 'binary)
            (encode-coding-string tail 'utf-8))))

(defun clipimg-upload--filename (clip)
  "Return the file name CLIP travels under."
  (concat (format-time-string "clipimg-%Y%m%d-%H%M%S" (clipimg-clip-time clip))
          "." (symbol-name (or (clipimg-clip-type clip) 'png))))

(defun clipimg-upload--type (clip)
  "Return the media type announced for CLIP."
  (format "image/%s" (or (clipimg-clip-type clip) 'png)))


;;;; Talking to the host

(defun clipimg-upload--response-body ()
  "Return the body of the response in the current buffer, as text."
  (goto-char (point-min))
  (re-search-forward "\r?\n\r?\n" nil 'move)
  (decode-coding-string
   (buffer-substring-no-properties (point) (point-max)) 'utf-8))

(defun clipimg-upload--post (url headers body)
  "Return what URL answers when BODY is posted to it with HEADERS.
Signal a `user-error' when the host stays silent or turns the upload
down, quoting what it said."
  (let* ((url-request-method "POST")
         (url-request-extra-headers headers)
         (url-request-data body)
         (url-user-agent clipimg-upload--user-agent)
         (buffer (url-retrieve-synchronously url t t clipimg-upload-timeout)))
    (unless buffer
      (user-error "No answer from %s" url))
    (unwind-protect
        (with-current-buffer buffer
          (let ((status url-http-response-status)
                (answer (clipimg-upload--response-body)))
            (unless (and status (<= 200 status) (< status 300))
              (user-error "%s answered %s: %s" url status
                          (string-trim (truncate-string-to-width answer 200))))
            answer))
      (kill-buffer buffer))))

(defun clipimg-upload--json-value (body path)
  "Return the string at PATH in the JSON of BODY.
PATH is a list of symbols naming one object key each."
  (let ((value (condition-case nil
                   (json-parse-string body :object-type 'alist :array-type 'list)
                 (error (user-error "Not the JSON an upload answers with: %s"
                                    (string-trim body))))))
    (dolist (key path)
      (setq value (and (consp value) (alist-get key value))))
    (if (stringp value)
        value
      (user-error "No %s in what the host answered: %s"
                  (mapconcat #'symbol-name path ".") (string-trim body)))))

(defun clipimg-upload--answer (service body)
  "Return the URL in BODY, the way SERVICE carries it."
  (let ((answer (clipimg-upload--property service :answer)))
    (cond ((eq answer 'text) (string-trim body))
          ((functionp answer) (funcall answer body))
          ((consp answer) (clipimg-upload--json-value body answer))
          (t (user-error "%s does not say how it answers"
                         (clipimg-upload-label service))))))


;;;; Uploading

(defun clipimg-upload-send (clip &optional service)
  "Upload CLIP to SERVICE and return the URL it answers with.
SERVICE defaults to `clipimg-upload-service'.  Nothing is asked here,
so a command that calls this should have asked already."
  (let ((service (or service clipimg-upload-service)))
    (when-let* ((problem (clipimg-upload-problem service)))
      (user-error "%s" problem))
    (let* ((secret (clipimg-upload--secret service))
           (boundary (clipimg-upload--boundary))
           (body (clipimg-upload--body boundary
                                       (clipimg-upload--fields service secret)
                                       (clipimg-upload--property service :file-field)
                                       (clipimg-upload--filename clip)
                                       (clipimg-upload--type clip)
                                       (clipimg-clip-data clip))))
      (clipimg-upload--answer
       service
       (clipimg-upload--post (clipimg-upload--property service :url)
                             (clipimg-upload--headers service boundary secret)
                             body)))))

(defun clipimg-upload--confirm (clip service)
  "Ask whether CLIP may go to SERVICE, naming the size and the retention."
  (y-or-n-p (format "Upload %s to %s (%s)? "
                    (file-size-human-readable (length (clipimg-clip-data clip)))
                    (clipimg-upload-label service)
                    (clipimg-upload-retention service))))

(defun clipimg-upload-url (clip &optional service)
  "Ask, then upload CLIP to SERVICE, and return the URL it answers with."
  (let ((service (or service clipimg-upload-service)))
    (unless (clipimg-upload--confirm clip service)
      (user-error "Nothing uploaded"))
    (message "clipimg: uploading to %s..." (clipimg-upload-label service))
    (clipimg-upload-send clip service)))

(defun clipimg-upload-to-kill-ring (clip &optional service)
  "Upload CLIP to SERVICE and put the URL on the kill ring.
The URL lands on the system clipboard too, replacing the image there,
because the next thing anyone does with it is paste it somewhere else.
The menu is unharmed: it works on a snapshot taken when it opened."
  (let ((url (clipimg-upload-url clip service)))
    (kill-new url)
    (message "clipimg: %s" url)
    url))

(defun clipimg-upload-insert (clip &optional service)
  "Upload CLIP to SERVICE and insert the URL at point."
  (when buffer-read-only
    (user-error "Buffer %s is read only" (buffer-name)))
  (insert (clipimg-upload-url clip service)))

;;;###autoload
(defun clipimg-upload ()
  "Upload the image on the clipboard and put its URL on the kill ring."
  (interactive)
  (clipimg-upload-to-kill-ring (clipimg-clipboard-clip-or-error)))

(provide 'clipimg-upload)
;;; clipimg-upload.el ends here
