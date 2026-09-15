;;; clipimg-upload-tests.el --- Tests for clipimg uploading -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;;; Commentary:
;; The request an upload builds, and what it makes of the answer.  Nothing
;; here reaches the network: the transport is the seam every spec stops at.
;;
;;; Code:

(require 'buttercup)
(require 'clipimg-upload)

(defconst clipimg-upload-tests--data
  (unibyte-string #x89 #x50 #x4e #x47 #x0d #x0a #x1a #x0a)
  "The eight byte signature every PNG starts with.")

(defconst clipimg-upload-tests--services
  '((carrier
     :label "Carrier"
     :url "https://example.com/upload"
     :file-field "image"
     :fields (("kind" . "screenshot"))
     :credential (:host "example.com" :header "Authorization"
                  :format "Token %s" :default "SHIPPED")
     :answer (result url)
     :retention "a week"))
  "A host of its own, so no spec rests on what a shipped entry happens to say.
No host that ships carries its secret in a header, and this one does.")

(defun clipimg-upload-tests--clip ()
  "Return a clip standing in for one taken off the clipboard."
  (clipimg-clip-create :data clipimg-upload-tests--data
                       :type 'png
                       :width 700 :height 220
                       :time (encode-time 0 30 10 15 9 2026)))

(defun clipimg-upload-tests--response (status body)
  "Return a buffer holding an HTTP response of STATUS carrying BODY."
  (let ((buffer (generate-new-buffer " *clipimg-upload-test*")))
    (with-current-buffer buffer
      (insert "HTTP/1.1 " (number-to-string status) " Whatever\r\n"
              "Content-Type: application/json\r\n"
              "\r\n" body)
      (set (make-local-variable 'url-http-response-status) status))
    buffer))

(describe "the service table"
  (it "knows every service it ships"
    (expect (clipimg-upload-service-names)
            :to-equal '(litterbox catbox 0x0 uguu tmpfiles imgbb)))

  (it "uploads to a host that needs no account by default"
    (expect clipimg-upload-service :to-be 'litterbox)
    (expect (clipimg-upload--property 'litterbox :credential) :to-be nil))

  (it "asks Litterbox to keep the image as long as it will"
    (expect (clipimg-upload--property 'litterbox :url)
            :to-equal "https://litterbox.catbox.moe/resources/internals/api.php")
    (expect (clipimg-upload--property 'litterbox :file-field)
            :to-equal "fileToUpload")
    (expect (alist-get "time" (clipimg-upload--property 'litterbox :fields)
                       nil nil #'equal)
            :to-equal "72h"))

  (it "labels a service it has a name for"
    (expect (clipimg-upload-label 'litterbox) :to-equal "Litterbox"))

  (it "falls back to the symbol when an entry names nothing"
    (let ((clipimg-upload-services '((mine :url "https://example.com"))))
      (expect (clipimg-upload-label 'mine) :to-equal "mine")))

  (it "says when a service is unknown"
    (expect (clipimg-upload-problem 'nowhere)
            :to-equal "nowhere is not a known upload service"))

  (it "says nothing about a service it can reach"
    (expect (clipimg-upload-problem 'litterbox) :to-be nil))

  (it "leaves a missing credential to upload time, so no menu unlocks a keyring"
    (spy-on 'auth-source-search :and-return-value nil)
    (expect (clipimg-upload-problem 'imgbb) :to-be nil)
    (expect 'auth-source-search :not :to-have-been-called)))

(describe "the multipart body"
  (it "carries the constant fields, then the file, then the terminator"
    (expect (clipimg-upload--body
             "BOUND" '(("reqtype" . "fileupload")) "fileToUpload"
             "shot.png" "image/png" "BYTES")
            :to-equal
            (concat "--BOUND\r\n"
                    "Content-Disposition: form-data; name=\"reqtype\"\r\n"
                    "\r\n"
                    "fileupload\r\n"
                    "--BOUND\r\n"
                    "Content-Disposition: form-data; name=\"fileToUpload\";"
                    " filename=\"shot.png\"\r\n"
                    "Content-Type: image/png\r\n"
                    "\r\n"
                    "BYTES\r\n"
                    "--BOUND--\r\n")))

  (it "needs no constant field"
    (expect (clipimg-upload--body "B" nil "file" "a.png" "image/png" "X")
            :to-equal
            (concat "--B\r\n"
                    "Content-Disposition: form-data; name=\"file\";"
                    " filename=\"a.png\"\r\n"
                    "Content-Type: image/png\r\n"
                    "\r\n"
                    "X\r\n"
                    "--B--\r\n")))

  (it "is unibyte, so the bytes of an image go out untouched"
    (let ((body (clipimg-upload--body "B" nil "file" "a.png" "image/png"
                                      clipimg-upload-tests--data)))
      (expect (multibyte-string-p body) :to-be nil)
      (expect (string-search clipimg-upload-tests--data body) :not :to-be nil)))

  (it "encodes a field value that is not ASCII"
    (let ((body (clipimg-upload--body "B" '(("name" . "мяу")) "file"
                                      "a.png" "image/png" "X")))
      (expect (multibyte-string-p body) :to-be nil)
      (expect (string-search (encode-coding-string "мяу" 'utf-8) body)
              :not :to-be nil)))

  (it "draws a boundary that is not in the bytes it divides"
    (expect (string-search (clipimg-upload--boundary)
                           clipimg-upload-tests--data)
            :to-be nil)
    (expect (clipimg-upload--boundary) :not :to-equal (clipimg-upload--boundary))))

(describe "naming the file"
  (it "stamps the name with the time the clip was taken"
    (expect (clipimg-upload--filename (clipimg-upload-tests--clip))
            :to-equal "clipimg-20260915-103000.png"))

  (it "announces the media type of the clip"
    (expect (clipimg-upload--type (clipimg-upload-tests--clip))
            :to-equal "image/png")
    (expect (clipimg-upload--type (clipimg-clip-create :data "x" :type 'jpeg))
            :to-equal "image/jpeg")))

(describe "the secret a service needs"
  (it "finds it in auth-source under the host of the entry"
    (spy-on 'auth-source-search :and-return-value '((:secret "key123")))
    (expect (clipimg-upload--secret 'imgbb) :to-equal "key123")
    (expect (spy-calls-args-for 'auth-source-search 0)
            :to-equal '(:host "api.imgbb.com" :max 1)))

  (it "calls the secret when auth-source hands over a function"
    (spy-on 'auth-source-search
            :and-return-value (list (list :secret (lambda () "hidden"))))
    (expect (clipimg-upload--secret 'imgbb) :to-equal "hidden"))

  (it "signals when a service that needs one has none"
    (spy-on 'auth-source-search :and-return-value nil)
    (expect (clipimg-upload--secret 'imgbb) :to-throw 'user-error))

  (it "does without one the service marks optional"
    (spy-on 'auth-source-search :and-return-value nil)
    (expect (clipimg-upload--secret 'catbox) :to-be nil))

  (it "asks for nothing when the service takes no credential"
    (spy-on 'auth-source-search)
    (expect (clipimg-upload--secret 'litterbox) :to-be nil)
    (expect 'auth-source-search :not :to-have-been-called))

  (it "falls back to the key the entry ships"
    (spy-on 'auth-source-search :and-return-value nil)
    (let ((clipimg-upload-services clipimg-upload-tests--services))
      (expect (clipimg-upload--secret 'carrier) :to-equal "SHIPPED")))

  (it "lets a user's own key win over the shipped one"
    (spy-on 'auth-source-search :and-return-value '((:secret "MINE")))
    (let ((clipimg-upload-services clipimg-upload-tests--services))
      (expect (clipimg-upload--secret 'carrier) :to-equal "MINE"))))

(describe "where the secret rides"
  (it "goes into the header the entry names, in the format it names"
    (let ((clipimg-upload-services clipimg-upload-tests--services))
      (expect (clipimg-upload--headers 'carrier "BOUND" "abc")
              :to-equal '(("Content-Type" . "multipart/form-data; boundary=BOUND")
                          ("Authorization" . "Token abc")))
      (expect (clipimg-upload--fields 'carrier "abc")
              :to-equal '(("kind" . "screenshot")))))

  (it "goes into the form field the entry names"
    (expect (clipimg-upload--fields 'imgbb "key123")
            :to-equal '(("key" . "key123")))
    (expect (clipimg-upload--headers 'imgbb "BOUND" "key123")
            :to-equal '(("Content-Type" . "multipart/form-data; boundary=BOUND"))))

  (it "joins the constant fields of the service"
    (expect (clipimg-upload--fields 'catbox "hash")
            :to-equal '(("reqtype" . "fileupload") ("userhash" . "hash"))))

  (it "leaves the field out when there is no secret"
    (expect (clipimg-upload--fields 'catbox nil)
            :to-equal '(("reqtype" . "fileupload"))))

  (it "leaves the header out when there is no secret"
    (let ((clipimg-upload-services clipimg-upload-tests--services))
      (expect (clipimg-upload--headers 'carrier "BOUND" nil)
              :to-equal '(("Content-Type" . "multipart/form-data; boundary=BOUND"))))))

(describe "reading the answer"
  (it "takes a bare URL as it stands, without the newline"
    (expect (clipimg-upload--answer 'litterbox "https://litter.catbox.moe/a.png\n")
            :to-equal "https://litter.catbox.moe/a.png"))

  (it "digs a path out of JSON"
    (expect (clipimg-upload--answer
             'tmpfiles "{\"status\":\"success\",\"data\":{\"url\":\"https://tmpfiles.org/1/a.png\"}}")
            :to-equal "https://tmpfiles.org/1/a.png"))

  (it "signals when the path is not there"
    (expect (clipimg-upload--answer 'tmpfiles "{\"data\":{}}") :to-throw 'user-error))

  (it "signals when the answer is not JSON at all"
    (expect (clipimg-upload--answer 'tmpfiles "<html>gone</html>") :to-throw 'user-error))

  (it "calls a function an entry gives instead"
    (let ((clipimg-upload-services
           '((mine :url "https://example.com"
                   :answer (lambda (body) (concat "https://" (string-trim body)))))))
      (expect (clipimg-upload--answer 'mine "host/a.png ")
              :to-equal "https://host/a.png"))))

(describe "posting"
  (it "returns the body the host answered with"
    (spy-on 'url-retrieve-synchronously
            :and-call-fake (lambda (&rest _)
                             (clipimg-upload-tests--response 200 "{\"ok\":1}")))
    (expect (clipimg-upload--post "https://example.com" nil "body")
            :to-equal "{\"ok\":1}"))

  (it "quotes the host when it turns the upload down"
    (spy-on 'url-retrieve-synchronously
            :and-call-fake (lambda (&rest _)
                             (clipimg-upload-tests--response 413 "too large")))
    (expect (clipimg-upload--post "https://example.com" nil "body")
            :to-throw 'user-error))

  (it "signals when the host says nothing at all"
    (spy-on 'url-retrieve-synchronously :and-return-value nil)
    (expect (clipimg-upload--post "https://example.com" nil "body")
            :to-throw 'user-error)))

(describe "sending a clip"
  (before-each
    (spy-on 'clipimg-upload--post
            :and-return-value "https://litter.catbox.moe/a.png\n"))

  (it "returns the URL the host answered with"
    (expect (clipimg-upload-send (clipimg-upload-tests--clip) 'litterbox)
            :to-equal "https://litter.catbox.moe/a.png"))

  (it "posts the bytes of the clip to the URL of the service"
    (clipimg-upload-send (clipimg-upload-tests--clip) 'litterbox)
    (let ((arguments (spy-calls-args-for 'clipimg-upload--post 0)))
      (expect (nth 0 arguments)
              :to-equal "https://litterbox.catbox.moe/resources/internals/api.php")
      (expect (string-search clipimg-upload-tests--data (nth 2 arguments))
              :not :to-be nil)
      (expect (string-search "name=\"time\"\r\n\r\n72h" (nth 2 arguments))
              :not :to-be nil)
      (expect (string-search
               "name=\"fileToUpload\"; filename=\"clipimg-20260915-103000.png\""
               (nth 2 arguments))
              :not :to-be nil)))

  (it "carries the secret of a service that needs one"
    (spy-on 'auth-source-search :and-return-value '((:secret "key123")))
    (spy-on 'clipimg-upload--post
            :and-return-value "{\"data\":{\"url\":\"https://i.ibb.co/a.png\"}}")
    (expect (clipimg-upload-send (clipimg-upload-tests--clip) 'imgbb)
            :to-equal "https://i.ibb.co/a.png")
    (expect (string-search "key123" (nth 2 (spy-calls-args-for 'clipimg-upload--post 0)))
            :not :to-be nil))

  (it "takes the service from the setting when none is named"
    (let ((clipimg-upload-service 'litterbox))
      (expect (clipimg-upload-send (clipimg-upload-tests--clip))
              :to-equal "https://litter.catbox.moe/a.png")))

  (it "refuses a service it does not know"
    (expect (clipimg-upload-send (clipimg-upload-tests--clip) 'nowhere)
            :to-throw 'user-error)
    (expect 'clipimg-upload--post :not :to-have-been-called)))

(describe "asking first"
  (before-each
    (spy-on 'clipimg-upload--post
            :and-return-value "https://litter.catbox.moe/a.png\n"))

  (it "names the service, the size and the retention"
    (spy-on 'y-or-n-p :and-return-value nil)
    (ignore-errors (clipimg-upload-url (clipimg-upload-tests--clip) 'litterbox))
    (expect (spy-calls-args-for 'y-or-n-p 0)
            :to-equal '("Upload 8 to Litterbox (72 hours)? ")))

  (it "sends nothing when the answer is no"
    (spy-on 'y-or-n-p :and-return-value nil)
    (expect (clipimg-upload-url (clipimg-upload-tests--clip) 'litterbox)
            :to-throw 'user-error)
    (expect 'clipimg-upload--post :not :to-have-been-called))

  (it "sends when the answer is yes"
    (spy-on 'y-or-n-p :and-return-value t)
    (expect (clipimg-upload-url (clipimg-upload-tests--clip) 'litterbox)
            :to-equal "https://litter.catbox.moe/a.png"))

  (it "puts the URL on the kill ring"
    (spy-on 'y-or-n-p :and-return-value t)
    (spy-on 'kill-new)
    (clipimg-upload-to-kill-ring (clipimg-upload-tests--clip) 'litterbox)
    (expect 'kill-new :to-have-been-called-with "https://litter.catbox.moe/a.png"))

  (it "inserts the URL at point"
    (spy-on 'y-or-n-p :and-return-value t)
    (with-temp-buffer
      (clipimg-upload-insert (clipimg-upload-tests--clip) 'litterbox)
      (expect (buffer-string) :to-equal "https://litter.catbox.moe/a.png")))

  (it "inserts nothing into a read only buffer"
    (spy-on 'y-or-n-p :and-return-value t)
    (with-temp-buffer
      (setq buffer-read-only t)
      (expect (clipimg-upload-insert (clipimg-upload-tests--clip) 'litterbox)
              :to-throw 'user-error))))

;;; clipimg-upload-tests.el ends here
