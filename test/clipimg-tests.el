;;; clipimg-tests.el --- Tests for clipimg -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;;; Commentary:
;; The clip, the clipboard readers and the file a clip is written to.
;;
;;; Code:

(require 'buttercup)
(require 'clipimg)

(defconst clipimg-tests-fixture
  (expand-file-name "fixture.png"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "A 700x220 PNG with two columns of text.")

(defun clipimg-tests-bytes ()
  "Return the fixture image as raw bytes."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally clipimg-tests-fixture)
    (buffer-string)))

(describe "clipimg--image-bytes"
  (it "accepts data Emacs recognizes as an image"
    (expect (clipimg--image-bytes (clipimg-tests-bytes)) :not :to-be nil))

  (it "rejects text, empty data and nil"
    (expect (clipimg--image-bytes "hello") :to-be nil)
    (expect (clipimg--image-bytes "") :to-be nil)
    (expect (clipimg--image-bytes nil) :to-be nil)))

(describe "clipimg--selection-bytes"
  (before-each
    (spy-on 'display-graphic-p :and-return-value t))

  (it "reads the first offered type of clipimg-clipboard-types"
    (let ((bytes (clipimg-tests-bytes)))
      (spy-on 'gui-get-selection
              :and-call-fake (lambda (_selection type)
                               (pcase type
                                 ('TARGETS [TARGETS image/tiff image/png])
                                 ('image/png bytes)
                                 (_ ""))))
      (expect (clipimg--selection-bytes) :to-equal bytes)))

  (it "returns nil when no offered type holds an image"
    (spy-on 'gui-get-selection
            :and-call-fake (lambda (_selection type)
                             (if (eq type 'TARGETS) [TARGETS STRING] "")))
    (expect (clipimg--selection-bytes) :to-be nil))

  (it "reads nothing on a terminal frame"
    (spy-on 'display-graphic-p :and-return-value nil)
    (spy-on 'gui-get-selection)
    (expect (clipimg--selection-bytes) :to-be nil)
    (expect 'gui-get-selection :not :to-have-been-called)))

(describe "clipimg-clipboard-clip"
  (it "describes what it read"
    (spy-on 'clipimg--selection-bytes :and-return-value (clipimg-tests-bytes))
    (let ((clip (clipimg-clipboard-clip)))
      (expect (clipimg-clip-type clip) :to-be 'png)
      (expect (clipimg-clip-data clip) :to-equal (clipimg-tests-bytes))
      (expect (clipimg-clip-time clip) :not :to-be nil)))

  (it "falls back to a helper program when the selection has nothing"
    (spy-on 'clipimg--selection-bytes :and-return-value nil)
    (spy-on 'clipimg--helper-bytes :and-return-value (clipimg-tests-bytes))
    (expect (clipimg-clip-type (clipimg-clipboard-clip)) :to-be 'png))

  (it "returns nil when neither reader answers"
    (spy-on 'clipimg--selection-bytes :and-return-value nil)
    (spy-on 'clipimg--helper-bytes :and-return-value nil)
    (expect (clipimg-clipboard-clip) :to-be nil)
    (expect (clipimg-clipboard-clip-or-error) :to-throw 'user-error)))

(describe "clipimg--dimensions"
  (it "reports the pixel size when Emacs can measure an image"
    (assume (display-images-p) "needs a display that can measure images")
    (expect (clipimg--dimensions (clipimg-tests-bytes)) :to-equal '(700 . 220)))

  (it "returns nil rather than failing when it cannot measure"
    (expect (clipimg--dimensions "not an image") :to-be nil)))

(describe "clipimg-clip-summary"
  (it "names the type, the size in pixels and the size in bytes"
    (expect (clipimg-clip-summary
             (clipimg-clip-create :data "12345" :type 'png :width 700 :height 220))
            :to-equal "PNG 700x220, 5"))

  (it "leaves the pixel size out when it is unknown"
    (expect (clipimg-clip-summary (clipimg-clip-create :data "12345" :type 'tiff))
            :to-equal "TIFF, 5")))

(describe "clipimg-clip-file"
  (after-each
    (clipimg--delete-temp-files))

  (it "writes the bytes once and keeps the same file"
    (let* ((bytes (clipimg-tests-bytes))
           (clip (clipimg-clip-create :data bytes :type 'png))
           (file (clipimg-clip-file clip)))
      (expect (file-exists-p file) :to-be-truthy)
      (expect (file-name-extension file) :to-equal "png")
      (expect (clipimg--file-bytes file) :to-equal bytes)
      (expect (clipimg-clip-file clip) :to-equal file)))

  (it "forgets its files once they are deleted"
    (let ((file (clipimg-clip-file (clipimg-clip-create :data "x" :type 'png))))
      (clipimg--delete-temp-files)
      (expect (file-exists-p file) :to-be nil)
      (expect clipimg--temp-files :to-be nil))))

(describe "clipimg-clip-filename"
  (it "stamps the name with the time the clip was taken"
    (expect (clipimg-clip-filename
             (clipimg-clip-create :data "x" :type 'png
                                  :time (encode-time 0 30 10 15 9 2026)))
            :to-equal "clipimg-20260915-103000.png"))

  (it "takes the extension of the format it is handed"
    (expect (clipimg-clip-filename
             (clipimg-clip-create :data "x" :type 'tiff
                                  :time (encode-time 0 30 10 15 9 2026))
             'png)
            :to-equal "clipimg-20260915-103000.png"))

  (it "names a clip of unknown type after the format it would be written in"
    (expect (file-name-extension
             (clipimg-clip-filename (clipimg-clip-create :data "x")))
            :to-equal "png")))

(describe "clipimg-convert"
  (it "turns the fixture into another format, and Emacs calls it one"
    (assume (clipimg-converter) "needs ImageMagick or GraphicsMagick")
    (let ((jpeg (clipimg-convert (clipimg-tests-bytes) 'png 'jpeg)))
      (expect (image-type-from-data jpeg) :to-be 'jpeg)
      (expect jpeg :not :to-equal (clipimg-tests-bytes))))

  (it "refuses an answer with anything in front of the image"
    ;; ImageMagick 7 warns about the name `convert' on standard error, and
    ;; a converter reading that stream back into the image data hands over
    ;; bytes no viewer opens.  Emacs' own `image-convert' does.
    (spy-on 'clipimg-converter :and-return-value '("magick"))
    (spy-on 'call-process-region
            :and-call-fake
            (lambda (start end &rest _)
              (delete-region start end)
              (insert "WARNING: the convert command is deprecated\n")
              (insert-file-contents-literally clipimg-tests-fixture)
              0))
    (expect (clipimg-convert (clipimg-tests-bytes) 'png 'jpeg)
            :to-throw 'user-error))

  (it "refuses when the converter fails"
    (spy-on 'clipimg-converter :and-return-value '("magick"))
    (spy-on 'call-process-region :and-return-value 1)
    (expect (clipimg-convert (clipimg-tests-bytes) 'png 'jpeg)
            :to-throw 'user-error))

  (it "says what it needs when nothing on PATH can convert"
    (spy-on 'clipimg-converter :and-return-value nil)
    (expect (clipimg-convert "x" 'png 'jpeg) :to-throw 'user-error)))

(describe "clipimg-clip-format"
  (it "keeps the format asked for when the clip is already in it"
    (spy-on 'clipimg-converter :and-return-value nil)
    (expect (clipimg-clip-format (clipimg-clip-create :data "x" :type 'png) 'png)
            :to-be 'png))

  (it "keeps the format asked for when something can convert to it"
    (spy-on 'clipimg-converter :and-return-value '("magick"))
    (expect (clipimg-clip-format (clipimg-clip-create :data "x" :type 'tiff) 'png)
            :to-be 'png))

  (it "falls back to the clip when nothing can convert"
    (spy-on 'clipimg-converter :and-return-value nil)
    (expect (clipimg-clip-format (clipimg-clip-create :data "x" :type 'tiff) 'png)
            :to-be 'tiff))

  (it "falls back to the clip when no format is asked for"
    (expect (clipimg-clip-format (clipimg-clip-create :data "x" :type 'tiff))
            :to-be 'tiff)))

(describe "clipimg-clip-bytes"
  (it "hands the bytes over untouched for the format the clip is in"
    (spy-on 'clipimg-convert)
    (let ((clip (clipimg-clip-create :data "bytes" :type 'png)))
      (expect (clipimg-clip-bytes clip 'png) :to-equal "bytes")
      (expect (clipimg-clip-bytes clip) :to-equal "bytes")
      (expect 'clipimg-convert :not :to-have-been-called)))

  (it "converts to a format the clip is not in"
    (spy-on 'clipimg-convert :and-return-value "converted")
    (expect (clipimg-clip-bytes (clipimg-clip-create :data "bytes" :type 'tiff) 'png)
            :to-equal "converted")
    (expect 'clipimg-convert :to-have-been-called-with "bytes" 'tiff 'png)))

(describe "clipimg-clip-preview"
  (it "carries the summary as text so a terminal frame shows something"
    (let ((preview (clipimg-clip-preview
                    (clipimg-clip-create :data (clipimg-tests-bytes) :type 'png
                                         :width 700 :height 220))))
      (expect (substring-no-properties preview) :to-equal "PNG 700x220, 6.7k"))))

;;; clipimg-tests.el ends here
