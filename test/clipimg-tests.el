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

(describe "clipimg-clip-preview"
  (it "carries the summary as text so a terminal frame shows something"
    (let ((preview (clipimg-clip-preview
                    (clipimg-clip-create :data (clipimg-tests-bytes) :type 'png
                                         :width 700 :height 220))))
      (expect (substring-no-properties preview) :to-equal "PNG 700x220, 6.7k"))))

;;; clipimg-tests.el ends here
