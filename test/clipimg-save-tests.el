;;; clipimg-save-tests.el --- Tests for clipimg saving -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;;; Commentary:
;; Where a save goes, what format it takes, and what lands on disk.  The
;; specs that matter here write real files into a temporary directory and
;; read them back with `image-type-from-data'.  A spec asserting that a
;; converter was called would pass on bytes no viewer can open, which is
;; exactly how the converter Emacs itself reaches for behaves here.
;;
;;; Code:

(require 'buttercup)
(require 'clipimg-save)

(defconst clipimg-save-tests-fixture
  (expand-file-name "fixture.png"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "A 700x220 PNG with two columns of text.")

(defvar clipimg-save-tests--dir nil
  "Throwaway directory the specs write into.")

(defun clipimg-save-tests--clip ()
  "Return a clip of the fixture, taken at a time of its own."
  (clipimg-clip-create :data (clipimg--file-bytes clipimg-save-tests-fixture)
                       :type 'png
                       :width 700 :height 220
                       :time (encode-time 0 30 10 15 9 2026)))

(defun clipimg-save-tests--clip-of (type)
  "Return a clip of image type TYPE, for specs that never look at the bytes."
  (clipimg-clip-create :data "x" :type type
                       :time (encode-time 0 30 10 15 9 2026)))

(defun clipimg-save-tests--in (name)
  "Return NAME under the directory of the running spec."
  (expand-file-name name clipimg-save-tests--dir))

(defun clipimg-save-tests--sniff (file)
  "Return the image type Emacs reads off the bytes of FILE."
  (image-type-from-data (clipimg--file-bytes file)))

(describe "the format a name asks for"
  (it "takes the extension of the name"
    (expect (clipimg-save--format "a.png" 'tiff) :to-be 'png)
    (expect (clipimg-save--format "/tmp/a.webp" 'png) :to-be 'webp))

  (it "calls both spellings of a format by the name Emacs gives it"
    (expect (clipimg-save--format "a.jpg" 'png) :to-be 'jpeg)
    (expect (clipimg-save--format "a.jpeg" 'png) :to-be 'jpeg)
    (expect (clipimg-save--format "a.tif" 'png) :to-be 'tiff))

  (it "minds no shouting"
    (expect (clipimg-save--format "A.PNG" 'tiff) :to-be 'png)
    (expect (clipimg-save--format "A.JPG" 'png) :to-be 'jpeg))

  (it "falls back when the name carries no extension"
    (expect (clipimg-save--format "shot" 'png) :to-be 'png)
    (expect (clipimg-save--format "shot" 'tiff) :to-be 'tiff)))

(describe "the prompt"
  (before-each
    (spy-on 'y-or-n-p :and-return-value t))

  (it "starts in the directory of the setting, under the stamped name"
    (spy-on 'read-file-name :and-return-value "/tmp/clipimg-spec.png")
    (spy-on 'clipimg-save-write)
    (let ((clipimg-save-directory "/tmp/pictures"))
      (clipimg-save-file (clipimg-save-tests--clip)))
    (expect (spy-calls-args-for 'read-file-name 0)
            :to-equal '("Save image as: " "/tmp/pictures/" nil nil
                        "clipimg-20260915-103000.png")))

  (it "asks nothing when the caller already named the file"
    (spy-on 'read-file-name)
    (spy-on 'clipimg-save-write)
    (clipimg-save-file (clipimg-save-tests--clip) "/tmp/clipimg-spec.png")
    (expect 'read-file-name :not :to-have-been-called)))

(describe "the format the prompt offers"
  (before-each
    (spy-on 'y-or-n-p :and-return-value t)
    (spy-on 'read-file-name :and-return-value "/tmp/clipimg-spec.png")
    (spy-on 'clipimg-save-write))

  (defun clipimg-save-tests--offered (clip)
    "Return the name the prompt starts CLIP off with."
    (clipimg-save-file clip)
    (nth 4 (spy-calls-args-for 'read-file-name 0)))

  (it "offers the format of the setting for a clip in another one"
    (spy-on 'clipimg-converter :and-return-value '("magick"))
    (let ((clipimg-save-default-format 'png))
      (expect (clipimg-save-tests--offered (clipimg-save-tests--clip-of 'tiff))
              :to-equal "clipimg-20260915-103000.png")))

  (it "offers the format of the clip when nothing on PATH can convert"
    ;; Offering png on a machine that cannot make one would put the error
    ;; where the default is, which is the one place it must not be.
    (spy-on 'clipimg-converter :and-return-value nil)
    (let ((clipimg-save-default-format 'png))
      (expect (clipimg-save-tests--offered (clipimg-save-tests--clip-of 'tiff))
              :to-equal "clipimg-20260915-103000.tiff")))

  (it "offers the format of the clip when the setting names none"
    (spy-on 'clipimg-converter :and-return-value '("magick"))
    (let ((clipimg-save-default-format nil))
      (expect (clipimg-save-tests--offered (clipimg-save-tests--clip-of 'tiff))
              :to-equal "clipimg-20260915-103000.tiff"))))

(describe "what the prompt says while a name is typed"
  (it "stays quiet while an extension is half typed"
    (spy-on 'clipimg-converter :and-return-value nil)
    (expect (clipimg-save-format-problem
             "shot.p" (clipimg-save-tests--clip-of 'tiff))
            :to-be nil))

  (it "stays quiet for the format the clip is already in"
    (spy-on 'clipimg-converter :and-return-value nil)
    (expect (clipimg-save-format-problem
             "shot.tiff" (clipimg-save-tests--clip-of 'tiff))
            :to-be nil))

  (it "stays quiet when a converter is there"
    (spy-on 'clipimg-converter :and-return-value '("magick"))
    (expect (clipimg-save-format-problem
             "shot.png" (clipimg-save-tests--clip-of 'tiff))
            :to-be nil))

  (it "names what the typed format needs when nothing can make it"
    (spy-on 'clipimg-converter :and-return-value nil)
    (expect (clipimg-save-format-problem
             "shot.png" (clipimg-save-tests--clip-of 'tiff))
            :to-equal "PNG needs ImageMagick or GraphicsMagick")
    (expect (clipimg-save-format-problem
             "shot.jpg" (clipimg-save-tests--clip-of 'tiff))
            :to-equal "JPEG needs ImageMagick or GraphicsMagick"))

  (it "says it once per answer, not once per keystroke"
    (spy-on 'clipimg-converter :and-return-value nil)
    (spy-on 'minibuffer-message)
    (let* ((typed "shot.png")
           (watch (clipimg-save--name-watcher (clipimg-save-tests--clip-of 'tiff))))
      (spy-on 'minibuffer-contents :and-call-fake (lambda () typed))
      (funcall watch)
      (funcall watch)
      (expect (spy-calls-count 'minibuffer-message) :to-equal 1)
      (setq typed "shot.tiff")
      (funcall watch)
      (setq typed "shot.png")
      (funcall watch)
      (expect (spy-calls-count 'minibuffer-message) :to-equal 2))))

(describe "writing a file"
  (before-each
    (setq clipimg-save-tests--dir (make-temp-file "clipimg-save-tests" t)))

  (after-each
    (delete-directory clipimg-save-tests--dir t)
    (setq clipimg-save-tests--dir nil))

  (it "puts the bytes of the clip on disk, and they open as an image"
    (let ((file (clipimg-save-write (clipimg-save-tests--clip)
                                    (clipimg-save-tests--in "shot.png"))))
      (expect (file-exists-p file) :to-be-truthy)
      (expect (clipimg--file-bytes file)
              :to-equal (clipimg--file-bytes clipimg-save-tests-fixture))
      (expect (clipimg-save-tests--sniff file) :to-be 'png)))

  (it "needs no converter for the format the clip is already in"
    (spy-on 'clipimg-converter :and-return-value nil)
    (expect (clipimg-save-tests--sniff
             (clipimg-save-write (clipimg-save-tests--clip)
                                 (clipimg-save-tests--in "shot.png")))
            :to-be 'png))

  (it "makes the directory the name asks for"
    (let ((file (clipimg-save-write (clipimg-save-tests--clip)
                                    (clipimg-save-tests--in "shots/2026/a.png"))))
      (expect (file-exists-p file) :to-be-truthy)))

  (it "turns the image into the format the extension asks for"
    (assume (clipimg-converter) "needs ImageMagick or GraphicsMagick")
    (dolist (case '(("shot.jpg" . jpeg) ("shot.tiff" . tiff)))
      (let ((file (clipimg-save-write (clipimg-save-tests--clip)
                                      (clipimg-save-tests--in (car case)))))
        (expect (clipimg-save-tests--sniff file) :to-be (cdr case))
        (expect (clipimg--file-bytes file)
                :not :to-equal (clipimg--file-bytes clipimg-save-tests-fixture)))))

  (it "leaves no file behind when the converter talks over the image"
    (spy-on 'clipimg-converter :and-return-value '("magick"))
    (spy-on 'call-process-region
            :and-call-fake
            (lambda (start end &rest _)
              (delete-region start end)
              (insert "WARNING: the convert command is deprecated\n")
              (insert-file-contents-literally clipimg-save-tests-fixture)
              0))
    (let ((file (clipimg-save-tests--in "shot.jpg")))
      (expect (clipimg-save-write (clipimg-save-tests--clip) file)
              :to-throw 'user-error)
      (expect (file-exists-p file) :to-be nil)))

  (it "leaves no file behind when the converter fails"
    (spy-on 'clipimg-converter :and-return-value '("magick"))
    (spy-on 'call-process-region :and-return-value 1)
    (let ((file (clipimg-save-tests--in "shot.jpg")))
      (expect (clipimg-save-write (clipimg-save-tests--clip) file)
              :to-throw 'user-error)
      (expect (file-exists-p file) :to-be nil)))

  (it "says what another format needs when nothing on PATH can convert"
    (spy-on 'clipimg-converter :and-return-value nil)
    (expect (clipimg-save-write (clipimg-save-tests--clip)
                                (clipimg-save-tests--in "shot.jpg"))
            :to-throw 'user-error)))

(describe "overwriting"
  (before-each
    (setq clipimg-save-tests--dir (make-temp-file "clipimg-save-tests" t)))

  (after-each
    (delete-directory clipimg-save-tests--dir t)
    (setq clipimg-save-tests--dir nil))

  (it "asks nothing when nothing is there"
    (spy-on 'y-or-n-p)
    (clipimg-save-file (clipimg-save-tests--clip) (clipimg-save-tests--in "a.png"))
    (expect 'y-or-n-p :not :to-have-been-called))

  (it "keeps the file that is there when the answer is no"
    (let ((file (clipimg-save-tests--in "a.png")))
      (write-region "not an image" nil file nil 'silent)
      (spy-on 'y-or-n-p :and-return-value nil)
      (expect (clipimg-save-file (clipimg-save-tests--clip) file)
              :to-throw 'user-error)
      (expect (clipimg--file-bytes file) :to-equal "not an image")))

  (it "replaces it when the answer is yes"
    (let ((file (clipimg-save-tests--in "a.png")))
      (write-region "not an image" nil file nil 'silent)
      (spy-on 'y-or-n-p :and-return-value t)
      (clipimg-save-file (clipimg-save-tests--clip) file)
      (expect (clipimg-save-tests--sniff file) :to-be 'png))))

(describe "clipimg-save"
  (before-each
    (setq clipimg-save-tests--dir (make-temp-file "clipimg-save-tests" t)))

  (after-each
    (delete-directory clipimg-save-tests--dir t)
    (setq clipimg-save-tests--dir nil))

  (it "puts what is on the clipboard where the prompt says"
    (let ((file (clipimg-save-tests--in "from-clipboard.png")))
      (spy-on 'clipimg-clipboard-clip
              :and-return-value (clipimg-save-tests--clip))
      (spy-on 'read-file-name :and-return-value file)
      (expect (clipimg-save) :to-equal file)
      (expect (clipimg-save-tests--sniff file) :to-be 'png)))

  (it "saves nothing when the clipboard holds no image"
    (spy-on 'clipimg-clipboard-clip :and-return-value nil)
    (spy-on 'read-file-name)
    (expect (clipimg-save) :to-throw 'user-error)
    (expect 'read-file-name :not :to-have-been-called)))

;;; clipimg-save-tests.el ends here
