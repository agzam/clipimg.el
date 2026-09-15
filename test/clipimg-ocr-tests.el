;;; clipimg-ocr-tests.el --- Tests for clipimg text recognition -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;;; Commentary:
;; Backend choice, the engines' arguments, and assembling Vision's runs.
;;
;;; Code:

(require 'buttercup)
(require 'clipimg-ocr)

(defconst clipimg-ocr-tests-fixture
  (expand-file-name "fixture.png"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "A 700x220 PNG with two columns of text.")

(defconst clipimg-ocr-tests-boxes
  '((:text "The quick brown fox" :x 0.0257 :y 0.7909 :height 0.1182)
    (:text "jumps over the lazy dog" :x 0.0200 :y 0.6000 :height 0.1364)
    (:text "clipimg 1234 test" :x 0.0257 :y 0.2455 :height 0.1273)
    (:text "second column" :x 0.5971 :y 0.8000 :height 0.1091)
    (:text "of text here" :x 0.5969 :y 0.6235 :height 0.1166))
  "What Vision returned for the fixture, in the order it returned it.")

(defconst clipimg-ocr-tests-clip
  (clipimg-clip-create :data "png bytes" :type 'png :path "/tmp/clipimg-fake.png")
  "A clip whose file is already on disk, so nothing is written.")

(describe "clipimg-ocr-backend"
  (it "takes the first backend this machine can run"
    (let ((clipimg-ocr-backend 'auto)
          (clipimg-ocr-backends
           '((absent :available-p ignore :recognize ignore)
             (present :available-p always :recognize ignore))))
      (expect (clipimg-ocr-backend) :to-be 'present)))

  (it "keeps a backend named outright, available or not"
    (let ((clipimg-ocr-backend 'absent)
          (clipimg-ocr-backends '((absent :available-p ignore :recognize ignore))))
      (expect (clipimg-ocr-backend) :to-be 'absent)))

  (it "falls back to the preferred backend when none is available"
    (let ((clipimg-ocr-backend 'auto)
          (clipimg-ocr-backends
           '((first :available-p ignore :recognize ignore)
             (second :available-p ignore :recognize ignore))))
      (expect (clipimg-ocr-backend) :to-be 'first))))

(describe "clipimg-ocr-problem"
  (it "reports an unknown backend"
    (let ((clipimg-ocr-backends nil))
      (expect (clipimg-ocr-problem 'nope) :to-match "not a known OCR backend")))

  (it "reports a backend this machine cannot run"
    (let ((clipimg-ocr-backends
           '((absent :label "Absent" :available-p ignore :recognize ignore))))
      (expect (clipimg-ocr-problem 'absent) :to-match "not available")))

  (it "says nothing about a backend that can run"
    (let ((clipimg-ocr-backends '((here :available-p always :recognize ignore))))
      (expect (clipimg-ocr-problem 'here) :to-be nil))))

(describe "clipimg-ocr-recognize"
  (it "hands the file and the options to the backend"
    (let* ((seen nil)
           (clipimg-ocr-backends
            `((fake :available-p always
                    :recognize ,(lambda (&rest arguments) (setq seen arguments) "text")))))
      (expect (clipimg-ocr-recognize clipimg-ocr-tests-clip 'fake "eng" 'sparse)
              :to-equal "text")
      (expect seen :to-equal (list "/tmp/clipimg-fake.png" "eng" 'sparse))))

  (it "falls back to the configured language and layout"
    (let* ((seen nil)
           (clipimg-ocr-language "deu")
           (clipimg-ocr-layout 'auto)
           (clipimg-ocr-backends
            `((fake :available-p always
                    :recognize ,(lambda (&rest arguments) (setq seen arguments) "")))))
      (clipimg-ocr-recognize clipimg-ocr-tests-clip 'fake)
      (expect (cdr seen) :to-equal (list "deu" 'auto))))

  (it "refuses a backend that cannot run"
    (let ((clipimg-ocr-backends '((absent :available-p ignore :recognize ignore))))
      (expect (clipimg-ocr-recognize clipimg-ocr-tests-clip 'absent)
              :to-throw 'user-error))))

(describe "clipimg-ocr--tesseract"
  (it "turns the layout into a page segmentation mode"
    (let (seen)
      (spy-on 'clipimg-ocr--output
              :and-call-fake (lambda (&rest arguments) (setq seen arguments) "text\n"))
      (clipimg-ocr--tesseract "/tmp/x.png" nil 'sparse)
      (expect seen :to-equal '("tesseract" "/tmp/x.png" "-" "--psm" "11"))))

  (it "passes a language only when there is one"
    (let (seen)
      (spy-on 'clipimg-ocr--output
              :and-call-fake (lambda (&rest arguments) (setq seen arguments) ""))
      (clipimg-ocr--tesseract "/tmp/x.png" "eng" 'block)
      (expect seen :to-equal '("tesseract" "/tmp/x.png" "-" "-l" "eng" "--psm" "6"))))

  (it "returns an empty string when tesseract is absent"
    (spy-on 'clipimg-ocr--output :and-return-value nil)
    (expect (clipimg-ocr--tesseract "/tmp/x.png" nil 'block) :to-equal "")))

(describe "clipimg-ocr--same-line-p"
  (it "joins boxes that overlap by more than half the shorter one"
    (expect (clipimg-ocr--same-line-p (nth 0 clipimg-ocr-tests-boxes)
                                      (nth 3 clipimg-ocr-tests-boxes))
            :to-be-truthy))

  (it "separates boxes on different rows"
    (expect (clipimg-ocr--same-line-p (nth 0 clipimg-ocr-tests-boxes)
                                      (nth 1 clipimg-ocr-tests-boxes))
            :to-be nil)))

(describe "clipimg-ocr--assemble"
  (it "puts side by side runs on one line under block"
    (expect (clipimg-ocr--assemble clipimg-ocr-tests-boxes 'block)
            :to-equal (concat "The quick brown fox second column\n"
                              "jumps over the lazy dog of text here\n"
                              "clipimg 1234 test")))

  (it "gives every run a line of its own under sparse, in reading order"
    (expect (clipimg-ocr--assemble clipimg-ocr-tests-boxes 'sparse)
            :to-equal (concat "The quick brown fox\nsecond column\n"
                              "jumps over the lazy dog\nof text here\n"
                              "clipimg 1234 test")))

  (it "leaves the caller's list alone"
    (let ((runs (copy-sequence clipimg-ocr-tests-boxes)))
      (clipimg-ocr--assemble runs 'block)
      (expect (plist-get (car runs) :text) :to-equal "The quick brown fox")))

  (it "returns an empty string for no runs"
    (expect (clipimg-ocr--assemble nil 'block) :to-equal "")))

(describe "clipimg-ocr--vision-boxes"
  (it "reads the script's JSON into box plists"
    (expect (clipimg-ocr--vision-boxes
             "[{\"text\":\"hi\",\"x\":0.1,\"y\":0.8,\"height\":0.1}]")
            :to-equal '((:text "hi" :x 0.1 :y 0.8 :height 0.1))))

  (it "reads no runs out of an empty answer"
    (expect (clipimg-ocr--vision-boxes "  \n") :to-be nil)))

(describe "clipimg-ocr--kill"
  (it "keeps the image on the clipboard while it fills the kill ring"
    (let (clipboard-enabled)
      (spy-on 'kill-new :and-call-fake
              (lambda (&rest _) (setq clipboard-enabled select-enable-clipboard)))
      (clipimg-ocr--kill "text")
      (expect 'kill-new :to-have-been-called-with "text")
      (expect clipboard-enabled :to-be nil))))

(describe "clipimg-ocr--render"
  (after-each
    (when-let* ((buffer (get-buffer clipimg-ocr-buffer-name)))
      (kill-buffer buffer)))

  (it "shows the preview, then the text"
    (let ((clip (clipimg-clip-create :data "12345" :type 'png :width 700 :height 220)))
      (with-current-buffer (clipimg-ocr--render clip "read text" nil)
        (expect (buffer-substring-no-properties (point-min) (point-max))
                :to-equal "PNG 700x220, 5\n\nread text")
        (expect (clipimg-ocr-text) :to-equal "read text")
        (expect major-mode :to-be 'clipimg-ocr-mode)
        (expect buffer-read-only :to-be-truthy))))

  (it "says so when the engine found nothing"
    (let ((clip (clipimg-clip-create :data "12345" :type 'png)))
      (with-current-buffer (clipimg-ocr--render clip "  " nil)
        (expect (clipimg-ocr-text) :to-equal "no text found")))))

(describe "the engines on the fixture"
  (it "reads the fixture with tesseract"
    (assume (clipimg-ocr--tesseract-available-p) "tesseract is not installed")
    (let ((text (clipimg-ocr--tesseract clipimg-ocr-tests-fixture nil 'block)))
      (expect text :to-match "The quick brown fox")
      (expect text :to-match "clipimg 1234 test")))

  (it "reads the fixture with Vision"
    (assume (clipimg-ocr--vision-available-p) "Vision needs macOS")
    (let ((text (clipimg-ocr--vision clipimg-ocr-tests-fixture nil 'block)))
      (expect text :to-match "The quick brown fox second column")
      (expect text :to-match "clipimg 1234 test")))

  (it "lists the languages tesseract has"
    (assume (clipimg-ocr--tesseract-available-p) "tesseract is not installed")
    (let ((clipimg-ocr--tesseract-language-cache nil))
      (expect (clipimg-ocr--tesseract-languages) :to-contain "eng")))

  (it "lists the languages Vision has"
    (assume (clipimg-ocr--vision-available-p) "Vision needs macOS")
    (let ((clipimg-ocr--vision-language-cache nil))
      (expect (clipimg-ocr--vision-languages) :to-contain "en-US"))))

;;; clipimg-ocr-tests.el ends here
