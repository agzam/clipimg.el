;;; clipimg-menu-tests.el --- Tests for the clipimg menu -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;;; Commentary:
;; What the menu reads out of its arguments, and what its header says.
;;
;;; Code:

(require 'buttercup)
(require 'clipimg-menu)

(describe "the clipimg prefix"
  (it "is a transient"
    (expect (get 'clipimg 'transient--prefix) :not :to-be nil))

  (it "opens on nothing when the clipboard holds no image"
    (spy-on 'clipimg-clipboard-clip :and-return-value nil)
    (expect (clipimg) :to-throw 'user-error)))

(describe "reading the menu arguments"
  (it "takes the backend, language and layout the arguments name"
    (spy-on 'clipimg-menu--args
            :and-return-value '("--backend=tesseract" "--language=deu" "--layout=sparse"))
    (expect (clipimg-menu--backend) :to-be 'tesseract)
    (expect (clipimg-menu--language) :to-equal "deu")
    (expect (clipimg-menu--layout) :to-be 'sparse))

  (it "falls back to the settings when the arguments name nothing"
    (spy-on 'clipimg-menu--args :and-return-value nil)
    (spy-on 'clipimg-ocr-backend :and-return-value 'vision)
    (let ((clipimg-ocr-layout 'auto))
      (expect (clipimg-menu--backend) :to-be 'vision)
      (expect (clipimg-menu--language) :to-be nil)
      (expect (clipimg-menu--layout) :to-be 'auto))))

(describe "the header"
  (before-each
    (spy-on 'clipimg-menu--args :and-return-value nil))

  (it "names what is on the clipboard"
    (let ((clipimg-menu--clip (clipimg-clip-create :data "12345" :type 'png
                                                   :width 700 :height 220)))
      (expect (substring-no-properties (clipimg-menu--clipboard-line))
              :to-equal "Clipboard: PNG 700x220, 5")))

  (it "says the clipboard is empty when the menu holds nothing"
    (let ((clipimg-menu--clip nil))
      (expect (substring-no-properties (clipimg-menu--clipboard-line))
              :to-equal "Clipboard: empty")))

  (it "lists an engine that cannot run"
    (let ((clipimg-ocr-backend 'absent)
          (clipimg-ocr-backends
           '((absent :label "Absent" :available-p ignore :recognize ignore))))
      (expect (clipimg-menu--problems) :to-equal
              '("Absent is not available on this machine"))))

  (it "lists nothing when the engine can run"
    (let ((clipimg-ocr-backend 'here)
          (clipimg-ocr-backends '((here :available-p always :recognize ignore))))
      (expect (clipimg-menu--problems) :to-be nil))))

(describe "clipimg-menu--origin-read-only-p"
  (it "sees a read only buffer"
    (with-temp-buffer
      (setq buffer-read-only t)
      (spy-on 'clipimg-menu--origin-buffer :and-return-value (current-buffer))
      (expect (clipimg-menu--origin-read-only-p) :to-be-truthy)))

  (it "sees a writable buffer"
    (with-temp-buffer
      (spy-on 'clipimg-menu--origin-buffer :and-return-value (current-buffer))
      (expect (clipimg-menu--origin-read-only-p) :to-be nil))))

(describe "the menu actions"
  (before-each
    (spy-on 'clipimg-menu--args :and-return-value '("--backend=fake" "--layout=block"))
    (setq clipimg-menu--clip (clipimg-clip-create :data "x" :type 'png :path "/tmp/x.png")))

  (after-each
    (setq clipimg-menu--clip nil))

  (it "passes the clip and the options on"
    (spy-on 'clipimg-ocr-to-buffer)
    (clipimg-menu-ocr-buffer)
    (expect 'clipimg-ocr-to-buffer :to-have-been-called-with
            clipimg-menu--clip 'fake nil 'block))

  (it "inserts into the buffer the menu was opened from"
    (with-temp-buffer
      (let ((origin (current-buffer)))
        (spy-on 'clipimg-menu--origin-buffer :and-return-value origin)
        (spy-on 'clipimg-ocr-recognize :and-return-value "read text")
        (clipimg-menu-ocr-insert)
        (expect (buffer-string) :to-equal "read text"))))

  (it "replaces the clip when the clipboard is read again"
    (let ((fresh (clipimg-clip-create :data "y" :type 'png)))
      (spy-on 'clipimg-clipboard-clip-or-error :and-return-value fresh)
      (clipimg-menu-read-clipboard)
      (expect clipimg-menu--clip :to-be fresh))))

;;; clipimg-menu-tests.el ends here
