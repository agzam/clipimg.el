EMACS ?= emacs
ELPA_DIR = $(CURDIR)/.elpa
SANDBOX_DIR = $(CURDIR)/.sandbox
AUTOLOADS = $(CURDIR)/clipimg-autoloads.el
PACKAGE_FILES = clipimg.el clipimg-ocr.el clipimg-upload.el clipimg-menu.el
TEST_FILES = test/clipimg-tests.el test/clipimg-ocr-tests.el \
	test/clipimg-upload-tests.el test/clipimg-menu-tests.el

# Every Emacs invocation runs against a repo-local user-emacs-directory, so
# nothing touches the developer's real ~/.emacs.d (eln cache, auto-save list,
# package dir).
EMACS_BATCH = $(EMACS) -Q --batch --init-directory "$(SANDBOX_DIR)" \
	--eval "(setq package-user-dir \"$(ELPA_DIR)\")" \
	--eval "(require 'package)" \
	--eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\"))" \
	--eval "(package-initialize)"

EMACS_SANDBOX = $(EMACS) -Q --init-directory "$(SANDBOX_DIR)" \
	--eval "(setq package-user-dir \"$(ELPA_DIR)\")" \
	--eval "(require 'package)" \
	--eval "(package-initialize)"

.PHONY: help deps test check-hosts check-compile check-autoloads lint sandbox clean

help:
	@echo "Available commands:"
	@echo "  make deps             Install dependencies into .elpa"
	@echo "  make test             Run the test suites"
	@echo "  make check-hosts      Upload to every host for real and read it back"
	@echo "  make check-compile    Byte-compile with warnings as errors"
	@echo "  make check-autoloads  Generate and load autoloads"
	@echo "  make lint             Run package-lint and checkdoc"
	@echo "  make sandbox          Launch emacs -Q with clipimg loaded"
	@echo "  make clean            Remove build artifacts"

# transient ships with Emacs 29+, and package-install on the bare symbol keeps
# the built-in; the archive descriptor installs the MELPA version.
$(ELPA_DIR):
	@echo "Installing dependencies..."
	$(EMACS_BATCH) \
	--eval "(package-refresh-contents)" \
	--eval "(package-install (cadr (assq 'transient package-archive-contents)))" \
	--eval "(package-install 'buttercup)" \
	--eval "(package-install 'package-lint)"

deps: $(ELPA_DIR)

# The tesseract and Vision specs skip themselves when the engine is absent.
test: $(ELPA_DIR)
	$(EMACS_BATCH) --directory . \
	--eval "(setq buttercup-stack-frame-style 'omit)" \
	$(foreach file,$(TEST_FILES),-l $(file)) \
	--funcall buttercup-run

# Real uploads to real hosts, so it stays out of test and out of CI. Run it by
# hand before adding an entry to the table or trusting one that is already there.
check-hosts: $(ELPA_DIR)
	@echo "Uploading to every shipped host..."
	$(EMACS_BATCH) --directory . -l scripts/check-hosts.el --funcall check-hosts

# batch-byte-compile exits 1 when any file fails; byte-compile-file only
# returns nil, which a plain --eval would drop.
check-compile: $(ELPA_DIR)
	@echo "Checking byte-compilation..."
	$(EMACS_BATCH) --directory . \
	--eval "(setq byte-compile-error-on-warn t)" \
	-f batch-byte-compile $(PACKAGE_FILES); status=$$?; rm -f *.elc; exit $$status

check-autoloads:
	@echo "Generating and loading autoloads..."
	rm -f "$(AUTOLOADS)"
	$(EMACS) -Q --batch --init-directory "$(SANDBOX_DIR)" \
	--eval "(require 'loaddefs-gen)" \
	--eval "(loaddefs-generate \"$(CURDIR)\" \"$(AUTOLOADS)\")" \
	--eval "(load \"$(AUTOLOADS)\" nil 'nomessage)"

# checkdoc's verb check is on in Emacs 29 and 30 and off from 31; pinning it on
# makes every Emacs report the same docstring findings.
lint: $(ELPA_DIR)
	@echo "Running package-lint..."
	$(EMACS_BATCH) --directory . \
	--eval "(require 'package-lint)" \
	--eval "(setq package-lint-main-file \"$(CURDIR)/clipimg.el\")" \
	-f package-lint-batch-and-exit $(PACKAGE_FILES)
	@echo "Running checkdoc..."
	$(EMACS_BATCH) --directory . \
	--eval "(require 'checkdoc)" \
	--eval "(setq checkdoc-verb-check-experimental-flag t)" \
	--eval "(mapc #'checkdoc-file '($(patsubst %,\"%\",$(PACKAGE_FILES))))" \
	--eval "(when-let* ((buf (get-buffer \"*Warnings*\"))) \
	           (when (< 0 (buffer-size buf)) (kill-emacs 1)))"

sandbox: $(ELPA_DIR)
	$(EMACS_SANDBOX) --directory . \
	--eval "(require 'clipimg-menu)" \
	--eval "(message \"clipimg sandbox: package loaded\")"

clean:
	@echo "Cleaning build artifacts..."
	rm -f *.elc test/*.elc "$(AUTOLOADS)"
	rm -rf $(ELPA_DIR) $(SANDBOX_DIR)
