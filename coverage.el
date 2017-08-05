;;; coverage.el --- show coverage heatmap in the buffer.
;;
;; Copyright 2015 Google Inc.
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;      http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;
;;
;; Authors:  Lukasz Milewski <lmilewski@gmail.com>
;; Keywords: coverage heatmap testing
;; Location: github.com/google/coverage

;;; Commentary:
;;
;; Generate code coverage profiles and overlay GNU Emacs buffers with
;; that information.
;;
;;; Usage:
;;
;; Go
;;   0. Install Go (https://golang.org/dl/)
;;   1. Open examples/example.go
;;   2. Run `cov-show' command (M-x cov-show RET)
;;   3. Profit. When the compilation finishes the buffer will be
;;      overlayed with test coverage information (see go.png)
;;
;;  Hover over the code to see, in the echo area, how many times it
;;  was executed.
;;
;;  You can hide overlays with `cov-hide'. You can also modify the
;;  buffer with overlays and run `recompile' which will automatically
;;  update overlay information via compilation hooks.
;;
;; gcov
;;   0. Install gcc
;;   1. Open examples/main.c
;;   2. Compile the file with "-coverage" flag
;;      (gcc -coverage main.c).
;;      This creates ./a.out file
;;   3. Run ./a.out (this creates main.gcda and main.gcno files)
;;   4. Run `cov-show'
;;   5. When the compilation finishes, the buffer will be overlayed
;;      with test coverage information (see c.png).
;;
;; lcov is disabled by default (handles no modes and is not on the
;; `cov-tools' list). Mainly because lcov profiles are a result of
;; processing gcov profiles and we can handle gcov directly. The
;; reason to include lcov is that other tool that you are using might
;; output profile in lcov format. Having support built-in makes
;; integration easier.
;;
;;
;;; Commands:
;;
;; `cov-gen' runs compilation that generates coverage profile for the
;;           file. Compilation hooks refresh all affected buffers.
;;
;; `cov-show' shows the coverage profile for the current buffer by
;;           adding coverage overlays. Runs `cov-gen' if the profile
;;           doesn't exist.
;;
;; `cov-hide' removes coverage overlays from the buffer.
;;
;;
;;; History:
;;
;; The initial version was created in 2015 and was inspired by the Go
;; web coverage viewer. It was extended to generate coverage profiles,
;; automatically refresh viewed profiles when the code is recompiled,
;; and support easy extensions for other tools (gcov, lcov, etc.).
;;
;;; Code:

(require 'cl-lib)

(defgroup cov nil
  "Show coverage heatmap in the buffer."
  :group 'tools
  :prefix "cov-")

(defcustom cov-background t
  "When non-nil, add background overlays to the buffer.

The face `cov-background' is used to overlay code buffers."
  :type 'boolean
  :group 'cov)

(defcustom cov-tools '(go-coverage gcov)
  "List of tools recognized by this package."
  :type '(list symbol)
  :group 'cov)

(defcustom cov-source-for-file-func nil
  "Function that returns a profile source for given file.

It is called with one argument - file name for which the profile
source is requested.

It should return a dotted pair (tool-id . profile-fname), which
identifies a coverage profile, or nil if the source can't be
determined.

You may want to set this function if the profile for the file is
generated outside of Emacs (i.e. this package has no knowledge of
its existence)."
  :type 'function
  :group 'cov)

(defface cov-background '((t :foreground "gray35"))
  "Face used to overlay the buffer if `cov-background' is non-nil."
  :group 'cov)

(defface cov-nocoverage-face '((t :foreground  "#FF0000"))
  "Face used for code that is not covered by the profile."
  :group 'cov)

(defface cov-0-face '((t :foreground  "#808080"))
  "Face used for code that's barely covered by the profile."
  :group 'cov)

(defface cov-1-face '((t :foreground  "#748C83"))
  "Faces used for code covered by the profile."
  :group 'cov)

(defface cov-2-face '((t :foreground  "#689886"))
  "Faces used for code covered by the profile."
  :group 'cov)

(defface cov-3-face '((t :foreground  "#5CA489"))
  "Faces used for code covered by the profile."
  :group 'cov)

(defface cov-4-face '((t :foreground  "#50B08C"))
  "Faces used for code covered by the profile."
  :group 'cov)

(defface cov-5-face '((t :foreground  "#44BC8F"))
  "Faces used for code covered by the profile."
  :group 'cov)

(defface cov-6-face '((t :foreground  "#38C892"))
  "Faces used for code covered by the profile."
  :group 'cov)

(defface cov-7-face '((t :foreground  "#2CD495"))
  "Faces used for code covered by the profile."
  :group 'cov)

(defface cov-8-face '((t :foreground  "#20E098"))
  "Faces used for code covered by the profile."
  :group 'cov)

(defface cov-9-face '((t :foreground  "#14EC9B"))
  "Faces used for code covered by the profile."
  :group 'cov)


(cl-defstruct (cov-tool
               (:constructor nil)
               (:constructor cov-make-tool)
               (:copier cov-copy-tool))
  ;; Symbol identifying the tool.
  id
  ;; List of modes for which `cov-gen' can generate and process
  ;; coverage information.
  modes
  ;; A function that returns a command that generates profile data. A
  ;; path to a temporary file for output is given as an argument. If
  ;; nil, `cov-gen' is not available.
  compile-command
  ;; A function that runs on *compilation* buffer and returns a list
  ;; of absolute paths to coverage profiles created the compilation.
  ;; Should return nil if the buffer has no information about profiles
  ;; understood by the tool. Called with no arguments.
  parse-compilation-results
  ;; Function of one argument (file name) used to parse coverage
  ;; profile. It should return `cov-profile'.
  parse-profile
  ;; A function, called with two arguments: profile and profile
  ;; source, returning a string prepended to file names of coverage
  ;; profiles. Ignored if nil. Useful when the coverage profiles uses
  ;; relative paths. Run from *compilation* buffer or a buffer for
  ;; which the profile should be shown.
  prefix)

(cl-defstruct (cov-block
               (:constructor nil)
               (:constructor cov-make-block))
  ;; Lines start at 1. Columns start at 0. start-line and count must
  ;; be non-nil.
  start-line start-col end-line end-col count)

(cl-defun cov--block-region (block)
  "Calculate a buffer region covered by the BLOCK.

The returned region is a dotted pair (start . end)."
  ;; When start-col (or end-col) is nil, the block starts (or ends) at
  ;; the beginning of the line.
  ;;
  ;; When end-line is nil, the block ends at the line after
  ;; start-line.
  (cl-flet ((p (line col)
               (save-excursion
                 ;; To end up on line N, column C, go to line 1, move
                 ;; forward N-1 lines and move forward C characters.
                 (goto-char (point-min))
                 (forward-line (1- line))
                 (forward-char col)
                 (point))))
    (cons (p (cov-block-start-line block)
             (or (cov-block-start-col block) 0))
          (p (or (cov-block-end-line block)
                 (1+ (cov-block-start-line block)))
             (or (cov-block-end-col block) 0)))))

(cl-defstruct (cov-profile
               (:constructor nil)
               (:constructor cov-make-profile
                             (&key source
                              &aux (blocks (make-hash-table :test 'equal)))))
  source ; (tool-id . profile-fname)
  blocks ; file path (absolute if path-prefix is nil) -> block
  path-prefix) ; set if file names are not absolute

;;;###autoload
(cl-defun cov-add-block-to-profile (profile fname block)
  "Add BLOCK as part of PROFILE for the file FNAME.

FNAME should be an absolute path."
  (puthash fname
           (cons block (cov--profile-blocks-for-file profile fname))
           (cov-profile-blocks profile)))

(cl-defun cov--profile-blocks-for-file (profile fname)
  "Return all blocks from PROFILE for the file FNAME."
  (gethash
   (if (cov-profile-path-prefix profile)
       (substring fname (length (cov-profile-path-prefix profile)))
     fname)
   (cov-profile-blocks profile)))

(cl-defun cov--profile-files (profile &optional abs)
  "Return a list of absolute file names covered by the PROFILE.

If ABS is 'absolute then profile PATH-PREFIX is prepended to all results."
  (cl-loop for fname being the hash-keys of (cov-profile-blocks profile)
           collect (concat (or (and (eq abs 'absolute) (cov-profile-path-prefix profile)) "")
                           fname)))

(define-error 'cov--no-profile "no coverage profile")

(defvar cov--profile-sources (make-hash-table :test 'equal)
  "Map from file name to source of a profile.

A key (file name) is an absolute path to a file.

A value is source of a profile corresponding to the file.  It is
of the form (tool-id . profile-fname) where tool-id identifies
tool that generated the profile and profile-fname is an absolute
path to file with the profile data.")

(cl-defun cov--update-profile-source (profile)
  "Adds information about source of PROFILE to `cov--profile-sources'."
  (cl-loop for fname in (cov--profile-files profile 'absolute)
           do (puthash fname
                       (cov-profile-source profile)
                       cov--profile-sources)))

(defvar cov--show-profile nil
  "If not nil then the coverage profile is shown in the buffer.

If not nil and the coverage profile doesn't exist for the buffer
then it is shown as soon as the data is available (e.g. after
`cov-gen')")
(make-variable-buffer-local 'cov--show-profile)

;;;###autoload
(cl-defun cov-show (&optional profile source)
  "Show PROFILE information in the current buffer.

When PROFILE is nil (typically when run interactively), then find
profile for the current file in the global map of profiles.

SOURCE can be provided when run with prefix argument. This allows
choosing which file to use the input coverage profile.

SOURCE is a dotted pair (TOOL . FILE-NAME)."
  (interactive
   (list nil ; Profile is set when calling the function non-interactively.
         (and ; Set source if called with prefix arg. Otherwise infer it from buffer file name.
          current-prefix-arg
          (cons
           (intern (completing-read
                    "Tool to parse the profile: "
                    (mapcar 'symbol-name cov-tools)
                    nil t nil nil
                    (when cov-tools
                      (symbol-name (or (cov-tool-id (cov--tool-for-mode major-mode))
                                       (car cov-tools))))))
           (read-file-name "File with the profile: ")))))
  (condition-case err
      (with-temp-message (format "Rendering the coverage profile")
        (cov-hide)
        (setq cov--show-profile t)
        (and (buffer-modified-p)
             (user-error "Buffer is modified.  Save the buffer and run `cov-gen'"))
        (cl-letf* ((fname (buffer-file-name))
                   (source (or source
                               (and profile (cov-profile-source profile))
                               (and cov-source-for-file-func
                                    (funcall cov-source-for-file-func fname))
                               (gethash fname cov--profile-sources)
                               (signal 'cov--no-profile
                                       (list (format "No profile source for file %s" fname)))))
                   (profile (or profile (cov--parse-profile source)))
                   (blocks (or (cov--profile-blocks-for-file profile fname)))
                   (denom (cl-loop for block in blocks
                                   maximize (cov-block-count block) into max
                                   finally return max)))
          (unless blocks
            (signal 'cov--no-profile
                    (list (format "No blocks for %s" fname))))
          (when (file-newer-than-file-p fname (cdr source))
            (signal 'cov--no-profile
                    (list (format "%s is newer than %s" fname (cdr source)))))
          (cov--update-profile-source profile) ; Keep using the selected source.
          (when cov-background
            (cl-letf ((bg (make-overlay (point-min) (point-max))))
              (overlay-put bg 'cov t)
              (overlay-put bg 'face 'cov-background)))
          (cl-loop for block in blocks
                   do (cov--create-overlay block denom))))
    (cov--no-profile
     (when (y-or-n-p (format "%s.  Run `cov-gen' instead? " (cadr err)))
       (cov-gen)))))

(cl-defun cov--create-overlay (block denom)
  "Add a coverage overlay representing BLOCK to the current buffer.

If execution count is 0 then the face `cov-nocoverage-face' is
used. Otherwise faces between `cov-0-face' and `cov-9-face' are
used. Faces with higher number are used as the value of
  (cov-block-count BLOCK)
increases.

DENOM is maximum number of executions across all blocks for given
file and is used to normalize (cov-block-count BLOCK)."
  (cl-letf* ((count (cov-block-count block))
             (region (cov--block-region block))
             (start (car region))
             (end (cdr region))
             (face (if (eq (cov-block-count block) 0)
                       'cov-nocoverage-face
                     (cl-letf ((norm (if (equal denom 1)
                                         1 ; We are in set mode (only 0s and 1s).
                                       (/ (log (cov-block-count block))
                                          (log denom)))))
                       (cond ((<= norm 0.1) 'cov-0-face)
                             ((<= norm 0.2) 'cov-1-face)
                             ((<= norm 0.3) 'cov-2-face)
                             ((<= norm 0.4) 'cov-3-face)
                             ((<= norm 0.5) 'cov-4-face)
                             ((<= norm 0.6) 'cov-5-face)
                             ((<= norm 0.7) 'cov-6-face)
                             ((<= norm 0.8) 'cov-7-face)
                             ((<= norm 0.9) 'cov-8-face)
                             ((<= norm 1.0) 'cov-9-face)))))
             (o (make-overlay start end nil t nil)))
    (overlay-put o 'cov t)
    (overlay-put o 'face face)
    (overlay-put o 'help-echo (number-to-string count))))

(cl-defun cov--overlays (&optional buffer)
  "Return all coverage overlays in the BUFFER.

If the BUFFER is nil, then the current buffer is used."
  (sort
   (cl-loop for ov being the overlays of (or buffer (current-buffer))
            if (overlay-get ov 'cov)
            collect (list (overlay-start ov)
                          (overlay-end ov)
                          (overlay-get ov 'face)
                          (overlay-get ov 'help-echo)))
   (lambda (a b) (< (car a) (car b)))))

;;;###autoload
(cl-defun cov-hide ()
  "Remove all coverage overlays from the current buffers."
  (interactive)
  (setq cov--show-profile nil)
  (remove-overlays nil nil 'cov t))

(cl-defun cov--tool-for-mode (mode)
  "Return coverage tool for given major MODE."
  (cl-loop for tool-id in cov-tools
           for tool = (get tool-id 'coverage-tool)
           if (memq mode (cov-tool-modes tool))
           return (or tool
                      (error "There are no coverage tool for mode %s" mode))))

;;;###autoload
(cl-defun cov-gen ()
  "Run compile command registered for the major mode of current
buffer."
  (interactive)
  (cl-letf* ((tool (cov--tool-for-mode major-mode))
             (cmd (and tool (cov-tool-compile-command tool))))
    (when (null cmd)
      (error "Tool %s doesn't support `cov-gen'.  Generate the profile and set cov-source-for-file-func" (cov-tool-id tool)))
    (compile (funcall cmd (make-temp-file "cov")))))

(cl-defun cov--compilation-hook (buffer status)
  "Process results of the compilation buffer.

This function is added to the list of compilation hooks. It
parses compilation results in the BUFFER and updates the set of
known profile sources accordingly.

The buffer is processed only if the STATUS is 'finished\n'"
  (unless (equal status "finished\n")
    (cl-return-from cov--compilation-hook))
  (save-excursion
    (cl-loop for tool-id in cov-tools
             for tool = (get tool-id 'coverage-tool)
             for parser = (cov-tool-parse-compilation-results tool)
             if parser ; skip tools that don't support parsing compilation results.
             do (cl-loop for profile-fname in (progn
                                                (goto-char (point-min))
                                                (funcall parser))
                         do (cov--update-profile (cons tool-id profile-fname))))))

(add-to-list 'compilation-finish-functions 'cov--compilation-hook)

(cl-defun cov--update-profile (source)
  "Update all buffers affected by a change to SOURCE.

Used to refresh coverage overlays as files are recompiled.

SOURCE is a dotted pair (TOOL . FILE-NAME).

This function is run by the compilation results parser."
  (cl-letf* ((profile (cov--parse-profile source)))
    (cov--update-profile-source profile)
    ;; Refresh faces in all buffers that currently show the profile.
    (cl-loop for fname in (cov--profile-files profile 'absolute)
             for buffer = (find-buffer-visiting fname)
             if buffer
             do (unwind-protect
                    (with-current-buffer buffer
                      (when cov--show-profile
                        (cov-show profile)))))))

(cl-defun cov--parse-profile (source)
  "Parse a coverage profile from SOURCE.

Must be called either from the compilation buffer or from the
buffer for which the profile is parsed.

SOURCE is a dotted pair (TOOL . FILE-NAME).
"
  (condition-case nil
      (with-temp-message
          (format "Parsing coverage profile %s" (cdr source))
        (cl-letf* ((tool (get (car source) 'coverage-tool))
                   (profile
                    (funcall (cov-tool-parse-profile tool) (cdr source)))
                   (prefix
                    (and (cov-tool-prefix tool)
                         (funcall (cov-tool-prefix tool) profile source)))
                   (blocks (cov-profile-blocks profile))
                   (sorted-blocks (make-hash-table :test 'equal)))
          ;; Sort all blocks by :start-line, ascending.
          (maphash (lambda (k v)
                     (puthash k
                              (sort v (lambda (b1 b2)
                                        (< (cov-block-start-line b1)
                                           (cov-block-start-line b2))))
                              sorted-blocks))
                   blocks)
          (setf (cov-profile-blocks profile) sorted-blocks)
          (setf (cov-profile-path-prefix profile) prefix)
          profile))
    (file-error
     (signal 'cov--no-profile
             (list (format "Profile source %s doesn't exist" source))))))

;; Tool: Go

(cl-defun cov--parse-go-coverege (profile-fname)
  "Parse PROFILE-FNAME as Go coverage profile."
  ;; The format is line based.
  ;;   First line:
  ;;     mode: {count,atomic,set}
  ;;   Following lines:
  ;;     <file-path>:<start-line>.<start-col>,<end-line>.<end-col> <statements-count> <value>
  (with-temp-buffer
    (insert-file-contents profile-fname)
    (cl-letf ((profile (cov-make-profile
                        :source (cons 'go-coverage profile-fname))))
      (goto-char (point-min))
      (while (re-search-forward "^\\(.+\\):\\([0-9]+\\).\\([0-9]+\\),\\([0-9]+\\).\\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\)$" nil t)
        (cov-add-block-to-profile
         profile
         (match-string 1)
         (cov-make-block :start-line (string-to-number (match-string 2))
                         ;; 1- because in Emacs columns start at 0 but
                         ;; in Go they start at 1.
                         :start-col (1- (string-to-number (match-string 3)))
                         :end-line (string-to-number (match-string 4))
                         :end-col (string-to-number (match-string 5))
                         :count (string-to-number (match-string 7)))))
      profile)))

(put 'go-coverage
     'coverage-tool
     (cov-make-tool
      :id 'go-coverage
      :modes '(go-mode gopher-mode)
      :compile-command (lambda (out-fname)
                         (concat "go test -covermode=count -coverprofile="
                                 out-fname))
      :parse-compilation-results (lambda ()
                                   (when (re-search-forward "^go\\s-+test.*-coverprofile=\\([-~/[:alnum:]_.${}#%,:]+\\)" nil t)
                                     (list (match-string 1))))
      :parse-profile 'cov--parse-go-coverege
      :prefix (lambda (profile source)
                ;; Use 'go list' to avoid handling GOPATH, vendoring etc.
                (cl-letf ((files (cov--profile-files profile)))
                  (when files
                    (concat (file-name-as-directory (car (cov--go-list-root files))) "src/"))))))

(cl-defun cov--go-list-root (files)
  (apply 'process-lines
         (append '("go" "list" "-f" "{{.Root}}")
                 (mapcar 'file-name-directory files))))

;; Tool: lcov

(cl-defun cov--parse-lcov (profile-fname)
  "Parse PROFILE-FNAME as lcov profile."
  ;; The format is line based.
  ;;   TN:<test name>
  ;;   SF:<absolute path to the source file>
  ;;   FN:<line number of function start>,<function name> for each function
  ;;   DA:<line number>,<execution count> for each instrumented line
  ;;   LH:<number of lines with an execution count> greater than 0
  ;;   LF:<number of instrumented lines>
  ;;   end_of_record
  (with-temp-buffer
    (insert-file-contents profile-fname)
    (goto-char (point-min))
    (cl-letf ((profile (cov-make-profile
                        :source (cons 'lcov profile-fname)))
              (cur-file nil))
      (while (not (eobp))
        (cond
         ((looking-at "SF:\\(.*\\)")
          (setq cur-file (match-string 1)))
         ((looking-at "DA:\\([0-9]+\\),\\([0-9]+\\)")
          (when (or (null cur-file) (equal cur-file ""))
            (error "Blocks must be added to files"))
          (cov-add-block-to-profile
           profile
           cur-file
           (cov-make-block
            :start-line (string-to-number (match-string 1))
            :count (string-to-number (match-string 2))))))
        (forward-line))
      profile)))

;; lcov is here just as an example. It requires generating gcov output
;; first, so gcov itself can be used instead.
(put 'lcov
     'coverage-tool
     (cov-make-tool
      :id 'lcov
      :modes '()
      :compile-command (lambda (out-fname)
                         (concat "lcov --directory . --capture --output-file="
                                 out-fname))
      :parse-compilation-results (lambda ()
                                   (when (re-search-forward "^lcov .*--capture.*--output-file=\\([-~/[:alnum:]_.${}#%,:]+\\)" nil t)
                                     (list (match-string 1))))
      :parse-profile 'cov--parse-lcov))

;; Tool: gcov

(cl-defun cov--gcov-source-for-file (fname)
  "Return gcov file name for file FNAME.

Return nil if there is no gcov file for FNAME. Set this function
as `cov-source-for-file-func' to integrate with gcov.

To create gcov files run the compiler with -coverage flag on
*absolute* paths of the source code. Then run the resulting
binary. Finally run gcov for all source files (or use `cov-gen').
"
  (cl-letf ((profile-fname (concat fname ".gcov")))
    (when (file-exists-p profile-fname)
      (cons 'gcov profile-fname))))

(cl-defun cov--parse-gcov (profile-fname)
  "Parse PROFILE-FNAME as gcov profile."
  ;; The format is line based:
  ;;
  ;; 0:Source:<filename>   the profile is for file <filename>
  ;;        N: L:<line>    line L was executed N times
  ;;    #####: L:<line>    line L was not executed
  ;;
  ;;    Other lines are irrelevant.
  (with-temp-buffer
    (insert-file-contents profile-fname)
    (goto-char (point-min))
    (cl-letf ((cur-file nil)
              (profile (cov-make-profile
                        :source (cons 'gcov profile-fname))))
      (while (not (eobp))
        (cond
         ((looking-at "\\s-*-:\\s-*0:Source:\\(.*\\)")
          (setq cur-file (match-string 1)))
         ((looking-at "\\s-*\\(\[0-9]+\\):\\s-*\\(\[0-9]+\\):")
          (message "shit2")
          (cov-add-block-to-profile
           profile
           cur-file
           (cov-make-block
            :start-line (string-to-number (match-string 2))
            :count (string-to-number (match-string 1)))))
         ((looking-at "\\s-*#####:\\s-*\\(\[0-9]+\\):")
          (cov-add-block-to-profile
           profile
           cur-file
           (cov-make-block
            :start-line (string-to-number (match-string 1))
            :count 0))))
        (forward-line))
      profile)))

;; Requires that *.gcda files are already in the same directory as the
;; source. To create them:
;;   - compile with -coverage flag
;;   - run the resulting binary
(put 'gcov
     'coverage-tool
     (cov-make-tool
      :id 'gcov
      :modes '(c-mode c++-mode)
      :compile-command (lambda (out-fname) "gcov *.gcda")
      :parse-compilation-results (lambda ()
                                   (cl-loop while (re-search-forward "^.*:creating '\\([^']+\\)'" nil t)
                                            collect (concat default-directory (match-string 1))))
      :parse-profile 'cov--parse-gcov
      :prefix (lambda (profile source)
                (file-name-directory (cdr source)))))

(provide 'coverage)
;;; coverage.el ends here
