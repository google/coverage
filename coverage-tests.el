;;; coverage-ert.el --- test for coverage.el.
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
;;; Commentary:
;;
;;; Code:

(require 'coverage)
(require 'ert)
(require 'cl-lib)

(cl-defmacro cov--with-test-parse-profile (var tool-id file-content &rest body)
  `(cl-letf* ((temp-fname (make-temp-file "cov-test-"))
              (unused (with-temp-file temp-fname (insert ,file-content)))
              (,var (cov--parse-profile (cons ,tool-id temp-fname))))
     ,@body))

(ert-deftest cov-parses-go-coverage-profile-test ()
  (cl-letf (((symbol-function 'cov--go-list-root) (lambda (files) '("dummy-prefix/"))))
    (cov--with-test-parse-profile
     profile
     'go-coverage
     "mode: count
example.com/project/coverage1.go:3.21,5.9 2 100
example.com/project/coverage1.go:17.2,17.10 1 100
example.com/project/coverage1.go:6.2,7.6 1 10
example.com/project/coverage1.go:8.2,9.6 1 15
example.com/project/coverage1.go:10.2,11.6 1 25
example.com/project/coverage1.go:12.2,13.6 1 50
example.com/project/coverage1.go:14.2,15.6 1 0
example.com/project/coverage2.go:3.16,5.2 1 1
example.com/project/coverage2.go:7.16,9.2 1 0
example.com/project/coverage3.go:3.16,5.2 1 1
example.com/project/coverage3.go:7.16,9.2 1 2
"
     (should (cov-profile-p profile))
     (should (equal (car (cov-profile-source profile))
                    'go-coverage))
     (should (file-exists-p (cdr (cov-profile-source profile))))
     (should (equal (sort (cov--profile-files profile) 'string<)
                    '("example.com/project/coverage1.go"
                      "example.com/project/coverage2.go"
                      "example.com/project/coverage3.go")))
     (should (equal (sort (cov--profile-files profile 'absolute) 'string<)
                    '("dummy-prefix/src/example.com/project/coverage1.go"
                      "dummy-prefix/src/example.com/project/coverage2.go"
                      "dummy-prefix/src/example.com/project/coverage3.go")))
     (should (equal (cov-profile-path-prefix profile)
                    "dummy-prefix/src/"))
     (should (equal (cov--profile-blocks-for-file profile "dummy-prefix/src/example.com/project/coverage1.go")
                    (mapcar (lambda (block)
                              (cov-make-block
                               :start-line (elt block 0)
                               :start-col (elt block 1)
                               :end-line (elt block 2)
                               :end-col (elt block 3)
                               :count (elt block 4)))
                            '((3 20 5 9 100) ; Sorted by :start-line
                              (6 1 7 6 10)
                              (8 1 9 6 15)
                              (10 1 11 6 25)
                              (12 1 13 6 50)
                              (14 1 15 6 0)
                              (17 1 17 10 100))))))))

(ert-deftest cov-parses-gcov-profile-test ()
  (cov--with-test-parse-profile
   profile
   'gcov
   "        -:    0:Source:main.c
        -:    0:Graph:main.gcno
        -:    0:Data:main.gcda
        -:    0:Runs:1
        -:    0:Programs:1
        -:    1:// -*- compile-command: \"gcc -coverage main.c baz.c in/q.c\" -*-
        -:    2:#include <stdio.h>
        -:    3:#include \"foo.h\"
        -:    4:#include \"baz.h\"
        -:    5:#include \"in/q.h\"
        -:    6:
        -:    7:void foo(int i) {
      100:    8:  if (i < 50) {
       50:    9:    return;
        -:   10:  }
       50:   11:  if (i < 75) {
       25:   12:    return;
        -:   13:  }
       25:   14:  if (i < 90) {
       15:   15:    return;
        -:   16:  }
       10:   17:  if (i < 100) {
       10:   18:    return;
        -:   19:  }
    #####:   20:  return;
      100:   21:}
        -:   22:
        -:   23:int qq(void);
        -:   24:
        -:   25:int main(void) {
      202:   26:  for (int i = 0; i < 100; i++) {
      100:   27:    foo(i);
      100:   28:    bar(i);
      100:   29:  }
        1:   30:  baz();
        1:   31:  q();
        1:   32:  w();
        1:   33:  return 0;
        -:   34:}
"
   (should (cov-profile-p profile))
   (should (equal (car (cov-profile-source profile)) 'gcov))
   (should (file-exists-p (cdr (cov-profile-source profile))))
   (should (equal (cov--profile-files profile) '("main.c")))
   ;; In case of gcov, the profile files is in the same directory
   ;; as the source.
   (cl-letf ((prefix (file-name-directory (cdr (cov-profile-source profile)))))
     (should (equal (cov--profile-files profile 'absolute)
                    (list (concat prefix "main.c"))))
     (should (equal (cov-profile-path-prefix profile) prefix))
     (should (equal (cov--profile-blocks-for-file profile (concat prefix "main.c"))
                    (mapcar (lambda (block)
                              (cov-make-block
                               :start-line (elt block 0)
                               :start-col (elt block 1)
                               :end-line (elt block 2)
                               :end-col (elt block 3)
                               :count (elt block 4)))
                            '((8 nil nil nil 100)
                              (9 nil nil nil 50)
                              (11 nil nil nil 50)
                              (12 nil nil nil 25)
                              (14 nil nil nil 25)
                              (15 nil nil nil 15)
                              (17 nil nil nil 10)
                              (18 nil nil nil 10)
                              (20 nil nil nil 0)
                              (21 nil nil nil 100)
                              (26 nil nil nil 202)
                              (27 nil nil nil 100)
                              (28 nil nil nil 100)
                              (29 nil nil nil 100)
                              (30 nil nil nil 1)
                              (31 nil nil nil 1)
                              (32 nil nil nil 1)
                              (33 nil nil nil 1))))))))

(ert-deftest cov-parses-lcov-profile-test ()
  (cov--with-test-parse-profile
   profile
   'lcov
   "TN:
SF:/prefix/path/baz.c
DA:4,1
DA:5,1
DA:6,1
LF:3
LH:3
end_of_record
TN:
SF:/prefix/path/foo.h
DA:3,100
DA:4,50
DA:6,50
DA:7,150
LF:4
LH:4
end_of_record
TN:
SF:/prefix/path/in/q.c
DA:4,1
DA:9,1
LF:2
LH:2
end_of_record
"
   (should (cov-profile-p profile))
   (should (equal (car (cov-profile-source profile)) 'lcov))
   (should (file-exists-p (cdr (cov-profile-source profile))))
   ;; Paths in the profile file are absolute, so there is no prefix.
   (should (equal (cov-profile-path-prefix profile) nil))
   (should (equal (sort (cov--profile-files profile) 'string<)
                  '("/prefix/path/baz.c"
                    "/prefix/path/foo.h"
                    "/prefix/path/in/q.c")))
   (should (equal (sort (cov--profile-files profile 'absolute) 'string<)
                  '("/prefix/path/baz.c"
                    "/prefix/path/foo.h"
                    "/prefix/path/in/q.c")))
   (should (equal (cov--profile-blocks-for-file profile "/prefix/path/baz.c")
                  (mapcar (lambda (block)
                            (cov-make-block
                             :start-line (elt block 0)
                             :start-col (elt block 1)
                             :end-line (elt block 2)
                             :end-col (elt block 3)
                             :count (elt block 4)))
                          '((4 nil nil nil 1)
                            (5 nil nil nil 1)
                            (6 nil nil nil 1)))))))

(ert-deftest cov-parse-profile-fails-with-no-profile-on-errors-test ()
  (should
   (condition-case nil
       (ignore (cov--parse-profile (cons 'go-coverage "nonexistent-file")))
     (cov--no-profile t))))

;; cov-show tests. Check that displeying given coverage file for given
;; source produces expected overlays.

(defvar cov--test-profile "mode: count
<fname>:3.21,5.9 2 100
<fname>:17.2,17.10 1 100
<fname>:6.2,7.6 1 10
<fname>:8.2,9.6 1 15
<fname>:10.2,11.6 1 25
<fname>:12.2,13.6 1 50
<fname>:14.2,15.6 1 0
")

;; Matches cov--test-profile output with background overlay. Sorted by
;; start point.
;; (start end face echo-area)
(defvar cov--test-profile-expected-overlays
  '((1 180 cov-background nil)
    (39 61 cov-9-face "100")
    (63 82 cov-4-face "10")
    (83 102 cov-5-face "15")
    (103 122 cov-6-face "25")
    (123 143 cov-8-face "50")
    (144 165 cov-nocoverage-face "0")
    (169 178 cov-9-face "100")))

;; For use with cov--test-profile
(defvar cov--test-code "package coverage

func foo(n int) int {
	var c int
	switch {
	case n < 10:
		c++
	case n < 25:
		c++
	case n < 50:
		c++
	case n < 100:
		c++
	case n < 1000:
		c++
	}
	return c
}
")

(cl-defmacro cov-with-test-files (&rest body)
  ""
  `(cl-letf* ((cov--profile-sources (make-hash-table :test 'equal))
              (cov-background t)
              (cov-tools '(go-coverage lcov gcov))
              (cov-source-for-file-func nil)
              (gopath temporary-file-directory)
              (srcdir (concat gopath "src/"))
              (ignored (mkdir srcdir t))
              (srcfname (make-temp-file srcdir))
              ((symbol-function 'cov--go-list-root) (lambda (files) (list gopath)))
              (source (cons 'go-coverage (make-temp-file "cov-test-"))))
     ;; Create test profile file.
     (with-temp-file (cdr source)
       (insert (replace-regexp-in-string "<fname>"
                                         (file-name-nondirectory srcfname)
                                         cov--test-profile)))
     ;; Create test code file.
     (with-temp-file srcfname
       (insert cov--test-code))
     (find-file-literally srcfname)
     ;; Run the test.
     ,@body
     ;; Cleanup.
     (find-file-literally srcfname)
     (kill-buffer)
     (delete-file srcfname)
     (delete-file (cdr source))))

(ert-deftest cov-show-from-profile-test ()
  (cov-with-test-files
   (cov-show (cov--parse-profile source))
   (should (equal (cov--overlays)
                  cov--test-profile-expected-overlays))))

(ert-deftest cov-show-from-source-test ()
  (cov-with-test-files
   (cov-show nil source)
   (should (equal (cov--overlays)
                  cov--test-profile-expected-overlays))))

(ert-deftest cov-show-global-test ()
  (cov-with-test-files
   (cov--update-profile source) ; Parse the profile and remember results globally.
   (cov-show) ; Based on global list of sources.
   (should (equal (cov--overlays)
                  cov--test-profile-expected-overlays))))

(ert-deftest cov-show-suggests-cov-gen-on-modified-buffers ()
  (should
   (with-temp-buffer
     (insert "dummy modifications")
     (condition-case err
         (ignore (cov-show))
       (user-error
        (should (string-match "`cov-gen'" (cadr err)))
        t)))))

(ert-deftest cov-show-suggests-cov-gen-on-missing-source ()
  (should
   (with-temp-buffer
     (insert "dummy modifications")
     (condition-case err
         (ignore (cov-show nil (cons 'go-coverage "doesnt-eixt")))
       (user-error
        (should (string-match "`cov-gen'" (cadr err)))
        t)))))

(ert-deftest cov-show-remembers-used-profiles-test ()
  (cov-with-test-files
   (cov-show (cov--parse-profile source))
   (cov-show) ; Fails without (cov-show profile) which remembers used profile.
   (should (equal (cov--overlays)
                  cov--test-profile-expected-overlays))))

(ert-deftest cov-hide-test ()
  (cov-with-test-files
   (cov-show (cov--parse-profile source))
   (cov-hide)
   (should (null (cov--overlays)))))

;; Tests for compilation hooks.

(ert-deftest cov--compilation-hook-triggers-coverage-display ()
  (cov-with-test-files
   ;; Simulate cov-show.
   (find-file-literally srcfname)
   (setq cov--show-profile t)
   ;; Fake compilation buffer and run hooks.
   (with-temp-buffer
     (insert
      (replace-regexp-in-string
       "<fname>"
       (cdr source)
       "-*- mode: compilation; default-directory: \"~/dummy-prefix/src/example.com/project/\" -*-
Compilation started at Mon Aug 17 13:37:00

go test -covermode=count -coverprofile=<fname>
PASS
coverage: 83.3% of statements
ok  	bitbucket.org/LMMilewski/playground/coverage	0.003s

Compilation finished at Mon Aug 17 13:37:01
"))
     (cov--compilation-hook (current-buffer) "finished\n"))
   ;; Check that expected overlays exist.
   (should (equal (cov--overlays)
                  cov--test-profile-expected-overlays))))


(provide 'coverage-ert)
;;; coverage-tests.el ends here
