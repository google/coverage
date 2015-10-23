;;; cov-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (cov-gen cov-hide cov-show cov-add-block-to-profile)
;;;;;;  "coverage" "coverage.el" (21998 8152 0 0))
;;; Generated autoloads from coverage.el

(autoload 'cov-add-block-to-profile "coverage" "\
Add BLOCK as part of PROFILE for the file FNAME.

FNAME should be an absolute path.

\(fn PROFILE FNAME BLOCK)" nil nil)

(autoload 'cov-show "coverage" "\
Show PROFILE information in the current buffer.

When PROFILE is nil (typically when run interactively), then find
profile for the current file in the global map of profiles.

SOURCE can be provided when run with prefix argument. This allows
choosing which file to use the input coverage profile.

SOURCE is a dotted pair (TOOL . FILE-NAME).

\(fn &optional PROFILE SOURCE)" t nil)

(autoload 'cov-hide "coverage" "\
Remove all coverage overlays from the current buffers.

\(fn)" t nil)

(autoload 'cov-gen "coverage" "\
Run compile command registered for the major mode of current
buffer.

\(fn)" t nil)

;;;***

(provide 'cov-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; cov-autoloads.el ends here
