;;; macro-math-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (macro-math-eval-and-round-region macro-math-eval-region)
;;;;;;  "macro-math" "macro-math.el" (20864 3068 227214 88000))
;;; Generated autoloads from macro-math.el

(autoload 'macro-math-eval-region "macro-math" "\
Evaluate the marked mathematical expression and replace it with the result.
With arg COPY-TO-KILL-RING or prefix arg, don't replace the region, but
save the result to the kill-ring.  When DIGITS is non-nil, or a numeric
prefix arg is given, it determines the number of decimal digits to round
to.

\(fn BEG END &optional COPY-TO-KILL-RING DIGITS)" t nil)

(autoload 'macro-math-eval-and-round-region "macro-math" "\
Call `macro-math-eval-region' and round the number to DIGITS places.
If DIGITS is nil, `macro-math-rounding-precision' will be used.

\(fn BEG END &optional DIGITS)" t nil)

;;;***

;;;### (autoloads nil nil ("macro-math-pkg.el") (20864 3068 246841
;;;;;;  0))

;;;***

(provide 'macro-math-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; macro-math-autoloads.el ends here
