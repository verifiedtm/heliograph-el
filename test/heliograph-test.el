;;; heliograph-test.el --- smoke tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'heliograph)

(ert-deftest heliograph-solar-output-in-range ()
  (let ((sun (heliograph--solar-position
              (encode-time 0 0 12 21 6 2026 0))))
    (should (<= -90.0 (car sun) 90.0))
    (should (<= 0.0 (cadr sun)))
    (should (< (cadr sun) 360.0))))

(ert-deftest heliograph-lunar-output-in-range ()
  (let ((moon (heliograph--lunar-illumination)))
    (should (<= 0.0 (car moon) 100.0))
    (should (member (cdr moon) '("WAXING" "WANING")))))

(ert-deftest heliograph-renders-terminal-buffer ()
  (with-temp-buffer
    (heliograph-mode)
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _) nil)))
      (heliograph--render))
    (goto-char (point-min))
    (should (search-forward "HELIOGRAPHIC COMPUTING AUTHORITY" nil t))
    (should (search-forward "SOLAR ALTITUDE" nil t))
    (should (search-forward "INSTRUMENTATION READY" nil t))))

(provide 'heliograph-test)
;;; heliograph-test.el ends here
