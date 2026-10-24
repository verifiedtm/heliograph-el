;;; heliograph-benchmark.el --- SVG construction benchmark -*- lexical-binding: t; -*-
;; Run: emacs -Q --batch -L . -l heliograph.el -l test/heliograph-benchmark.el
;; Measures DOM construction and serialization, excluding GUI rasterization.
(require 'heliograph)
(require 'benchmark)
(let ((snapshot (heliograph--snapshot)))
  (cl-letf (((symbol-function 'svg-image)
             (lambda (svg &rest _)
               (with-temp-buffer
                 (svg-print svg)
                 (buffer-string)))))
    (dolist (context '((:window-width 800 :window-height 600)
                       (:window-width 2000 :window-height 1400)))
      (when (boundp 'heliograph--daily-cache)
        (setq heliograph--daily-cache nil heliograph--plate-cache nil))
      (let ((cold (benchmark-run 1
                    (heliograph-render-center-astrolabe snapshot context))))
        (princ (format "%S cold: %.3f ms; " context (* 1000 (car cold)))))
      (let ((warm (benchmark-run 50
                    (heliograph-render-center-astrolabe snapshot context))))
        (princ (format "repeated: %.3f ms/render (%d GCs)\n"
                       (* 20 (car warm)) (cadr warm)))))))
