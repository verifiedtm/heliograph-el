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


(ert-deftest heliograph-station-formatting ()
  (let ((heliograph-station "hyperion")
        (heliograph-station-formatter #'heliograph-default-station-formatter))
    (should (equal (heliograph--station-label) "HYPERION"))))

(ert-deftest heliograph-location-fallback ()
  (let ((heliograph-latitude 40.0)
        (heliograph-longitude -73.0)
        (heliograph-location-provider (lambda () nil)))
    (should (equal (heliograph--location) '(40.0 . -73.0)))))

(ert-deftest heliograph-location-provider-coordinates ()
  (let ((heliograph-location-provider (lambda () '(51.5 . -0.1))))
    (should (equal (heliograph--location) '(51.5 . -0.1)))))


(ert-deftest heliograph-geoclue-manual-fallback ()
  (let ((heliograph-location-provider 'geoclue)
        (heliograph--geoclue-fix nil)
        (heliograph-latitude 12.0)
        (heliograph-longitude 34.0))
    (should (equal (heliograph--location) '(12.0 . 34.0)))
    (setq heliograph--geoclue-fix '(56.0 . 78.0))
    (should (equal (heliograph--location) '(56.0 . 78.0)))))


(ert-deftest heliograph-solar-regression ()
  (dolist (fixture '(((0 0 12 21 6 2026 0) 26.571494956190925 80.9861764867905)
                     ((0 0 0 21 12 2026 0) -27.248332846216442 261.5751527331339)))
    (let ((sun (heliograph--solar-at (apply #'encode-time (car fixture))
                                    40.7128 -74.006)))
      (should (< (abs (- (car sun) (cadr fixture))) 1e-10))
      (should (< (abs (- (cadr sun) (caddr fixture))) 1e-10)))))

(ert-deftest heliograph-one-location-per-update ()
  (let* ((calls 0)
         (heliograph--daily-cache nil)
         (heliograph-location-provider
          (lambda () (cl-incf calls) '(40.0 . -73.0)))
         (snapshot (heliograph--snapshot)))
    (heliograph--daily-samples snapshot)
    (should (= calls 1))))

(ert-deftest heliograph-daily-cache-invalidation-and-bound ()
  (let* ((heliograph--daily-cache nil)
         (heliograph--observation-time (encode-time 0 0 12 21 6 2026))
         (snapshot (heliograph--snapshot))
         (calculate (symbol-function 'heliograph--daily-solar-samples))
         (calls 0))
    (cl-letf (((symbol-function 'heliograph--daily-solar-samples)
               (lambda (&rest args) (cl-incf calls) (apply calculate args))))
      (let ((first (heliograph--daily-samples snapshot)))
        (setq snapshot (plist-put snapshot :time
                                  (time-add (plist-get snapshot :time) 60)))
        (should (eq first (heliograph--daily-samples snapshot)))
        (should (= calls 1)))
      (setq snapshot (plist-put snapshot :latitude 12.0))
      (heliograph--daily-samples snapshot)
      (should (= calls 2))
      (dotimes (day 6)
        (setq snapshot (plist-put snapshot :time
                                  (encode-time 0 0 12 (+ 22 day) 6 2026)))
        (heliograph--daily-samples snapshot))
      (should (= calls 8))
      (should (= (length heliograph--daily-cache) 4)))))

(ert-deftest heliograph-civil-day-dst-sampling ()
  (let ((old-zone (getenv "TZ")))
    (unwind-protect
        (progn
          (set-time-zone-rule "America/New_York")
          (should (= 139 (length (heliograph--daily-solar-samples
                                 (encode-time 0 0 12 8 3 2026) 40.0 -73.0))))
          (should (= 151 (length (heliograph--daily-solar-samples
                                 (encode-time 0 0 12 1 11 2026) 40.0 -73.0)))))
      (set-time-zone-rule old-zone))))

(ert-deftest heliograph-projection-cardinals ()
  (dolist (fixture '((0 320.0 70.0) (90 460.0 210.0)
                     (180 320.0 350.0) (270 180.0 210.0)))
    (let ((point (heliograph--project-altaz 0 (car fixture) 320 210 140)))
      (should (< (abs (- (car point) (cadr fixture))) 1e-10))
      (should (< (abs (- (cdr point) (caddr fixture))) 1e-10))))
  (should (equal (heliograph--project-altaz 90 123 320 210 140)
                 '(320.0 . 210.0))))

(ert-deftest heliograph-composition-resize-and-cache-isolation ()
  (let* ((heliograph--plate-cache nil) (heliograph--daily-cache nil)
         (snapshot (heliograph--snapshot))
         (images nil))
    ;; Capture the real SVG DOM without needing a graphical frame.
    (cl-letf (((symbol-function 'svg-image)
               (lambda (svg &rest _) (push svg images) (list 'image :type 'svg))))
      (let ((small (heliograph-render-center-astrolabe
                    snapshot '(:window-width 800 :window-height 600)))
            (large (heliograph-render-center-astrolabe
                    snapshot '(:window-width 2000 :window-height 1400))))
        (should (= (heliograph-node-width small) 640))
        (should (= (heliograph-node-height small) 540))
        (should (= (heliograph-node-width large) 1056))
        (should (= (heliograph-node-height large) 891))
        (should (equal (dom-attr (car images) 'viewBox) "0 -60 640 540"))
        (should (equal (cddr (car images)) (cddr (cadr images))))
        (should (= (length heliograph--plate-cache) 1))
        (should (= (length heliograph--daily-cache) 1)))
      (let ((cached (copy-tree heliograph--plate-cache)))
        (heliograph-render-center-astrolabe
         (plist-put (copy-sequence snapshot) :alt 20)
         '(:window-width 800 :window-height 600))
        (should (equal cached heliograph--plate-cache))))))

(ert-deftest heliograph-time-navigation-consistency ()
  (with-temp-buffer
    (heliograph-mode)
    (setq heliograph--renderer 'text
          heliograph--observation-time (encode-time 0 0 12 21 6 2026 0))
    (let ((initial heliograph--observation-time))
      (heliograph-step-forward 2)
      (should (= (float-time (time-subtract heliograph--observation-time initial))
                 (* 2 heliograph-time-step-seconds)))
      (should (string-match-p "EPHEMERIS / HISTORICAL" (buffer-string)))
      (let* ((snapshot (heliograph--snapshot))
             (sun (heliograph--solar-at heliograph--observation-time
                                       (plist-get snapshot :latitude)
                                       (plist-get snapshot :longitude))))
        (should (= (car sun) (plist-get snapshot :alt)))
        (should (equal (plist-get snapshot :date)
                       (format-time-string "%Y-%m-%d %H:%M UTC"
                                           heliograph--observation-time t))))
      (heliograph-step-backward 2)
      (should (time-equal-p initial heliograph--observation-time))
      (heliograph-now)
      (should-not heliograph--observation-time)
      (should (string-match-p "EPHEMERIS / LIVE" (buffer-string))))))

(ert-deftest heliograph-custom-renderer-contract ()
  (let* ((snapshot (heliograph--snapshot))
         (heliograph-center-renderer
          (lambda (received context)
            (should (eq received snapshot))
            (should (numberp (plist-get context :window-width)))
            (make-heliograph-node :image 'custom :width 120 :height 80))))
    (should (eq (heliograph-node-image (heliograph--center-node snapshot)) 'custom))))

(ert-deftest heliograph-refresh-timer-lifecycle ()
  (let ((buffer (generate-new-buffer " *heliograph-timer-test*")))
    (unwind-protect
        (with-current-buffer buffer
          (heliograph-mode)
          (let ((heliograph-refresh-seconds 60))
            (heliograph--start-timer)
            (let ((first heliograph--timer))
              (heliograph--start-timer)
              (should-not (memq first timer-list))
              (should (memq heliograph--timer timer-list)))
            (let ((active heliograph--timer))
              (fundamental-mode)
              (should-not (memq active timer-list)))))
      (kill-buffer buffer))))

(ert-deftest heliograph-layout-centering-and-footer ()
  (dolist (geometry '((800 600) (2000 1400)))
    (with-temp-buffer
      (let ((alignments nil) (gaps nil))
        (cl-letf (((symbol-function 'window-body-width)
                   (lambda (&rest _) (car geometry)))
                  ((symbol-function 'window-body-height)
                   (lambda (&rest _) (cadr geometry)))
                  ((symbol-function 'frame-char-height) (lambda (&rest _) 16))
                  ((symbol-function 'heliograph--gui-align)
                   (lambda (position) (push position alignments)))
                  ((symbol-function 'heliograph--gui-gap)
                   (lambda (pixels) (push pixels gaps)))
                  ((symbol-function 'heliograph--gui-pair) (lambda (&rest _)))
                  ((symbol-function 'heliograph--gui-rule) #'ignore)
                  ((symbol-function 'insert-image) (lambda (&rest _)))
                  ((symbol-function 'string-pixel-width) (lambda (_) 60)))
          (heliograph--gui-render
           (heliograph--snapshot)
           (make-heliograph-node :image 'stub :width 640 :height 480))
          (should (member '(- center (320)) alignments))
          (should (cl-every (lambda (gap) (>= gap 0)) gaps))
          (should (= (apply #'+ gaps)
                     (max 0 (- (cadr geometry) 51 480 118)))))))))

(ert-deftest heliograph-night-projection ()
  (should (= (heliograph--altitude-radius 90 140) 0))
  (should (= (heliograph--altitude-radius 0 140) 140))
  (should (= (heliograph--altitude-radius -90 140) 172))
  (let ((last -1))
    (dotimes (i 181)
      (let ((radius (heliograph--altitude-radius (- 90 i) 140)))
        (should (>= radius last))
        (setq last radius)))))

(ert-deftest heliograph-horizon-crossing-and-open-path ()
  (let ((svg (svg-create 640 540)))
    (heliograph--draw-daily-ephemeris
     svg '((-10 350) (10 10) (-10 30)) 320 210 140 "gold" "ivory")
    (let ((lines (dom-by-tag svg 'line)))
      (should (= (length lines) 4))
      (should (< (abs (- (dom-attr (nth 0 lines) 'x2) 320.0)) 1e-10))
      (should (= (dom-attr (nth 0 lines) 'y2) 70.0))
      (should (= (dom-attr (nth 0 lines) 'x2)
                 (dom-attr (nth 1 lines) 'x1)))
      (should (equal (mapcar (lambda (line) (dom-attr line 'stroke-dasharray))
                            lines)
                     '("2,3" "none" "none" "2,3"))))))

(ert-deftest heliograph-night-marker-without-halo ()
  (let ((svg (svg-create 640 540)))
    (heliograph--draw-live-observation
     svg '(:alt -90 :az 90) 320 210 140 "gold" "ivory" "teal")
    (let ((circles (dom-by-tag svg 'circle)))
      (should (= (length circles) 1))
      (should (= (dom-attr (car circles) 'cx) 492.0))
      (should (equal (dom-attr (car circles) 'fill) "none")))
    (should (equal (dom-attr (car (dom-by-tag svg 'line)) 'stroke-dasharray)
                   "3,5"))))

(provide 'heliograph-test)
;;; heliograph-test.el ends here
