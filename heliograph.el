;;; heliograph.el --- solar observatory dashboard -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "28.1"))
;;; Commentary:
;; The Heliographic Computing Authority: graphical SVG and terminal console.
;;; Code:
(require 'cl-lib)
(require 'button)
(require 'solar)
(require 'svg nil t)

(defgroup heliograph nil "Heliographic observatory console." :group 'applications)
(defcustom heliograph-station (system-name) "Station callsign." :type 'string)
(defcustom heliograph-latitude 40.7128 "Observer latitude in degrees north." :type 'number)
(defcustom heliograph-longitude -74.006 "Observer longitude in degrees east." :type 'number)
(defcustom heliograph-refresh-seconds 60 "Seconds between visible updates; zero disables." :type 'integer)

(defface heliograph-gold '((t :foreground "#cda769")) "Solar ornament.")
(defface heliograph-heading '((t :foreground "#efd194" :weight bold)) "Headings.")
(defface heliograph-muted '((t :foreground "#99917c")) "Secondary information.")
(defface heliograph-ready '((t :foreground "#83b6aa" :weight bold)) "Operational status.")

(defvar-local heliograph--timer nil)
(defvar-local heliograph--busy nil)
(defvar-local heliograph--renderer 'auto
  "Renderer for this buffer: auto, svg, or text.")

(defun heliograph--solar-position (&optional time)
  "Approximate (ALTITUDE AZIMUTH) in degrees at TIME.
NOAA fractional-year approximation.  Not intended for navigation."
  (let* ((date (decode-time (or time (current-time)) 0))
         (day (time-to-days (encode-time 0 0 12
                                         (decoded-time-day date)
                                         (decoded-time-month date)
                                         (decoded-time-year date) 0)))
         (jan1 (time-to-days (encode-time 0 0 12 1 1
                                          (decoded-time-year date) 0)))
         (n (1+ (- day jan1)))
         (minutes (+ (* 60 (decoded-time-hour date))
                     (decoded-time-minute date)
                     (/ (decoded-time-second date) 60.0)))
         (gamma (* (/ (* 2.0 float-pi) 365.0)
                   (+ (1- n) (/ (- minutes 720) 1440.0))))
         (eqtime (* 229.18 (+ 0.000075 (* 0.001868 (cos gamma))
                              (* -0.032077 (sin gamma))
                              (* -0.014615 (cos (* 2 gamma)))
                              (* -0.040849 (sin (* 2 gamma))))))
         (decl (+ 0.006918 (* -0.399912 (cos gamma))
                  (* 0.070257 (sin gamma))
                  (* -0.006758 (cos (* 2 gamma)))
                  (* 0.000907 (sin (* 2 gamma)))
                  (* -0.002697 (cos (* 3 gamma)))
                  (* 0.00148 (sin (* 3 gamma)))))
         (ha (* (/ float-pi 180.0)
                (/ (- (mod (+ minutes eqtime
                              (* 4.0 heliograph-longitude)) 1440.0)
                      720.0) 4.0)))
         (lat (* (/ float-pi 180.0) heliograph-latitude))
         (alt (asin (max -1.0 (min 1.0
                                   (+ (* (sin lat) (sin decl))
                                      (* (cos lat) (cos decl) (cos ha)))))))
         (az (atan (sin ha)
                   (- (* (cos ha) (sin lat))
                      (* (tan decl) (cos lat))))))
    (list (* 180.0 (/ alt float-pi))
          (mod (+ 180.0 (* 180.0 (/ az float-pi))) 360.0))))

(defun heliograph--lunar-illumination ()
  "Return estimated lunar illumination percentage.
Simple synodic cycle, NOT a precision ephemeris."
  (let* ((days (/ (float-time
                   (time-subtract (current-time)
                                  (encode-time 0 0 18 6 1 2000 0))) 86400.0))
         (phase (mod (/ days 29.530588853) 1.0)))
    (cons (* 50.0 (- 1 (cos (* 2 float-pi phase))))
          (if (< phase 0.5) "WAXING" "WANING"))))

(defun heliograph--snapshot ()
  "Collect one observational snapshot."
  (let ((sun (heliograph--solar-position))
        (moon (heliograph--lunar-illumination)))
    (list :alt (car sun) :az (cadr sun)
          :illum (car moon) :phase (cdr moon)
          :date (format-time-string "%Y-%m-%d %H:%M UTC" nil t)
          :julian (+ 2440587.5 (/ (float-time) 86400.0)))))

(defun heliograph--dial (snapshot)
  "Return a graphical solar instrument for SNAPSHOT, or nil in a tty.
Azimuth is clockwise from north.  Altitude is shown as radial distance:
zenith at the center and the horizon on the outer solid circle."
  (when (and (display-graphic-p)
             (image-type-available-p 'svg)
             (fboundp 'svg-create))
    (let* ((width 640)
           (height 420)
           (cx 320.0)
           (cy 210.0)
           (horizon 140.0)
           (gold "#cda769")
           (bright "#efd194")
           (dim "#6e664f")
           (accent "#86b7aa")
           (s (svg-create width height))
           (alt (plist-get snapshot :alt))
           (az (plist-get snapshot :az))
           (theta (* float-pi (/ az 180.0)))
           (r (* horizon (/ (- 90.0 alt) 90.0)))
           (sx (+ cx (* r (sin theta))))
           (sy (- cy (* r (cos theta)))))
      ;; Engraved plate and concentric altitude rings.
      (svg-circle s cx cy 170 :stroke dim :stroke-width 1 :fill "none")
      (dolist (radius '(46.67 93.33 140))
        (svg-circle s cx cy radius :stroke gold :stroke-width 1
                    :fill "none"))
      (svg-circle s cx cy 153 :stroke dim :stroke-width 1
                  :stroke-dasharray "2,5" :fill "none")
      ;; Cardinal axes, diagonal guides, and graduated azimuth scale.
      (svg-line s (- cx 170) cy (+ cx 170) cy
                :stroke dim :stroke-width 1)
      (svg-line s cx (- cy 170) cx (+ cy 170)
                :stroke dim :stroke-width 1)
      (dolist (degrees '(45 135 225 315))
        (let* ((a (* float-pi (/ degrees 180.0)))
               (dx (* 139 (sin a)))
               (dy (* 139 (cos a))))
          (svg-line s (- cx dx) (+ cy dy) (+ cx dx) (- cy dy)
                    :stroke dim :stroke-width 0.7
                    :stroke-dasharray "2,5")))
      (dotimes (i 72)
        (let* ((a (* i (/ (* 2.0 float-pi) 72.0)))
               (major (zerop (mod i 6)))
               (inner (if major 157 163)))
          (svg-line s (+ cx (* inner (sin a)))
                    (- cy (* inner (cos a)))
                    (+ cx (* 170 (sin a)))
                    (- cy (* 170 (cos a)))
                    :stroke gold :stroke-width (if major 1.7 0.8))))
      ;; A restrained, typeset astronomical plate.
      (svg-text s "HCA / ARRAY 01" :x 22 :y 29
                :font-size 13 :font-family "monospace" :fill gold)
      (svg-text s "ZENITHAL SOLAR PROJECTION" :x 320 :y 54
                :font-size 13 :font-family "monospace"
                :text-anchor "middle" :fill gold)
      (dolist (mark '(("N" 320 24) ("E" 512 215)
                      ("S" 320 403) ("W" 128 215)))
        (svg-text s (car mark) :x (nth 1 mark) :y (nth 2 mark)
                  :font-size 14 :font-family "monospace"
                  :font-weight "bold" :text-anchor "middle" :fill bright))
      (svg-text s "60" :x (+ cx 8) :y (- cy 46.67)
                :font-size 10 :font-family "monospace" :fill dim)
      (svg-text s "30" :x (+ cx 8) :y (- cy 93.33)
                :font-size 10 :font-family "monospace" :fill dim)
      (svg-text s "HORIZON" :x (+ cx 11) :y (- cy horizon)
                :font-size 10 :font-family "monospace" :fill dim)
      ;; Current sun: do not falsely place a below-horizon sun on the rim.
      (when (>= alt 0.0)
        (svg-line s cx cy sx sy :stroke accent :stroke-width 2.2)
        (svg-circle s sx sy 13 :stroke gold :stroke-width 1
                    :fill "none" :stroke-opacity 0.65)
        (svg-circle s sx sy 7 :fill bright :stroke accent
                    :stroke-width 1.5))
      (svg-circle s cx cy 12 :fill "#7f5b2c"
                  :stroke bright :stroke-width 2)
      (svg-circle s cx cy 4 :fill bright)
      (when (< alt 0.0)
        (svg-text s "SOL BELOW HORIZON" :x cx :y 414
                  :font-size 11 :font-family "monospace"
                  :text-anchor "middle" :fill accent))
      (svg-image s :ascent 'center))))

(defun heliograph--line (s &optional face)
  "Insert S as one line with optional FACE."
  (insert (if face (propertize s 'face face) s) "\n"))

(defun heliograph--button (key description command)
  "Insert a clickable KEY and DESCRIPTION invoking COMMAND."
  (insert-text-button (format "[%s] %-20s" key description)
                      'face 'heliograph-gold
                      'follow-link t
                      'action (lambda (_button)
                                (call-interactively command))))

(defun heliograph--render-text ()
  "Replace current buffer contents with the observatory display."
  (unless heliograph--busy
    (let* ((heliograph--busy t)
           (inhibit-read-only t)
           (snapshot (heliograph--snapshot))
           (dial nil)
           (width (max 40 (min 74 (1- (window-body-width))))))
      (erase-buffer)
      (heliograph--line (make-string width ?═) 'heliograph-gold)
      (heliograph--line "  ☼ HELIOGRAPHIC COMPUTING AUTHORITY" 'heliograph-heading)
      (heliograph--line (format "  STATION %-18s %s"
                                heliograph-station (plist-get snapshot :date))
                        'heliograph-muted)
      (heliograph--line (make-string width ?─) 'heliograph-gold)
      (if dial
          (progn (insert "\n") (insert-image dial) (insert "\n\n"))
        (dolist (line '("                 ╲     │     ╱"
                        "            ╲     ╲    │    ╱     ╱"
                        "              ─────── ◉ ───────"
                        "            ╱     ╱    │    ╲     ╲"
                        "                 ╱     │     ╲"))
          (heliograph--line line 'heliograph-gold))
        (insert "\n"))
      (heliograph--line "  CELESTIAL OBSERVATION ARRAY / INSTRUMENT 01" 'heliograph-gold)
      (heliograph--line (make-string width ?─) 'heliograph-muted)
      (heliograph--line (format "  SOLAR ALTITUDE       %+7.2f°   SOLAR AZIMUTH   %7.2f°"
                                (plist-get snapshot :alt) (plist-get snapshot :az)))
      (heliograph--line (format "  LUNAR ILLUMINATION   %6.1f%%   PHASE           %s"
                                (plist-get snapshot :illum) (plist-get snapshot :phase)))
      (heliograph--line (format "  JULIAN DATE       %10.2f   ARCHIVE         ONLINE"
                                (plist-get snapshot :julian)))
      (heliograph--line (make-string width ?─) 'heliograph-muted)
      (insert "  ")
      (heliograph--button "F" "FILE ARCHIVE" #'find-file)
      (heliograph--button "P" "PROJECT REGISTRY" #'project-switch-project)
      (insert "\n  ")
      (heliograph--button "R" "RECENT RECORDS" #'recentf-open-files)
      (heliograph--button "C" "CONFIGURATION" #'customize)
      (insert "\n\n")
      (heliograph--line "  ● INSTRUMENTATION READY   /   G REFRESH   /   Q EXIT"
                        'heliograph-ready)
      (goto-char (point-min)))))


(defun heliograph--gui-align (position)
  "Insert a display-space that aligns to POSITION."
  (insert (propertize " " 'display `(space :align-to ,position))))

(defcustom heliograph-svg-glow 0.16
  "Opacity of the soft lettering underlay (0 to 1)."
  :type '(number :tag "Glow opacity")
  :group 'heliograph)

(defun heliograph--gui-text (text &optional gold)
  "Create one SVG image for TEXT, gold if GOLD, otherwise white.
Gold labels are two points larger than the white labels."
  (let* ((size (if gold 15 13))
         (color (if gold "#efd194" "#f1f0eb"))
         (width (ceiling (+ 12 (* (length text) size 0.74))))
         (canvas (svg-create width 24))
         (attrs (list :x 5 :y 18 :font-family "monospace"
                      :font-size size :text-anchor "start")))
    (apply #'svg-text canvas text
           (append attrs (list :fill "none" :stroke color
                               :stroke-opacity heliograph-svg-glow
                               :stroke-width 2.4 :stroke-linejoin "round")))
    (apply #'svg-text canvas text
           (append attrs (list :fill color)))
    (svg-image canvas)))

(defun heliograph--gui-pair (left right &optional heading)
  "Draw separate SVG components for LEFT and RIGHT.
HEADING renders the institutional left-side label in gold."
  (let ((left-image (heliograph--gui-text left heading))
        (right-image (heliograph--gui-text right nil)))
    (heliograph--gui-align '(12))
    (insert-image left-image)
    (heliograph--gui-align
     `(- right (,(car (image-size right-image t)))))
    (insert-image right-image)
    (insert "\n")))

(defun heliograph--gui-rule ()
  "Render a full-window-width ornamental rule."
  (let* ((w (max 1 (window-body-width nil t)))
         (canvas (svg-create w 3)))
    (svg-line canvas 0 1 w 1 :stroke "#9d8155" :stroke-width 1)
    (insert-image (svg-image canvas))
    (insert "\n")))

(defun heliograph--gui-gap (pixels)
  "Insert an exact PIXELS-high graphical spacer."
  (when (> pixels 0)
    (insert-image (svg-image (svg-create 1 pixels)))
    (insert "\n")))

(defun heliograph--gui-render (snapshot dial)
  "Render a full-window graphical SNAPSHOT with centered instrument DIAL.
The final telemetry panel is bottom-anchored when the window is tall
enough; smaller windows scroll naturally instead of overlapping."
  (let* ((height (window-body-height nil t))
         (lh (max 1 (frame-char-height)))
         ;; Header = 2 strips + rule; footer = rule, 4 strips,
         ;; rule, and one native interactive command line.
         (top-height (+ 48 3))
         (footer-height (+ 3 (* 4 24) 3 lh))
         (free (max 0 (- height top-height 420 footer-height)))
         (before (/ free 2))
         (after (- free before)))
    (heliograph--gui-pair "☼ HELIOGRAPHIC COMPUTING AUTHORITY"
                          "SYS / 077" t)
    (heliograph--gui-rule)
    (heliograph--gui-pair (format "STATION %s" heliograph-station)
                          (plist-get snapshot :date))
    (heliograph--gui-gap before)
    (when (>= (window-body-width nil t) 640)
      (heliograph--gui-align '(- center (320))))
    (insert-image dial)
    (insert "\n")
    (heliograph--gui-gap after)
    (heliograph--gui-rule)
    (heliograph--gui-pair "CELESTIAL OBSERVATION ARRAY / INSTRUMENT 01"
                          "EPHEMERIS / LIVE" t)
    (heliograph--gui-pair
     (format "SOLAR ALTITUDE    %+6.2f°" (plist-get snapshot :alt))
     (format "SOLAR AZIMUTH    %6.2f°" (plist-get snapshot :az)))
    (heliograph--gui-pair
     (format "LUNAR ILLUMINATION    %5.1f%%" (plist-get snapshot :illum))
     (format "LUNAR PHASE    %s" (plist-get snapshot :phase)))
    (heliograph--gui-pair
     (format "JULIAN DATE    %.2f" (plist-get snapshot :julian))
     "ARCHIVE    ONLINE")
    (heliograph--gui-rule)
    ;; Controls remain real Emacs buttons, not inert SVG lettering.
    (insert "  ")
    (heliograph--button "F" "FILE ARCHIVE" #'find-file)
    (insert " ")
    (heliograph--button "P" "PROJECT REGISTRY" #'project-switch-project)
    (insert " ")
    (heliograph--button "R" "RECENT RECORDS" #'recentf-open-files)
    (heliograph--gui-align `(- right (,(string-pixel-width "● READY"))))
    (insert (propertize "● READY" 'face 'heliograph-ready) "\n")))

(defun heliograph--render ()
  "Render graphical layout, or preserve the original text presentation."
  (if (and (not (eq heliograph--renderer 'text))
           (display-graphic-p)
           (image-type-available-p 'svg))
      (unless heliograph--busy
        (let ((heliograph--busy t)
              (inhibit-read-only t)
              (snapshot (heliograph--snapshot)))
          (erase-buffer)
          (heliograph--gui-render snapshot (heliograph--dial snapshot))
          (goto-char (point-min))))
    (heliograph--render-text)))

(defvar heliograph-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "f") #'find-file)
    (define-key map (kbd "p") #'project-switch-project)
    (define-key map (kbd "r") #'recentf-open-files)
    (define-key map (kbd "c") #'customize)
    (define-key map (kbd "g") #'heliograph-refresh)
    (define-key map (kbd "q") #'quit-window)
    map))

(defun heliograph--cancel-timer ()
  "Cancel the current buffer's refresh timer."
  (when (timerp heliograph--timer)
    (cancel-timer heliograph--timer)
    (setq heliograph--timer nil)))

(define-derived-mode heliograph-mode special-mode "Heliograph"
  "Major mode for the heliographic console."
  (setq-local truncate-lines t)
  (setq-local cursor-type nil)
  (add-hook 'kill-buffer-hook #'heliograph--cancel-timer nil t))

(defun heliograph--start-timer ()
  "Refresh only while the dashboard is visible."
  (heliograph--cancel-timer)
  (when (> heliograph-refresh-seconds 0)
    (let ((buffer (current-buffer)))
      (setq heliograph--timer
            (run-with-timer
             heliograph-refresh-seconds heliograph-refresh-seconds
             (lambda ()
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (when (get-buffer-window buffer t)
                     (heliograph--render))))))))))

;;;###autoload
(defun heliograph-refresh ()
  "Refresh the heliographic console."
  (interactive)
  (heliograph--render))

(defun heliograph--open (renderer)
  "Open the observatory console using RENDERER.
RENDERER is one of auto, svg, or text."
  (when (and (eq renderer 'svg)
             (not (and (display-graphic-p)
                       (image-type-available-p 'svg))))
    (user-error "SVG display is not available in this Emacs frame"))
  (let ((buffer (get-buffer-create
                 (pcase renderer
                   ('svg "*heliograph-svg*")
                   ('text "*heliograph-text*")
                   (_ "*heliograph*")))))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (unless (derived-mode-p 'heliograph-mode)
        (heliograph-mode))
      (setq-local heliograph--renderer renderer)
      (heliograph--render)
      (heliograph--start-timer))))

;;;###autoload
(defun heliograph ()
  "Open heliograph, choosing SVG or text for the current frame."
  (interactive)
  (heliograph--open 'auto))

;;;###autoload
(defun heliograph-svg ()
  "Open the graphical SVG observatory console."
  (interactive)
  (heliograph--open 'svg))

;;;###autoload
(defun heliograph-text ()
  "Open the text observatory console, even in graphical Emacs."
  (interactive)
  (heliograph--open 'text))

;;;###autoload
(defun heliograph-dashboard-widget ()
  "Insert heliographic console into Doom's dashboard buffer.
Use as the only dashboard rendering function on supported Doom versions."
  (setq-local heliograph--renderer 'auto)
  (heliograph--render)
  (heliograph--start-timer))

(provide 'heliograph)
;;; heliograph.el ends here
