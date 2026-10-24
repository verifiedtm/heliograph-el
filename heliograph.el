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
(defun heliograph-default-station-formatter (station)
  "Return STATION in uppercase for display."
  (upcase station))

(defcustom heliograph-station-formatter #'heliograph-default-station-formatter
  "Function taking the configured station name and returning display text."
  :type 'function :group 'heliograph)

(defun heliograph--station-label ()
  "Return the configured, formatted station label."
  (let ((name (funcall heliograph-station-formatter heliograph-station)))
    (unless (stringp name)
      (error "Heliograph station formatter must return a string"))
    name))

(defcustom heliograph-location-provider 'manual
  "Source of observer coordinates.
Use manual, or supply a function of no arguments returning a cons
cell (LATITUDE . LONGITUDE).  Nil or errors fall back to the configured
manual coordinates.  Providers are called synchronously; network or
D-Bus queries should instead maintain a cached fix and return promptly."
  :type '(choice (const :tag "Manual" manual)
          (const :tag "GeoClue2" geoclue)
          (function :tag "Cached location provider"))
  :group 'heliograph)

(defun heliograph--location ()
  "Return validated observer coordinates with manual fallback."
  (let* ((fallback (cons heliograph-latitude heliograph-longitude))
         (candidate
          (if (eq heliograph-location-provider 'manual)
              fallback
            (condition-case nil
                (if (eq heliograph-location-provider 'geoclue)
                    heliograph--geoclue-fix
                  (funcall heliograph-location-provider))
              (error nil)))))
    (if (and (consp candidate)
             (numberp (car candidate))
             (numberp (cdr candidate))
             (<= -90 (car candidate) 90)
             (<= -180 (cdr candidate) 180))
        candidate
      fallback)))


(defcustom heliograph-geoclue-desktop-id "emacs"
  "Desktop ID submitted to GeoClue for authorization (without .desktop).
This must correspond to an application desktop ID accepted by your
desktop's GeoClue authorization agent."
  :type 'string :group 'heliograph)

(defvar heliograph--geoclue-fix nil
  "Most recent GeoClue location as (LATITUDE . LONGITUDE), or nil.")
(defvar heliograph--geoclue-client nil)
(defvar heliograph--geoclue-signal nil)
(defvar heliograph--geoclue-state 'stopped)
(defconst heliograph--geoclue-service "org.freedesktop.GeoClue2")
(defconst heliograph--geoclue-client-interface "org.freedesktop.GeoClue2.Client")
(defconst heliograph--geoclue-location-interface "org.freedesktop.GeoClue2.Location")
(defconst heliograph--geoclue-properties "org.freedesktop.DBus.Properties")

(defun heliograph--geoclue-refresh ()
  "Update visible graphical heliograph buffers after a new location fix."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and heliograph--resize-enabled
                 (not (eq heliograph--renderer 'text))
                 (get-buffer-window buffer t))
        (with-selected-window (get-buffer-window buffer t)
          (heliograph--render))))))

(defun heliograph--geoclue-read-location (path)
  "Read latitude and longitude at GeoClue location PATH asynchronously."
  (when (and (stringp path) (not (equal path "/")))
    (dbus-call-method-asynchronously
     :system heliograph--geoclue-service path
     heliograph--geoclue-properties "Get"
     (lambda (latitude)
       (dbus-call-method-asynchronously
        :system heliograph--geoclue-service path
        heliograph--geoclue-properties "Get"
        (lambda (longitude)
          (let ((fix (cons latitude longitude)))
            (when (and (numberp latitude) (numberp longitude)
                       (<= -90 latitude 90) (<= -180 longitude 180))
              (setq heliograph--geoclue-fix fix)
              (heliograph--geoclue-refresh))))
        heliograph--geoclue-location-interface "Longitude"))
     heliograph--geoclue-location-interface "Latitude")))

(defun heliograph--geoclue-start-client (path)
  "Configure and start the GeoClue client at PATH."
  (setq heliograph--geoclue-client path)
  (setq heliograph--geoclue-signal
        (dbus-register-signal
         :system heliograph--geoclue-service path
         heliograph--geoclue-client-interface "LocationUpdated"
         (lambda (_old new)
           (heliograph--geoclue-read-location new))))
  ;; Chain asynchronous property writes before starting the client.
  (dbus-call-method-asynchronously
   :system heliograph--geoclue-service path
   heliograph--geoclue-properties "Set"
   (lambda (&rest _)
     (dbus-call-method-asynchronously
      :system heliograph--geoclue-service path
      heliograph--geoclue-properties "Set"
      (lambda (&rest _)
        (dbus-call-method-asynchronously
         :system heliograph--geoclue-service path
         heliograph--geoclue-client-interface "Start"
         (lambda (&rest _)
           (setq heliograph--geoclue-state 'active))))
      heliograph--geoclue-client-interface "RequestedAccuracyLevel"
      :variant :uint32 4))
   heliograph--geoclue-client-interface "DesktopId"
   :variant :string heliograph-geoclue-desktop-id))

(defun heliograph-geoclue-start ()
  "Request GeoClue city-level coordinates asynchronously.
A location fix may arrive later; manual coordinates are used meanwhile."
  (interactive)
  (unless (featurep 'dbus)
    (unless (require 'dbus nil t)
      (user-error "Emacs was built without D-Bus support")))
  (when (eq heliograph--geoclue-state 'stopped)
    (setq heliograph--geoclue-state 'starting)
    (condition-case err
        (dbus-call-method-asynchronously
         :system heliograph--geoclue-service
         "/org/freedesktop/GeoClue2/Manager"
         "org.freedesktop.GeoClue2.Manager" "GetClient"
         #'heliograph--geoclue-start-client)
      (error
       (setq heliograph--geoclue-state 'stopped)
       (message "Heliograph GeoClue unavailable: %s" (error-message-string err))))))

(defun heliograph-geoclue-stop ()
  "Stop GeoClue updates and clear the current fix."
  (interactive)
  (when heliograph--geoclue-signal
    (dbus-unregister-object heliograph--geoclue-signal)
    (setq heliograph--geoclue-signal nil))
  (when heliograph--geoclue-client
    (dbus-call-method-asynchronously
     :system heliograph--geoclue-service heliograph--geoclue-client
     heliograph--geoclue-client-interface "Stop" nil))
  (setq heliograph--geoclue-state 'stopped
        heliograph--geoclue-client nil
        heliograph--geoclue-fix nil))

(defun heliograph--geoclue-ensure-started ()
  "Initialize opt-in GeoClue integration without delaying startup."
  (when (and (eq heliograph-location-provider 'geoclue)
             (eq heliograph--geoclue-state 'stopped))
    (run-with-idle-timer 0.1 nil #'heliograph-geoclue-start)))

(defcustom heliograph-refresh-seconds 60 "Seconds between visible updates; zero disables." :type 'integer)

(defface heliograph-gold '((t :foreground "#cda769")) "Solar ornament.")
(defface heliograph-heading '((t :foreground "#efd194" :weight bold)) "Headings.")
(defface heliograph-muted '((t :foreground "#99917c")) "Secondary information.")
(defface heliograph-ready '((t :foreground "#83b6aa" :weight bold)) "Operational status.")

(defvar-local heliograph--timer nil)
(defvar-local heliograph--busy nil)
(defvar-local heliograph--renderer 'auto
  "Renderer for this buffer: auto, svg, or text.")

(defvar-local heliograph--observation-time nil
  "Selected observation time, or nil to follow the wall clock.")

(defun heliograph--solar-position (&optional time)
  "Return approximate solar coordinates at TIME using resolved location."
  (let ((location (heliograph--location)))
    (heliograph--solar-at (or time (current-time))
                         (car location) (cdr location))))

(defun heliograph--solar-at (time latitude longitude)
  "Approximate (ALTITUDE AZIMUTH) in degrees at TIME.
TIME and observer LATITUDE and LONGITUDE are explicit inputs.
NOAA fractional-year approximation.  Not intended for navigation."
  (let* ((date (decode-time time 0))
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
                              (* 4.0 longitude)) 1440.0)
                      720.0) 4.0)))
         (lat (* (/ float-pi 180.0) latitude))
         (alt (asin (max -1.0 (min 1.0
                                   (+ (* (sin lat) (sin decl))
                                      (* (cos lat) (cos decl) (cos ha)))))))
         (az (atan (sin ha)
                   (- (* (cos ha) (sin lat))
                      (* (tan decl) (cos lat))))))
    (list (* 180.0 (/ alt float-pi))
          (mod (+ 180.0 (* 180.0 (/ az float-pi))) 360.0))))

(defun heliograph--lunar-illumination (&optional time)
  "Return estimated lunar illumination percentage.
Simple synodic cycle, NOT a precision ephemeris."
  (let* ((days (/ (float-time
                   (time-subtract (or time (current-time))
                                  (encode-time 0 0 18 6 1 2000 0))) 86400.0))
         (phase (mod (/ days 29.530588853) 1.0)))
    (cons (* 50.0 (- 1 (cos (* 2 float-pi phase))))
          (if (< phase 0.5) "WAXING" "WANING"))))

(defun heliograph--observation-state ()
  "Resolve time and observer coordinates once for this update."
  (let ((time (or heliograph--observation-time (current-time)))
        (location (heliograph--location)))
    (list :time time :latitude (car location) :longitude (cdr location)
          :historical (not (null heliograph--observation-time)))))

(defun heliograph--snapshot ()
  "Collect consistent solar, lunar and calendar observations."
  (let* ((state (heliograph--observation-state))
         (time (plist-get state :time))
         (sun (heliograph--solar-at time (plist-get state :latitude)
                                   (plist-get state :longitude)))
         (moon (heliograph--lunar-illumination time)))
    (append state
            (list :alt (car sun) :az (cadr sun)
                  :illum (car moon) :phase (cdr moon)
                  :date (format-time-string "%Y-%m-%d %H:%M UTC" time t)
                  :julian (+ 2440587.5 (/ (float-time time) 86400.0))))))

(defun heliograph--ephemeris-label (snapshot)
  "Return the observation mode label for SNAPSHOT."
  (if (plist-get snapshot :historical)
      "EPHEMERIS / HISTORICAL" "EPHEMERIS / LIVE"))

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
           (point (heliograph--instrument-point
                   cx cy (* horizon (/ (- 90.0 alt) 90.0)) az))
           (sx (car point)) (sy (cdr point)))
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
      (svg-text s "ZENITHAL SOLAR PROJECTION" :x 32 :y 54
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

(cl-defstruct heliograph-node
  "A graphical center element with explicit pixel dimensions."
  image width height)

(defun heliograph-render-center-solar (snapshot _context)
  "Render the existing solar instrument from SNAPSHOT.
_CONTEXT is reserved for responsive instruments."
  (make-heliograph-node :image (heliograph--dial snapshot)
                        :width 640 :height 420))


(defun heliograph--instrument-point (cx cy radius azimuth)
  "Project RADIUS and AZIMUTH onto a north-up instrument at CX, CY."
  (let ((a (* float-pi (/ azimuth 180.0))))
    (cons (+ cx (* radius (sin a)))
          (- cy (* radius (cos a))))))

(defun heliograph--instrument-segment (svg cx cy radius start end color
                                           &optional width dash)
  "Draw an engraved circular arc from START to END degrees."
  (let ((a start))
    (while (< a end)
      (let* ((b (min end (+ a 3.0)))
             (p (heliograph--instrument-point cx cy radius a))
             (q (heliograph--instrument-point cx cy radius b)))
        (svg-line svg (car p) (cdr p) (car q) (cdr q)
                  :stroke color :stroke-width (or width 1)
                  :stroke-dasharray (or dash "none")))
      (setq a (+ a 3.0)))))

(defun heliograph--altitude-radius (alt horizon)
  "Return radius for ALT degrees, with a 32-unit nighttime annulus.
The visible hemisphere stays linear; negative altitudes are compressed."
  (if (>= alt 0.0)
      (* horizon (/ (- 90.0 alt) 90.0))
    (+ horizon (* 32.0 (/ (- alt) 90.0)))))

(defun heliograph--project-altaz (altitude azimuth cx cy horizon)
  "Project ALTITUDE and AZIMUTH around CX, CY with HORIZON radius."
  (heliograph--instrument-point
   cx cy (heliograph--altitude-radius altitude horizon) azimuth))

(defvar heliograph--daily-cache nil "Bounded cache of daily solar samples.")
(defvar heliograph--plate-cache nil "Bounded cache of nominal SVG plates.")
(defconst heliograph--cache-limit 4)

(defun heliograph--cache-value (cache key calculate)
  "Look up KEY in CACHE symbol, otherwise call CALCULATE and retain it.
Keys include all calculation or painting inputs.  Keep four recent entries."
  (let ((entry (assoc key (symbol-value cache))))
    (if entry
        (progn
          (set cache (cons entry (delq entry (symbol-value cache))))
          (cdr entry))
      (let ((value (funcall calculate)))
        (set cache (cl-subseq (cons (cons key value) (symbol-value cache))
                             0 (min heliograph--cache-limit
                                    (1+ (length (symbol-value cache))))))
        value))))

(defun heliograph--daily-solar-samples (time latitude longitude &optional step)
  "Calculate local civil-day samples at TIME, LATITUDE and LONGITUDE.
STEP is seconds between samples, default 600.  Include the next midnight;
use civil dates rather than assuming every day lasts 24 hours."
  (let* ((step (or step 600))
         (date (decode-time time))
         (day (decoded-time-day date))
         (month (decoded-time-month date))
         (year (decoded-time-year date))
         (start (encode-time 0 0 0 day month year))
         (end (encode-time 0 0 0 (1+ day) month year))
         (duration (float-time (time-subtract end start)))
         samples)
    (unless (> step 0) (error "Sampling step must be positive"))
    (let ((seconds 0))
      (while (< seconds duration)
        (push (heliograph--solar-at (time-add start seconds) latitude longitude)
              samples)
        (setq seconds (+ seconds step))))
    (push (heliograph--solar-at end latitude longitude) samples)
    (nreverse samples)))

(defun heliograph--daily-samples (snapshot)
  "Return cached daily samples for the resolved SNAPSHOT."
  (let* ((time (plist-get snapshot :time))
         (latitude (plist-get snapshot :latitude))
         (longitude (plist-get snapshot :longitude))
         (date (decode-time time))
         (key (list (decoded-time-year date) (decoded-time-month date)
                    (decoded-time-day date) (getenv "TZ")
                    (current-time-zone time) latitude longitude 'noaa-v1 600)))
    (heliograph--cache-value
     'heliograph--daily-cache key
     (lambda () (heliograph--daily-solar-samples time latitude longitude 600)))))

(defun heliograph--draw-solar-segment (svg start end night gold)
  "Draw a solar segment from START to END on SVG in GOLD.
NIGHT selects the dim engraved treatment."
  (svg-line svg (car start) (cdr start) (car end) (cdr end)
            :stroke gold :stroke-width (if night 0.9 1.8)
            :stroke-opacity (if night 0.5 1.0)
            :stroke-dasharray (if night "2,3" "none")))

(defun heliograph--draw-daily-ephemeris (svg samples cx cy horizon gold bright)
  "Paint the full local-day SAMPLES on SVG around CX, CY.
Split horizon crossings and use GOLD and BRIGHT for path and marks.
The midnight endpoints are deliberately not joined."
  (let (previous (i 0))
    (dolist (sun samples)
      (let* ((alt (car sun))
             (point (heliograph--project-altaz alt (cadr sun) cx cy horizon)))
        (when previous
          (let* ((last-alt (car previous))
                 (last-point (heliograph--project-altaz
                              last-alt (cadr previous) cx cy horizon)))
            (if (or (and (< last-alt 0) (> alt 0))
                    (and (> last-alt 0) (< alt 0)))
                (let* ((fraction (/ (- (float last-alt)) (- alt last-alt)))
                       ;; Interpolate along the short arc across north.
                       (delta (- (mod (+ (- (cadr sun) (cadr previous))
                                         180.0) 360.0) 180.0))
                       (crossing (heliograph--project-altaz
                                  0 (+ (cadr previous) (* fraction delta))
                                  cx cy horizon)))
                  (heliograph--draw-solar-segment
                   svg last-point crossing (< last-alt 0) gold)
                  (heliograph--draw-solar-segment
                   svg crossing point (< alt 0) gold))
              (heliograph--draw-solar-segment
               svg last-point point (or (< last-alt 0) (< alt 0)) gold))))
        (when (zerop (mod i 6))
          (svg-circle svg (car point) (cdr point) 1.8
                      :fill (if (< alt 0) gold bright)
                      :fill-opacity (if (< alt 0) 0.45 1.0)))
        (setq previous sun i (1+ i))))))

(defun heliograph--draw-instrument-plate (svg cx cy gold bright dim accent)
  "Paint static engraving on SVG around CX, CY in the supplied palette."
  ;; Outer engraved case and four altitude rings.  The central
  ;; point corresponds to the zenith; the 140px ring is the horizon.
  (dolist (ring '((140 . 1.3) (93.333 . 0.8)
                  (46.667 . 0.8) (202 . 1.0)))
    (svg-circle svg cx cy (car ring)
                :fill "none" :stroke (if (= (car ring) 202) dim gold)
                :stroke-width (cdr ring)))
  (heliograph--instrument-segment svg cx cy 192 0 360 dim 0.8 "2,5")
  (heliograph--instrument-segment svg cx cy 209 0 360 dim 0.7 "1,5")
  ;; Subtle segmented concentric arcs: visually elaborate, but
  ;; static like calibration/registration marks.
  (dolist (radius '(58 108 185 197))
    (heliograph--instrument-segment svg cx cy radius 18 73 dim 0.9)
    (heliograph--instrument-segment svg cx cy radius 196 252 dim 0.9))
  (heliograph--instrument-segment svg cx cy 118 112 178 accent 0.65 "2,4")
  (heliograph--instrument-segment svg cx cy 118 292 358 accent 0.65 "2,4")
  ;; Compass grid and angular reference guides.
  (dotimes (i 12)
    (let* ((angle (* i 30.0))
           (p (heliograph--instrument-point cx cy 0 angle))
           (q (heliograph--instrument-point cx cy 140 angle)))
      (svg-line svg (car p) (cdr p) (car q) (cdr q)
                :stroke dim :stroke-width (if (zerop (mod i 3)) 0.95 0.6)
                :stroke-dasharray (if (zerop (mod i 3)) "none" "3,6"))))
  ;; Major graduations every 30 degrees; half-degree minor ticks.
  (dotimes (i 720)
    (let* ((angle (/ i 2.0))
           (major (zerop (mod i 60)))
           (medium (zerop (mod i 10)))
           (inner (cond (major 185) (medium 194) (t 199)))
           (p (heliograph--instrument-point cx cy inner angle))
           (q (heliograph--instrument-point cx cy 205 angle)))
      (svg-line svg (car p) (cdr p) (car q) (cdr q)
                :stroke (if major bright gold)
                :stroke-width (if major 1.6 (if medium 0.9 0.45)))))
  ;; Calibrated azimuth labels just beyond the outer graduated ring.
  (dotimes (i 12)
    (let* ((angle (* i 30))
           (p (heliograph--instrument-point cx cy 219 angle)))
      (svg-text svg (format "%03d°" angle)
                :x (car p) :y (+ 3 (cdr p))
                :font-size 9 :font-family "monospace"
                :text-anchor "middle" :fill gold)))
  svg)

(defun heliograph--draw-instrument-foreground (svg cx cy gold bright dim)
  "Paint static pivot and inscriptions on SVG at CX, CY."
  ;; Mechanical pivot, etched center reticle, and four registry dots.
  (svg-circle svg cx cy 13 :stroke bright :stroke-width 1.3
              :fill "#201e1a")
  (svg-circle svg cx cy 8 :stroke gold :stroke-width 0.7 :fill "none")
  (svg-circle svg cx cy 3 :fill bright)
  (dolist (angle '(0 90 180 270))
    (let ((p (heliograph--instrument-point cx cy 140 angle)))
      (svg-circle svg (car p) (cdr p) 2.2 :fill bright)))
  ;; Plate inscriptions frame rather than obscure the mechanism.
  (svg-text svg "HCA / ARRAY 01" :x 16 :y 28
            :font-family "monospace" :font-size 12 :fill gold)
  (svg-text svg "ZENITHAL SOLAR PROJECTION"
            :x cx :y -47 :text-anchor "middle"
            :font-family "monospace" :font-size 11 :fill gold)
  (dolist (mark '(("N" 320 -23) ("E" 561 215)
                  ("S" 320 452) ("W" 79 215)))
    (svg-text svg (car mark) :x (nth 1 mark) :y (nth 2 mark)
              :font-family "monospace" :font-size 13
              :font-weight "bold" :text-anchor "middle" :fill bright))
  (svg-text svg "60°" :x (+ cx 9) :y (- cy 46.667)
            :font-family "monospace" :font-size 9 :fill dim)
  (svg-text svg "30°" :x (+ cx 9) :y (- cy 93.333)
            :font-family "monospace" :font-size 9 :fill dim)
  svg)

(defun heliograph--draw-live-observation (svg snapshot cx cy horizon gold bright accent)
  "Paint the time-dependent marker on SVG using SNAPSHOT and palette."
  (let* ((alt (plist-get snapshot :alt))
         (point (heliograph--project-altaz alt (plist-get snapshot :az)
                                           cx cy horizon)))
    (svg-line svg cx cy (car point) (cdr point)
              :stroke gold :stroke-width 1.2
              :stroke-opacity (if (< alt 0) 0.5 1.0)
              :stroke-dasharray "3,5")
    (when (>= alt 0.0)
      (svg-circle svg (car point) (cdr point) 12
                  :stroke gold :stroke-width 0.9
                  :stroke-opacity 0.6 :fill "none"))
    (svg-circle svg (car point) (cdr point) 6
                :stroke (if (< alt 0) gold bright) :stroke-width 1.5
                :stroke-opacity (if (< alt 0) 0.65 1.0)
                :fill (if (< alt 0) "none" bright))
    (when (< alt 0)
      (svg-text svg "SOL BELOW HORIZON"
                :x cx :y 474 :text-anchor "middle"
                :font-family "monospace" :font-size 10 :fill accent))
    svg))

(defun heliograph-render-center-astrolabe (snapshot context)
  "Render an elaborate calibrated solar instrument for SNAPSHOT.
Concentric altitude scales, azimuth engraving and the actual daily
solar path are combined with ornamental registration arcs.  The
instrument uses the same north-up zenithal projection as the basic
solar renderer.  Works without GeoClue using the manual location."
  (let* ((window-width (plist-get context :window-width))
         (window-height (plist-get context :window-height))
         ;; Leave room above and below the dial for stacked inscriptions.
         ;; On a spacious display, enlarge the entire SVG uniformly.
         ;; Keep room for the header, telemetry, and native buttons.
         (available (max 540 (- window-height 240)))
         (scale (if (and (> window-width 1000)
                         (> window-height 700))
                    (min 1.65 (/ available 540.0)
                         (/ window-width 640.0))
                  1.0))
         (width (round (* 640 scale)))
         (height (round (* 540 scale)))
         (cx 320.0) (cy 210.0) (horizon 140.0)
         (gold "#cda769") (bright "#efd194")
         (dim "#6e664f") (accent "#86b7aa")
         ;; A fixed coordinate system keeps every ring, arc, inscription,
         ;; and tick proportionate at all window sizes.
         (svg (svg-create width height :viewBox "0 -60 640 540")))
    (let* ((key (list 640 480 'zenithal-night-v2 gold bright dim accent))
           (plates (heliograph--cache-value
                    'heliograph--plate-cache key
                    (lambda ()
                      (let ((back (svg-create 640 480))
                            (front (svg-create 640 480)))
                        (heliograph--draw-instrument-plate
                         back cx cy gold bright dim accent)
                        (heliograph--draw-instrument-foreground
                         front cx cy gold bright dim)
                        (cons back front))))))
      ;; Copy cached DOMs: SVG helpers mutate their target tree.
      (dolist (child (cddr (copy-tree (car plates))))
        (dom-append-child svg child))
      (heliograph--draw-daily-ephemeris
       svg (heliograph--daily-samples snapshot) cx cy horizon gold bright)
      (heliograph--draw-live-observation
       svg snapshot cx cy horizon gold bright accent)
      (dolist (child (cddr (copy-tree (cdr plates))))
        (dom-append-child svg child)))
    (make-heliograph-node :image (svg-image svg :ascent 'center)
                          :width width :height height)))

(defcustom heliograph-center-renderer #'heliograph-render-center-astrolabe
  "Renderer function for the center of the graphical dashboard.
Called with SNAPSHOT and a context plist.  Must return a
`heliograph-node' with an image and positive pixel dimensions."
  :type 'function :group 'heliograph)

(defun heliograph--gui-context ()
  "Describe the available window geometry and presentation palette."
  (list :window-width (window-body-width nil t)
        :window-height (window-body-height nil t)
        :gold "#efd194" :white "#f1f0eb"
        :dim "#9d8155" :accent "#83b6aa"))

(defun heliograph--center-node (snapshot)
  "Render the center node for SNAPSHOT, validating its dimensions."
  (let ((node (funcall heliograph-center-renderer
                       snapshot (heliograph--gui-context))))
    (unless (and (heliograph-node-p node)
                 (heliograph-node-image node)
                 (integerp (heliograph-node-width node))
                 (> (heliograph-node-width node) 0)
                 (integerp (heliograph-node-height node))
                 (> (heliograph-node-height node) 0))
      (error "Heliograph center renderer must return a valid heliograph-node"))
    node))

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
                                (heliograph--station-label) (plist-get snapshot :date))
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
      (heliograph--line (heliograph--ephemeris-label snapshot) 'heliograph-muted)
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

(defun heliograph--gui-text (text &optional gold right-align)
  "Create one SVG image for TEXT, gold if GOLD, otherwise white.
Gold labels are two points larger than the white labels.
RIGHT-ALIGN anchors the lettering to the canvas's right inset."
  (let* ((size (if gold 15 13))
         (color (if gold "#efd194" "#f1f0eb"))
         (width (ceiling (+ 12 (* (length text) size 0.74))))
         (canvas (svg-create width 24))
         (attrs (list :x (if right-align (- width 5) 5)
                      :y 18 :font-family "monospace"
                      :font-size size
                      :text-anchor (if right-align "end" "start"))))
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
        (right-image (heliograph--gui-text right nil t)))
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

(defun heliograph--gui-render (snapshot node)
  "Render a full-window graphical SNAPSHOT with centered instrument NODE.
The final telemetry panel is bottom-anchored when the window is tall
enough; smaller windows scroll naturally instead of overlapping."
  (let* ((height (window-body-height nil t))
         (center-width (heliograph-node-width node))
         (center-height (heliograph-node-height node))
         (center-image (heliograph-node-image node))
         (lh (max 1 (frame-char-height)))
         ;; Header = 2 strips + rule; footer = rule, 4 strips,
         ;; rule, and one native interactive command line.
         (top-height (+ 48 3))
         (footer-height (+ 3 (* 4 24) 3 lh))
         (free (max 0 (- height top-height center-height footer-height)))
         (before (/ free 2))
         (after (- free before)))
    (heliograph--gui-pair "☼ HELIOGRAPHIC COMPUTING AUTHORITY"
                          "SYS / 077" t)
    (heliograph--gui-rule)
    (heliograph--gui-pair (format "STATION %s" (heliograph--station-label))
                          (plist-get snapshot :date))
    (heliograph--gui-gap before)
    (when (>= (window-body-width nil t) center-width)
      (heliograph--gui-align
       `(- center (,(floor center-width 2)))))
    (insert-image center-image)
    (insert "\n")
    (heliograph--gui-gap after)
    (heliograph--gui-rule)
    (heliograph--gui-pair "CELESTIAL OBSERVATION ARRAY / INSTRUMENT 01"
                          (heliograph--ephemeris-label snapshot) t)
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
  (setq-local heliograph--resize-enabled t)
  (heliograph--geoclue-ensure-started)
  (if (and (not (eq heliograph--renderer 'text))
           (display-graphic-p)
           (image-type-available-p 'svg))
      (unless heliograph--busy
        (let ((heliograph--busy t)
              (inhibit-read-only t)
              (snapshot (heliograph--snapshot)))
          (erase-buffer)
          (heliograph--gui-render snapshot (heliograph--center-node snapshot))
          (goto-char (point-min))))
    (heliograph--render-text)))

(defvar-local heliograph--resize-enabled nil
  "Non-nil when this buffer should reflow on window resize.")

(defun heliograph--window-size ()
  "Return the selected window's body size in pixels."
  (cons (window-body-width nil t) (window-body-height nil t)))

(defun heliograph--on-window-resize (frame)
  "Reflow visible graphical heliograph windows on FRAME resize.
Ignore text mode and windows whose pixel dimensions have not changed."
  (dolist (window (window-list frame 'no-minibuf))
    (when (window-live-p window)
      (let ((buffer (window-buffer window)))
        (with-current-buffer buffer
          (when (and heliograph--resize-enabled
                     (not (eq heliograph--renderer 'text))
                     (display-graphic-p frame)
                     (image-type-available-p 'svg))
            (let ((size (cons (window-body-width window t)
                              (window-body-height window t))))
              (unless (equal size (window-parameter window 'heliograph--size))
                (set-window-parameter window 'heliograph--size size)
                (with-selected-window window
                  (heliograph--render))))))))))

;; Registered once per Emacs session.  The callback does nothing unless a
;; graphical heliograph buffer is displayed in the resized frame.
(add-hook 'window-size-change-functions #'heliograph--on-window-resize)

(defcustom heliograph-time-step-seconds 600
  "Seconds moved by each observation-time step."
  :type 'natnum :group 'heliograph)

(defun heliograph-step-forward (&optional count)
  "Move observation time forward COUNT steps, default one."
  (interactive "p")
  (setq heliograph--observation-time
        (time-add (or heliograph--observation-time (current-time))
                  (* (or count 1) heliograph-time-step-seconds)))
  (heliograph--render))

(defun heliograph-step-backward (&optional count)
  "Move observation time backward COUNT steps, default one."
  (interactive "p")
  (heliograph-step-forward (- (or count 1))))

(defun heliograph-now ()
  "Return this dashboard to live observation time."
  (interactive)
  (setq heliograph--observation-time nil)
  (heliograph--render))

(defvar heliograph-mode-map
  (let ((map (make-sparse-keymap)))
    ;; `special-mode' otherwise inherits Space as a scrolling command.
    (define-key map (kbd "SPC") #'ignore)
    (define-key map (kbd "f") #'find-file)
    (define-key map (kbd "p") #'project-switch-project)
    (define-key map (kbd "r") #'recentf-open-files)
    (define-key map (kbd "c") #'customize)
    (define-key map (kbd "[") #'heliograph-step-backward)
    (define-key map (kbd "]") #'heliograph-step-forward)
    (define-key map (kbd "n") #'heliograph-now)
    (define-key map (kbd "g") #'heliograph-refresh)
    (define-key map (kbd "q") #'quit-window)
    map))

(declare-function evil-set-initial-state "evil-core" (mode state))
(declare-function evil-define-key* "evil-core" (state keymap key def &rest bindings))

;; Optional integration, regardless of which package loads first.
(with-eval-after-load 'evil
  (evil-set-initial-state 'heliograph-mode 'normal)
  ;; Override Evil's bracket prefixes while preserving Doom's leader map.
  (evil-define-key* '(normal motion) heliograph-mode-map
    (kbd "[") #'heliograph-step-backward
    (kbd "]") #'heliograph-step-forward
    (kbd "n") #'heliograph-now))

(defun heliograph--cancel-timer ()
  "Cancel the current buffer's refresh timer."
  (when (timerp heliograph--timer)
    (cancel-timer heliograph--timer)
    (setq heliograph--timer nil)))

(define-derived-mode heliograph-mode special-mode "Heliograph"
  "Major mode for the heliographic console."
  (setq-local truncate-lines t)
  (setq-local cursor-type nil)
  (add-hook 'kill-buffer-hook #'heliograph--cancel-timer nil t)
  (add-hook 'change-major-mode-hook #'heliograph--cancel-timer nil t))

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
                   (let ((window (get-buffer-window buffer t)))
                     (when window
                       (with-selected-window window
			 (heliograph--render))))))))))))

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
