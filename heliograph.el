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
  "Build graphical SVG dial for SNAPSHOT, or nil in terminal frames."
  (when (and (display-graphic-p) (image-type-available-p 'svg)
             (fboundp 'svg-create))
    (let* ((s (svg-create 640 336))
           (cx 320.0) (cy 168.0)
           (gold "#bc985d") (dim "#655d49") (pale "#f0ce8a")
           (az (* (/ float-pi 180.0) (plist-get snapshot :az)))
           ;; Zenith at center; horizon at r=125.  Below horizon uses r>125.
           (r (* 125.0 (/ (- 90.0 (plist-get snapshot :alt)) 90.0)))
           (px (+ cx (* r (sin az))))
           (py (- cy (* r (cos az)))))
      (dolist (radius '(45 85 125))
        (svg-circle s cx cy radius :fill "none"
                    :stroke gold :stroke-width 1))
      (svg-circle s cx cy 145 :fill "none" :stroke dim
                  :stroke-dasharray "3,5" :stroke-width 1)
      (dotimes (i 72)
        (let* ((theta (* (/ (* 2.0 float-pi) 72.0) i))
               (inner (if (zerop (mod i 6)) 134 141)))
          (svg-line s (+ cx (* inner (sin theta)))
                    (- cy (* inner (cos theta)))
                    (+ cx (* 150 (sin theta)))
                    (- cy (* 150 (cos theta)))
                    :stroke gold :stroke-width (if (zerop (mod i 6)) 2 1))))
      (svg-line s 154 cy 486 cy :stroke dim :stroke-width 1)
      (svg-line s cx 6 cx 330 :stroke dim :stroke-width 1)
      (svg-circle s cx cy 13 :fill "#84592b" :stroke pale :stroke-width 2)
      (svg-circle s cx cy 5 :fill pale)
      (when (<= r 150)
        (svg-line s cx cy px py :stroke pale :stroke-width 2)
        (svg-circle s px py 7 :fill pale))
      (dolist (mark '(("N" 320 11) ("E" 490 172)
                      ("S" 320 333) ("W" 150 172)))
        (svg-text s (car mark) :x (nth 1 mark) :y (nth 2 mark)
                  :font-size 12 :font-family "monospace"
                  :text-anchor "middle" :fill pale))
      (svg-text s "HCA / ARRAY 01" :x 8 :y 20 :font-size 12
                :font-family "monospace" :fill gold)
      (svg-text s "ZENITHAL PROJECTION" :x 632 :y 20
                :font-size 12 :font-family "monospace"
                :text-anchor "end" :fill gold)
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

(defun heliograph--render ()
  "Replace current buffer contents with the observatory display."
  (unless heliograph--busy
    (let* ((heliograph--busy t)
           (inhibit-read-only t)
           (snapshot (heliograph--snapshot))
           (dial (heliograph--dial snapshot))
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
                     (heliograph--render)))))))))

;;;###autoload
(defun heliograph-refresh ()
  "Refresh the heliographic console."
  (interactive)
  (heliograph--render))

;;;###autoload
(defun heliograph ()
  "Open the heliographic observatory console."
  (interactive)
  (let ((buffer (get-buffer-create "*heliograph*")))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (unless (derived-mode-p 'heliograph-mode)
        (heliograph-mode))
      (heliograph--render)
      (heliograph--start-timer))))

;;;###autoload
(defun heliograph-dashboard-widget ()
  "Insert heliographic console into Doom's dashboard buffer.
Use as the only dashboard rendering function on supported Doom versions."
  (heliograph--render)
  (heliograph--start-timer))

(provide 'heliograph)
;;; heliograph.el ends here
