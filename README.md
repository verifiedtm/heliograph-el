# heliograph

a fictional 1970s heliographic computing authority, implemented as a native emacs dashboard. graphical emacs draws an astronomical svg dial; terminal emacs uses the same layout with native text and an instrument placeholder. no browser, external process, or third-party elisp dependency.

## quick start

put `heliograph.el` somewhere on your emacs load path, then:

```elisp
(add-to-list 'load-path "~/src/heliograph-el")
(require 'heliograph)
(setq heliograph-station "WADJET"
      heliograph-latitude 40.7128
      heliograph-longitude -74.006)
```

evaluate `M-x heliograph` to open the standalone console.

select a renderer explicitly with `M-x heliograph-svg` or `M-x heliograph-text`. the text command works even in a graphical frame, while the svg command requires svg support. each opens its own buffer; refreshes keep its selected renderer.

All presentations share the same content and layout: a top header, centered instrument, and bottom telemetry and controls. Paired labels anchor to the left and right edges, stacking when space is tight. SVG enhances individual labels, rules, and the instrument; unavailable or failed elements fall back independently to native text. Window resizing redraws both modes; only the SVG center instrument scales, while the text placeholder keeps its fixed size. When the content exceeds the window height, it scrolls.

## doom dashboard integration

the standalone command is the recommended first test. doom dashboard internals vary by revision; versions with `+doom-dashboard-functions` support:

```elisp
(after! doom-dashboard
  (setq +doom-dashboard-functions '(heliograph-dashboard-widget)))
```

other revisions use `+dashboard-functions`:

```elisp
(after! doom-dashboard
  (setq +dashboard-functions '(heliograph-dashboard-widget)))
```

inspect your installed doom module before selecting the variable. this is experimental integration: doom controls buffer layout, redraw, and keymaps, so behavior can depend on the installed revision.

## controls

`f` find file, `p` switch project, `r` recent files, `c` customize, `g` refresh, `q` quit. controls are clickable buttons in gui and terminal emacs. direct single-key bindings apply in standalone heliograph mode.

## notes

- emacs 28.1+; the graphical dial requires svg image support.
- solar altitude/azimuth use approximate noaa equations; the lunar cycle is approximate. not navigation-grade.
- set your real latitude/longitude; the defaults are example new york coordinates.
- display refreshes at most once a minute while visible.
- the current svg is 640px wide and may clip on small graphical frames.
- 'archive online' and 'instrumentation ready' are currently decorative, not actual system-health indicators.
- no startup animation yet. this is the initial working prototype.

## roadmap

responsive layout; tested doom-version adapters; richer instrument engraving; accurate lunar ephemeris and astronomical twilight; optional staged startup animation; automated emacs test suite.

## custom center instruments

the graphical dashboard calls `heliograph-center-renderer` with a snapshot
and context plist (window size and palette). the renderer must return a
`heliograph-node` holding its image and pixel dimensions. the default is
`heliograph-render-center-solar`.

```elisp
(defun my-heliograph-center (snapshot context)
  (let* ((width 320)
         (height 180)
         (canvas (svg-create width height)))
    (svg-text canvas
              (format "SOL %+0.1f°" (plist-get snapshot :alt))
              :x 20 :y 90 :fill (plist-get context :gold))
    (make-heliograph-node :image (svg-image canvas)
                          :width width :height height)))

(setq heliograph-center-renderer #'my-heliograph-center)
```

for a more elaborate astronomical-clock instrument, select the opt-in astrolabe renderer:

```elisp
(setq heliograph-center-renderer #'heliograph-render-center-astrolabe)
```

it adds half-degree azimuth graduations, segmented registration arcs,
concentric altitude circles, and a calculated above-horizon solar path for
the local calendar day. the bright marker shows the current sun when it is
above the horizon. it uses the configured observer coordinates and does not
require geoclue.

refresh with `g`. the layout uses the returned dimensions rather than
hardcoded solar-dial dimensions. this affects graphical emacs only.

## station and observer location

The station defaults to `(system-name)` and its **display** is uppercased.
Override the display formatter without changing the underlying name:

```elisp
(setq heliograph-station-formatter
      (lambda (name) (format "OBSERVATORY %s" (upcase name))))
```

Latitude/longitude are configured manually by default:

```elisp
(setq heliograph-latitude 40.7128
      heliograph-longitude -74.006
      heliograph-location-provider 'manual)
```

For automatic location, set `heliograph-location-provider` to a *fast, cached*
zero-argument function returning `(LATITUDE . LONGITUDE)`. Nil, invalid
coordinates, or provider errors fall back to manual coordinates:

```elisp
(setq heliograph-location-provider
      (lambda () (and my-last-location
                      (cons (car my-last-location)
                            (cdr my-last-location)))))
```

A GeoClue2 D-Bus adapter is a future integration; the package does **not** yet
contact GeoClue or locate the user automatically. The provider must not block
startup waiting for a location fix.

## geoclue2 on linux

GeoClue is optional and opt-in. Install the service, e.g. on Debian:

```sh
sudo apt install geoclue-2.0
```

In Doom `config.el`:

```elisp
(after! heliograph
  (setq heliograph-latitude 40.7128
        heliograph-longitude -74.006
        heliograph-geoclue-desktop-id "emacs"
        heliograph-location-provider 'geoclue))
```

Use `M-x heliograph-geoclue-start` to request a fix or
`M-x heliograph-geoclue-stop` to end updates. Normally startup triggers
the request automatically when the geoclue provider is selected.

The adapter uses asynchronous system-bus D-Bus calls and requests
**city-level accuracy**. GeoClue requires authorization for the configured
desktop ID, which must match an acceptable application desktop identity;
availability depends on the desktop session and its location agent.
If no fix arrives, manual coordinates remain in use. No coordinates are
saved to disk by heliograph.

To inspect a fix:

```elisp
heliograph--geoclue-fix
```

Do not add broad system-wide location permissions just to make the
dashboard work. If the desktop agent denies access, inspect the GeoClue
journal and authorization policy first.

The D-Bus integration has not yet been tested against a live GeoClue
daemon in CI; the unit tests run without a location service.
