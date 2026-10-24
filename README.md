# heliograph

a fictional 1970s heliographic computing authority, implemented as a native emacs dashboard. graphical emacs draws an astronomical svg dial; terminal emacs draws a unicode solar diagram. no browser, external process, or third-party elisp dependency.

## quick start

put `heliograph.el` somewhere on your emacs load path, then:

```elisp
(add-to-list 'load-path "~/src/heliograph-el")
(require 'heliograph)
(setq heliograph-station "WADJET"
      heliograph-latitude 40.7128
      heliograph-longitude -74.006)
```

evaluate `M-x heliograph` to open the standalone console.\n\nselect a renderer explicitly with `M-x heliograph-svg` or `M-x heliograph-text`. the text command works even in a graphical frame, while svg requires svg support. each opens its own buffer; refreshes keep its selected renderer.

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
