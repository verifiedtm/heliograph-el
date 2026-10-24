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

evaluate `M-x heliograph` to open the standalone console.

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
