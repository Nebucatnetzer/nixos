;; -*- lexical-binding: t; -*-
;; Feature flags for the portable terminal (emacs-nox) az-emacs build.
;; Scoped to basic coding / file editing plus the occasional org-mode note.
;; Deliberately left off: GUI-only features (enable-font, enable-emojis,
;; enable-scroll-bar, disable-fringe, enable-pdf-tools) which touch
;; graphics-only APIs, and the heavier enable-email (mu4e), enable-notes
;; (denote) and enable-langtool (languagetool) stacks that aren't needed in
;; a portable terminal editor.
(setq enable-color-theme t)
(setq enable-org t)
;; Copy to the host clipboard from terminal frames via OSC 52.
(setq enable-clipetty t)
