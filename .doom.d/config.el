;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")
;; Making emacs find latex (so that C-c C-x C-l works on orgmode)
(setenv "PATH" (concat ":/Library/TeX/texbin/" (getenv "PATH")))
(add-to-list 'exec-path "/Library/TeX/texbin/")
;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;Default font didn't have *bold* face. Setting Fira Code fixed this.
;;https://github.com/railwaycat/homebrew-emacsmacport/issues/233
(setq doom-font (font-spec :family "Fira Code" :size 12))
;; pseudo "live preview" - hide markup
(setq org-hide-emphasis-markers t)
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-homage-white)
;; Good themes
;; doom-henna, tango, dichromacy (default is very similar, dich is a lil better), whiteboard
;; doom-shades-of-purple
;; doom-solarized-light
;; doom-challenger-deep
;; doom-solarized-dark
;; doom-tomorrow-day
;; doom-homage-black
;; doom-tokyo-night bad python syntax highlighting - fix with python package?
;; doom-city-lights
;; doom-one-light
;; doom-flatwhite
;; doom-ephemeral
;; doom-earl-grey - light
;; doom-ir-black
;; doom-meltbus
;; doom-rouge
;; doom-dark+
;; doom-nova, dichromacy
;;
;;
;; dark
;; doom-one
;; doom-horizon
;; doom-laserwave
;; doom-material
;; doom-moonlight
;; doom-outrun-electric
;; doom-snazzy
;; doom-rouge
;; doom-snazzy
;; doom-spacegrey
;; doom-tokyo-night

;; light
;; doom-plain
;; doom-homage-white
;; doom-ayu-light
;; doom-tomorrow-day
;; doom-one-light
;; doom-nord-light


;; C-x C-s works in normal mode, but not insert mode.
;; Disable yasnippet completion when trying to save the file in insert mode
;; https://www.reddit.com/r/emacs/comments/kftv15/doom_emacs_problems_rebinding_keys/
(map! :i "C-x C-s" nil)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
;(setq org-roam-directory "~/roam")
;(setq org-attach-id-dir "~/roam/.attach")
;(setq org-agenda-files (quote ("~/org/"
;                               "~/roam"
;                               )
;                        )
;      )
; Instead of having two separate folders, I'm putting roam inside org folder itself.
; This way, I dont have to maintain two different folders and paths in variables.
(setq org-roam-directory "~/org/roam")
(setq org-attach-id-dir "~/org/roam/.attach")
(setq org-agenda-files (quote ("~/org/"
                               "~/org/roam"
                               "~/org/roam/daily"
                               )
                        )
      )

; org-roam sections. unlinked references may make things slow
; https://github.com/org-roam/org-roam/blob/main/doc/org-roam.org#configuring-what-is-displayed-in-the-buffer
(setq org-roam-mode-sections
      (list #'org-roam-backlinks-section
            #'org-roam-reflinks-section
            #'org-roam-unlinked-references-section
            ))

; drag drop attachments
; (require 'org-download)

;; Make leader key popup faster
(require 'which-key)
(setq which-key-idle-delay 0.1)

; When inserting a roam node after a space in evil, link is inserted under the cursor, which ends in bad UX
; https://stackoverflow.com/questions/61243822/why-does-pressing-escape-go-back-one-character
; Also here:
; In evil mode when the cursor is in normal mode, you would expect the filelink to be written after the block cursor, in accordance with how paste works in evil mode.
; This is useful because you don't have to press space twice if you are habituated in evil, or spacemacs.
; https://github.com/org-roam/org-roam/issues/4
; Workaround an upstream issue with evil, as described in https://github.com/syl20bnr/spacemacs/issues/14137
(defadvice org-roam-node-insert (around append-if-in-evil-normal-mode activate compile)
        "If in evil normal mode and cursor is on a whitespace character, then go into
         append mode first before inserting the link. This is to put the link after the
         space rather than before."
        (let ((is-in-evil-normal-mode (and (bound-and-true-p evil-mode)
                                          (not (bound-and-true-p evil-insert-state-minor-mode))
                                          (looking-at "[[:blank:]]"))))
          (if (not is-in-evil-normal-mode)
              ad-do-it
            (evil-append 0)
            ad-do-it
            (evil-normal-state))))
;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
