;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; set overall transparency. even text will be transparent
;; you can also use doom/set-frame-opacity
;; emacs-29 has background only transparency but doom doesn't support v29
(add-to-list 'default-frame-alist '(alpha . 95))


;; pointing python command to conda version for org babel
(setq org-babel-python-command "/Users/aswin/miniconda/bin/python")

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Increasing granularity of Emacs undo
(setq evil-want-fine-undo t)

;; Shortcut for diary / dailies
;; (map! :leader
;;       :desc "Org Roam Go to Date"
;;       "d" #'org-roam-dailies-goto-date)

;; Setting a variable through key binding
;; Setting a variable with a custom value : https://stackoverflow.com/questions/378972/how-do-i-make-an-emacs-keybinding-to-set-a-variable
(defun toggle-emphasis-markers()
  "Toggle emphasis markers"
  (interactive) ;; You need to make a function interactive to call it with keybindings
  (setq org-hide-emphasis-markers nil))

(map! :after org
      :map org-mode-map
      :leader
      :desc "Toggle emphasis markers"
      "d" #'toggle-emphasis-markers)

;; instead of a toggle, set org-agenda-start-with-log-mode to t to
;; start agenda in log mode by default
;; (map! :after org
;;      :map org-mode-map
;;      :leader
;;      :desc "Org Agenda Log Mode"
;;      "l" #'org-agenda-log-mode)

;; adding deeplinks for zotero
(after! org
  ;; custom link types
(org-link-set-parameters "zotero"
  :follow (lambda (path) (shell-command (concat "open zotero:" path))))
;; add timestamp when completing tasks
(setq org-log-done t)
;; always open org-agenda in log-mode
;; https://stackoverflow.com/questions/22394394/orgmode-a-report-of-tasks-that-are-done-within-the-week
(setq org-agenda-start-with-log-mode t)
;; Latex preview font is too tiny. Increase scale
(plist-put org-format-latex-options :scale 4.0)

;; automatically create a CREATED timestamp in property drawer
;; https://emacs.stackexchange.com/questions/35751/how-to-add-a-created-field-to-any-todo-task
;; it fits the standard formatting - SCHEDULED, DEADLINE and CLOSED under heading,
;; CREATED in property drawer
;; https://www.reddit.com/r/orgmode/comments/q6myiz/are_timestamps_allowed_inside_the_properties/
;; (defun my/log-todo-creation-date (&rest ignore)
;;   "Log TODO creation time in the property drawer under the key 'CREATED'."
;;   (when (and (org-get-todo-state)
;;              (not (org-entry-get nil "CREATED")))
;;     (org-entry-put nil "CREATED" (format-time-string (cdr org-time-stamp-formats)))))

;; (advice-add 'org-insert-todo-heading :after #'my/log-todo-creation-date)
;; (advice-add 'org-insert-todo-heading-respect-content :after #'my/log-todo-creation-date)
;; (advice-add 'org-insert-todo-subheading :after #'my/log-todo-creation-date)
;; (add-hook 'org-after-todo-state-change-hook #'my/log-todo-creation-date)


;; default doom bindings for org-mode are defined in ~/.emacs.d/modules/lang/org/config.el
;; use below bindings to overwrite
;; https://github.com/doomemacs/doomemacs/issues/516
;;  (setq org-todo-keywords
;;        '((sequence
        ;;    "TODO(t!)"  ; A task that needs doing & is ready to do
        ;;    "PROJ(p)"  ; A project, which usually contains other tasks
        ;;    "LOOP(r)"  ; A recurring task
        ;;    "STRT(s)"  ; A task that is in progress
        ;;    "WAIT(w)"  ; Something external is holding up this task
        ;;    "HOLD(h)"  ; This task is paused/on hold because of me
        ;;    "IDEA(i)"  ; An unconfirmed and unapproved task or notion
        ;;    "|"
        ;;    "DONE(d)"  ; Task successfully completed
        ;;    "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
        ;;   (sequence
        ;;    "[ ](T)"   ; A task that needs doing
        ;;    "[-](S)"   ; Task is in progress
        ;;    "[?](W)"   ; Task is being held up or paused
        ;;    "|"
        ;;    "[X](D)")  ; Task was completed
        ;;   (sequence
        ;;    "|"
        ;;    "OKAY(o)"
        ;;    "YES(y)"
        ;;    "NO(n)"))
        ;; org-todo-keyword-faces
        ;; '(("[-]"  . +org-todo-active)
        ;;   ("STRT" . +org-todo-active)
        ;;   ("[?]"  . +org-todo-onhold)
        ;;   ("WAIT" . +org-todo-onhold)
        ;;   ("HOLD" . +org-todo-onhold)
        ;;   ("PROJ" . +org-todo-project)
        ;;   ("NO"   . +org-todo-cancel)
        ;;   ("KILL" . +org-todo-cancel)))
)

;; heic heif support - partial. opens images inside emacs if you do
;; SPC m a o > open attachment. However, doesn't open outside by default and
;; inline display doesn't work anyway. Atleast, not like this.
;; Ask in reddit. Until then, keep the default so you can open Preview with the image
;; by pressing RET
;;(when (and (image-type-available-p 'image-io)
;;            (not (boundp 'imagemagick-render-type)))
;;      ;; Image I/O is used as a fallback of ImageMagick.
;;      (setq imagemagick-enabled-types t)
;;      (setq imagemagick-types-inhibit
;;            (cons 'XML (delq 'PDF imagemagick-types-inhibit)))
;;      (imagemagick-register-types))

;; Dont cache gpg pass
(setq epa-file-cache-passphrase-for-symmetric-encryption nil)

;; How do I make TAB insert the current completion?
;; https://github.com/company-mode/company-mode/issues/640
;; :bind (:map company-active-map ("<tab>" . company-complete-selection))
;; Wait for company to load
;; SPC h m : describe-mode to check details and keymap of company mode
;; Deactivate return and remap tab mapping
(map! :after company
      :map company-active-map
      "<tab>" #'company-complete-selection
      "TAB" #'company-complete-selection
      "<return>" nil
      "RET" nil)

;; Changing evil-escape key chord to jj instead of jk
;; https://github.com/doomemacs/doomemacs/blob/44eb11c028b9372e6f121639cea538da5b8d23b7/modules/editor/evil/config.el#L256
;; https://github.com/doomemacs/doomemacs/issues/1946
(after! evil-escape (setq evil-escape-key-sequence "jj"))

;; Customize doom-modeline
(display-battery-mode t)
(display-time-mode t)

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
            ;#'org-roam-unlinked-references-section
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
