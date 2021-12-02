;;; blamer.el --- Show git blame info about current line           -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Artur Yaroshenko

;; Author: Artur Yaroshenko <artawower@protonmail.com>
;; URL: https://github.com/artawower/blamer.el
;; Package-Requires: ((emacs "27.1") (a "1.0.0"))
;; Version: 0.3.5

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Package for displaying git information about the current line or
;; about several selected lines. Works with git only.

;;; Code:

(require 'files)
(require 'subr-x)
(require 'simple)
(require 'time-date)
(require 'tramp)
(require 'a)

(defconst blamer--regexp-info
  (concat "^(?\\(?1:[^\s]+\\) [^\s]*[[:blank:]]?\(\\(?2:[^\n]+\\)"
          "\s\\(?3:[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)"
          "\s\\(?4:[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)")

  "Regexp for extract data from blame message.
1 - commit hash
2 - author
3 - date
3 - time")

(defconst blamer--commit-message-regexp "\n\n[\s]+\\(?1:[^\']+\\):?"
  "Regexp for commit message parsing.")

(defconst blamer--git-author-cmd "git config --get user.name"
  "Command for getting current git user name.")

(defconst blamer--git-repo-cmd "git rev-parse --is-inside-work-tree"
  "Command for detect git repo.")

(defconst blamer--git-blame-cmd "git blame -L %s,%s %s"
  "Command for get blame of current line.")

(defconst blamer--git-commit-message "git log -n1 %s"
  "Command for get commit message.")

(defgroup blamer nil
  "Show commit info at the end of a current line."
  :group 'tools)

(defcustom blamer-author-formatter "%s, "
  "Format string for author name."
  :group 'blamer
  :type 'string)

(defcustom blamer-datetime-formatter "%s "
  "Format string for datetime."
  :group 'blamer
  :type 'string)

(defcustom blamer-commit-formatter "◉ %s"
  "Format string for commit message."
  :group 'blamer
  :type 'string)

(defcustom blamer-entire-formatter "   %s"
  "Format entire commit string."
  :group 'blamer
  :type 'string)


(defcustom blamer-idle-time 0.5
  "Seconds before commit info show."
  :group 'blamer
  :type 'float)

(defcustom blamer-min-offset 60
  "Minimum symbols before insert commit info."
  :group 'blamer
  :type 'integer)

(defcustom blamer-prettify-time-p t
  "Enable relative prettified format for datetime."
  :group 'blamer
  :type 'boolean)

(defcustom blamer-offset-per-symbol nil
  "Its a crutch :< for font less then original editor font. Use carefully.
Will add additional space for each BLAMER-OFFSET-PER-SYMBOL"
  :group 'blamer
  :type 'integer)

(defcustom blamer-type 'both
  "Type of blamer.
'visual - show blame only for current line
'selected - show blame only for selected line
'both - both of them"
  :group 'blamer
  :type '(choice (const :tag "Visual only" visual)
                 (const :tag "Visual and selected" both)
                 (const :tag "Selected only" selected)))

(defcustom blamer-self-author-name "You"
  "Message for commits where you are author."
  :group 'blamer
  :type 'string)

(defcustom blamer-max-lines 30
  "Maximum blamed lines."
  :group 'blamer
  :type 'integer)

(defcustom blamer-max-commit-message-length 30
  "Max length of commit message.
Commit message with more characters will be truncated with ellipsis at the end"
  :group 'blamer
  :type 'integer)

(defcustom blamer-uncommitted-changes-message "Uncommitted changes"
  "Message for uncommitted lines."
  :group 'blamer
  :type 'string)

(defcustom blamer-view 'overlay
  "View for commit message. Can be 'overlay and 'overlay-right."
  :group 'blamer
  :type '(choice (const :tag "Overlay right" overlay-right)
                 (const :tag "Overlay" overlay)))

(defcustom blamer-smart-background-p t
  "When enable blamer will try to find background.
If not will use background from `blamer-face'"
  :group 'blamer
  :type 'boolean)

(defface blamer-face
  '((t :foreground "#7a88cf"
       :background nil
       :italic t))
  "Face for blamer info."
  :group 'blamer)

(defcustom blamer-bindings nil
  "List of bindings.

Callback function applying plist argument:
:commit-hash - hash of clicked commit
:commit-author - author name after formatting
:raw-commit-author - raw author username if exist.
:commit-date - date of commit. (string field)
:commit-time - commit's time. (string field)
:commit-message - message of commit. If not exist will be get from
`blamer-uncommitted-changes-message' variable.
:raw-commit-message - Full message of commit.

For example you can pass such bindings for message
author name by left click and copying commit hash by right click.
\(without backslash)

'((<mapvar> . (lambda (commit-info) (message (plist-get :commit-author))))
  (<mapvar> . (lambda (commit-info) (kill-new (plist-get :commit-hash)))))"
  :group 'blamer
  :type 'alist)

(defcustom blamer-tooltip-function nil
  "Function to compute the tooltip contents of the popup message."
  :group 'blamer
  :type '(choice (const :tag "Keybindings prompt" blamer-tooltip-keybindings)
                 (const :tag "Commit message" blamer-tooltip-commit-message)
                 (const :tag "Info about author" blamer-tooltip-author-info)
                 (const :tag "No tooltip" nil)))

(defvar blamer-idle-timer nil
  "Current timer before commit info showing.")

(defvar blamer--previous-line-number nil
  "Line number of previous popup.")

(defvar blamer--previous-window-width nil
  "Previous window width.")

(defvar blamer--previous-line-length nil
  "Current line number length for detect rerender function.")

(defvar blamer--previous-region-active-p nil
  "Was previous state is active region?")

(defvar blamer--overlays '()
  "Current active overlays for git blame messages.")

(defvar-local blamer--current-author nil
  "Git.name for current repository.")

(defun blamer-tooltip-commit-message (commit-info)
  "This function can be use as `blamer-tooltip-function', to show the commit message from COMMIT-INFO."
  (plist-get commit-info :raw-commit-message))

(defun blamer-tooltip-author-info (commit-info)
  "This function can be use as `blamer-tooltip-function', to show the author from COMMIT-INFO."
  (format "%s (%s)" (plist-get commit-info :commit-author) (plist-get commit-info :raw-commit-author)))

(defun blamer-tooltip-keybindings (_commit-info)
  "This function can be use as `blamer-tooltip-function', to show the available `blamer-bindings'."
  (if (> (length blamer-bindings) 0)
      (mapconcat (lambda (bind) (format "%s - %s" (car bind) (cdr bind))) blamer-bindings "\n")
     nil))

(defun blamer--git-exist-p ()
  "Return t if .git exist."
  (let* ((git-exist-stdout (shell-command-to-string blamer--git-repo-cmd)))
    (string-match "^true" git-exist-stdout)))

(defun blamer--clear-overlay (&rest _)
  "Clear last overlay."
  (dolist (ov blamer--overlays)
    (delete-overlay ov))
  (setq blamer--overlays '()))

(defun blamer--git-cmd-error-p (cmd-res)
  "Return t if CMD-RES contain error."
  (string-match-p  "^fatal:" cmd-res))

(defun blamer--truncate-time (time)
  "Remove seconds from TIME string."
  (string-join (butlast (split-string time ":")) ":"))

(defun blamer--prettify-time (date time)
  "Prettify DATE and TIME for nice commit message."
  (let* ((parsed-time (decoded-time-set-defaults (parse-time-string (concat date "T" time))))
         (now (decode-time (current-time)))
         (seconds-ago (float-time (time-since (concat date "T" time))))
         (minutes-ago (if (eq seconds-ago 0) 0 (floor (/ seconds-ago 60))))
         (hours-ago (if (eq minutes-ago 0) 0 (floor (/ minutes-ago 60))))
         (days-ago (if (eq hours-ago 0) 0 (floor (/ hours-ago 24))))
         (weeks-ago (if (eq days-ago 0) 0 (floor (/ days-ago 7))))
         (months-ago (if (eq days-ago 0) 0 (floor (/ days-ago 30))))
         (years-ago (if (eq months-ago 0) 0 (floor (/ months-ago 12)))))

    (cond ((or (time-equal-p now parsed-time) (< seconds-ago 60)) "Now")
          ((< minutes-ago 60) (format "%s minutes ago" minutes-ago))
          ((= hours-ago 1) (format "Hour ago"))
          ((< hours-ago 24) (format "%s hours ago" hours-ago))
          ((= days-ago 1) "Yesterday")
          ((< days-ago 7) (format "%s days ago" days-ago))
          ((= weeks-ago 1) "Last week")
          ((<= weeks-ago 4) (format "%s weeks ago" weeks-ago))
          ((= months-ago 1) "Previous month")
          ((< months-ago 12) (format "%s months ago" months-ago))
          ((= years-ago 1) "Previous year")
          ((< years-ago 10) (format "%s years ago" years-ago))
          (t (concat date " " (blamer--truncate-time time))))))

(defun blamer--format-datetime (date time)
  "Format DATE and TIME."
  (if (and blamer-datetime-formatter date time)
      (format blamer-datetime-formatter (if blamer-prettify-time-p
                                            (blamer--prettify-time date time)
                                          (concat date " " (blamer--truncate-time time))))
    ""))

(defun blamer--format-author (author)
  "Format AUTHOR name/you."
  (if (and author blamer-author-formatter)
      (format blamer-author-formatter (string-trim author))
    ""))

(defun blamer--format-commit-message (commit-message)
  "Format COMMIT-MESSAGE."
  (if (and blamer-commit-formatter commit-message)
      (format blamer-commit-formatter commit-message)
    ""))

(defun blamer--format-entire-message (msg)
  "Final format for entire built MSG."
  (if blamer-entire-formatter
      (format blamer-entire-formatter msg)
    msg))

(defun blamer--format-commit-info (commit-hash
                                   commit-message
                                   author
                                   date
                                   time
                                   &optional
                                   offset
                                   commit-info)
  "Format commit info into display message.
COMMIT-HASH - hash of current commit.
COMMIT-MESSAGE - message of current commit, can be null
AUTHOR - name of commiter
DATE - date in format YYYY-DD-MM
TIME - time in format HH:MM:SS
OFFSET - additional offset for commit message
COMMIT-INFO - all the commit information, for `blamer--apply-bindings'"
  (ignore commit-hash)

  (let* ((uncommitted (string= author "Not Committed Yet"))
         (author (if uncommitted blamer-self-author-name author))
         (commit-message (if uncommitted blamer-uncommitted-changes-message commit-message))
         (formatted-message (blamer--format-entire-message (concat (blamer--format-author author)
                                                                   (if (not uncommitted) (blamer--format-datetime date time) "")
                                                                   (blamer--format-commit-message commit-message))))

         (formatted-message (propertize formatted-message
                                        'face (flatten-tree
                                               (a-merge (face-all-attributes 'blamer-face (selected-frame))
                                                        `((:background ,(blamer--get-background-color)))))
                                        'cursor t))
         (formatted-message (blamer--apply-tooltip formatted-message commit-info))
         (formatted-message (blamer--apply-bindings formatted-message commit-info))
         (additional-offset (if blamer-offset-per-symbol
                                (/ (string-width formatted-message) blamer-offset-per-symbol) 0))
         ;; NOTE https://github.com/Artawower/blamer.el/issues/8
         (line-number-offset (if (bound-and-true-p display-line-numbers-mode)
                                 (+ (or (ignore-errors (line-number-display-width)) 0) 2)
                               0))
         (offset (cond ((eq blamer-view 'overlay-right) (- (window-width)
                                                           (string-width formatted-message)
                                                           (string-width (thing-at-point 'line))
                                                           line-number-offset))
                       (offset offset)
                       (t 0)))
         (offset (+ additional-offset offset))
         (offset (if (> offset 0) offset 0))
         (alignment-text (propertize (make-string (+ additional-offset offset) ?\s)
                                     'face `(:background ,(blamer--get-background-color))
                                     'cursor t)))

    (concat alignment-text formatted-message)))

(defun blamer--get-commit-message (hash)
  "Get commit message by provided HASH.
Return nil if error."
  (let* ((git-commit-res (shell-command-to-string (format blamer--git-commit-message hash)))
         (has-error (or (blamer--git-cmd-error-p git-commit-res) (eq (length git-commit-res) 0)))
         commit-message)
    (unless has-error
      (string-match blamer--commit-message-regexp git-commit-res)
      (setq commit-message (match-string 1 git-commit-res))
      (setq commit-message (replace-regexp-in-string "\n" " " commit-message))
      (setq commit-message (string-trim commit-message))
      (list (truncate-string-to-width commit-message blamer-max-commit-message-length nil nil "...") commit-message))))

(defun blamer--parse-line-info (blame-msg)
  "Parse BLAME-MSG and create a plist."
  (string-match blamer--regexp-info blame-msg)
  (let* ((commit-hash (match-string 1 blame-msg))
         (raw-commit-author (match-string 2 blame-msg))
         (commit-author (if (and (string= raw-commit-author blamer--current-author) blamer-self-author-name)
                            blamer-self-author-name
                          raw-commit-author))
         (commit-date (match-string 3 blame-msg))
         (commit-time (match-string 4 blame-msg))
         (commit-messages (blamer--get-commit-message commit-hash))
         (commit-message (when blamer-commit-formatter (nth 0 commit-messages)))
         (raw-commit-message (nth 1 commit-messages))
         (parsed-commit-info `(:commit-hash ,commit-hash
                               :commit-author ,commit-author
                               :commit-date ,commit-date
                               :commit-time ,commit-time
                               :raw-commit-author ,raw-commit-author
                               :commit-message ,commit-message
                               :raw-commit-message ,raw-commit-message)))

    parsed-commit-info))

(defun blamer--create-popup-msg (commit-info)
  "Handle current COMMIT-INFO."
  (let* ((offset (max (- (or blamer-min-offset 0) (length (thing-at-point 'line))) 0))
         (commit-author (plist-get commit-info :commit-author))
         (popup-message (blamer--format-commit-info (plist-get commit-info :commit-hash)
                                                    (plist-get commit-info :commit-message)
                                                    commit-author
                                                    (plist-get commit-info :commit-date)
                                                    (plist-get commit-info :commit-time)
                                                    offset
                                                    commit-info)))

    (when (and commit-author (not (eq commit-author "")))
      popup-message)))

(defun blamer--get-background-color ()
  "Return color of background under current cursor position."
  (move-end-of-line nil)
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))

    (cond ((not blamer-smart-background-p) (face-attribute 'blamer-face :background))
          ((region-active-p) (face-attribute 'region :background))
          ((bound-and-true-p hl-line-mode) (face-attribute 'hl-line :background))
          ((not face) 'unspecified)
          (t (face-attribute (or (car-safe face) face) :background)))))

(defun blamer--get-local-name (filename)
  "Return local FILENAME if path is in the tramp format."
  (if (file-remote-p default-directory)
      (tramp-file-name-localname
       (tramp-dissect-file-name filename))
    filename))

(defun blamer--apply-tooltip(text commit-info)
  "Compute the toolip from `blamer-tooltip-function' and COMMIT-INFO."
  (let ((help-echo (if (and blamer-tooltip-function (functionp blamer-tooltip-function))
                          (funcall blamer-tooltip-function commit-info)
                        nil)))
    (if help-echo
        (propertize text 'help-echo help-echo 'pointer 'hand)
      text)))

(defun blamer--apply-bindings (text commit-info)
  "Apply defined bindings to TEXT and pass COMMIT-INFO to callback."
  (let ((map (make-sparse-keymap)))
    (dolist (mapbind blamer-bindings)
      (define-key map (kbd (car mapbind))
        (lambda ()
          (interactive)
          (funcall (cdr mapbind) commit-info))))
    (setq text (propertize text 'keymap map)))
  text)

(defun blamer--render ()
  "Render text about current line commit status."
  (with-current-buffer (window-buffer (get-buffer-window))
    (save-restriction
      (widen)
      (let* ((end-line-number (if (region-active-p)
                                  (save-excursion
                                    (goto-char (region-end))
                                    (line-number-at-pos))
                                (line-number-at-pos)))
             (start-line-number (if (region-active-p)
                                    (save-excursion
                                      (goto-char (region-beginning))
                                      (line-number-at-pos))
                                  (line-number-at-pos)))
             (file-name (blamer--get-local-name (buffer-file-name)))
             (cmd (format blamer--git-blame-cmd start-line-number end-line-number file-name))
             (blame-cmd-res (shell-command-to-string cmd))
             (blame-cmd-res (butlast (split-string blame-cmd-res "\n"))))

        (blamer--clear-overlay)

        (save-excursion
          (when (region-active-p)
            (goto-char (region-beginning)))

          (dolist (cmd-msg blame-cmd-res)
            (unless (blamer--git-cmd-error-p cmd-msg)
              (let* ((commit-info (blamer--parse-line-info cmd-msg))
                     (popup-msg (blamer--create-popup-msg commit-info))
                     (ov (progn
                           (move-end-of-line nil)
                           (make-overlay (point) (point) nil t t))))
                (when popup-msg
                  (overlay-put ov 'after-string popup-msg)
                  (overlay-put ov 'intangible t)
                  (overlay-put ov 'window (get-buffer-window))
                  (add-to-list 'blamer--overlays ov)
                  (forward-line))))))))))

(defun blamer--render-commit-info-with-delay ()
  "Render commit info with delay."
  (when blamer-idle-timer
    (cancel-timer blamer-idle-timer))

  (setq blamer-idle-timer
        (run-with-idle-timer (or blamer-idle-time 0) nil 'blamer--render)))

(defun blamer--preserve-state ()
  "Preserve current editor state for next iteration."
  (setq blamer--previous-line-number (line-number-at-pos))
  (setq blamer--previous-window-width (window-width))
  (setq blamer--previous-line-length (length (thing-at-point 'line)))
  (setq blamer--previous-region-active-p (region-active-p)))

(defun blamer--try-render ()
  "Render current line if is .git exist."
  (let* ((long-line-p (and (region-active-p)
                           (> (count-lines (region-beginning) (region-end)) blamer-max-lines)))
         (region-deselected-p (and blamer--previous-region-active-p (not (region-active-p))))
         (clear-overlays-p (or long-line-p region-deselected-p)))

    (when clear-overlays-p
      (blamer--clear-overlay))

    (when (and (not long-line-p)
               (or (eq blamer-type 'both)
                   (and (eq blamer-type 'visual) (not (use-region-p)))
                   (and (eq blamer-type 'selected) (use-region-p)))
               (or (not blamer--previous-line-number)
                   (not (eq blamer--previous-window-width (window-width)))
                   (not (eq blamer--previous-line-number (line-number-at-pos)))
                   (not (eq blamer--previous-line-length (length (thing-at-point 'line))))))

      (blamer--clear-overlay)
      (blamer--preserve-state)
      (blamer--render-commit-info-with-delay))))

(defun blamer--reset-state ()
  "Reset all state after blamer mode is disabled."
  (if blamer-idle-timer
      (cancel-timer blamer-idle-timer))

  (blamer--clear-overlay)
  (setq blamer-idle-timer nil)
  (setq blamer--previous-line-number nil)
  (setq blamer--previous-window-width nil)
  (remove-hook 'post-command-hook #'blamer--try-render t)
  (remove-hook 'window-state-change-hook #'blamer--try-render t)
  (when (bound-and-true-p company-mode)
            (advice-remove 'company-pseudo-tooltip-show-at-point #'blamer--clear-overlay)))

;;;###autoload
(define-minor-mode blamer-mode
  "Blamer mode.
Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When `blamer-mode' is enabled, the popup message with commit info
will appear after BLAMER-IDLE-TIME. It works only inside git repo"
  :init-value nil
  :global nil
  :lighter nil
  :group 'blamer
  (unless (or blamer-author-formatter
              blamer-commit-formatter
              blamer-datetime-formatter)
    (message "Your have to provide at least one formatter for blamer.el"))

  (let ((is-git-repo (blamer--git-exist-p)))
    (when (and (not blamer--current-author)
               blamer-author-formatter
               is-git-repo)
      (setq-local blamer--current-author (replace-regexp-in-string "\n\\'" "" (shell-command-to-string blamer--git-author-cmd))))
    (if (and blamer-mode (buffer-file-name) is-git-repo)
        (progn
          (add-hook 'post-command-hook #'blamer--try-render nil t)
          (add-hook 'window-state-change-hook #'blamer--try-render nil t)
          (when (bound-and-true-p company-mode)
            (advice-add 'company-pseudo-tooltip-show-at-point :before #'blamer--clear-overlay)))
      (blamer--reset-state))))

;;;###autoload
(define-globalized-minor-mode
  global-blamer-mode
  blamer-mode
  (lambda ()
    (unless blamer-mode
      (blamer-mode)))
  :group 'blamer)

(provide 'blamer)
;;; blamer.el ends here
