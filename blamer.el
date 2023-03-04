;;; blamer.el --- Show git blame info about current line           -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Artur Yaroshenko

;; Author: Artur Yaroshenko <artawower@protonmail.com>
;; URL: https://github.com/artawower/blamer.el
;; Package-Requires: ((emacs "27.1") (posframe "1.1.7"))
;; Version: 0.6.1

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
;; about several selected lines.  Works with git only.

;;; Code:

(require 'files)
(require 'simple)
(require 'time-date)
(require 'tramp)
(require 'posframe)
(require 'url)
(require 'vc-git)
(require 'seq)

(eval-when-compile
  (require 'subr-x))

(defconst blamer--regexp-info
  (concat "^(?\\(?1:[^ ]*\\).*[[:blank:]]?\(\\(?2:[^\n]+\\)"
          "\s\\(?3:[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)"
          "\s\\(?4:[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)")

  "Regexp for extract data from blame message.
1 - commit hash
2 - author
3 - date
3 - time")

(defconst blamer--commit-message-regexp "\n\n[\s]+\\(?1:[^\']+\\):?"
  "Regexp for commit message parsing.")

(defconst blamer--git-author-cmd '("config" "--get" "user.name")
  "Command for getting current git user name via `vc-git'.")

(defconst blamer--git-repo-cmd '("rev-parse" "--is-inside-work-tree")
  "Command for detect git repo.")

(defconst blamer--git-blame-cmd '("blame"  "-L")
  "Command for get blame of current line.")

(defconst blamer--git-commit-message '("log"  "-n1")
  "Command for get commit message.")

(defconst blamer--current-time-text "Now"
  "Text for lines that were created right now.")

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
  "Its a crutch :< for font less then original editor font.  Use carefully.
Will add additional space for each BLAMER-OFFSET-PER-SYMBOL"
  :group 'blamer
  :type 'integer)

(defcustom blamer-type 'both
  "Type of blamer.
\\=visual - show blame only for current line
\\=selected - show blame only for selected line
\\=both - both of them

This types are used only for single line blame.
\\=overlay-popup - show commit info inside pretty overlay
\\=posframe-popup - show commit info inside pretty posframe window"
  :group 'blamer
  :type '(choice (const :tag "Visual only" visual)
                 (const :tag "Pretty overlay popup" overlay-popup)
                 (const :tag "Pretty posframe popup" posframe-popup)
                 (const :tag "Visual and selected" both)
                 (const :tag "Selected only" selected)))

(defcustom blamer--overlay-popup-position 'bottom
  "Position of rendered popup.
Could be `bottom', `top' and `smart' position.
When smart position is enabled blamer will try to calculate better
place to paste popup."
  :group 'blamer
  :type '(choice (const :tag "Top" top)
                 (const :tag "Bottom" bottom)
                 (const :tag "Smart detection" smart)))

(defcustom blamer-self-author-name (if (string-empty-p user-full-name) "You"
                                     user-full-name)
  "Message for commits where you are author."
  :group 'blamer
  :type 'string)

(defcustom blamer-max-lines 30
  "Maximum blamed lines."
  :group 'blamer
  :type 'integer)

(defcustom blamer-border-lines '(?╭ ?─ ?╮ ?│ ?╯ ?╰ )
  "List of lines and borders.  When value is nil, borders will not preset.
Be careful! This config highly coupled with your current face!

Alternative preset: \\=(?┌ ?─ ?┐ ?│ ?┘ ?└)"
  :group 'blamer
  :type 'alist)

(defcustom blamer-max-commit-message-length 30
  "Max length of commit message.
Commit message with more characters will be truncated with ellipsis at the end.
Also, when `blamer-type' is overlay-popup this value is used for max tooltip
length."
  :group 'blamer
  :type 'integer)

(defcustom blamer-uncommitted-changes-message "Uncommitted changes"
  "Message for uncommitted lines."
  :group 'blamer
  :type 'string)

(defcustom blamer-view 'overlay
  "View for commit message.  Can be \\=overlay and \\=overlay-right."
  :group 'blamer
  :type '(choice (const :tag "Overlay right" overlay-right)
                 (const :tag "Overlay" overlay)))

(defcustom blamer-smart-background-p t
  "When enable blamer will try to find background.
If not will use background from `blamer-face'"
  :group 'blamer
  :type 'boolean)

(defcustom blamer-force-truncate-long-line t
  "When `truncate-lines' mode is disabled you can force truncate long commit lines."
  :group 'blamer
  :type 'boolean)

(defcustom blamer-show-avatar-p t
  "Show avatar in popup.
This feature required Emacs built with `imagemagick'"
  :group 'blamer
  :type 'boolean)

(defcustom blamer-avatar-size 96
  "Size of avatar."
  :group 'blamer
  :type 'int)

(defcustom blamer-avatar-ratio '(3 . 3)
  "Image ration for avatar."
  :group 'blamer
  :type 'cons)

(defcustom blamer-avatar-cache-time 604800
  "Time in seconds to cache avatar.  Default value is 1 week."
  :group 'blamer
  :type 'int)

(defcustom blamer-avatar-folder "~/.blamer/avatars/"
  "Folder for avatars."
  :group 'blamer
  :type 'string)

(defcustom blamer-avatar-regexps-uploaders-alist
  '(("github" . ("https://api.github.com/search/users" blamer--github-avatar-uploader))
    ("gitlab" . ("https://gitlab.com" blamer--gitlab-avatar-uploader)))
  "List of regexps for uploaders.
Each element is a cons cell (REGEXP . FUNCTION)."
  :group 'blamer
  :type 'list)

(defcustom blamer-fallback-avatar-config
  '("http://www.gravatar.com/avatar" blamer--fallback-avatar-uploader)
  "Fallback avatar config."
  :group 'blamer
  :type 'list)


(defcustom blamer-posframe-configurations
  '(:left-fringe 16
                 :right-fringe 16
                 :y-pixel-offset 20
                 :x-pixel-offset -20
                 :internal-border-width 1
                 :internal-border-color "#61AFEF"
                 :lines-truncate t
                 :accept-focus nil)
  "Plist of configurations for `posframe-show' method."
  :group 'blamer
  :type 'plist)

(defface blamer-face
  '((t :foreground "#7a88cf"
       :background unspecified
       :italic t))
  "Face for blamer info."
  :group 'blamer)

(defface blamer-pretty-border-face
  `((t :inherit font-lock-variable-name-face
       :italic nil
       :background unspecified
       :height ,(face-attribute 'default :height)
       :font ,(face-attribute 'default :font)))
  "Face for pretty blamer borders."
  :group 'blamer)

(defface blamer-pretty-commit-message-face
  `((t :inherit font-lock-string-face
       :italic nil
       :height ,(face-attribute 'default :height)
       :font ,(face-attribute 'default :font)))
  "Face for pretty commit messages."
  :group 'blamer)

(defface blamer-pretty-meta-keywords-face
  `((t :inherit font-lock-function-name-face
       :italic nil
       :height ,(face-attribute 'default :height)
       :font ,(face-attribute 'default :font)))
  "Face for pretty keywords."
  :group 'blamer)

(defface blamer-pretty-meta-data-face
  `((t :inherit font-lock-variable-name-face
       :italic nil
       :height ,(face-attribute 'default :height)
       :font ,(face-attribute 'default :font)))

  "Face for pretty meta information."
  :group 'blamer)

(defcustom blamer-bindings nil
  "List of bindings.

Callback function applying plist argument:
:commit-hash - hash of clicked commit
:commit-author - author name after formatting
:raw-commit-author - raw author username if exist.
:commit-date - date of commit. (string field)
:commit-time - commit time. (string field)
:commit-message - message of commit.  If not exist will be get from
`blamer-uncommitted-changes-message' variable.
:raw-commit-message - Full message of commit.

For example you can pass such bindings for message
author name by left click and copying commit hash by right click.
\(without backslash)

\\=((<mapvar> . (lambda (commit-info) (message (plist-get :commit-author))))
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
  "Current line number length for detect render function.")

(defvar blamer--previous-point nil
  "Last preserved blamer point.")

(defvar blamer--previous-region-active-p nil
  "Was previous state is active region?")

(defvar blamer--overlays '()
  "Current active overlays for git blame messages.")

(defvar blamer--block-render-p nil
  "Lock rendering, useful for external packages.")

(defvar blamer--buffer-name "*blamer*"
  "Name of buffer for git blame messages.")

(defvar-local blamer--current-author nil
  "Git.name for current repository.")

(defun blamer-tooltip-commit-message (commit-info)
  "This function can be use as `blamer-tooltip-function'.
Will show the commit message from COMMIT-INFO."
  (plist-get commit-info :raw-commit-message))

(defun blamer-tooltip-author-info (commit-info)
  "This function can be use as `blamer-tooltip-function'.
Will show the author from COMMIT-INFO."
  (format "%s (%s)" (plist-get commit-info :commit-author) (plist-get commit-info :raw-commit-author)))

(defun blamer-tooltip-keybindings (_commit-info)
  "This function can be use as `blamer-tooltip-function'.
Will show the available `blamer-bindings'."
  (if (> (length blamer-bindings) 0)
      (mapconcat (lambda (bind) (format "%s - %s" (car bind) (cdr bind))) blamer-bindings "\n")
    nil))

(defun blamer--git-exist-p ()
  "Return t if .git exist."
  (when-let* ((file-name (blamer--get-local-name (buffer-file-name))))
    (vc-backend file-name)))

(defun blamer--clear-overlay ()
  "Clear last overlay."
  (dolist (ov blamer--overlays)
    (delete-overlay ov))
  (setq blamer--overlays '()))

(defun blamer--git-cmd-error-p (cmd-res)
  "Return t if CMD-RES contain error."
  (or (not cmd-res) (string-match-p  "^fatal:" cmd-res)))

(defun blamer--truncate-time (time)
  "Remove seconds from TIME string."
  (string-join (butlast (split-string time ":")) ":"))

;; TODO: pass argument for pretty format to vc-git--run-command-string
(defun blamer--prettify-time (date time)
  "Prettify DATE and TIME for nice commit message."
  (let* ((seconds-ago (float-time (time-since (concat date "T" time))))
         (minutes-ago (if (eq seconds-ago 0) 0 (floor (/ seconds-ago 60))))
         (hours-ago (if (eq minutes-ago 0) 0 (floor (/ minutes-ago 60))))
         (days-ago (if (eq hours-ago 0) 0 (floor (/ hours-ago 24))))
         (weeks-ago (if (eq days-ago 0) 0 (floor (/ days-ago 7))))
         (months-ago (if (eq days-ago 0) 0 (floor (/ days-ago 30))))
         (years-ago (if (eq months-ago 0) 0 (floor (/ months-ago 12)))))

    (cond ((<= seconds-ago 60) blamer--current-time-text)
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

(defun blamer--get-line-number-column-width ()
  "Return char size of left line numbers when line numbers enabled."
  (if (bound-and-true-p display-line-numbers-mode)
      (+ (or (ignore-errors (round (line-number-display-width 'columns))) 0) 0)
    0))

(defun blamer--plist-merge (&rest plists)
  "Create a single property list from all PLISTS."
  (let ((rtn (pop plists)))
    (dolist (plist plists rtn)
      (setq rtn (plist-put rtn
                           (pop plist)
                           (pop plist))))))

(defun blamer--real-window-width ()
  "Get real visible width of the window.
Taking into account the line number column."
  ;; NOTE https://github.com/Artawower/blamer.el/issues/8
  (let ((line-number-offset (blamer--get-line-number-column-width)))
    (- (window-width) line-number-offset)))

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
AUTHOR - name of committer
DATE - date in format YYYY-DD-MM
TIME - time in format HH:MM:SS
OFFSET - additional offset for commit message
COMMIT-INFO - all the commit information, for `blamer--apply-bindings'"
  (ignore commit-hash)

  (let* ((uncommitted (string= author "Not Committed Yet"))
         (author (if uncommitted blamer-self-author-name author))
         (commit-message (if uncommitted blamer-uncommitted-changes-message commit-message))
         (formatted-message (blamer--format-entire-message (concat (blamer--format-author author)
                                                                   (blamer--format-datetime date time)
                                                                   (blamer--format-commit-message commit-message))))

         (formatted-message (propertize formatted-message
                                        'face (blamer--plist-merge
                                               (flatten-tree (face-all-attributes 'blamer-face (selected-frame)))
                                               `(:background ,(blamer--get-background-color)))
                                        'cursor t))
         (formatted-message (blamer--apply-tooltip formatted-message commit-info))
         (formatted-message (blamer--apply-bindings formatted-message commit-info))
         (additional-offset (if blamer-offset-per-symbol
                                (/ (string-width formatted-message) blamer-offset-per-symbol) 0))

         (offset (cond ((eq blamer-view 'overlay-right) (- (blamer--real-window-width)
                                                           (string-width formatted-message)
                                                           (string-width (thing-at-point 'line))))
                       (offset offset)
                       (t 0)))
         (offset (+ additional-offset offset))
         (offset (if (> offset 0) offset 0))
         (alignment-text (propertize (make-string (+ additional-offset offset) ?\s)
                                     'face `(:background ,(blamer--get-background-color))
                                     'cursor t)))

    (concat alignment-text formatted-message)))

(defun blamer--get-commit-messages (hash)
  "Get commit message by provided HASH.
Return nil if error."
  (let* ((git-commit-res (apply #'vc-git--run-command-string nil (append blamer--git-commit-message (list hash))))
         (has-error (or (blamer--git-cmd-error-p git-commit-res) (eq (length git-commit-res) 0)))
         commit-message)
    (unless has-error
      (string-match blamer--commit-message-regexp git-commit-res)
      (setq commit-message (match-string 1 git-commit-res))
      (split-string commit-message "\n"))))

(defun blamer--parse-line-info (blame-msg &optional include-avatar-p)
  "Parse BLAME-MSG and create a plist.

When INCLUDE-AVATAR-P provided and non-nil,
avatar will be downloaded and included in the plist."
  (string-match blamer--regexp-info blame-msg)
  (let* ((commit-hash (string-trim (match-string 1 blame-msg)))
         (raw-commit-author (match-string 2 blame-msg))
         (uncommitted (string= raw-commit-author "Not Committed Yet"))
         (commit-author (if (and (string= raw-commit-author blamer--current-author) blamer-self-author-name)
                            blamer-self-author-name
                          raw-commit-author))
         (commit-date (match-string 3 blame-msg))
         (commit-time (match-string 4 blame-msg))
         (commit-messages (blamer--get-commit-messages commit-hash))
         (author-email (vc-git--run-command-string nil "log" "-n1" "--pretty=format:%ae" commit-hash))
         (commit-message (car commit-messages))
         (commit-message (when commit-message (string-trim commit-message)))
         (commit-message (when (and blamer-commit-formatter commit-message)
                           (truncate-string-to-width
                            commit-message
                            blamer-max-commit-message-length nil nil "...")))
         (commit-description (cdr commit-messages))
         (raw-commit-message (nth 1 commit-messages))
         (avatar (when (and
                        blamer-show-avatar-p
                        include-avatar-p
                        (not uncommitted)
                        (display-graphic-p))
                   (blamer--get-avatar author-email)))
         (parsed-commit-info `(:commit-hash ,commit-hash
                                            :author-email ,author-email
                                            :commit-author ,commit-author
                                            :commit-date ,commit-date
                                            :commit-time ,commit-time
                                            :commit-avatar ,avatar
                                            :raw-commit-author ,raw-commit-author
                                            :commit-message ,commit-message
                                            :commit-description ,commit-description
                                            :raw-commit-message ,raw-commit-message)))

    parsed-commit-info))

(defun blamer--github-avatar-uploader (remote-url file-path author-email)
  "Download the author avatar from REMOTE-URL using the AUTHOR-EMAIL to FILE-PATH."
  (let* ((url (concat remote-url "?q=" author-email))
         (response (url-retrieve-synchronously url))
         (response-data (with-current-buffer response
                          (goto-char (point-min))
                          (search-forward "\n\n")
                          (json-read)))
         (items (cdr (assoc 'items response-data)))
         (first-item (and (> (length items) 0) (elt items 0)))
         (avatar-url (and first-item (cdr (assoc 'avatar_url first-item)))))

    (if avatar-url
        (blamer--upload-avatar avatar-url file-path)
      (funcall (cadr blamer-fallback-avatar-config)
               (car blamer-fallback-avatar-config)
               file-path author-email))))


(defun blamer--gitlab-avatar-url (author-email)
  "Get the avatar URL for the email address AUTHOR-EMAIL on GitLab."
  (let ((url (format "https://gitlab.com/api/v4/avatar?email=%s" author-email)))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (let ((json-object-type 'hash-table))
        (let ((json-data (json-read)))
          (gethash "avatar_url" json-data))))))

(defun blamer--fallback-avatar-uploader (remote-url file-path author-email)
  "Fallback function for upload avatars and save it.
FILE-PATH - path to save avatar.
AUTHOR-EMAIL - email of author.
REMOTE-URL - url of resource to download avatar."
  (blamer--upload-avatar (format "%s/%s?d=identicon" remote-url (md5 author-email)) file-path))

(defun blamer--gitlab-avatar-uploader (remote-url file-path author-email)
  "Upload avatar from REMOTE-URL for gitlab using AUTHOR-EMAIL to FILE-PATH."
  (when-let ((url (format "%s/api/v4/avatar?email=%s" remote-url author-email))
             (response (url-retrieve-synchronously url))
             (json-string (with-current-buffer response
                            (buffer-substring (point) (point-max))))
             (json-object-type 'hash-table)
             (json-data (json-read-from-string json-string))
             (avatar-url (gethash "avatar_url" json-data)))
    (blamer--upload-avatar avatar-url file-path)))

(defun blamer--upload-avatar (url file-path)
  "Download the avatar from URL and save it to the path as FILE-PATH.
HOST-NAME is the name of the host for caching.
host folder.
If the file already exists, return the path to the existing file."
  (unless (file-exists-p (file-name-directory file-path))
    (make-directory (file-name-directory file-path) t))
  (url-copy-file url file-path t)
  file-path)


(defun blamer--find-avatar-uploader (remote-ref)
  "Find the avatar uploader function and REMOTE-REF that match the provided STRING.

The uploader functions and URLs are defined in
`blamer-avatar-regexps-uploaders-alist`."
  (let ((uploader-pair (assoc-default remote-ref blamer-avatar-regexps-uploaders-alist 'string-match)))
    (if uploader-pair
        uploader-pair
      blamer-fallback-avatar-config)))

(defun blamer--get-avatar (author-email)
  "Get avatar url for AUTHOR or AUTHOR-EMAIL.
Function return current path for avatar url.
Works only for github right now."
  (when-let* ((remote-ref (vc-git--run-command-string nil "ls-remote" "--get-url" "origin"))
              (uploader-fn-path (blamer--find-avatar-uploader remote-ref))
              (uploader-path (car uploader-fn-path))
              (uploader-fn (car (cdr uploader-fn-path)))
              (host-name (when (string-match "https?://\\(?1:.*\\)/?" uploader-path)
                           (match-string 1 uploader-path)))
              (host-name (car (split-string host-name "/")))
              (folder (concat (file-name-as-directory blamer-avatar-folder)
                              (file-name-as-directory host-name)))
              (filename (format "%s.png" author-email))
              (file-path (concat folder filename)))

    (if (and (file-exists-p file-path)
             (not (blamer--cache-outdated-p file-path)))
        file-path
      (funcall uploader-fn uploader-path file-path author-email))))

(defun blamer--cache-outdated-p (file-path)
  "Check if the cache for FILE-PATH is outdated."
  (let* ((attributes (file-attributes file-path))
         (creation-time (nth 5 attributes))
         (current-time (current-time)))

    (> (time-to-seconds (time-subtract current-time creation-time))
       blamer-avatar-cache-time)))

(defun blamer--get-available-width-before-window-end ()
  "Return count of chars before window end."

  (- (blamer--real-window-width)
     (save-excursion
       (end-of-line)
       (- (point) (beginning-of-visual-line)))))

(defun blamer--maybe-normalize-truncated-line (text)
  "Disable line break for truncated line by truncated TEXT for available width."
  (if (and (not truncate-lines) blamer-force-truncate-long-line)
      (truncate-string-to-width text (- (blamer--get-available-width-before-window-end) (blamer--get-line-number-column-width)))
    text))

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
                                                    commit-info))
         (popup-message (blamer--maybe-normalize-truncated-line popup-message)))

    (when (and commit-author (not (string= commit-author "")))
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
  (if (and (file-remote-p default-directory) filename)
      (tramp-file-name-localname (tramp-dissect-file-name filename))
    filename))

(defun blamer--apply-tooltip(text commit-info)
  "Compute the tooltip from `blamer-tooltip-function', TEXT and COMMIT-INFO."
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

(defun blamer--prettify-border (border)
  "Apply font face for pretty-string BORDER."
  (propertize border 'face 'blamer-pretty-border-face))

(defun blamer--prettify-keyword (keyword)
  "Apply font face for KEYWORD of pretty popup."
  (propertize keyword 'face 'blamer-pretty-meta-keywords-face))

(defun blamer--prettify-meta-data (meta-data)
  "Apply font face for META-DATA of pretty popup."
  (propertize meta-data 'face 'blamer-pretty-meta-data-face))

(defun blamer--prettify-commit-message (commit-message)
  "Apply font face for COMMIT-MESSAGE of pretty popup."
  (propertize (string-trim commit-message) 'face 'blamer-pretty-commit-message-face))

(defun blamer--format-pretty-tooltip (msgs)
  "Draw a pretty tooltip from MSGS alist with respecting max possible length.
Return cons of result and count of lines."
  (let* ((enable-borders-p blamer-border-lines)
         (border-offset (if enable-borders-p 2 0))
         (left-right-padding 2)
         (render-length (- blamer-max-commit-message-length (+ border-offset left-right-padding)))
         (popup-center (/ render-length 2))
         (left-offset (if (eq blamer--overlay-popup-position 'smart)
                          (if (< (current-column) popup-center)
                              (current-column)
                            (- (current-column) popup-center))
                        (save-excursion
                          (back-to-indentation)
                          (current-column))))
         (left-offset-line (blamer--prettify-border (make-string left-offset ? )))
         (top-left-corner (when enable-borders-p (blamer--prettify-border (char-to-string (nth 0 blamer-border-lines)))))
         (border-horizontal-line (when enable-borders-p (blamer--prettify-border (make-string render-length (nth 1 blamer-border-lines)))))
         (top-right-corner (when enable-borders-p (blamer--prettify-border (char-to-string (nth 2 blamer-border-lines)))))
         (border-vertical-line (when enable-borders-p (blamer--prettify-border (char-to-string (nth 3 blamer-border-lines)))))
         (bottom-right-corner (when enable-borders-p (blamer--prettify-border (char-to-string (nth 4 blamer-border-lines)))))
         (bottom-left-corner (when enable-borders-p (blamer--prettify-border (char-to-string (nth 5 blamer-border-lines)))))
         (padding (blamer--prettify-border " "))
         (res (concat left-offset-line top-left-corner border-horizontal-line top-right-corner "\n"))
         splited-msgs)

    (dolist (m msgs)
      (setq splited-msgs (append splited-msgs (seq-partition m (- render-length 2)))))


    (dolist (m splited-msgs)
      (setq res (concat res
                        left-offset-line
                        border-vertical-line padding
                        m
                        (when (< (length m) render-length) (blamer--prettify-border (make-string (- render-length (length m) left-right-padding) ? )))
                        padding border-vertical-line "\n")))

    (setq res (concat res left-offset-line bottom-left-corner border-horizontal-line bottom-right-corner))
    (if (eq blamer--overlay-popup-position 'top)
        (setq res (concat res "\n"))
      (setq res (concat "\n" res)))

    res))

(defun blamer--create-popup-text-content (commit-info)
  "Create the content of the popup from COMMIT-INFO.
Return list of strings."

  (let* ((commit-hash (blamer--prettify-meta-data (plist-get commit-info :commit-hash)))
         (msg-list `(,(when commit-hash (concat (blamer--prettify-keyword "hash:   ")
                                                (blamer--prettify-meta-data commit-hash)))
                     ,(concat (blamer--prettify-keyword "author: ")
                              (blamer--prettify-meta-data (plist-get commit-info :commit-author)))
                     ,(concat (blamer--prettify-keyword "date:   ")
                              (blamer--prettify-meta-data
                               (concat (plist-get commit-info :commit-date) " " (plist-get commit-info :commit-time))))
                     ,(blamer--prettify-border " "))))
    msg-list))


(defun blamer--render-overlay-popup (commit-info)
  "Render COMMIT-INFO as pretty overlay with border."
  (let* ((msg-list (blamer--create-popup-text-content commit-info))
         (commit-descriptions (mapcar #'blamer--prettify-commit-message (plist-get commit-info :commit-description)))
         (commit-message (blamer--prettify-commit-message (or (plist-get commit-info :commit-message) "")))
         (msg-lines (append msg-list (list commit-message) commit-descriptions))
         (msg (blamer--format-pretty-tooltip msg-lines))
         (beg (save-excursion (beginning-of-line) (point)))
         (ov (save-excursion (move-end-of-line nil)
                             (make-overlay beg (point) nil t t))))

    (if (eq blamer--overlay-popup-position 'top)
        (overlay-put ov 'before-string msg)
      (overlay-put ov 'after-string msg))

    (overlay-put ov 'intangible t)
    (overlay-put ov 'window (get-buffer-window))
    (add-to-list 'blamer--overlays ov)))

(defun blamer--render-right-overlay (commit-info)
  "Render COMMIT-INFO as overlay at right position."
  (when-let ((ov (progn (move-end-of-line nil)
                        (make-overlay (point) (point) nil t t)))
             (popup-msg (blamer--create-popup-msg commit-info)))

    (overlay-put ov 'after-string popup-msg)
    (overlay-put ov 'intangible t)
    (overlay-put ov 'window (get-buffer-window))
    (add-to-list 'blamer--overlays ov)))


(defun blamer--build-pretty-content (commit-info)
  "Build pretty content inside separated buffer from COMMIT-INFO."
  (let* ((msg-list (blamer--create-popup-text-content commit-info))
         (commit-avatar (plist-get commit-info :commit-avatar))
         (insert-avatar-p (and blamer-show-avatar-p
                               (image-type-available-p 'imagemagick)
                               commit-avatar))
         (commit-message (or (plist-get commit-info :commit-message) ""))
         ;; TODO: find better way to calculate image size for pretty paddings between commit info
         ;; (hw (* (frame-char-width) blamer-avatar-size))
         (commit-info-prefix (if insert-avatar-p "  " "")))
    (save-excursion
      (with-current-buffer (get-buffer-create blamer--buffer-name)
        (erase-buffer)
        (insert "\n")
        (when (and (image-type-available-p 'imagemagick) insert-avatar-p)
          (insert-sliced-image
           (create-image commit-avatar 'imagemagick nil :height blamer-avatar-size :width blamer-avatar-size)
           nil nil (car blamer-avatar-ratio) (cdr blamer-avatar-ratio)))
        (goto-char (point-min))
        (forward-line 1)

        (dolist (m msg-list)
          (end-of-line)
          (insert (concat commit-info-prefix m))
          (unless insert-avatar-p (insert "\n"))
          (forward-line 1))
        (when (and commit-message (not (string= commit-message "")))
          (when insert-avatar-p (insert "\n"))
          (insert (blamer--prettify-commit-message (or (plist-get commit-info :commit-message) "")))
          (insert "\n"))

        (insert (string-join (mapcar #'blamer--prettify-commit-message (plist-get commit-info :commit-description)) "\n"))
        (insert "\n")))
    (with-current-buffer (get-buffer-create blamer--buffer-name) (buffer-string))))

(defun blamer--render-posframe-popup (commit-info)
  "Render COMMIT-INFO as posframe popup."
  (when (and (fboundp 'posframe-workable-p) (posframe-workable-p))
    (let ((pretty-content (blamer--build-pretty-content commit-info)))
      (apply 'posframe-show
             (append `(,blamer--buffer-name)
                     (blamer--plist-merge
                      blamer-posframe-configurations
                      `(:string , pretty-content)))))))

(defun blamer--render-line-overlay (commit-info &optional type)
  "Render COMMIT-INFO overlay by optional TYPE.
when not provided `blamer-type' will be used."
  (cond ((eq (or type blamer-type) 'overlay-popup) (blamer--render-overlay-popup commit-info))
        ((eq (or type blamer-type) 'posframe-popup) (blamer--render-posframe-popup commit-info))
        (t (blamer--render-right-overlay commit-info))))

(defun blamer--render (&optional type)
  "Render text about current line commit status.
TYPE - is optional argument that can replace global `blamer-type' variable."

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
             (file-name (when file-name (replace-regexp-in-string " " "\\\\\  " file-name)))
             (include-avatar-p (member type '(posframe-popup overlay-popup)))
             (blame-cmd-res (when file-name
                              (apply #'vc-git--run-command-string file-name
                                     (append blamer--git-blame-cmd
                                             (list (format "%s,%s" start-line-number end-line-number))))))
             (blame-cmd-res (when blame-cmd-res (butlast (split-string blame-cmd-res "\n")))))

        (blamer--clear-overlay)

        (save-excursion
          (when (region-active-p)
            (goto-char (region-beginning)))

          (dolist (cmd-msg blame-cmd-res)
            (unless (blamer--git-cmd-error-p cmd-msg)
              (let ((commit-info (blamer--parse-line-info cmd-msg include-avatar-p)))
                (blamer--render-line-overlay commit-info type)
                (forward-line)))))))))

(defun blamer--safety-render (&optional type)
  "Function for checking current active blamer type before rendering with delay.
Optional TYPE argument will override global `blamer-type'."
  (let ((blamer-type (or type blamer-type)))
    (unless (or (and (or (eq blamer-type 'overlay-popup)
                         (eq blamer-type 'visual))
                     (use-region-p))
                blamer--block-render-p)
      (blamer--render blamer-type))))

(defun blamer--render-commit-info-with-delay ()
  "Render commit info with delay."
  (when blamer-idle-timer
    (cancel-timer blamer-idle-timer))

  (setq blamer-idle-timer
        (run-with-idle-timer (or blamer-idle-time 0) nil 'blamer--safety-render)))

(defun blamer--preserve-state ()
  "Preserve current editor state for next iteration."
  (setq blamer--previous-line-number (line-number-at-pos))
  (setq blamer--previous-window-width (blamer--real-window-width))
  (setq blamer--previous-line-length (length (thing-at-point 'line)))
  (setq blamer--previous-point (point))
  (setq blamer--previous-region-active-p (region-active-p)))

(defun blamer--try-render (&optional local-type)
  "Render current line if is .git exist.
LOCAL-TYPE is force replacement of current `blamer-type' for handle rendering."

  (let* ((long-line-p (and (region-active-p)
                           (> (count-lines (region-beginning) (region-end)) blamer-max-lines)))
         (region-deselected-p (and blamer--previous-region-active-p (not (region-active-p))))
         (clear-overlays-p (or long-line-p region-deselected-p))
         (type (or local-type blamer-type)))

    (when (and clear-overlays-p (not blamer--block-render-p))
      (blamer--clear-overlay))

    (when (and (not long-line-p)
               (not blamer--block-render-p)
               (or (eq type 'both)
                   (and (eq type 'visual) (not (use-region-p)))
                   (and (eq type 'overlay-popup) (not (use-region-p)))
                   (and (eq type 'selected) (use-region-p)))
               (or (not blamer--previous-line-number)
                   (not (eq blamer--previous-window-width (blamer--real-window-width)))
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
  (setq blamer--previous-point nil)
  (when (not (eq nil (get-buffer blamer--buffer-name)))
    (posframe-hide blamer--buffer-name))
  (remove-hook 'post-command-hook #'blamer--try-render t)
  (remove-hook 'window-state-change-hook #'blamer--try-render t))

;;;###autoload
(define-minor-mode blamer-mode
  "Blamer mode.
Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When `blamer-mode' is enabled, the popup message with commit info
will appear after BLAMER-IDLE-TIME.  It works only inside git repo"
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
      (setq-local blamer--current-author (replace-regexp-in-string "\n\\'" "" (apply #'vc-git--run-command-string nil blamer--git-author-cmd))))
    (when (and (buffer-file-name) is-git-repo)
      (if blamer-mode
          (progn
            (add-hook 'post-command-hook #'blamer--try-render nil t)
            (add-hook 'window-state-change-hook #'blamer--try-render nil t))
        (blamer--reset-state)))))

;;;###autoload
(define-globalized-minor-mode
  global-blamer-mode
  blamer-mode
  (lambda ()
    (unless blamer-mode
      (blamer-mode)))
  :group 'blamer)

(defun blamer--reset-state-once ()
  "Reset all blamer side-effect like overlay/timers only once."
  (when (or (not blamer--previous-point)
            (not (eq blamer--previous-point (point)))
            (not (eq blamer--previous-window-width (blamer--real-window-width)))
            (not (eq blamer--previous-line-length (length (thing-at-point 'line)))))
    (blamer--reset-state)
    (remove-hook 'post-command-hook #'blamer--reset-state-once t)
    (remove-hook 'window-state-change-hook #'blamer--reset-state-once t)
    (when blamer-mode
      (add-hook 'post-command-hook #'blamer--try-render nil t)
      (add-hook 'window-state-change-hook #'blamer--try-render nil t))))

;;;###autoload
(defun blamer-show-commit-info (&optional type)
  "Show commit info from git blame.

TYPE - optional parameter, by default will use `overlay-popup'."
  (interactive)
  (when (blamer--git-exist-p)
    (blamer--reset-state)
    (blamer--render (or type 'overlay-popup))
    (blamer--preserve-state)
    (add-hook 'post-command-hook #'blamer--reset-state-once nil t)))

;;;###autoload
(defun blamer-show-posframe-commit-info ()
  "Show commit info from git blame using posframe."
  (interactive)
  (blamer-show-commit-info 'posframe-popup))

(provide 'blamer)
;;; blamer.el ends here
