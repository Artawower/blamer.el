;;; blamer.el --- Show git blame info about current line           -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Artur Yaroshenko

;; Author: Artur Yaroshenko <artawower@protonmail.com>
;; URL: https://github.com/artawower/blamer.el
;; Package-Requires: ((emacs "24.4"))
;; Version: 0.1

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
;; Package for showing current line commit info with idle.
;; Works with git only.

;;; Code:

(require 'subr-x)
(require 'simple)

(defconst blamer--regexp-info
  (concat "\\(?1:^[a-z0-9]+\\) [^\s]*[[:blank:]]?\(\\(?2:[^\n]+\\)"
          "\s\\(?3:[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)"
          "\s\\(?4:[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)")

  "Regexp for extract data from blame message.
1 - commit hash
2 - author
3 - date
3 - time")

(defconst blamer--commit-message-regexp "\n\n[\s]+\\(?1:[^.]+\\):?"
  "Regexp for commit message parsing.")

(defconst blamer--git-author-cmd "git config --get user.name"
  "Command for getting current git user name")

(defconst blamer--git-repo-cmd "git rev-parse --is-inside-work-tree"
  "Command for detect git repo.")

(defconst blamer--git-blame-cmd "git blame -L %s,%s %s"
  "Command for get blame of current line.")

(defconst blamer--git-commit-message "git log -n1 %s"
  "Command for get commit message.")

(defgroup blamer nil
  "Show commit info at the end of a current line."
  :group 'tools)

(defcustom blamer--prefix "   ◉ "
  "String inserted before commit info."
  :group 'blamer
  :type 'string)

(defcustom blamer--time-enabled-p t
  "Show time of commit"
  :group 'blamer
  :type 'boolean)

(defcustom blamer--commit-message-enabled-p t
  "Show commit message whether this flat is t."
  :group 'blamer
  :type 'boolean)

(defcustom blamer--author-enabled-p t
  "Show time of commit"
  :group 'blamer
  :type 'boolean)

(defcustom blamer--idle-time 0.5
  "Seconds before commit info show"
  :group 'blamer
  :type 'float)

(defcustom blamer--min-offset 60
  "Minimum symbols before insert commit info"
  :group 'blamer
  :type 'integer)

;; TODO: add type choice
(defcustom blamer--type 'both
  "Type of blamer.
'visual - show blame only for current line
'selected - show blame only for selected line
'both - both of them"
  :group 'blamer
  :type '(choice (const :tag "Visual only" visual)
                 (const :tag "Visual and selected" both)
                 (const :tag "Selected only" selected)))

(defcustom blamer--max-lines 30
  "Maximum blamed lines"
  :group 'blamer
  :type 'integer)

(defcustom blamer--max-commit-message-length 30
  "Max length of commit message.
Commit message with more characters will be truncated with ellipsis at the end"
  :group 'blamer
  :type 'integer)

(defface blamer--face
  '((t :foreground "#7a88cf"
       :background nil
       :italic t))
  "Face for blamer info."
  :group 'blamer)


(defvar blamer--idle-timer nil
  "Current timer before commit info showing.")

(defvar blamer--previous-line-number nil
  "Line number of previous popup.")

(defvar blamer--previous-line-length nil
  "Current line number length for detect rerender function.")

(defvar blamer--previous-region-active-p nil
  "Was previous state is active region?")

(defvar blamer--overlays '()
  "Current active overlays for git blame messages.")

(defvar-local blamer--current-author nil
  "git.name for current repository.")


(defun blamer--git-exist-p ()
  "Return t if .git exist."
  (let* ((git-exist-stdout (shell-command-to-string blamer--git-repo-cmd)))
    (string-match "^true" git-exist-stdout)))

(defun blamer--clear-overlay ()
  "Clear last overlay."
  (dolist (ov blamer--overlays)
    (delete-overlay ov))
  (setq blamer--overlays '()))

(defun blamer--git-cmd-error-p (cmd-res)
  "Return t if CMD-RES contain error"
  (string-match-p  "^fatal:" cmd-res))

(defun blamer--prettify-time (date time)
  "Prettify DATE and TIME for nice commit message"
  (let* ((parsed-time (parse-time-string (concat date "T" time)))
         (now (decode-time (current-time)))
         (month-diff (- (nth 4 now) (nth 4 parsed-time)))
         (days-diff (- (nth 3 now) (nth 3 parsed-time)))
         (same-year-p (= (nth 5 now) (nth 5 parsed-time)))
         (same-year-and-month-p (and same-year-p (= month-diff 0)))
         (same-day-p (and same-year-and-month-p (= days-diff 0)))
         (hours-diff (- (nth 2 now) (nth 2 parsed-time)))
         (same-hour-p (and same-day-p (= hours-diff 0)))
         (minutes-diff (- (nth 1 now) (nth 2 parsed-time)))

         (pretty-date (cond ((time-equal-p now parsed-time) "Now")
                            (same-hour-p (format "%s minutes ago" minutes-diff))
                            ((and same-day-p (<= hours-diff 5)) (format "%s hours ago" hours-diff))
                            ;; TODO: add yesterday
                            (same-day-p (concat "Today " time))
                            ((and same-year-and-month-p (<= days-diff 3)) (format  "%s days ago" days-diff))
                            ((and (= month-diff 1) same-year-p) "Previous month")
                            ((and same-year-p (<= month-diff 3)) (format "%s month ago" month-diff))
                            (t (concat date " " time )))))
    (format "[%s]" pretty-date)))

(defun blamer--format-commit-info (commit-hash
                                   commit-message
                                   author
                                   date
                                   time
                                   &optional
                                   offset)
  "Format commit info into display message.
COMMIT-HASH - hash of current commit.
COMMIT-MESSAGE - message of current commit, can be null
AUTHOR - name of commiter
DATE - date in format YYYY-DD-MM
TIME - time in format HH:MM:SS
OFFSET - additional offset for commit message"


  (concat (make-string (or offset 0) ? )
          (or blamer--prefix "")
          (if blamer--author-enabled-p (concat author " ") "")
          (if blamer--time-enabled-p (concat (blamer--prettify-time date time) " ") "")
          (if commit-message (format " (%s)" commit-message) "")))

(defun blamer--get-commit-message (hash)
  "Get commit message by provided HASH.
Return nil if error."
  (let* ((git-commit-res (shell-command-to-string (format blamer--git-commit-message hash)))
         (has-error (blamer--git-cmd-error-p git-commit-res))
         commit-message)

    (when (not has-error)
      (string-match blamer--commit-message-regexp git-commit-res)
      (setq commit-message (match-string 1 git-commit-res))
      (setq commit-message (replace-regexp-in-string "\n" " " commit-message))
      (setq commit-message (string-trim commit-message))
      (truncate-string-to-width commit-message blamer--max-commit-message-length nil nil "..."))))

(defun blamer--get-background-color ()
  "Return color of background under current cursor position."
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))

    (cond ((region-active-p) (face-attribute 'region :background))
          ((boundp 'hl-line-mode) (face-attribute 'hl-line :background))
          (t (face-attribute face :background)))))

(defun blamer--render ()
  "Render text about current line commit status."
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
         (file-name (buffer-file-name))
         (cmd (format blamer--git-blame-cmd start-line-number end-line-number file-name))
         (blame-cmd-res (shell-command-to-string cmd))
         (blame-cmd-res (butlast (split-string blame-cmd-res "\n")))
         commit-message popup-message error commit-hash commit-author commit-date commit-time ov offset)

    (blamer--clear-overlay)

    ;; TODO: reduce responsability
    (save-excursion
      (if (region-active-p)
          (goto-char (region-beginning)))

      (dolist (cmd-msg blame-cmd-res)
        ;; (message "start %s, end %s iterator %s" start-line-number end-line-number navigator)
        (setq error (blamer--git-cmd-error-p cmd-msg))
        (when (not error)
          (setq offset (max (- (or blamer--min-offset 0) (length (thing-at-point 'line))) 0))
          (string-match blamer--regexp-info cmd-msg)
          (setq commit-hash (match-string 1 cmd-msg))
          (setq commit-author (match-string 2 cmd-msg))
          (setq commit-author (if (string= commit-author blamer--current-author) "You" commit-author))
          (setq commit-date (match-string 3 cmd-msg))
          (setq commit-time (match-string 4 cmd-msg))
          (setq commit-message (if blamer--commit-message-enabled-p
                                   (blamer--get-commit-message commit-hash)))
          (setq popup-message (blamer--format-commit-info commit-hash
                                                          commit-message
                                                          commit-author
                                                          commit-date
                                                          commit-time
                                                          offset))
          (setq popup-message (propertize popup-message
                                          'face `(:inherit (blamer--face :background ,(blamer--get-background-color)))
                                          'cursor t)))

        (when (and commit-author (not (eq commit-author "")))
          (end-of-line)
          (setq ov (make-overlay (point) (point) nil t t))
          (overlay-put ov 'after-string popup-message)
          (overlay-put ov 'intangible t)
          (add-to-list 'blamer--overlays ov)
          (forward-line))))))


(defun blamer--render-commit-info-with-delay ()
  "Render commit info with delay."
  (if blamer--idle-timer
      (cancel-timer blamer--idle-timer))

  (setq blamer--idle-timer
        (run-with-idle-timer (or blamer--idle-time 0) nil 'blamer--render)))

(defun blamer--try-render ()
  "Render current line if is .git exist."
  ;; TODO: refactor it >.<
  (let* ((long-line-p (and (region-active-p)
                           (> (count-lines (region-beginning) (region-end)) blamer--max-lines)))
         (region-deselected-p (and blamer--previous-region-active-p (not (region-active-p))))
         (clear-overlays-p (or long-line-p region-deselected-p)))


    ;; (message "deselected: %s clear %s" region-deselected-p clear-overlays-p)
    (when clear-overlays-p
      (blamer--clear-overlay))


    (when (and (or (eq blamer--type 'both)
                   (and (eq blamer--type 'visual) (not (use-region-p)))
                   (and (eq blamer--type 'selected) (use-region-p)))
               (or (not blamer--previous-line-number)
                   (not (eq blamer--previous-line-number (line-number-at-pos)))
                   (not (eq blamer--previous-line-length (length (thing-at-point 'line))))))
      ;; TODO: move this check to run local mode?)

      (blamer--clear-overlay)
      (setq blamer--previous-line-number (line-number-at-pos))
      (setq blamer--previous-line-length (length (thing-at-point 'line)))
      (setq blamer--previous-region-active-p (region-active-p))
      (blamer--render-commit-info-with-delay))))

(defun blamer--reset-state ()
  "Reset all state after blamer mode is disabled."
  (if blamer--idle-timer
      (cancel-timer blamer--idle-timer))

  (blamer--clear-overlay)
  (setq blamer--idle-timer nil)
  (setq blamer--previous-line-number nil)
  (remove-hook 'post-command-hook #'blamer--try-render t))

;;;###autoload
(define-minor-mode blamer-mode
  "Blamer mode.
Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When blamer-mode is enabled, the popup message with commit info
will appear after BLAMER--IDLE-TIME. It works only inside git repo"
  :init-value nil
  :global nil
  :lighter nil
  :group 'blamer
  (let ((is-git-repo (blamer--git-exist-p)))
    (when (and blamer--author-enabled-p is-git-repo)
      (setq-local blamer--current-author (substring (shell-command-to-string blamer--git-author-cmd) 0 -1)))

    (if (and blamer-mode (buffer-file-name) is-git-repo)
        (progn
          (add-hook 'post-command-hook #'blamer--try-render nil t))
      (blamer--reset-state))))

;;;###autoload
(define-globalized-minor-mode
  global-blamer-mode
  blamer-mode
  (lambda ()
    (when (not blamer-mode)
      (blamer-mode))))

(provide 'blamer)
;;; blamer.el ends here
