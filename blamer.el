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

(defcustom blamer--min-offset 20
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

(defcustom blamer--max-commit-message-length 50
  "Max length of commit message.
Commit message with more characters will be truncated with ellipsis at the end"
  :group 'blamer
  :type 'integer)

(defface blamer--face
  '((t :foreground "#7a88cf"
       :background nil
       :height 120
       :italic t))
  "Face for blamer"
  :group 'blamer)

(defvar blamer--git-repo-cmd "git rev-parse --is-inside-work-tree"
  "Command for detect git repo.")

(defvar blamer--git-blame-cmd "git blame -L %s,%s %s"
  "Command for get blame of current line.")

(defvar blamer--git-commit-message "git log -n1 %s"
  "Command for get commit message.")

(defvar blamer--idle-timer nil
  "Current timer before commit info showing.")

(defvar blamer--previous-line-number nil
  "Line number of previous popup.")

(defvar blamer--previous-line-length nil
  "Current line number length for detect rerender function.")

(defvar blamer--current-overlay nil
  "Current overlay for git blame message.")

;; TODO Add tests
(defvar blamer--regexp-info
  (concat "\\(?1:^[a-z0-9]+\\) [^\s]*[[:blank:]]?\(\\(?2:[^\n]+\\)"
          "\s\\(?3:[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)"
          "\s\\(?4:[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)")

  "Regexp for extract data from blame message.
1 - commit hash
2 - author
3 - date
3 - time")

(defvar blamer--commit-message-regexp "\n\n[\s]+\\(?1:[^.]+\\):?"
  "Regexp for commit message parsing.")

(defun blamer--git-exist-p ()
  "Return t if .git exist."
  (let* ((git-exist-stdout (shell-command-to-string blamer--git-repo-cmd)))
    (string-match "^true" git-exist-stdout)))

(defun blamer--clear-overlay ()
  "Clear last overlay."
  (if blamer--current-overlay
      (delete-overlay blamer--current-overlay)))

(defun blamer--git-cmd-error-p (cmd-res)
  "Return t if CMD-RES contain error"
  (string-match-p  "^fatal:" cmd-res))

(defun blamer--prettify-time (date time)
  "Prettify DATE and TIME for nice commit message"
  ;; TODO: implement
  (concat "[" date " " time "]"))

(defun blamer--format-commit-info (commit-hash
                                   commit-message
                                   author
                                   date
                                   time
                                   &optional offset)
  "Format commit info into display message.
COMMIT-HASH - hash of current commit.
COMMIT-MESSAGE - message of current commit, can be null
AUTHOR - name of commiter
DATE - date in format YYYY-DD-MM
TIME - time in format HH:MM:SS
OFFSET - additional offset for commit message"

  ;; TODO: add function for prettify current time
  ;; (message "hash - %s\n commit-message - %s\n author - %s\n date - %s\n time - %s\n"
  ;;          commit-hash commit-message author date time)
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
    (if hl-line-mode
        (face-attribute 'hl-line :background)
      (face-attribute face :background))))

(defun blamer--render-current-line ()
  "Render text about current line commit status."
  (let* ((line-number (line-number-at-pos))
         (file-name (buffer-file-name))
         (cmd (format blamer--git-blame-cmd line-number line-number file-name))
         (offset (or blamer--min-offset 0))
         (offset (max (- offset (length (thing-at-point 'line))) 0))
         (blame-cmd-res (shell-command-to-string cmd))
         commit-message popup-message error commit-hash commit-author commit-date commit-time)

    ;; (message "cmd %s: res %s" cmd blame-cmd-res)
    (setq error (blamer--git-cmd-error-p blame-cmd-res))
    (when (not error)
      (string-match blamer--regexp-info blame-cmd-res)
      (setq commit-hash (match-string 1 blame-cmd-res))
      (setq commit-author (match-string 2 blame-cmd-res))
      (setq commit-date (match-string 3 blame-cmd-res))
      (setq commit-time (match-string 4 blame-cmd-res))
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
      (blamer--clear-overlay)
      (setq blamer--current-overlay (make-overlay (line-end-position) (line-end-position) nil t t))
      (overlay-put blamer--current-overlay 'after-string popup-message)
      (overlay-put blamer--current-overlay 'intangible t))))


(defun blamer--render-commit-info-with-delay ()
  "Render commit info with delay."
  (setq blamer--idle-timer
        (run-with-idle-timer (or blamer--idle-time 0) nil 'blamer--render-current-line)))

(defun blamer--try-render-current-line ()
  "Render current line if is .git exist."
  (when (and (or (not blamer--previous-line-number)
                 (not (eq blamer--previous-line-number (line-number-at-pos)))
                 (not (eq blamer--previous-line-length (length (thing-at-point 'line)))))
             (blamer--git-exist-p))
    (blamer--clear-overlay)
    (setq blamer--previous-line-number (line-number-at-pos))
    (setq blamer--previous-line-length (length (thing-at-point 'line)))
    (blamer--render-commit-info-with-delay)))

(defun blamer--reset-state ()
  "Reset all state after blamer mode is disabled."
  (blamer--clear-overlay)
  (setq blamer--idle-timer nil)
  (setq blamer--previous-line-number nil)
  (remove-hook 'post-command-hook #'blamer--try-render-current-line t))

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
  (if (and blamer-mode (buffer-file-name))
      (add-hook 'post-command-hook #'blamer--try-render-current-line nil t)
    (blamer--reset-state)))

;;;###autoload
(define-globalized-minor-mode
  global-blamer-mode
  blamer-mode
  (lambda ()
    (when (not blamer-mode)
      (blamer-mode))))

(provide 'blamer)
;;; blamer.el ends here
