;;; blamer.el --- show git blame info about current line           -*- lexical-binding: t; -*-

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

(defcustom blamer--time-enabled-p t
  "Show time of commit"
  :group 'blamer
  :type 'bool)

(defcustom blamer--author-enabled-p t
  "Show time of commit"
  :group 'blamer
  :type 'bool)

(defcustom blamer--idle-time 0.5
  "Seconds before commit info show"
  :group 'blamer
  :type 'float)

(defface blamer--face
  '((t :foreground "#7a88cf"
       :background "#2f334d"
       :italic t))
  "Face for blamer"
  :group 'blamer)

;; TODO: remove it after tests
;; (set-face-attribute 'blamer--face nil
;;    :foreground "#7a88cf"
;;        :background "#2f334d"
;;        :italic t)


(defvar blamer--git-repo-cmd "git rev-parse --is-inside-work-tree")
(defvar blamer--git-blame-cmd "git blame -L %s,%s %s")
(defvar blamer--git-commit-message "git log -n1 %s")
(defvar blamer--idle-timer nil
  "Current timer before commit info showing.")
(defvar blamer--previous-line-number nil
  "Line number of previous popup.")

(defun blamer--current-line-commit-info ()
  "Return information about current line or nil if .git not found")

(defun blamer--git-exist-p ()
  "Return t if .git exist."
  (let* ((git-exist-stdout (shell-command-to-string blamer--git-repo-cmd)))
    (string-match "^true" git-exist-stdout)))

(defvar blamer--current-overlay nil
  "Current overlay for git blame message.")

(defvar blamer--current-point nil
  "Current overlay for git blame message.")

(defun blamer--clear-overlay ()
  "Clear last overlay."
  (if blamer--current-overlay
      (delete-overlay blamer--current-overlay)))

(defun blamer--format-commit-info (commit-hash
                                   not-commited-yet-p
                                   &optional
                                   author
                                   date
                                   time)
  "Format commit info into display message."
  (format "   ◉ %s, %s (%s)" author (concat date " " time) commit-hash))

(defun blamer--render-current-line ()
  "Render text about current line commit status."
  (let* ((line-number (line-number-at-pos))
         (file-name (buffer-name))
         (cmd (format blamer--git-blame-cmd line-number line-number file-name))
         (blame-cmd-res (shell-command-to-string cmd))
         commit-message popup-message error commit-hash commit-author commit-date commit-time)

    (setq error (string-match-p  "^fatal:" blame-cmd-res))
    (if error
        (ignore)
      (progn
        ;; (message "Me: %s" blame-cmd-res)
        (string-match
         (concat "\\(?1:^[a-z0-9]+\\) \(\\(?2:[^\n]+\\)"
                 " \\(?3:[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\) "
                 "\\(?4:[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)")
                 blame-cmd-res)
         ;; TODO: commit hash get here
         (setq commit-hash (match-string 1 blame-cmd-res))
         (setq commit-author (match-string 2 blame-cmd-res))
         (setq commit-date (match-string 3 blame-cmd-res))
         (setq commit-time (match-string 4 blame-cmd-res))

         (setq commit-message (progn
                                ;; TODO: commit message get here
                                (string-match "^n" blame-cmd-res)))
         (setq popup-message (blamer--format-commit-info commit-hash
                                                         nil
                                                         commit-author
                                                         commit-date
                                                         commit-time))
         (setq popup-message (propertize popup-message 'face 'blamer--face 'cursor t)))

        (blamer--clear-overlay)

        (setq blamer--current-overlay (make-overlay (line-end-position) (line-end-position) nil t t))
        (overlay-put blamer--current-overlay 'after-string popup-message)
        (overlay-put blamer--current-overlay 'intangible t)
        (overlay-put blamer--current-overlay 'face 'bold)
        (setq blamer--current-point (point)))))


(defun blamer--render-commit-info-with-delay ()
  "Render commit info with delay."
  (setq blamer--idle-timer
        (run-with-idle-timer blamer--idle-time nil 'blamer--render-current-line)))

(defun blamer--try-render-current-line ()
  "Render current line if is .git exist."
  (if (and (or (not blamer--previous-line-number)
               (not (eq blamer--previous-line-number (line-number-at-pos))))
           (blamer--git-exist-p))
      (progn
        (blamer--clear-overlay)
        (setq blamer--previous-line-number (line-number-at-pos))
        (blamer--render-commit-info-with-delay))))

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
  (if blamer-mode
      (add-hook 'post-command-hook #'blamer--try-render-current-line nil t)
    (blamer--reset-state)))

(provide 'blamer)
;;; blamer.el ends here
