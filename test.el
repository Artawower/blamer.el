;;; test.el --- Testing for blamer  -*- lexical-binding:t -*-

(require 'blamer)

;;; Code:

(ert-deftest test-parse-regexp ()
  "Test blame parser regexp."
  (let* ((blame-msg-1 "(44307898 (artawower 2021-10-16 14:06:06 +0300 241)          commit-message")
         (blame-msg-2 "(b3a30c6ae (Andrey Tokarev 2020-12-16 16:28:09 +0800 4) $tooltip-border-radius: 2px;)")
         (blame-msg-3 "(b3130q6ze (@s!~range$><name-+\/ 2020-12-16 16:28:09 +0800 4) $tooltip-border-radius: 2px;)"))
    (string-match blamer--regexp-info blame-msg-1)
    (should (equal (match-string 1 blame-msg-1) "44307898"))
    (should (equal (match-string 2 blame-msg-1) "artawower"))
    (should (equal (match-string 3 blame-msg-1) "2021-10-16"))
    (should (equal (match-string 4 blame-msg-1) "14:06:06"))

    (string-match blamer--regexp-info blame-msg-2)
    (should (equal (match-string 1 blame-msg-2) "b3a30c6ae"))
    (should (equal (match-string 2 blame-msg-2) "Andrey Tokarev"))
    (should (equal (match-string 3 blame-msg-2) "2020-12-16"))
    (should (equal (match-string 4 blame-msg-2) "16:28:09"))

    (string-match blamer--regexp-info blame-msg-3)
    (should (equal (match-string 1 blame-msg-3) "b3130q6ze"))
    (should (equal (match-string 2 blame-msg-3) "@s!~range$><name-+\/"))
    (should (equal (match-string 3 blame-msg-3) "2020-12-16"))
    (should (equal (match-string 4 blame-msg-3) "16:28:09"))))



(ert-deftest test-parse-commit-message ()
  "Test commit parser regexp."
  (let ((commit-msg-1 "commit 3783fafc6e14cb3fc27fbede7bb039262437e192
Author: artawower <artawower@mail.ru>
Date:   Thu Oct 14 21:59:43 2021 +0300

    refactor, global mode, rerender after line changed

")
        (commit-msg-2 "commit 3dec28d846da16b4f02958fe7173bf2f0f2bd0bf
Author: Deins S@s~1!] <user783@gmail.com>
Date:   Fri Feb 28 12:45:39 2020 +0400

    Very bing commit. С несколькикми языками. and its so illogical cause its realy big.
But! What we can do with this enormous commit message? Nothing. 123955!!.~

"))

    (string-match blamer--commit-message-regexp commit-msg-1)
    (should (equal (match-string 1 commit-msg-1) "refactor, global mode, rerender after line changed\n\n"))

    (string-match blamer--commit-message-regexp commit-msg-2)
    (should (equal (match-string 1 commit-msg-2) "Very bing commit. С несколькикми языками. and its so illogical cause its realy big.
But! What we can do with this enormous commit message? Nothing. 123955!!.~\n\n"))
    ))


(defun format-date-to-string (date)
  "Format date to test string."
  (format-time-string "%Y-%m-%d" date))

(defun format-time-to-string (time)
  "Format time to test string."
  (format-time-string "%H:%M:%S" time))

(ert-deftest test-pretty-time-format ()
  "Test prettified time format."
  (let* ((yesterday (time-subtract (current-time) (days-to-time 1)))
         (formatted-date (format-date-to-string yesterday))
         (formatted-time (format-time-to-string yesterday))
         (12-minutes-ago (time-subtract (current-time) (seconds-to-time 760)))
         (59-minutes-ago (time-subtract (current-time) (seconds-to-time 3540)))
         (2-hours-ago (time-subtract (current-time) (seconds-to-time (* 60 60 2))))
         (1-hour-ago (time-subtract (current-time) (seconds-to-time (* 60 60))))
         (23-hours-ago (time-subtract (current-time) (seconds-to-time (* 60 60 23))))
         (2-days-ago (time-subtract (current-time) (seconds-to-time (* 60 60 24  2))))
         (week-ago (time-subtract (current-time) (seconds-to-time (* 60 60 24 7))))
         (4-weeks-ago (time-subtract (current-time) (seconds-to-time (* 60 60 24 7 4))))
         (month-ago (time-subtract (current-time) (seconds-to-time (* 60 60 24 35))))
         (9-months-ago (time-subtract (current-time) (seconds-to-time (* 60 60 24 30 9))))
         (year-ago (time-subtract (current-time) (seconds-to-time (* 60 60 24 366))))
         (4-years-ago (time-subtract (current-time) (seconds-to-time (* 60 60 24 365 4))))
         (11-years-ago (time-subtract (current-time) (seconds-to-time (* 60 60 24 365 11))))
         (11-years-ago-prettified (concat
                                   (format-date-to-string 11-years-ago)
                                   " "
                                   (format-time-to-string 11-years-ago))))

    (should (equal "Yesterday" (blamer--prettify-time formatted-date formatted-time)))

    (should (equal "Now" (blamer--prettify-time (format-date-to-string (current-time))
                                                (format-time-to-string (current-time)))))

    (should (equal "12 minutes ago" (blamer--prettify-time (format-date-to-string 12-minutes-ago)
                                                           (format-time-to-string 12-minutes-ago))))

    (should (equal "59 minutes ago" (blamer--prettify-time (format-date-to-string 59-minutes-ago)
                                                           (format-time-to-string 59-minutes-ago))))
    (should (equal "59 minutes ago" (blamer--prettify-time (format-date-to-string 59-minutes-ago)
                                                           (format-time-to-string 59-minutes-ago))))
    (should (equal "2 hours ago" (blamer--prettify-time (format-date-to-string 2-hours-ago)
                                                        (format-time-to-string 2-hours-ago))))
    (should (equal "Hour ago" (blamer--prettify-time (format-date-to-string 1-hour-ago)
                                                     (format-time-to-string 1-hour-ago))))
    (should (equal "23 hours ago" (blamer--prettify-time (format-date-to-string 23-hours-ago)
                                                         (format-time-to-string 23-hours-ago))))
    (should (equal "2 days ago" (blamer--prettify-time (format-date-to-string 2-days-ago)
                                                       (format-time-to-string 2-days-ago))))
    (should (equal "Last week" (blamer--prettify-time (format-date-to-string week-ago)
                                                      (format-time-to-string week-ago))))
    (should (equal "4 weeks ago" (blamer--prettify-time (format-date-to-string 4-weeks-ago)
                                                        (format-time-to-string 4-weeks-ago))))
    (should (equal "Previous month" (blamer--prettify-time (format-date-to-string month-ago)
                                                           (format-time-to-string month-ago))))
    (should (equal "9 months ago" (blamer--prettify-time (format-date-to-string 9-months-ago)
                                                         (format-time-to-string 9-months-ago))))
    (should (equal "Previous year" (blamer--prettify-time (format-date-to-string year-ago)
                                                          (format-time-to-string year-ago))))
    (should (equal "4 years ago" (blamer--prettify-time (format-date-to-string 4-years-ago)
                                                        (format-time-to-string 4-years-ago))))
    (setq )
    (should (equal 11-years-ago-prettified (blamer--prettify-time (format-date-to-string 11-years-ago)
                                                                                     (format-time-to-string 11-years-ago))))
    ))
