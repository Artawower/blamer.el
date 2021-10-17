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
