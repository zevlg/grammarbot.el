;;; grammarbot.el --- Emacs interface to grammarbot.io  -*- lexical-binding:t -*-

;; Copyright (C) 2020 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Tue Jan 28 15:17:19 2020
;; Keywords: dictionary, hypermedia
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))
;; Version: 1.0
(defconst grammarbot-version "1.0")

;; grammarbot.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; grammarbot.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with grammarbot.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'json)

(defgroup grammarbot nil
  "Interface to the grammarbot.io."
  :prefix "grammarbot-"
  :group 'hypermedia)

(defcustom grammarbot-language "en-US"
  "Language to use."
  :type '(choice (const :tag "American English" "en-US")
                 (const :tag "British English" "en-GB"))
  :group 'grammarbot)

(defcustom grammarbot-api-key nil
  "*Your API Key uptained from http://grammarbot.io.
Without API Key there is a limit up to 100 requests per day."
  :type 'string-or-null-p
  :group 'grammarbot)

(defcustom grammarbot-highlight-face 'isearch
  "Face used to highlight text with issues."
  :type 'face
  :group 'grammarbot)

(defconst grammarbot-api-url "http://api.grammarbot.io/v2/check"
  "API URL to grammarbot service.")

(defun grammarbot--api-check (text api-key lang)
  "Check the TEXT for grammatical errors using grammarbot service.
API-KEY, API Key to use, by default `grammarbot-api-key' is used.
LANG, language of the TEXT, by default `grammarbot-language' is used."
  (with-temp-buffer
    (url-insert-file-contents
     (concat grammarbot-api-url "?" "text=" text "&lang=" lang
             (when grammarbot-api-key
               (concat "&api-key=" api-key))))

    (goto-char (point-min))
    (let ((json-object-type 'plist))
      (json-read))))

(defun grammarbot-issues-stat (results)
  "Return issues statistics using RESULTS from `grammarbot--api-check',"
  (let ((matches (plist-get results :matches))
        (issues-alist nil))
    (seq-doseq (match matches)
      (let* ((issue-type (or (plist-get (plist-get match :rule) :issueType)
                             "unknown"))
             (issue (assoc issue-type issues-alist)))
        (if issue
            (cl-incf (cdr issue))
          (setq issues-alist (cons (cons issue-type 1) issues-alist)))))
    issues-alist))

(defun grammarbot-check (text &optional api-key lang)
  "Check the TEXT for grammatical errors using grammarbot service.
API-KEY, API Key to use, by default `grammarbot-api-key' is used.
LANG, language of the TEXT, by default `grammarbot-language' is used."
  (grammarbot--api-check
   text (or api-key grammarbot-api-key) (or lang grammarbot-language)))

(defvar grammarbot--start 0)
(defvar grammarbot--overlay nil)

(defun grammarbot--issue-highlight (match)
  "Highlight text corresponding to issue MATCH."
  (if match
      (let* ((mstart (+ grammarbot--start (plist-get match :offset)))
             (mend (+ mstart (plist-get match :length))))
        (if grammarbot--overlay
            (move-overlay grammarbot--overlay mstart mend (current-buffer))

          (setq grammarbot--overlay (make-overlay mstart mend))
          (overlay-put grammarbot--overlay 'priority 1001)
          (overlay-put grammarbot--overlay 'face grammarbot-highlight-face)))

    (when grammarbot--overlay
      (delete-overlay grammarbot--overlay))))

(defun grammarbot--issue-replace (match rep-idx)
  "Replace text corresponding to MATCH with replacement."
  (let* ((rep (plist-get (aref (plist-get match :replacements) rep-idx)
                         :value))
         (mstart (+ grammarbot--start (plist-get match :offset)))
         (mend (+ mstart (plist-get match :length))))
    (save-excursion
      (goto-char mend)
      (insert rep)
      (delete-region mstart mend))

    (cl-incf grammarbot--start (- (length rep) (- mend mstart)))))

(defun grammarbot--issue-interactively (match)
  "Interactively perform issue resolving for the MATCH."
  (goto-char (+ grammarbot--start (plist-get match :offset)))
  (grammarbot--issue-highlight match)
  (read-key "Choose replacement: ")
  (grammarbot--issue-replace match 0))

;;;###autoload
(defun grammarbot (start end &optional arg)
  "Interactively check grammar for region or the buffer.
If region is selected, then check region only.
Otherwise check whole buffer.
If prefix ARG is given, then automatically replace issues with
the first suggestion, i.e. grammarbot in batch mode."
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end) current-prefix-arg)
     (list (point-min) (point-max) current-prefix-arg)))

  (deactivate-mark)
  (let* ((result (grammarbot--api-check
                  (buffer-substring-no-properties start end)
                  grammarbot-api-key grammarbot-language))
         (matches (plist-get result :matches))
         (grammarbot--start start))
    (if (> (length matches) 0)
        (unwind-protect
            (save-excursion
              (seq-doseq (match matches)
                (if arg
                    (grammarbot--issue-replace match 0)
                  (grammarbot--issue-interactively match))))
          (grammarbot--issue-highlight nil))

      (message "GrammarBot v%s detected no errors"
               (plist-get (plist-get result :software) :version)))))

(provide 'grammarbot)

;;; grammarbot.el ends here
