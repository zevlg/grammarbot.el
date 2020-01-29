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

(defcustom grammarbot-max-replacements 10
  "Maximum number of replacements to show.
10 at maximum."
  :type 'integer
  :group 'grammarbot)

(defcustom grammarbot-accept-single-choice
  '((:issueType "grammar" :category "GRAMMAR"))
  "List of issues to automatically accept if issue of given type."
  :type 'list
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

(defvar grammarbot-choices-buffer "*Grammarbot Choices*")
(defvar grammarbot-choices-display-buffer-action
  '((display-buffer-reuse-window display-buffer-in-side-window)
    (side . top) (window-height . fit-window-to-buffer)))

(defvar grammarbot--overlay nil)

;; NOTE: `match-point' is cons in form (MARKER . MATCH)
(defun grammarbot--issue-highlight (match-point)
  "Highlight text corresponding to issue MATCH."
  (if match-point
      (let* ((mstart (car match-point))
             (mend (+ mstart (plist-get (cdr match-point) :length))))
        (if grammarbot--overlay
            (move-overlay grammarbot--overlay mstart mend (current-buffer))

          (setq grammarbot--overlay (make-overlay mstart mend))
          (overlay-put grammarbot--overlay :grammarbot-match-point match-point)
          (overlay-put grammarbot--overlay 'priority 1001)
          (overlay-put grammarbot--overlay 'face grammarbot-highlight-face)))

    (when grammarbot--overlay
      (delete-overlay grammarbot--overlay))))

(defun grammarbot--choices-buffer (match-point)
  "Create and fill choices buffer, return buffer."
  (let ((match (cdr match-point)))
    (with-current-buffer (get-buffer-create grammarbot-choices-buffer)
      (erase-buffer)
;      (insert (plist-get match :shortMessage) ": ")
      (insert (plist-get match :message) "\n")
      (let ((max-rlen (apply 'max (mapcar (lambda (rep)
                                            (length (plist-get rep :value)))
                                          (plist-get match :replacements))))
            (rep-idx 0))
        (seq-doseq (rep (plist-get match :replacements))
          (insert "(" (propertize (number-to-string rep-idx) 'face 'bold) ")")
          (insert " " (plist-get rep :value) " ")
          (insert (make-string (- max-rlen (length (plist-get rep :value))) ?\s))
          (cl-incf rep-idx)))
      (goto-char (point-min))
      (current-buffer))))

(defun grammarbot--keys-as-rep-idx (keys)
  "Convert KEYS to replacement index.
Return digit or nil."
  (let ((num-key (and (= (length keys) 1) (aref keys 0))))
    (when (and (>= num-key ?0) (<= num-key ?9))
      (- num-key ?0))))

(defun grammarbot--issue-choose (match-point)
  "Show/hide replacements candidates for MATCH-POINT.
Return selected replacement index."
  (if match-point
      (let* ((nreps (length (plist-get (cdr match-point) :replacements)))
             (read-prompt (concat (format "0%s to replace, "
                                          (if (> nreps 1)
                                              (format "-%d" nreps)
                                            ""))
                                  "SPC to leave unchanged, "
                                  "C-g to cancel"))
             (got-valid-keys nil) (ret-rep-idx nil))
        (display-buffer (grammarbot--choices-buffer match-point)
                        grammarbot-choices-display-buffer-action)

        ;; Read key with replacement index
        (while (not got-valid-keys)
          (let ((key (read-key read-prompt)))
            (cond ((when (and (>= key ?0) (<= key ?9)
                              (< (- key ?0) nreps))
                     (setq ret-rep-idx (- key ?0))
                     (setq got-valid-keys t)))
                  ((= key ?\C-g) (keyboard-quit))
                  ((= key ?\s) (setq got-valid-keys t)))))
        ret-rep-idx)

    (when (get-buffer grammarbot-choices-buffer)
      (kill-buffer grammarbot-choices-buffer))))

(defun grammarbot--issue-replace (match-point rep-idx)
  "Replace text corresponding to MATCH-POINT with REP-IDX's replacement."
  (let* ((match (cdr match-point))
         (rep (plist-get (aref (plist-get match :replacements) rep-idx) :value)))
    (save-excursion
      (goto-char (car match-point))
      (insert rep)
      (delete-char (plist-get match :length)))))

(defun grammarbot--issue-interactively (match-point)
  "Interactively perform issue resolving for the MATCH-POINT."
  (goto-char (car match-point))
  (grammarbot--issue-highlight match-point)

  (when-let ((rep-idx (grammarbot--issue-choose match-point)))
    (grammarbot--issue-replace match-point rep-idx)))

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
         (match-points
          (mapcar (lambda (match)
                    (cons (copy-marker
                           (+ start (plist-get match :offset)) t)
                          match))
                  (plist-get result :matches))))
    (if match-points
        (unwind-protect
            (save-excursion
              (seq-doseq (match-pnt match-points)
                (if arg
                    (grammarbot--issue-replace match-pnt 0)
                  (grammarbot--issue-interactively match-pnt))))

          (grammarbot--issue-highlight nil)
          (grammarbot--issue-choose nil))

      (message "GrammarBot v%s detected no errors"
               (plist-get (plist-get result :software) :version)))))

(provide 'grammarbot)

;;; grammarbot.el ends here
