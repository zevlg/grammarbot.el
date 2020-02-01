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

;; #+OPTIONS: \n:t timestamp:nil
;; #+TITLE: grammarbot.el
;; #+STARTUP: showall
;;
;; #+BEGIN_QUOTE
;; This file is automatically generated from {{{ellit-filename(verbatim)}}} by
;; [[https://github.com/zevlg/ellit-org.el][GitHub#ellit-org.el]] tool.
;; Do not edit manually.
;; #+END_QUOTE
;;
;; =grammarbot.el= is GNU Emacs interface to wonderful
;; [[https://www.grammarbot.io][GrammarBot]] service.
;;
;; =grammarbot.el= interface is similar to =ispell=.
;; There is only one command {{{kbd(M-x grammarbot RET)}}} to check
;; either buffer or active region.

;;; Code:
(require 'json)

(defgroup grammarbot nil
  "Interface to the grammarbot.io."
  :prefix "grammarbot-"
  :group 'hypermedia)

;; * Customizable Options
;;
;; You can customize =grammarbot.el= with {{{kbd(M-x customize-group RET grammarbot RET)}}} using Emacs [[https://www.gnu.org/software/emacs/manual/html_node/emacs/Easy-Customization.html#Easy-Customization][Easy Customization Interface]]
;;
;; Or just tweak following user options in your =init.el=:
;; - User Option: ~grammarbot-language~
;;
;;   {{{vardoc1(grammarbot-language)}}}
;;   Default value: {{{eval(grammarbot-language)}}}
;;
;;   Please note that only English is supported at the moment
;;   ("en-US" or "en-GB").
(defcustom grammarbot-language "en-US"
  "Language to use."
  :type '(choice (const :tag "American English" "en-US")
                 (const :tag "British English" "en-GB"))
  :group 'grammarbot)

;; - User Option: ~grammarbot-show-stats~, default: {{{eval(grammarbot-show-stats)}}}
;;
;;   {{{vardoc(grammarbot-show-stats)}}}
;;
;;   Stats are shown in echo area with message like:
;;   #+BEGIN_EXAMPLE
;;   Grammarbot issues: 2, ignored: 1, replaced: 1
;;   #+END_EXAMPLE
(defcustom grammarbot-show-stats t
  "Non-nil to show statistics for all found issues after check."
  :type 'boolean
  :group 'grammarbot)

;; - User Option: ~grammarbot-api-key~
;;
;;   {{{vardoc1(grammarbot-api-key)}}}
(defcustom grammarbot-api-key nil
  "*Your API Key obtained from http://grammarbot.io.
Without API Key there is a limit up to 100 requests per day."
  :type 'string-or-null-p
  :group 'grammarbot)

(defcustom grammarbot-highlight-face 'isearch
  "Face used to highlight text with issues."
  :type 'face
  :group 'grammarbot)

;; - User Option: ~grammarbot-accept-single-choice-rules~
;;
;;   {{{vardoc1(grammarbot-accept-single-choice-rules)}}}
;;
;;   Consider we are checking next sentence with =grammarbot.el=:
;;   #+BEGIN_EXAMPLE
;;   However it is wise to automatically accept single replacements
;;   #+END_EXAMPLE
;;
;;   While checking, =*Grammarbot Choices*= buffer pops up for the issue:
;;   #+BEGIN_EXAMPLE
;;   Did you forget a comma after a conjunctive/linking adverb?
;;   Rule: (:id "SENT_START_CONJUNCTIVE_LINKING_ADVERB_COMMA" :subId "1" :description "Commas after conjunctive/linking adverbs in front of a new sentence." :issueType "typographical" :urls [(:value "https://writing.wisc.edu/Handbook/ConjAdv.html")] :category (:id "PUNCTUATION" :name "Punctuation"))
;;
;;   (0) However,
;;   #+END_EXAMPLE
;;
;;   As you can see, issue has single replacement - "However,".  For
;;   single replacement issues corresponding issue rule is shown.
;;
;;   You can customize ~grammarbot-accept-single-choice-rules~ to
;;   automatically accept such issue and do the auto-replacement.  To
;;   do so, add:
;;   #+BEGIN_SRC emacs-lisp
;;     (add-to-list 'grammarbot-accept-single-choice-rules
;;                  '(:issueType "typographical" :category (:id "PUNCTUATION")))
;;   #+END_SRC
(defcustom grammarbot-accept-single-choice-rules nil
  "List of rules for issues to automatically accept single choice replacement.
Each match has `:rule' property.  If any of element in
`grammarbot-accept-single-choice-rules' structurally matches
`:rule' property of the match, then such match is automatically
accepted.
Examples of elements:
  (:issueType \"grammar\" :category (:id \"GRAMMAR\"))
    Automatically accept grammar errors."
  :type 'list
  :options '(((:issueType "grammar" :category (:id "GRAMMAR"))))
  :group 'grammarbot)

;; - User Option: ~grammarbot-ignore-single-choice-rules~
;;
;;   {{{vardoc1(grammarbot-ignore-single-choice-rules)}}}
;;
;;   Elements of the list are in same format as fors
;;   ~grammarbot-accept-single-choice-rules~.
;;
;;   By default "Whitespace repetition" and "Sentence starts with an
;;   uppercase letter" issues are ignored.
(defcustom grammarbot-ignore-single-choice-rules
  '((:id "WHITESPACE_RULE" :description "Whitespace repetition (bad formatting)" :issueType "whitespace" :category (:id "TYPOGRAPHY"))
    (:id "UPPERCASE_SENTENCE_START" :description "Checks that a sentence starts with an uppercase letter" :issueType "typographical" :category (:id "CASING")))
  "List of rules for issues to automatically ignore single choice replacement.
Elements are same as for `grammarbot-accept-single-choice-rules'."
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
  "Return issues statistics using RESULTS from `grammarbot--api-check',."
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
  "Highlight text corresponding to MATCH-POINT issue."
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
  "Create choices buffer for issue for MATCH-POINT.
Return the buffer."
  (let* ((match (cdr match-point))
         (replacements (plist-get match :replacements)))
    (with-current-buffer (get-buffer-create grammarbot-choices-buffer)
      (erase-buffer)
      (insert (plist-get match :message) "\n")
      (when (= 1 (length replacements))
        (insert (format "Rule: %S\n" (plist-get match :rule))))
      (insert "\n")
      (let ((max-rlen (apply 'max (mapcar (lambda (rep)
                                            (length (plist-get rep :value)))
                                          replacements)))
            (rep-idx ?0))
        (seq-doseq (rep replacements)
          (when (= rep-idx (+ ?9 1))
            (setq rep-idx ?a))
          (insert "(" (propertize (char-to-string rep-idx) 'face 'bold) ")")
          (insert " " (plist-get rep :value) " ")
          (insert (make-string (- max-rlen (length (plist-get rep :value))) ?\s))
          (when (>= (current-column) fill-column)
            (insert "\n"))
          (cl-incf rep-idx)))
      (goto-char (point-min))
      (current-buffer))))

(defun grammarbot--keys-as-rep-idx (keys)
  "Convert KEYS to replacement index.
Return nil or replacement index number."
  (let ((num-key (and (= (length keys) 1) (aref keys 0))))
    (cond ((and (>= num-key ?0) (<= num-key ?9))
           (- num-key ?0))
          ((and (>= num-key ?a) (<= num-key ?z))
           (+ 10 (- num-key ?a))))))

(defun grammarbot--issue-choose (match-point)
  "Show/hide replacements candidates for MATCH-POINT.
Return selected replacement index or nil if MATCH-POINT ignored."
  (if match-point
      (let* ((nreps (length (plist-get (cdr match-point) :replacements)))
             (read-prompt
              (concat (format "0%s to replace, "
                              (cond ((< nreps 2) "")
                                    ((< nreps 11) (format "-%d" (1- nreps)))
                                    (t (concat "-" (char-to-string
                                                    (+ nreps (- ?a 11)))))))
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
  "Interactively perform issue resolving for the MATCH-POINT.
Return non-nil if issue has been replaced with one of the replacements."
  (goto-char (car match-point))
  (grammarbot--issue-highlight match-point)

  (when-let ((rep-idx (grammarbot--issue-choose match-point)))
    (grammarbot--issue-replace match-point rep-idx)))

(defun grammarbot--plist-part-p (plist part)
  "Return non-nil PART is the structural part of the PLIST."
  (let ((part-alist (json--plist-to-alist part)))
    (cl-every (lambda (part-elem)
                (let ((part-val (cdr part-elem))
                      (plist-val (plist-get plist (car part-elem))))
                  (cond ((and (listp part-val) (listp plist-val))
                         (grammarbot--plist-part-p plist-val part-val))
                        (t (equal part-val plist-val)))))
              part-alist)))

(defun grammarbot--issue-accept-p (match-point)
  "Return non-nil if MATCH-POINT should be automatically accepted.
Only issues with single choice replacements are accepted.
See `grammarbot-accept-single-choice-rules'."
  (let ((match (cdr match-point)))
    (and (= 1 (length (plist-get match :replacements)))
         grammarbot-accept-single-choice-rules
         (cl-some (apply-partially 'grammarbot--plist-part-p
                                   (plist-get match :rule))
                  grammarbot-accept-single-choice-rules))))

(defun grammarbot--issue-ignore-p (match-point)
  "Return non-nil if MATCH-POINT should be automatically ignored.
Only issues with single (or zero) choice replacements are ignored.
See `grammarbot-ignore-single-choice-rules'."
  (let ((match (cdr match-point)))
    (and (< (length (plist-get match :replacements)) 2)
         grammarbot-ignore-single-choice-rules
         (cl-some (apply-partially 'grammarbot--plist-part-p
                                   (plist-get match :rule))
                  grammarbot-ignore-single-choice-rules))))

(defsubst grammarbot--stat-inc (stat which)
  (plist-put stat which (1+ (plist-get stat which))))

;;;###autoload
(defun grammarbot (start end &optional arg)
  "Interactively check grammar for region or the buffer.
If region is selected, then check region only from START to END.
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
                  (plist-get result :matches)))
         (stats '(:ignored 0 :replaced 0)))
    (if match-points
        (unwind-protect
            (save-excursion
              (seq-doseq (match-pnt match-points)
                (cond ((or arg (grammarbot--issue-accept-p match-pnt))
                       (grammarbot--issue-replace match-pnt 0)
                       (grammarbot--stat-inc stats :replaced))

                      ((grammarbot--issue-ignore-p match-pnt)
                       (grammarbot--stat-inc stats :ignored))

                      (t
                       (if (grammarbot--issue-interactively match-pnt)
                           (grammarbot--stat-inc stats :replaced)
                         (grammarbot--stat-inc stats :ignored))))))

          (grammarbot--issue-highlight nil)
          (grammarbot--issue-choose nil)
          (when grammarbot-show-stats
            (message "GrammarBot issues: %d, ignored: %d, replaced: %d"
                     (length match-points)
                     (plist-get stats :ignored)
                     (plist-get stats :replaced))))

      (when grammarbot-show-stats
        (message "GrammarBot v%s detected no issues"
                 (plist-get (plist-get result :software) :version))))))

(provide 'grammarbot)

;;; grammarbot.el ends here
