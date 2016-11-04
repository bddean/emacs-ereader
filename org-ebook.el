;;; org-ebook.el --- Org-link type for ereader.el

;; Author: Ben Dean <bendean837@gmail.com>
;; Version: 0.0.0
;; Package-Requires: ((org "8.3.3") (s "1.10.0"))
;; Keywords: epub, ebook, org, org-link
;; URL: https://github.com/bddean/ereader-el

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
;; Create links to ebooks, including specific phrases, to open in
;; ereader-mode. In the future other backends should be supported.

;;; Code:

(require 'org)
(require 's)
(require 'ereader)

(org-link-set-parameters "ebook"
                         :follow #'org-ebook-open
                         :store #'org-ebook-store-link)

(defun org-ebook-parse-path (path)
  (let* ((components (s-split "::" path))
         (result (list :filename (pop components)))
         operator search)
    (cl-dolist (c components)
      (setq operator (elt c 0)
            search (substring c 1))
      (plist-put result
                 (cond ((eq operator ?@) :chapter)
                       ((eq operator ?$) :quote)
                       (t (error "Invalid search type %c" operator)))
                 search))
    result))

(defun org-ebook-open (path)
  "Visit the part of the ebook at PATH.

PATH is a file location followed by any number of search terms
delimited by \"::\", which can take two forms.

  - @ / chapter search: Adding '::@YYY' to a path goes to the
    beginning a chapter containing the string YYY, and limits
    subsequent searches to that chapter.

  - $ / text search: Adding '::$ZZZ' finds text matching ZZZ,
    where spaces stand for any sequence of whitespace.

Example: [[ebook.epub::@Ch 1::$It was a dark and stormy night]]"
  (save-match-data
    (let* ((components (org-ebook-parse-path path))
           chapter quote)
      (find-file-other-window (plist-get components :filename))
      (goto-char (point-min))
      (save-restriction
        (when (setq chapter (plist-get components :chapter))
          (widen )
          (goto-char
           (ereader-chapter-position
            (cl-rassoc chapter ereader-chapters
                       :test (lambda (key item) (s-contains? key item)))))
					;; TODO narrow to chapter
          ;; (narrow-to-page)
					)
        (when (setq quote (plist-get components :quote))
          (search-forward-lax-whitespace quote nil t)
          (goto-char (match-beginning 0))))))) 

(defcustom org-ebook-chapter-size 15
  "How many characters of the chapter name to include in a link"
  :type '(integer)
  :group 'org-link)

(defcustom org-ebook-quote-size 4
  "Length on an ebook quote in a link, in words"
  :type '(integer)
  :group 'org-link)

(defun org-ebook-store-link ()
  "Store a link to an ebook"
  (when (eq major-mode 'ereader-mode)
    (let* ((chapter (ereader-current-chapter))
           (orig-pos (point))
           (quote
            (s-trim
             (s-collapse-whitespace
              (save-excursion
                (forward-word org-ebook-quote-size)
                (buffer-substring-no-properties orig-pos (point))))))
					 (link
						(concat "ebook:" (buffer-file-name)
										(when chapter
											(concat "::@" (s-left org-ebook-chapter-size chapter)))
										"::$" quote)))

      (org-store-link-props
       :type "ebook"
       :link link
       :description
       (if chapter
           (format "`%s...' (%s, %s)"
                   quote
                   (car (last (s-split "[ \t\n]" ereader-meta-creator)))
                   (car (s-split-words chapter)))
         (format "%s... (%s)" quote (car (last (s-split "[ \t\n]" ereader-meta-creator)))))))))

(provide 'org-ebook)

;;; org-ebook.el ends here
