(require 'org)
(require 's)

(org-add-link-type "ebook" #'org-ebook-open)
(add-hook 'org-store-link-functions #'org-ebook-store-link)

(defun org-ebook-open (path)
  "Visit the part of the ebook at PATH.

PATH is a file location followed by any number of search terms
delimited by \"::\", which can take two forms.

  - @ / chapter search: Adding '::@YYY' to a path goes to the
    beginning a chapter containing the string YYY, and limits
    subsequent searches to that chapter.

  - $ / text search: Adding '::$ZZZ' finds text matching ZZZ,
    where spaces stand for any sequence of whitespace.

Example: [[ebook.epub::@Ch 1::$'It was a dark and stormy night']]"
  (save-match-data
    (let* ((components (s-split "::" path))
           (filename (pop components))
           (operator nil) (search nil))
      (find-file-other-window filename)
      (save-restriction
        (cl-dolist (c components)
          (setq operator (elt c 0)
                search (substring c 1))
          (cond ((eq operator ?@)
                 (widen)
                 (goto-char
                  (ereader-chapter-position
                   (cl-rassoc search ereader-chapters
                              :test (lambda (key item) (s-contains? key item)))))
                 (narrow-to-page))
                ((eq operator ?$)
                 ;; Use isearch function so that space stands for any sequence of
                 ;; whitespace characters
                 (let ((search-whitespace-regexp "\\s-+"))
                   (isearch-search-string search nil nil))
                 (goto-char (match-beginning 0)))
                (t
                 (error "Invalid search type %c" operator)))))))) 

(defcustom org-ebook-chapter-size 15
  "How many characters of the chapter name to include in a link"
  :type '(integer)
  :group 'org-link)

(defcustom org-ebook-excerpt-size 4
  "Length on an ebook excerpt in a link, in words"
  :type '(integer)
  :group 'org-link)

(defun org-ebook-store-link ()
  "Store a link to an ebook"
  (when (eq major-mode 'ereader-mode)
    (let* ((chapter (ereader-current-chapter))
           (orig-pos (point))
           (excerpt
            (s-trim
             (s-collapse-whitespace
              (save-excursion
                (forward-word org-ebook-excerpt-size)
                (buffer-substring-no-properties orig-pos (point)))))))
      (setq link
            (concat "ebook:" (buffer-file-name)
                    (when chapter
                      (concat "::@" (s-left org-ebook-chapter-size chapter)))
                    "::$" excerpt))
      (org-store-link-props
       :type "ebook"
       :link link
       :description (format "`%s...' (%s, %s)"
                            excerpt
                            (car (last (s-split "[ \t\n]" ereader-meta-creator)))
                            (car (s-split-words chapter)))))))












