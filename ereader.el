;;; ereader.el --- Major mode for reading ebooks with org-mode integration

;; Author: Ben Dean <bendean837@gmail.com>
;; Version: 0.0.0
;; Package-Requires: ((emacs "24.4") (dash "2.12.1") (s "1.10.0") (xml+ "0.0.0"))
;; Keywords: epub, ebook
;; URL: https://github.com/bddean/emacs-ereader

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
;; Only .epub files are supported so far

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'picture)
(require 's)
(require 'shr)
(require 'url-util)
(require 'view)
(require 'xml+)

(condition-case nil
		(require 'dom)
	(file-error
	 (defun dom-attr (node attr)
		 "Get html attribute for shr version 24."
     (cdr (assq (intern (concat ":" (symbol-name attr)))
                node)))))

(defvar ereader-media-types
  '(("image/jpeg" . ereader-display-image)
    ("application/xhtml+xml" . ereader-display-html)))

(defun ereader-display-image (cwd item)
  "Insert image relative to directory CWD and described by xml node ITEM into the buffer."
  (insert-image
   (create-image (concat cwd "/" (cdr (assoc 'href (xml-node-attributes item)))))
   (cdr (assoc 'id (xml-node-attributes item)))
   (insert "\n")))


;; Variables for metadata
(defvar-local ereader-meta-creator nil "creator of book")
(defvar-local ereader-meta-title nil "title of book")
(defvar-local ereader-meta-subject nil "subject of book")
(defvar-local ereader-meta-isbn nil "isbn of book")
(defvar-local ereader-meta-publisher nil "publisher of book")

(defvar-local ereader-base nil "Base url for document")


(defcustom ereader-annotation-files nil
  "Notes files for ebooks.
Alist mapping ebook titles (values of the variable
`ereader-meta-title' in ebook buffers) to Org notes containing
annotations.

Text in an org file after a link appear as annotations to the
linked position.
"
	:group 'ereader)

(defvar-local ereader-annotations '() "List of positions of annotations")

(defun ereader-load-annotations ()
	"Load annotations from the associated Orgmode file.
An annotation is simply an org-link to a position in the ebook,
followed by some text.  The text following the annotation can be
displayed in the right margin.

This function is called interactively instead of automatically
because it is slow.

See `ereader-annotation-files', `ereader-hide-annotation',
`ereader-show-annotation', `ereader-hide-all-annotations', and
`ereader-show-all-annotations'."
  (interactive)
  (require 'org-ebook)
  (read-only-mode -1)
  (setq ereader-annotations '())
  (let ((ebook-file (buffer-file-name))
        link path path-parts path-quote begin end annotation)
    (dolist (notes (cdr (assoc ereader-meta-title ereader-annotation-files)))
      ;; TODO use temp buffer instead of `find-file-noselect' here and in
      ;; similar cases.
      ;; http://emacs.stackexchange.com/questions/2868/whats-wrong-with-find-file-noselect
      (with-current-buffer (find-file-noselect notes)
        (save-excursion
          (goto-char (point-min))
          (let ((org-link-search-failed nil))
            ;; TODO use re-search instead
            (cl-flet ((message (&rest args) nil)) (org-next-link))
            (while (not org-link-search-failed)
              (setq link (org-element-link-parser))
              (setq path (org-link-unescape (org-element-property :path link)))
              (when (s-prefix-p "ebook:" (org-element-property :raw-link link))
                (setq path-parts (org-ebook-parse-path path))

                ;; Get text for margin note
                (setq annotation
                      (cond ((org-in-item-p)
                             (buffer-substring (org-element-property :end link)
                                               (save-excursion (org-end-of-item) (point))))
                            (t "Note")))
                (setq annotation (replace-regexp-in-string "^\n+" "" annotation))
                (setq annotation (propertize annotation
                                             'face 'font-lock-comment-face
                                             'file (buffer-file-name)
                                             'position (point)
                                             'help-echo (format "%s, line %d"
                                                                (file-name-nondirectory
                                                                 (buffer-file-name))
                                                                (org-current-line))))


                (when (equal ebook-file
                             (expand-file-name (plist-get path-parts :filename)))
                  (save-window-excursion
                    (org-ebook-open path)
                    (add-to-list 'ereader-annotations (set-marker (make-marker) (point)))
                    (if (and (setq path-quote (plist-get path-parts :quote))
                             (looking-at (regexp-quote path-quote)))
                        (setq begin (point) end (match-end 0))
                      (setq begin (line-beginning-position)
                            end (line-end-position)))

                    (add-text-properties begin end (list 'face 'underline
                                                         'ereader-annotation annotation)))))
              (cl-flet ((message (&rest args) nil)) (org-next-link))))))))
  (read-only-mode 1))

;; TODO with-silent-modifications
(defun ereader-hide-annotation ()
	"Hide annotation at point."
  (interactive)
  (read-only-mode -1)
  (let ((annotation (get-text-property (point) 'ereader-annotation)))
    (unless annotation (error "No annotation here"))
    (save-excursion
      (dotimes (_ (length (s-lines annotation)))
        (move-beginning-of-line nil)
        (picture-forward-column shr-width)
        (delete-region (point) (line-end-position))
        (forward-line)))
    (read-only-mode 1)))

(defun ereader-show-annotation ()
	"Show annotation at point."
  (interactive)
  (ereader-hide-annotation)
  (read-only-mode -1)
  (save-excursion
    (let ((annotation (get-text-property (point) 'ereader-annotation))
          lines)
      (unless annotation (error "No annotation here"))
      (setq lines (s-lines annotation))
      (dolist (l lines)
        (move-beginning-of-line nil)
        (picture-forward-column shr-width)
        (delete-region (point) (line-end-position))
        (picture-forward-column 5)
        (insert l)
        (forward-line))))
  (read-only-mode 1))

(defun ereader-hide-all-annotations ()
	"Hide all annotations in the document."
  (interactive)
  (dolist (p ereader-annotations)
    (goto-char p)
    (ereader-hide-annotation)))

(defun ereader-show-all-annotations ()
	"Show all annotations in the document."
  (interactive)
  (dolist (m ereader-annotations)
    (goto-char (marker-position m))
    (ereader-show-annotation)))

(defface ereader-link
  '((t (:inherit link)))
  "Font for link elements."
  :group 'ereader)

(defvar ereader-link-map
  (let ((map (make-sparse-keymap)))
    (define-key map [follow-link] 'mouse-face)
    (define-key map "\r" 'ereader-follow-link)
    (define-key map "v" 'ereader-follow-link)
    (define-key map [mouse-2] 'ereader-follow-link)
    map))


(defun ereader-html-tag-title (cont)
  ;; Don't display these
  )

(defun ereader-html-tag-a (cont)
  (let ((url (dom-attr cont 'href))
        (start (point)))
    (shr-generic cont)
    ;; TODO for non-local urls fall back on original shr function

    ;; Url points relative to current directory
    (setq url (file-relative-name
               (concat (file-name-directory ereader--current-source-file) url)
               ereader-base))

    (add-text-properties start (point)
                         (list 'ereader-target url
                               ;; 'help-echo TITLE ;; TODO Get from TOC or something
                               'follow-link t
                               'mouse-face 'highlight
                               'keymap ereader-link-map
                               'face 'ereader-link
                               ))))

(defvar ereader--current-source-file nil "Internal: Path to HTML currently being parsed.")

;; TODO Support SVGs that contain relative links
(defun ereader-html-tag-img (cont)
  (let ((url (dom-attr cont 'src))
				(basedir (file-name-directory ereader--current-source-file)))
		(insert-image
		 (create-image (expand-file-name url basedir)))))

(defvar-local ereader-links '()
  "Alist of link names to marker destinations")

(defvar-local ereader-html-current-file nil
  "Implicity paramater to `ereader-html-tag-div', which needs to
  know the name of the file being parsed for the link target
  name")


(defvar-local ereader-chapters nil
  "Store chapters for an ereader buffer in the form (linkname, chapter)")

(defadvice shr-descend (before ereader-anchor-storage activate)
  "Store link targets for `ereader-mode'."
  (when (and (listp dom) (equal major-mode 'ereader-mode))
    (let ((id (cdr
							 (or (assq :id (cdr dom))			 ;; Emacs 24
									 (assq 'id (cdr dom))		 ;; Emacs 25
									 ))))
      (when id
        (add-to-list 'ereader-links
                     (cons (format "%s#%s" ereader-html-current-file id)
                           (set-marker (make-marker) (point))))))))

(defun ereader-follow-link ()
	"Follow an link, for example from the Table of Contents."
  (interactive)
  (push-mark)
  (let ((target (url-unhex-string (get-text-property (point) 'ereader-target))))
    (if (string-prefix-p "http" target)
        (browse-url  target)
      (let ((target-mark (cdr (assoc (car (split-string target "#")) ereader-links))))
        (if target-mark
            (progn
              (goto-char (marker-position target-mark))
              (recenter-top-bottom 4))
          (message "Link target not found"))))))

(defun ereader-display-html (cwd item)
  (let* ((filename (cdr (assoc 'href (xml-node-attributes item))))
         (href (concat cwd "/" filename))
         (html nil))

    (with-current-buffer (find-file-noselect href nil 'rawfile)
			(setq ereader--current-source-file href)
      (setq html (libxml-parse-xml-region (point-min) (point-max)))
      (kill-buffer))

    (add-to-list 'ereader-links
                 (cons filename (set-marker (make-marker) (point))))
    (let ((ereader-html-current-file filename)
          (shr-external-rendering-functions '((a . ereader-html-tag-a)
                                              (img . ereader-html-tag-img)
                                              (title . ereader-html-tag-title))))
      (shr-insert-document html))))

(defun ereader-chapter-position (c)
  (if (and c (car c))
      (let ((link (assoc (car c) ereader-links)))
        (if (cdr link)
            (marker-position (cdr link))
          0))
    0))

(defun ereader-read-epub (epub-filename)
  (let ((extracted-dir (concat (make-temp-file
                                (concat (file-name-base epub-filename) "-")
                                'directory) "/"))
        opmf-file content manifest manifest-items spine toc-id toc-html root-dir)
    (setq ereader-base extracted-dir)

    (call-process "unzip" nil nil nil "-d" extracted-dir epub-filename)

    (with-current-buffer
        (find-file-noselect (concat extracted-dir "/META-INF/container.xml") nil 'rawfile)
      (setq opmf-file
            (concat extracted-dir
                    (cdr
                     (assq 'full-path
                           (xml-node-attributes
                            (xml+-query-first (libxml-parse-xml-region (point-min) (point-max))
                                              '((container) > (rootfiles) > (rootfile)))))))
            root-dir (file-name-directory opmf-file))
      (kill-buffer))

    (with-current-buffer (find-file-noselect opmf-file nil 'rawfile)
      (setq content (libxml-parse-xml-region (point-min) (point-max)))
      (kill-buffer))

    ;; Save metadata
    (setq ereader-meta-creator (xml+-node-text
                                (xml+-query-first content '(> (package) > (metadata) >
                                                              (creator)))))
    (setq ereader-meta-title (xml+-node-text
                              (xml+-query-first content '(> (package) > (metadata) >
                                                            (title)))))
    (setq ereader-meta-subject (xml+-node-text
                                (xml+-query-first content '(> (package) > (metadata) >
                                                              (subject)))))
    (setq ereader-meta-isbn (xml+-node-text
                             (xml+-query-first content
                                               '(> (package) > (metadata) >
                                                   (identifier :scheme "ISBN")))))
    (setq ereader-meta-publisher (xml+-node-text
                                  (xml+-query-first content '((metadata) > (publisher)))))

    ;; Parse Table of Contents
    (let (toc-file toc-el toc-href)
      (setq toc-el (xml+-query-first content '((guide) (reference :type "toc"))))
      (when toc-el
        (setq toc-href
              (s-split "#"
                       (cdr
                        (assq 'href
                              (xml-node-attributes
                               toc-el))))))


      (setq toc-file (car toc-href))
      (setq toc-id   (cadr toc-href))
      (with-current-buffer
          (find-file-noselect (concat root-dir "/" toc-file) nil 'rawfile)
        (setq toc-html (libxml-parse-html-region (point-min) (point-max)))
        (kill-buffer)))

    (dolist (link (xml+-query-all toc-html '((body)
                                             ;; TODO get a structured outline
																						 (a))))
      (add-to-list 'ereader-chapters
                   (cons
                    (cdr (assq 'href (xml-node-attributes link)))
                    (xml+-node-text link))))

		;; The manifest section of content.opmf describes resources (chapters,
		;; images, table of contents etc), and the spine puts them in order
		(setq manifest-items nil)
    (setq manifest (assoc 'manifest (xml-node-children content)))
    (dolist (item (xml-node-children manifest))
			(add-to-list 'manifest-items (cons (cdr (assoc 'id (xml-node-attributes item)))
																				 item)))

		(setq spine (assoc 'spine (xml-node-children content)))
    (dolist (pos (xml-node-children spine))
			(let* ((id (cdr (assoc 'idref (xml-node-attributes pos))))
						 (item (cdr (assoc id manifest-items))))
				(let ((interpreter (cdr (assoc
																 (cdr (assoc 'media-type (xml-node-attributes item)))
																 ereader-media-types))))
					(when interpreter
            (condition-case err
                (funcall interpreter root-dir item)
              (error
               (insert (format "error: %S\n" err))
               (insert (format "from literal contents: %S" item))))
						(insert "\n")))))

    ;; We've found out where the chapters are; now put them in order
    (sort
     ereader-chapters
     (lambda (a b) (< (ereader-chapter-position a)
                      (ereader-chapter-position b))))))

(defun ereader-current-chapter ()
  (cdr (let ((possibilities ereader-chapters))
         (when possibilities
           (while (and possibilities (car possibilities) (cl-second possibilities)
                       (or (< (point) (ereader-chapter-position
                                       (car possibilities)))
                           (> (point) (ereader-chapter-position
                                       (cl-second possibilities)))))
             (setq possibilities (cdr possibilities)))
           (car possibilities)))))

;; TODO Sometimes buries buffer
(defun ereader-message-chapter ()
	"Display the name of the current chapter."
  (interactive)
  (message (ereader-current-chapter)))

(defun ereader-goto-chapter ()
	"Prompt for a chapter from the TOC, and go there."
  (interactive)
  (let* ((chapter-name (completing-read "Open chapter: " (-map 'cdr ereader-chapters)))
         (chapter (rassoc chapter-name ereader-chapters))
         (link (assoc (car chapter) ereader-links)))
    (goto-char (marker-position (cdr link)))
    (recenter-top-bottom 4)))

(defvar ereader-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "G" #'ereader-goto-chapter)
    (define-key map "g" #'ereader-goto-chapter)
    (define-key map "c" #'ereader-message-chapter)
    (define-key map "R" #'ereader-load-annotations)
    (define-key map "l" #'org-store-link)
    (define-key map (kbd "S-SPC") #'scroll-down-command)
    (define-key map " " #'scroll-up-command)
    (define-key map "a" #'ereader-show-annotation)
    (define-key map "A" #'ereader-hide-annotation)
    (define-key map "m" #'ereader-show-all-annotations)
    (define-key map "M" #'ereader-hide-all-annotations)
    map))

(defun ereader-write-file (&optional file)
  (error "Saving not yet supported"))

(define-derived-mode ereader-mode view-mode "Ereader"
  "Major mode for reading ebooks
\\{ereader-mode-map}"
  (auto-save-mode 0)
  (setq truncate-lines 1)
  (buffer-disable-undo)
  (make-local-variable 'ereader-links)
	(make-local-variable 'revert-buffer-function) ;; TODO
  (make-local-variable 'require-final-newline)
	(setq require-final-newline nil)
	(make-local-variable 'local-enable-local-variables)
	(setq local-enable-local-variables nil)

  (add-hook 'write-contents-functions 'ereader-write-file nil t)


  (setq buffer-read-only nil)
  (delete-region (point-min) (point-max))
  (save-excursion
    (ereader-read-epub (buffer-file-name)))

  (setq buffer-read-only t)
  (set-buffer-modified-p nil))

(add-to-list 'auto-mode-alist '("\\.epub$" . ereader-mode))

(provide 'ereader)

;;; ereader.el ends here
