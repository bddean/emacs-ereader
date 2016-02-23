(require 'dash)
(require 's)
(require 'xml+)
(require 'picture)

(defvar ereader-media-types
  '(("image/jpeg" . ereader-display-image)
    ("application/xhtml+xml" . ereader-display-html)))

(defun ereader-display-image (cwd item)
  "Insert image item described by xml node ITEM into the buffer"
  (insert-image
   (create-image (concat cwd "/" (cdr (assoc 'href (xml-node-attributes item)))))
   (cdr (assoc 'id (xml-node-attributes item)))
   (insert "\n")))

(defvar ereader-annotation-files
  '(("After Virtue: A Study in Moral Theory, Third Edition" .
     ("/home/ben/notes/ethics/ethics.org"))))

(defvar-local ereader-annotations '() "List of positions of annotations")

;; TODO requires org-ebook
(defun ereader-load-annotations ()
  (interactive)
  (read-only-mode -1)
  (setq ereader-annotations '())
  (let ((ebook-file (buffer-file-name))
        link path path-parts path-quote begin end annotation)
    (dolist (notes (cdr (assoc ereader-meta-title ereader-annotation-files)))
      (with-current-buffer (find-file-noselect notes)
        (save-excursion
          (beginning-of-buffer)
          (let ((org-link-search-failed nil))
            ;; TODO use re-search instead
            (flet ((message (&rest args) nil)) (org-next-link))
            (while (not org-link-search-failed)
              (print (point))
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
              (flet ((message (&rest args) nil)) (org-next-link)))))))
    ;; (setq ereader-annotations annotation-positions)
    )
  (read-only-mode 1))

;; TODO with-silent-modifications
(defun ereader-hide-annotation ()
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
  (interactive)
  (dolist (p ereader-annotations)
    (goto-char p)
    (ereader-hide-annotation)))

(defun ereader-show-all-annotations ()
  (interactive)
  (dolist (m ereader-annotations)
    (goto-char (marker-position m))
    (ereader-show-annotation)
    )
  )

;; Variables for metadata
(defvar-local ereader-meta-creator nil "creator of book")
(defvar-local ereader-meta-title nil "title of book")
(defvar-local ereader-meta-subject nil "subject of book")
(defvar-local ereader-meta-isbn nil "isbn of book")
(defvar-local ereader-meta-publisher nil "publisher of book")

(defface ereader-link
  '((t (:inherit link)))
  "Font for link elements."
  :group 'ereader)

(defun ereader-html-tag-a (cont)
  (let ((url (cdr (assq :href cont)))
        (start (point)))
    (shr-generic cont)
    ;; TODO for non-local urls fall back on shr function
    (add-text-properties start (point)
                         (list 'ereader-target url
                               ;; 'help-echo TITLE ;; TODO Get from TOC or something
                               'follow-link t ;; TODO What does this do?
                               'mouse-face 'highlight
                               'keymap ereader-link-map
                               'face 'ereader-link
                               ))))

(defvar-local ereader-links '()
  "Alist of link names to marker destinations")

(defvar-local ereader-html-current-file nil
  "Implicity paramater to `ereader-html-tag-div', which needs to
  know the name of the file being parsed for the link target
  name")


(defvar-local ereader-chapters nil
  "Store chapters for an ereader buffer in the form (linkname, chapter)")

(defadvice shr-descend (before ereader-anchor-storage activate)
  "Store link targets for ereader-mode"
  (when (equal major-mode 'ereader-mode)
    (let ((id (assq :id (cdr dom))))
      (when id
        (add-to-list 'ereader-links
                     (cons (format "%s#%s" ereader-html-current-file (cdr id))
                           (set-marker (make-marker) (point))))))))

(defun ereader-follow-link ()
  (interactive)
  (push-mark)
  (let ((target (get-text-property (point) 'ereader-target)))
    (goto-char (marker-position
                (cdr (assoc target ereader-links))))
    (recenter-top-bottom 4)))

(defun ereader-display-html (cwd item)
  (let* ((filename (cdr (assoc 'href (xml-node-attributes item))))
         (href (concat cwd "/" filename))
         (html nil))

    (with-current-buffer (find-file-noselect href nil 'rawfile)
      (setq html (libxml-parse-xml-region (point-min) (point-max)))
      (kill-buffer))

    (add-to-list 'ereader-links
                 (cons (file-name-nondirectory filename)
                       (set-marker (make-marker) (point))))
    (let ((ereader-html-current-file (file-name-nondirectory filename))
          (shr-external-rendering-functions '((a . ereader-html-tag-a))))
      (shr-insert-document html))))

(defun ereader-chapter-position (c)
  "Get the character position of the chapter represented by cons
cell C"
  (if (and c (car c))
      (let ((link (assoc (car c) ereader-links)))
        (if (cdr link)
            (marker-position (cdr link))
          0))
    0))

(defun ereader-read-epub (epub-filename)
  (let ((extracted-dir (make-temp-file
                        (concat (file-name-base epub-filename) "-")
                        'directory))
        (content nil)
        (manifest nil)
        (guide nil)
        (toc-id nil)
        (toc-html nil))
    (call-process "unzip" nil nil nil "-d" extracted-dir epub-filename)

    ;; content.opmf contains the struture of the epub file
    (with-current-buffer
        (find-file-noselect (concat extracted-dir "/content.opf") nil 'rawfile)
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
    (setq guide (assoc 'guide (xml-node-children content)))
    (let ((toc-file nil)
          (toc-href
           (s-split
            "#"
            (cdr (assq 'href
											 (xml-node-attributes
												(-first (lambda (reference)
																	(equal (cdr (assq 'type (xml-node-attributes reference)))
																				 "toc"))
																(xml-node-children guide))))))))


      (setq toc-file (car toc-href))
      (setq toc-id   (cadr toc-href))
      (with-current-buffer
          (find-file-noselect (concat extracted-dir "/" toc-file) nil 'rawfile)
        (setq toc-html (libxml-parse-html-region (point-min) (point-max)))
        (kill-buffer)))

    (dolist (link (xml+-query-all toc-html '((body)
                                             ;; TODO get a structured outline
																						 (a))))
      (add-to-list 'ereader-chapters
                   (cons 
                    (cdr (assq 'href (xml-node-attributes link)))
                    (xml+-node-text link))))

    (setq manifest (assoc 'manifest (xml-node-children  content)))
    (dolist (item (xml-node-children manifest))
      (let ((interpreter (cdr (assoc
                               (cdr (assoc 'media-type (xml-node-attributes item)))
                               ereader-media-types))))
        (when interpreter
          (funcall interpreter extracted-dir item)
          (insert "\n"))))

    ;; We've found out where the chapters are; now put them in order
    (sort 
     ereader-chapters
     (lambda (a b) (< (ereader-chapter-position a)
                      (ereader-chapter-position b))))))

(defun ereader-current-chapter ()
  (cdr (let ((possibilities ereader-chapters))
         (while (and possibilities (car possibilities) (second possibilities)
                     (or (< (point) (ereader-chapter-position
                                     (car possibilities)))
                         (> (point) (ereader-chapter-position
                                     (cl-second possibilities)))))
           (setq possibilities (cdr possibilities)))
         (car possibilities))))

(defun ereader-message-chapter ()
  (interactive)
  (message (ereader-current-chapter)))

(defun ereader-goto-chapter ()
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
    (define-key map "r" #'ereader-load-annotations)
    (define-key map "l" #'org-store-link)
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

(put 'ereader-mode 'mode-class 'special)

(add-to-list 'auto-mode-alist '("\\.epub$" . ereader-mode))

(defvar ereader-link-map
  (let ((map (make-sparse-keymap)))
    (define-key map [follow-link] 'mouse-face)
    (define-key map "\r" 'ereader-follow-link)
    (define-key map "v" 'ereader-follow-link)
    (define-key map [mouse-2] 'ereader-follow-link)
    map))

(provide 'ereader-mode)
