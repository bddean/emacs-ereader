(require 'dash)
(require 's)
(require 'xml+)

(defvar ereader-media-types
  '(("image/jpeg" . ereader-display-image)
    ("application/xhtml+xml" . ereader-display-html)))

(defun ereader-display-image (cwd item)
  "Insert image item described by xml node ITEM into the buffer"
  (insert-image
   (create-image (concat cwd "/" (cdr (assoc 'href (xml-node-attributes item)))))
   (cdr (assoc 'id (xml-node-attributes item)))
   (insert "\n")))

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

(defun ereader-html-tag-div (cont)
  (let ((id (assq :id cont)))
    (when id
      (add-to-list 'ereader-links
                   (cons (format "%s#%s" ereader-html-current-file (cdr id))
                         (set-marker (make-marker) (point)))))
    (shr-tag-div cont)))

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

    (let ((ereader-html-current-file (file-name-nondirectory filename))
          (shr-external-rendering-functions
           '((a . ereader-html-tag-a)
             (div . ereader-html-tag-div))))
      (shr-insert-document html))))


(defun ereader-chapter-position (c)
  "c is cons cell TODO doc"
  (if (and c (car c))
      (let ((link (assoc (car c) ereader-links)))
        (if (cdr link)
            (marker-position (cdr link))
          0))
    0)))

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
																						 (div :id "toc")
																						 ((p :class "tocfm") (p :class "toc"))
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
         (while (and possibilities (car possibilities)
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
    map))

(defun ereader-write-file (&optional file)
  (error "Saving not yet supported"))

(define-derived-mode ereader-mode view-mode "Ereader"
  "Major mode for reading ebooks
\\{ereader-mode-map}"
  (auto-save-mode 0)
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
