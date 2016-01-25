(require 'dash)
 (require 's)

(defvar ereader-media-types
  '(("image/jpeg" . ereader-display-image)
    ("application/xhtml+xml" . ereader-display-html)))

(defun ereader-display-image (cwd item)
  "Insert image item described by xml node ITEM into the buffer"
  (insert-image
   (create-image (concat cwd "/" (cdr (assoc 'href (xml-node-attributes item)))))
   (cdr (assoc 'id (xml-node-attributes item))))
  (insert "\n")
  )

(defface ereader-link
  '((t (:inherit link)))
  "Font for link elements."
  :group 'ereader)

(defun ereader-html-tag-a (cont)
  (print (assq :href cont))

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
                         (set-marker (make-marker) (point))))))
  (print cont)
  (shr-tag-div cont))

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
    (find-file href)
    (setq html (libxml-parse-xml-region (point-min) (point-max)))
    (kill-buffer)

    (let ((ereader-html-current-file (file-name-nondirectory filename))
          (shr-external-rendering-functions
           '((a . ereader-html-tag-a)
             (div . ereader-html-tag-div))))
      (shr-insert-document html))))

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
    (find-file (concat extracted-dir "/content.opf"))
    (setq content (libxml-parse-xml-region (point-min) (point-max)))
    (kill-buffer)

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
      (find-file (concat extracted-dir "/" toc-file))
      (setq toc-html (libxml-parse-html-region (point-min) (point-max)))
      (kill-buffer))

    (dolist (link (xml-query-all toc-html '((body)
                                       (div :id "toc")
                                       ((p :class "tocfm") (p :class "toc"))
                                       (a))))
      (add-to-list 'ereader-chapters
                   (cons 
                    (cdr (assq 'href (xml-node-attributes link)))
                    (xml-node-text link)

                     )))

    ;; TODO handle metadata and spine
    ;; (assoc 'metadata (xml-node-children  content))

    (setq manifest (assoc 'manifest (xml-node-children  content)))
    (dolist (item (xml-node-children manifest))
      (let ((interpreter (cdr (assoc
                               (cdr (assoc 'media-type (xml-node-attributes item)))
                               ereader-media-types))))
        (when interpreter
          (funcall interpreter extracted-dir item)
          (insert "\n"))))

    ;; TODO now that we know the pages of each chapter, sort the chapter list
    ))

(defun ereader-goto-chapter ()
  (interactive)
  (let* ((chapter-name (completing-read "Open chapter: " (-map 'cdr ereader-chapters)))
         (chapter (rassoc chapter-name ereader-chapters))
         (link (assoc (car chapter) ereader-links)))
    (goto-char (marker-position (cdr link)))
    (recenter-top-bottom 4)))

(define-derived-mode ereader-mode special-mode "Ereader"
  "Major mode for reading ebooks
\\{ereader-mode-map}"
  (make-local-variable 'ereader-links)
  (read-only-mode 1))

(defvar ereader-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" 'ereader-goto-chapter)
    map))

(defvar ereader-link-map
  (let ((map (make-sparse-keymap)))
    (define-key map [follow-link] 'mouse-face)
    (define-key map "\r" 'ereader-follow-link)
    (define-key map "v" 'ereader-follow-link)
    (define-key map [mouse-2] 'ereader-follow-link)
    map))

;; Test snippet of code until we hook into find-file properly
(progn
  (when (get-buffer "epub-test")
    (kill-buffer "epub-test"))
  (pop-to-buffer "epub-test")
  
  (ereader-mode)
  (read-only-mode -1)
  (ereader-read-epub
   "/home/ben/notes/MacIntyre, Alasdair/After Virtue_ A Study in Moral Theory, Third Edition/After Virtue_ A Study in Moral Theory, Third Edition - Alasdair MacIntyre.epub")
  (read-only-mode 1))
