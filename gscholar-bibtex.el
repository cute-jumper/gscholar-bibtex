;;; gscholar-bibtex.el --- Retrieve BibTeX from Google Scholar

;; Copyright (C) 2014  Junpeng Qiu

;; Author: Junpeng Qiu <qjpchmail@gmail.com>
;; Keywords: convenience

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

;;

;;; Code:

(defvar gscholar-bibtex-version "0.1"
  "gscholar-bibtex version number")

(defvar gscholar-bibtex-caller-buffer nil
  "Buffer that calls gscholar-bibtex")

(defvar gscholar-bibtex-urls-cache nil
  "Cache for all the urls of BibTeX entries")

(defvar gscholar-bibtex-entries-cache nil
  "Cache for the retrieved BibTeX entries")

(defvar gscholar-bibtex-database-file nil
  "Default BibTeX database file")

(defvar gscholar-bibtex-item-height 3
  "The height for each item")

(defvar gscholar-bibtex-result-buffer-name "*Google Scholar Search Results*"
  "Buffer name for Google Scholar search results")

(defvar gscholar-bibtex-entry-buffer-name "*BibTeX entry from Google Scholar*"
  "Buffer name for BibTeX entry")

(defvar gscholar-bibtex-help
  "[n/p] next/previous; [TAB] show BibTeX entry; [A/W] append/write to database;\
 [a/w] append/write to file; [c] close BibTeX entry window; [q] quit;"
"Help string for gscholar-bibtex")

;; Face related
(defface gscholar-bibtex-title
  '((t (:height 1.4 :foreground "light sea green")))
  "Face for title")

(defface gscholar-bibtex-subtitle
  '((t (:height 1.0)))
  "Face for subtitle")

(defvar gcholar-bibtex-highlight-item-overlay
  (let ((ov (make-overlay 1 1)))
    (overlay-put ov 'face 'highlight)
    ov)
  "Overlay for item highlight")

(defun gscholar-bibtex--move-to-line (N)
  (goto-char (point-min))
  (forward-line (1- N)))

(defun gscholar-bibtex-prettify-title (s)
  (propertize s 'face 'gscholar-bibtex-title))

(defun gscholar-bibtex-prettify-subtitle (s)
  (propertize s 'face 'gscholar-bibtex-subtitle))

(defun gscholar-bibtex-highlight-current-item-hook ()
  (save-excursion
    (let* ((line (gscholar-bibtex--current-beginning-line))
           (beg (progn (gscholar-bibtex--move-to-line line) (point)))
           (end (progn (gscholar-bibtex--move-to-line (+ line 3)) (point))))
      (move-overlay gcholar-bibtex-highlight-item-overlay beg end
                    (current-buffer))
      (message gscholar-bibtex-help))))

;; Major mode
(setq gscholar-bibtex-local-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map "n" 'gscholar-bibtex-next-item)
        (define-key map "p" 'gscholar-bibtex-previous-item)
        (define-key map (kbd "<tab>") 'gscholar-bibtex-retrieve-bibtex)
        (define-key map "A" 'gscholar-bibtex-append-bibtex-to-database)
        (define-key map "W" 'gscholar-bibtex-write-bibtex-to-database)
        (define-key map "a" 'gscholar-bibtex-append-bibtex-to-file)
        (define-key map "w" 'gscholar-bibtex-write-bibtex-to-file)
        (define-key map "c" 'gcholar-bibtex-quit-entry-window)
        (define-key map "q" 'gscholar-bibtex-quit-gscholar-window)
        map))

(define-derived-mode gscholar-bibtex-mode fundamental-mode "gscholar-bibtex"
  (use-local-map gscholar-bibtex-local-mode-map)
  (setq buffer-read-only t)
  (add-hook 'post-command-hook 'gscholar-bibtex-highlight-current-item-hook
            nil t))

(defun gscholar-bibtex-string-trim (str)
  (while (string-match "\\`^\n+\\|^\\s-+\\|\\s-+$\\|\n+\\|\r+\\|^\r\\'" str)
    (setq str (replace-match "" t t str)))
  str)

(defun gscholar-bibtex-guard ()
  (unless (eq major-mode 'gscholar-bibtex-mode)
    (error "Error: you are not in `gscholar-bibtex-mode'!")))

(defun gscholar-bibtex--current-beginning-line ()
  (1+ (* (/ (1- (line-number-at-pos)) gscholar-bibtex-item-height)
         gscholar-bibtex-item-height)))

(defun gscholar-bibtex-next-item ()
  (interactive)
  (gscholar-bibtex-guard)
  (gscholar-bibtex--move-to-line (+ (gscholar-bibtex--current-beginning-line)
                                    gscholar-bibtex-item-height)))

(defun gscholar-bibtex-previous-item ()
  (interactive)
  (gscholar-bibtex-guard)
  (gscholar-bibtex--move-to-line (- (gscholar-bibtex--current-beginning-line)
                                    gscholar-bibtex-item-height)))

(defun gscholar-bibtex-retrieve-bibtex ()
  (interactive)
  (gscholar-bibtex-guard)
  (let* ((index (/ (1- (line-number-at-pos)) gscholar-bibtex-item-height))
         (bibtex-entry (progn (when (string=
                                     ""
                                     (elt gscholar-bibtex-entries-cache index))
                                (with-current-buffer
                                    (url-retrieve-synchronously
                                     (concat "http://scholar.google.com"
                                             (nth
                                              index
                                              gscholar-bibtex-urls-cache)))
                                  (gscholar-bibtex-delete-response-header)
                                  (aset gscholar-bibtex-entries-cache index
                                        (buffer-string))))
                              (elt gscholar-bibtex-entries-cache index)))
         (entry-buffer (get-buffer-create gscholar-bibtex-entry-buffer-name))
         (entry-window (get-buffer-window entry-buffer))
         (gscholar-window (selected-window)))
    (with-current-buffer entry-buffer
      (erase-buffer)
      (insert bibtex-entry)
      (bibtex-mode)
      (goto-char (point-min)))
    (unless entry-window
      (select-window (split-window-below))
      (switch-to-buffer entry-buffer)
      (select-window gscholar-window))))

(defun gscholar-bibtex--write-bibtex-to-file-impl (&optional append)
  (gscholar-bibtex-guard)
  (gscholar-bibtex-retrieve-bibtex)
  (unless gscholar-bibtex-database-file
    (setq gscholar-bibtex-database-file
          (call-interactively
           '(lambda (filename)
              (interactive "Fgscholar-bibtex database file: ")
              filename))))
  (if gscholar-bibtex-database-file
      (with-current-buffer (get-buffer gscholar-bibtex-entry-buffer-name)
        (write-region nil nil gscholar-bibtex-database-file append))
    (error "Please set `gscholar-bibtex-database-file' first.")))

(defun gscholar-bibtex-append-bibtex-to-database ()
  (interactive)
  (gscholar-bibtex--write-bibtex-to-file-impl t))

(defun gscholar-bibtex-write-bibtex-to-database ()
  (interactive)
  (gscholar-bibtex--write-bibtex-to-file-impl nil))

(defun gscholar-bibtex-append-bibtex-to-file ()
  (interactive)
  (gscholar-bibtex-guard)
  (gscholar-bibtex-retrieve-bibtex)
  (let ((filename (call-interactively
                   '(lambda (filename)
                      (interactive "FAppend bibtex entry to file: ")
                      filename))))
    (with-current-buffer (get-buffer gscholar-bibtex-entry-buffer-name)
      (write-region nil nil filename t))))

(defun gscholar-bibtex-write-bibtex-to-file ()
  (interactive)
  (gscholar-bibtex-guard)
  (gscholar-bibtex-retrieve-bibtex)
  (with-current-buffer (get-buffer gscholar-bibtex-entry-buffer-name)
    (write-region nil nil (call-interactively
                           '(lambda (filename)
                              (interactive "FWrite bibtex entry to file: ")
                              filename)))))

(defun gcholar-bibtex-quit-entry-window ()
  (interactive)
  (gscholar-bibtex-guard)
  (let ((entry-window (get-buffer-window gscholar-bibtex-entry-buffer-name)))
    (when entry-window
      (select-window entry-window)
      (delete-window))))

(defun gscholar-bibtex-quit-gscholar-window ()
  (interactive)
  (gscholar-bibtex-guard)
  (let ((gscholar-window (selected-window))
        (entry-window (get-buffer-window gscholar-bibtex-entry-buffer-name))
        (caller-window (get-buffer-window gscholar-bibtex-caller-buffer)))
    (when entry-window
      (select-window entry-window)
      (delete-window)
      (select-window gscholar-window))
    (if (or (eq caller-window gscholar-window)
            (eq caller-window entry-window)
            (not (buffer-live-p gscholar-bibtex-caller-buffer)))
        (next-buffer)
      (if caller-window
          (progn (delete-window) (select-window caller-window))
        (switch-to-buffer gscholar-bibtex-caller-buffer))))
  (message ""))

(defun gscholar-bibtex-delete-response-header ()
  "Delete response's header in order to feed into libxml parser"
  (let (header-end)
    (ignore-errors
      (goto-char (point-min))
      (delete-region (point-min)
                     (1+ (search-forward-regexp "^$")))
      (goto-char (point-min)))))

(defun gscholar-bibtex-regex-search (buffer callback)
  (with-current-buffer buffer
    (let (retval)
      (goto-char (point-min))
      (nreverse (funcall callback retval)))))

(defun gscholar-bibtex-get-bibtex-urls (buffer)
  (gscholar-bibtex-regex-search
   buffer
   '(lambda (retval)
      (while (re-search-forward "\\(/scholar\.bib.*?\\)\"" nil t)
        (setq retval
              (cons
               (replace-regexp-in-string
                "amp;" "" (match-string-no-properties 1)) retval)))
      retval)))

(defun gscholar-bibtex-get-titles (buffer)
  (gscholar-bibtex-regex-search
   buffer
   '(lambda (retval)
      (while (re-search-forward "<h3.*?</h3>" nil t)
        (setq retval (cons (match-string-no-properties 0) retval)))
      (setq retval
            (mapcar
             (lambda (s)
               (gscholar-bibtex-string-trim
                (replace-regexp-in-string "<.*?>\\|\\[.*?\\]" "" s))) retval))
      retval)))

(defun gscholar-bibtex-get-subtitles (buffer)
  (gscholar-bibtex-regex-search
   buffer
   '(lambda (retval)
      (while (re-search-forward "<div class=\"gs_a\">\\(.*?\\)</div>" nil t)
        (setq retval (cons (match-string-no-properties 1) retval)))
      (setq retval
            (mapcar
             (lambda (s)
               (gscholar-bibtex-string-trim
                (replace-regexp-in-string
                 "&hellip;"
                 "..."
                 (replace-regexp-in-string "<.*?>\\|\\[.*?\\]" "" s)))) retval))
      retval)))

(defun gscholar-bibtex (query)
  (interactive "sQuery: ")
  (let* ((url-request-method "GET")
         (random-id (format "%016x" (random (expt 16 16))))
         (url-request-extra-headers
          `(("Cookie" . ,(concat "GSP=ID=" random-id ":CF=4"))))
         (query-result-buffer
          (url-retrieve-synchronously
           (concat  "http://scholar.google.com/scholar?q="
                    (url-hexify-string
                     (replace-regexp-in-string " " "\+" query)))))
         (gscholar-buffer (get-buffer-create gscholar-bibtex-result-buffer-name)))
    (with-current-buffer query-result-buffer
      (gscholar-bibtex-delete-response-header))
    (setq gscholar-bibtex-caller-buffer (current-buffer))
    (setq gscholar-bibtex-urls-cache
          (gscholar-bibtex-get-bibtex-urls query-result-buffer))
    (setq titles (gscholar-bibtex-get-titles query-result-buffer))
    (setq subtitles (gscholar-bibtex-get-subtitles query-result-buffer))
    (setq gscholar-bibtex-entries-cache
          (make-vector (length gscholar-bibtex-urls-cache) ""))
    (unless (get-buffer-window gscholar-buffer)
      (switch-to-buffer-other-window gscholar-buffer))
    (setq buffer-read-only nil)
    (erase-buffer)
    (goto-char (point-min))
    (dotimes (i (length titles))
      (insert "* " (gscholar-bibtex-prettify-title (nth i titles)))
      (newline-and-indent)
      (insert "  "
              (gscholar-bibtex-prettify-subtitle (nth i subtitles)) "\n\n"))
    (goto-char (point-min))
    (gscholar-bibtex-mode)))

(when (and (boundp 'evil-emacs-state-modes)
           (not (member 'gscholar-bibtex-mode evil-emacs-state-modes)))
  (add-to-list 'evil-emacs-state-modes 'gscholar-bibtex-mode))

(provide 'gscholar-bibtex)
;;; gscholar-bibtex.el ends here
