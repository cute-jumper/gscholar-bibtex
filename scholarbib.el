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

(setq scholar-local-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "j" '(lambda () (interactive) (forward-line 3)))
    (define-key map "k" '(lambda () (interactive) (forward-line -3)))
    (define-key map (kbd "C-n") '(lambda () (interactive) (forward-line 3)))
    (define-key map (kbd "C-p") '(lambda () (interactive) (forward-line -3)))
    (define-key map "b" 'retrieve-bibtex)
    map))
(define-derived-mode scholar-mode fundamental-mode "google scholar results"
  (use-local-map scholar-local-mode-map)
  (setq buffer-read-only t)
  (setq cursor-type 'bar)
  (add-hook 'post-command-hook 'highlight-current-item-hook nil t))
(defun string-trim (str)
  (while (string-match "\\`^\n+\\|^\\s-+\\|\\s-+$\\|\n+\\|\r+\\|^\r\\'" str)
    (setq str (replace-match "" t t str)))
  str)

(defface mytitle '((t (:height 1.4 :foreground "light sea green"))) "face for title")
(defface mysubtitle '((t (:height 1.0))) "face for subtitle")
(defface highlight-item '((t (:background "wheat"))) "face for overlay")
(defvar highlight-item-overlay (make-overlay 1 1))
(overlay-put highlight-item-overlay 'face 'highlight)
(defun highlight-current-item-hook ()
  (save-excursion
    (let ((beg (progn (beginning-of-line) (point)))
          (end (progn (forward-line 3) (beginning-of-line) (point))))
      (move-overlay highlight-item-overlay beg end (current-buffer))
      (message "[j] next; [k] previous"))))

(defun retrieve-bibtex ()
  (interactive)
  (let* ((index (/ (1- (line-number-at-pos)) 3))
         (buf-content (with-current-buffer (url-retrieve-synchronously (concat "http://scholar.google.com" (nth index bib-urls))) (delete-response-header) (buffer-string)))
         (buf (get-buffer-create "*bib from google scholar*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert buf-content)
      (bibtex-mode))
    (switch-to-buffer-other-window buf)
    (switch-to-buffer-other-window (get-buffer "scholar"))))

(defun delete-response-header ()
  "Delete response's header in order to feed into libxml parser"
  (let (header-end)
    (ignore-errors
      (goto-char (point-min))
      (delete-region (point-min)
                     (1- (search-forward "@")))
      (goto-char (point-min)))))

;; (let ((url-request-method "GET")
;;       (url-request-extra-headers '(("Cookie" . "GSP=ID=70b16c23a6d247db:CF=4"))))
;;   (switch-to-buffer (url-retrieve-synchronously (concat  "http://scholar.google.com/scholar?q=" (url-hexify-string "energy+aware")))))

;; (with-current-buffer (find-file-noselect "~/tmp/scholar.html")
;;   (let ((retval))
;;     (goto-char (point-min))
;;     (while (re-search-forward "\\(/scholar\.bib.*?\\)\"" nil t)
;;       (setq retval (cons (match-string-no-properties 1) retval)))
;;     (nreverse retval)))

;; (with-current-buffer (find-file-noselect "~/tmp/scholar.html")
;;   (let ((retval))
;;     (goto-char (point-min))
;;     (while (re-search-forward "<h3.*?</h3>" nil t)
;;       (setq retval (cons (match-string-no-properties 0) retval)))
;;     (setq retval (mapcar (lambda (s) (replace-regexp-in-string "<.*?>\\|\\[.*?\\]" "" s)) retval))
;;     (message "%s" (nreverse retval))))

(defun get-bib-urls ()
  (let (retval)
    (goto-char (point-min))
    (while (re-search-forward "\\(/scholar\.bib.*?\\)\"" nil t)
      (setq retval (cons (replace-regexp-in-string "amp;" "" (match-string-no-properties 1)) retval)))
    (nreverse retval)))

(defun get-titles ()
  (let (retval)
    (goto-char (point-min))
    (while (re-search-forward "<h3.*?</h3>" nil t)
      (setq retval (cons (match-string-no-properties 0) retval)))
    (setq retval (mapcar (lambda (s) (string-trim (replace-regexp-in-string "<.*?>\\|\\[.*?\\]" "" s))) retval))
    (nreverse retval)))

(defun get-subtitles ()
  (let (retval)
    (goto-char (point-min))
    (while (re-search-forward "<div class=\"gs_a\">\\(.*?\\)</div>" nil t)
      (setq retval (cons (match-string-no-properties 1) retval)))
    (setq retval (mapcar (lambda (s) (string-trim (replace-regexp-in-string "&hellip;" "..." (replace-regexp-in-string "<.*?>\\|\\[.*?\\]" "" s)))) retval))
    (nreverse retval)))


(defun prettify-title (s)
  (propertize s 'face 'mytitle))
(defun prettify-subtitle (s)
  (propertize s 'face 'mysubtitle))

(let (bib-urls titles subtitles (buf (get-buffer-create "scholar")))
  (with-current-buffer (find-file-noselect "~/tmp/scholar.html")
    (setq bib-urls (get-bib-urls))
    (setq titles (get-titles))
    (setq subtitles (get-subtitles)))
  (switch-to-buffer-other-window buf)
  (setq buffer-read-only nil)
  (erase-buffer)
  (beginning-of-buffer)
  (dotimes (i (length titles))
    (insert "* " (prettify-title (nth i titles)))
    (newline-and-indent)
    (insert "  " (prettify-subtitle (nth i subtitles)) "\n\n"))
  (beginning-of-buffer)    
  (scholar-mode))


(add-to-list 'evil-emacs-state-modes 'scholar-mode)

(provide 'scholarbib)
;;; scholarbib.el ends herek
