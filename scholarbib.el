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
    (define-key map "j" 'org-forward-heading-same-level)
    (define-key map "k" 'org-backward-heading-same-level)
    (define-key map "b" 'retrieve-bibtex)
    map))
(define-derived-mode scholar-mode org-mode "google scholar results"
  (use-local-map scholar-local-mode-map)
  ;;(setq buffer-read-only t)
  )

(defun retrieve-bibtex ()
  (interactive)
  (let* ((index (line-number-at-pos))
         (buf (url-retrieve-synchronously (concat "http://scholar.google.com" (nth index bib-urls)))))
    (with-current-buffer buf
      (delete-response-header)
      (bibtex-mode))
    (switch-to-buffer-other-window buf)))

(defun delete-response-header ()
  "Delete response's header in order to feed into libxml parser"
  (let (header-end)
    (goto-char (point-min))
    (delete-region (point-min)
                   (1- (search-forward "@")))
    (goto-char (point-min))))

(let ((url-request-method "GET")
      (url-request-extra-headers '(("Cookie" . "GSP=ID=70b16c23a6d247db:CF=4"))))
  (switch-to-buffer (url-retrieve-synchronously (concat  "http://scholar.google.com/scholar?q=" (url-hexify-string "energy+aware")))))

(with-current-buffer (find-file-noselect "~/tmp/scholar.html")
  (let ((retval))
    (goto-char (point-min))
    (while (re-search-forward "\\(/scholar\.bib.*?\\)\"" nil t)
      (setq retval (cons (match-string-no-properties 1) retval)))
    (nreverse retval)))

(with-current-buffer (find-file-noselect "~/tmp/scholar.html")
  (let ((retval))
    (goto-char (point-min))
    (while (re-search-forward "<h3.*?</h3>" nil t)
      (setq retval (cons (match-string-no-properties 0) retval)))
    (setq retval (mapcar (lambda (s) (replace-regexp-in-string "<.*?>\\|\\[.*?\\]" "" s)) retval))
    (message "%s" (nreverse retval))))

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
    (setq retval (mapcar (lambda (s) (replace-regexp-in-string "<.*?>\\|\\[.*?\\]" "" s)) retval))
    (nreverse retval)))

(defun get-subtitles ()
  (let (retval)
    (goto-char (point-min))
    (while (re-search-forward "<div class=\"gs_a\">\\(.*?\\)</div>" nil t)
      (setq retval (cons (match-string-no-properties 1) retval)))
    (setq retval (mapcar (lambda (s) (replace-regexp-in-string "&hellip;" "..." (replace-regexp-in-string "<.*?>\\|\\[.*?\\]" "" s))) retval))
    (nreverse retval)))

(let (bib-urls titles subtitles (buf (generate-new-buffer "scholar")))
  (with-current-buffer (find-file-noselect "~/tmp/scholar.html")
    (setq bib-urls (get-bib-urls))
    (setq titles (get-titles))
    (setq subtitles (get-subtitles)))
  (with-current-buffer buf
    (beginning-of-buffer)
    (scholar-mode)
    (dotimes (i (length titles))
      (message "%d" i)
      (org-insert-heading)
      (insert (nth i titles) "\n")
      (indent-according-to-mode)
      (insert (nth i subtitles) "\n")))
  (switch-to-buffer-other-window buf))


(provide 'scholarbib)
;;; scholarbib.el ends here
