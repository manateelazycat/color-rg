;;; color-rg.el --- Search and refacotry code with rg

;; Filename: color-rg.el
;; Description: Search and refacotry code with rg
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-08-26 14:22:12
;; Version: 0.1
;; Last-Updated: 2018-08-26 14:22:12
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/color-rg.el
;; Keywords:
;; Compatibility: GNU Emacs 27.0.50
;;
;; Features that might be required by this library:
;;
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Search and refacotry code with rg.
;;
;; I'm a big fans of color-moccur.el, this extension is used for tribute color-moccur.el !!!
;;

;;; Installation:
;;
;; Put color-rg.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'color-rg)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET color-rg RET
;;

;;; Change log:
;;
;; 2018/08/26
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require


;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OS Config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (featurep 'cocoa)
  ;; Initialize environment from user's shell to make eshell know every PATH by other shell.
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Group ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup color-rg nil
  "Search and refacotry code base on ripgrep."
  :group 'color-rg)

(defcustom color-rg-buffer "*color-rg*"
  "The buffer name of search result."
  :type 'string
  :group 'color-rg)

(defcustom color-rg-temp-buffer " *color-rg temp* "
  "The buffer name of clone temp buffer"
  :type 'string
  :group 'color-rg)

(defcustom color-rg-mode-hook '()
  "color-rg mode hook."
  :type 'hook
  :group 'color-rg-mode)

(defface color-rg-header-line-text
  '((t (:foreground "Green3" :bold t)))
  "Face for header line text."
  :group 'color-rg)

(defface color-rg-header-line-keyword
  '((t (:foreground "Gold" :bold t)))
  "Face for header line keyword."
  :group 'color-rg)

(defface color-rg-header-line-directory
  '((t (:foreground "DodgerBlue" :bold t)))
  "Face for header line directory."
  :group 'color-rg)

(defface color-rg-header-line-edit-mode
  '((t (:foreground "Gold" :bold t)))
  "Face for header line edit mode."
  :group 'color-rg)

(defface color-rg-file
  '((t (:foreground "DodgerBlue" :bold t)))
  "Face for filepath."
  :group 'color-rg)

(defface color-rg-line-number
  '((t (:foreground "gray35")))
  "Face for line number."
  :group 'color-rg)

(defface color-rg-column-number
  '((t (:foreground "gray35")))
  "Face for column number."
  :group 'color-rg)

(defface color-rg-position-splitter
  '((t (:foreground "gray25")))
  "Face for position splitter."
  :group 'color-rg)

(defface color-rg-match
  '((t (:foreground "Gold3" :bold t)))
  "Face for keyword match."
  :group 'color-rg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar color-rg-temp-buffers nil
  "The temp buffers use to kill temp buffer after quit color-rg.")

(defvar color-rg-window-configuration nil
  "Save window configuration before search,
used to restore window configuration after finish search.")

(defvar color-rg-hit-count 0
  "Search keyword hit counter.")

(defvar color-rg-regexp-file "^[/\\~].*"
  "Regexp to match filename.")

(defvar color-rg-regexp-split-line "\n\n"
  "Regexp to match empty line between two files.")

(defvar color-rg-regexp-position "^\\([1-9][0-9]*\\):\\([1-9][0-9]*\\):"
  "Regexp to match line/column string.")

(defvar color-rg-changed-lines nil
  "The list that record the changed lines.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; color-rg mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar color-rg-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "j") 'color-rg-jump-next-keyword)
    (define-key map (kbd "k") 'color-rg-jump-prev-keyword)
    (define-key map (kbd "h") 'color-rg-jump-next-file)
    (define-key map (kbd "l") 'color-rg-jump-prev-file)
    (define-key map (kbd "RET") 'color-rg-open-file)
    (define-key map (kbd "e") 'color-rg-enable-edit-mode)
    (define-key map (kbd "q") 'color-rg-quit)
    map)
  "Keymap used by `color-rg-mode'.")

(define-derived-mode color-rg-mode text-mode "color-rg"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'color-rg-mode)
  (setq mode-name "color-rg")
  (color-rg-highlight-keywords)
  (use-local-map color-rg-mode-map)
  (run-hooks 'color-rg-mode-hook)
  (add-hook 'compilation-filter-hook 'color-rg-filter nil t)
  )

(defun color-rg-highlight-keywords ()
  "Highlight keywords."
  ;; Add keywords for highlight.
  (font-lock-add-keywords
   nil
   '(
     ("^\\([1-9][0-9]*\\)\\(:\\)\\([1-9][0-9]*\\)\\(:\\)" 1 'color-rg-line-number)
     ("^\\([1-9][0-9]*\\)\\(:\\)\\([1-9][0-9]*\\)\\(:\\)" 2 'color-rg-position-splitter)
     ("^\\([1-9][0-9]*\\)\\(:\\)\\([1-9][0-9]*\\)\\(:\\)" 3 'color-rg-column-number)
     ("^\\([1-9][0-9]*\\)\\(:\\)\\([1-9][0-9]*\\)\\(:\\)" 4 'color-rg-position-splitter)
     ("^[/\\~].*" . 'color-rg-file)
     ))
  ;; NOTE:
  ;; Because search line maybe just contains *half* of string/comment that make rest content of buffer mark as string.
  ;; So we need turn off comment/string font-lock through set `font-lock-keywords-only'.
  (set (make-local-variable 'font-lock-keywords-only) t)
  ;; Enable font lock.
  (font-lock-mode 1))

(defun color-rg-filter ()
  "Handle match highlighting escape sequences inserted by the rg process.
This function is called from `compilation-filter-hook'."
  (save-excursion
    (forward-line 0)
    (let ((end (point)) beg)
      (goto-char compilation-filter-start)
      (forward-line 0)
      (setq beg (point))
      ;; Only operate on whole lines so we don't get caught with part of an
      ;; escape sequence in one chunk and the rest in another.
      (when (< (point) end)
        (setq end (copy-marker end))
        ;; Highlight filename.
        (while (re-search-forward "^\033\\[[0]*m\033\\[35m\\(.*?\\)\033\\[[0]*m$" end 1)
          (replace-match (concat (propertize (match-string 1)
                                             'face nil 'font-lock-face 'color-rg-file))
                         t t))
        (goto-char beg)

        ;; Highlight rg matches and delete marking sequences.
        (while (re-search-forward "\033\\[[0]*m\033\\[[3]*1m\033\\[[3]*1m\\(.*?\\)\033\\[[0]*m" end 1)
          (replace-match (propertize (match-string 1)
                                     'face nil 'font-lock-face 'color-rg-match)
                         t t)
          (setq color-rg-hit-count (+ color-rg-hit-count 1)))
        ;; Delete all remaining escape sequences
        (goto-char beg)
        (while (re-search-forward "\033\\[[0-9;]*[0mK]" end 1)
          (replace-match "" t t))))
    ))

(defun color-rg-update-header-line ()
  (setq header-line-format (format "%s%s%s%s%s%s"
                                   (propertize "[COLOR-RG] Search '" 'font-lock-face 'color-rg-header-line-text)
                                   (propertize search-keyword 'font-lock-face 'color-rg-header-line-keyword)
                                   (propertize "' in directory: " 'font-lock-face 'color-rg-header-line-text)
                                   (propertize search-directory 'font-lock-face 'color-rg-header-line-directory)
                                   (propertize " Mode: " 'font-lock-face 'color-rg-header-line-text)
                                   (propertize edit-mode 'font-lock-face 'color-rg-header-line-edit-mode)
                                   ))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utils functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun color-rg-search (keyword directory)
  (let* ((search-command (format "rg %s %s --column --color=always" keyword directory)))
    ;; Erase or create search result.
    (when (get-buffer color-rg-buffer)
      (kill-buffer color-rg-buffer))
    (generate-new-buffer color-rg-buffer)
    (setq color-rg-changed-lines nil)
    ;; Run search command.
    (with-current-buffer color-rg-buffer
      ;; Start command.
      (compilation-start search-command 'color-rg-mode)
      ;; Set header line.
      (set (make-local-variable 'search-keyword) keyword)
      (set (make-local-variable 'search-directory) directory)
      (set (make-local-variable 'edit-mode) "View")
      (color-rg-update-header-line)
      (read-only-mode 1)
      )
    ;; Pop search buffer.
    (pop-to-buffer color-rg-buffer)
    (goto-char (point-min))
    ))

(defun color-rg-read-input ()
  (let* ((current-symbol (color-rg-pointer-string))
         (input-string (string-trim (read-string (format "COLOR-RG Search (%s): " current-symbol)))))
    (when (string-blank-p input-string)
      (setq input-string current-symbol))
    input-string))

(defun color-rg-pointer-string ()
  (if (use-region-p)
      ;; Get region string if mark is set.
      (buffer-substring-no-properties (region-beginning) (region-end))
    ;; Get current symbol but remove prefix char before return.
    (let ((current-symbol (thing-at-point 'symbol)))
      (cond ((string-prefix-p "." current-symbol)
             (string-remove-prefix current-symbol))
            ((string-prefix-p "#" current-symbol)
             (string-remove-prefix current-symbol))
            (t current-symbol)))
    ))

(defun color-rg-find-next-position (regexp)
  (save-excursion
    (end-of-line)
    (search-forward-regexp regexp nil t)))

(defun color-rg-get-match-buffer (filepath)
  (catch 'find-match
    (dolist (buffer (buffer-list))
      (when (string-equal (buffer-file-name buffer) filepath)
        (throw 'find-match buffer)))
    nil))

(defun color-rg-current-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))

(defun color-rg-after-change-function (beg end leng-before)
  (let* ((change-line (save-excursion
                        (goto-char beg)
                        (line-number-at-pos)))
         start end
         change-line-content
         original-line-content)
    (with-current-buffer color-rg-buffer
      (save-excursion
        (goto-line change-line)
        (beginning-of-line)
        (search-forward-regexp color-rg-regexp-position nil t)
        (setq start (point))
        (end-of-line)
        (setq end (point))
        (setq change-line-content (buffer-substring-no-properties start end)))
      )
    (with-current-buffer color-rg-temp-buffer
      (save-excursion
        (goto-line change-line)
        (beginning-of-line)
        (search-forward-regexp color-rg-regexp-position nil t)
        (setq start (point))
        (end-of-line)
        (setq end (point))
        (setq original-line-content (buffer-substring-no-properties start end)))
      )
    (if (string-equal change-line-content original-line-content)
        (setq color-rg-changed-lines (remove change-line color-rg-changed-lines))
      (add-to-list 'color-rg-changed-lines change-line))
    ))

(defun color-rg-kill-temp-buffer ()
  (when (get-buffer color-rg-temp-buffer)
    (kill-buffer color-rg-temp-buffer)
    (setq color-rg-changed-lines nil)))

(defun color-rg-clone-to-temp-buffer ()
  (color-rg-kill-temp-buffer)
  (with-current-buffer color-rg-buffer
    (add-hook 'kill-buffer-hook 'color-rg-kill-temp-buffer nil t)
    (generate-new-buffer color-rg-temp-buffer)
    (append-to-buffer color-rg-temp-buffer (point-min) (point-max))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun color-rg (&optional keyword directory)
  (interactive)
  ;; Save window configuration before do search.
  (setq color-rg-window-configuration (current-window-configuration))
  ;; Set `enable-local-variables' to :safe, avoid emacs ask annoyingly question when open file by color-rg.
  (setq enable-local-variables :safe)
  ;; Reset hit count.
  (setq color-rg-hit-count 0)
  ;; Search.
  (let* ((search-keyboard
          (if keyword
              keyword
            (color-rg-read-input)))
         (search-directory
          (if directory
              directory
            default-directory)))
    (color-rg-search search-keyboard search-directory)))

(defun color-rg-jump-next-keyword ()
  (interactive)
  (let* ((next-position (color-rg-find-next-position color-rg-regexp-position)))
    (if next-position
        (progn
          (goto-char next-position)
          (color-rg-open-file))
      (message "Reach to last line."))))

(defun color-rg-jump-prev-keyword ()
  (interactive)
  (let ((prev-match-pos
         (if (save-excursion (search-backward-regexp color-rg-regexp-position nil t))
             (let* ((first-search-line
                     (save-excursion
                       (search-backward-regexp color-rg-regexp-position nil t)
                       (line-number-at-pos))))
               (if (equal first-search-line (line-number-at-pos))
                   ;; Search previous again if first search is same line of point.
                   (save-excursion
                     (beginning-of-line)
                     (search-backward-regexp color-rg-regexp-position nil t))
                 (save-excursion (search-backward-regexp color-rg-regexp-position nil t)))
               )
           nil)))
    (if prev-match-pos
        (progn
          (goto-char prev-match-pos)
          (color-rg-open-file))
      (message "Reach to first line."))))

(defun color-rg-jump-next-file ()
  (interactive)
  (let*  ((next-position (color-rg-find-next-position color-rg-regexp-file)))
    (if next-position
        (progn
          (goto-char next-position)
          (forward-line)
          (color-rg-open-file))
      (message "Reach to last file."))))

(defun color-rg-jump-prev-file ()
  (interactive)
  (let ((prev-match-pos
         (if (save-excursion (search-backward-regexp color-rg-regexp-file nil t))
             (let* ((first-search-line
                     (save-excursion
                       (search-backward-regexp color-rg-regexp-file nil t)
                       (line-number-at-pos)))
                    (prev-empty-line
                     (save-excursion
                       (search-backward-regexp color-rg-regexp-split-line)
                       (line-number-at-pos))))
               (if (and (> first-search-line prev-empty-line)
                        (not (color-rg-current-line-empty-p)))
                   ;; Search filename previous again if first search is current file result area.
                   (save-excursion
                     (search-backward-regexp color-rg-regexp-split-line)
                     (search-backward-regexp color-rg-regexp-file nil t))
                 (save-excursion (search-backward-regexp color-rg-regexp-file nil t)))
               )
           nil)))
    (if prev-match-pos
        (progn
          (goto-char prev-match-pos)
          (forward-line)
          (color-rg-open-file))
      (message "Reach to first file."))))

(defun color-rg-open-file ()
  (interactive)
  (let* ((match-file
          (save-excursion
            (search-backward-regexp color-rg-regexp-file nil t)
            (string-remove-suffix "\n" (buffer-substring-no-properties (beginning-of-thing 'line) (end-of-thing 'line)))))
         (match-line (progn
                       (beginning-of-line)
                       (string-to-number (buffer-substring-no-properties (beginning-of-thing 'symbol) (end-of-thing 'symbol)))))
         (match-column (progn
                         (search-forward ":")
                         (string-to-number (buffer-substring-no-properties (beginning-of-thing 'symbol) (end-of-thing 'symbol)))))
         (match-buffer (color-rg-get-match-buffer match-file)))
    (save-excursion
      ;; Open file in other window.
      (find-file-other-window match-file)
      ;; Push to temp list if file's buffer is not exist.
      (unless match-buffer
        (push (current-buffer) color-rg-temp-buffers))
      ;; Jump to match position.
      (goto-line match-line)
      (goto-column (- match-column 1)))
    ;; Keep cursor in search buffer's window.
    (select-window (get-buffer-window color-rg-buffer))
    ;; Ajust column position.
    (beginning-of-line)
    (search-forward-regexp color-rg-regexp-position)
    (forward-char (- match-column 1))
    ))

(defun color-rg-enable-edit-mode ()
  (interactive)
  ;; Clone content to temp buffer.
  (color-rg-clone-to-temp-buffer)
  ;; Update header-line.
  (set (make-local-variable 'edit-mode) "Edit")
  (color-rg-update-header-line)
  ;; Turn off readonly mode.
  (read-only-mode -1)
  ;; Clean keymap.
  (use-local-map nil)
  ;; Set edit area.
  (let (start end)
    ;; Make all buffer with readonly text property.
    (let ((inhibit-read-only t))
      (save-excursion
        (put-text-property 1 2 'front-sticky '(read-only))
        (put-text-property (point-min) (point-max) 'read-only t)
        ))
    ;; Make all code with edit property.
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (while (setq start (search-forward-regexp color-rg-regexp-position nil t))
          (setq start (point))
          (end-of-line)
          (setq end (point))
          (put-text-property (1- start) end 'read-only nil)))
      ))
  ;; Add change monitor.
  (add-hook 'after-change-functions 'color-rg-after-change-function nil t)
  )

(defun color-rg-quit ()
  (interactive)
  ;; Kill temp buffer open by color-rg.
  (dolist (temp-buffer color-rg-temp-buffers)
    (kill-buffer temp-buffer))
  (setq color-rg-temp-buffers nil)
  ;; Kill search buffer.
  (kill-buffer color-rg-buffer)
  ;; Restore window configuration before search.
  (when color-rg-window-configuration
    (set-window-configuration color-rg-window-configuration)
    (setq color-rg-window-configuration nil)))

(provide 'color-rg)

;;; color-rg.el ends here
