;;; color-rg.el --- Search and refacotry code with rg

;; Filename: color-rg.el
;; Description: Search and refacotry code with rg
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-08-26 14:22:12
;; Version: 5.6
;; Last-Updated: 2020-05-04 17:52:55
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/color-rg.el
;; Keywords:
;; Compatibility: GNU Emacs 27.0.50
;;
;; Features that might be required by this library:
;;
;; `cl-lib' `subr-x' `grep'
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
;; color-rg is search and refactoring tool based on ripgrep.
;;
;; I'm a big fan of color-moccur.el, this extension's name is used for tribute color-moccur.el!
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
;; If you use Mac, you also need install `exec-path-from-shell'
;;

;;; Customize:
;;
;; `color-rg'
;;
;; All of the above can customize by:
;;      M-x customize-group RET color-rg RET
;;

;;; Change log:
;;
;; 2020/12/31
;;      * Add new option `color-rg-search-compressed-file'.
;;
;; 2020/05/04
;;      * Add new command `color-rg-insert-current-line'.
;;
;; 2020/04/28
;;      * Add new option `color-rg-mac-load-path-from-shell'.
;;
;; 2020/03/14
;;      * Split window and select if color-buffer is not exist in windows.
;;      * Add keybinding notify.
;;
;; 2019/12/23
;;      * Support search tramp path.
;;
;; 2019/08/08
;;      * Use `cl-lib' instead of `cl' to avoid `Package cl is deprecated' message in the minibuffer.
;;
;; 2019/07/22
;;      * Add option `color-rg-max-column'.
;;
;; 2019/07/15
;;      * Don't print "Mark Set" message when call `color-rg-open-file' function.
;;
;; 2019/07/14
;;      * Use `inhibit-message' optimize the speed of `color-rg-replace-all-matches' when awesome-tray is enable.
;;
;; 2019/05/18
;;      * Remove dash.el dependence.
;;
;; 2019/05/15
;;      * improve new function: `color-rg-search-input-in-current-file' and `color-rg-search-symbol-in-current-file'
;;      * rename `files' to `globs', make it clarify
;;
;; 2019/05/15
;;      * Add new functions: `color-rg-search-input-in-current-file' and `color-rg-search-symbol-in-current-file'
;;
;; 2019/04/13
;;      * View the function name when navigate in match line.
;;      * Fix nil error of which-function.
;;
;; 2019/04/06
;;      * Add commands: `color-rg-search-input-in-project' and `color-rg-search-symbol-in-project'.
;;
;; 2019/03/20
;;      * Add `ignore-errors' to make sure cursor will back to color-rg buffer.
;;
;; 2019/03/07
;;      * add `olor-rg-rerun-change-exclude-files' which can filter searched files by glob, This is the opposite of `olor-rg-rerun-change-files'
;;      * add `color-rg-customized-search' which give users more power and freedom.
;;
;; 2019/01/29
;;      * Automatically expand the block of matching keywords in org files and adjust column along with org link syntax.
;;      * Jump to correct column positions in multi-byte strings, such as, mixed string of Chinese and English.
;;
;; 2019/01/25
;;      * Move two chars backward when current link is format as [[link]]
;;
;; 2019/01/24
;;      * Expand org block if current file is *.org file.
;;
;; 2018/12/27
;;      * Use `pulse-momentary-highlight-region' instead `thing-edit-flash-line'.
;;
;; 2018/12/09
;;      * Fix bug of `color-rg-in-string-p' when cursor at left side of string.
;;
;; 2018/12/06
;;      * Fix typo of `insert-translated-name-current-parse-state', it should be `color-rg-current-parse-state'
;;
;; 2018/12/02
;;      * Use `get-text-property' improve algorithm of `color-rg-in-string-p'.
;;
;; 2018/11/16
;;      * Add `color-rg-file-extension' function to support more granular extension filtering, such as, js.erb and html.erb.
;;
;; 2018/11/12
;;      * Remove Mac color, use hex color instead.
;;
;; 2018/11/01
;;      * Remove `projectile' depend.
;;
;; 2018/10/29
;;      * Use `string-trim' instead `s-trim' to remove require of `s.el'
;;
;; 2018/10/19
;;      * Add option `color-rg-kill-temp-buffer-p'.
;;
;; 2018/10/18
;;      * Add `color-rg-rerun-change-files' to files search files by GLOB. default files is "everything".
;;      * Add new functions:
;;              `color-rg-search-symbol-with-type'
;;              `color-rg-search-project-with-type'
;;              `color-rg-search-project-rails-with-type'
;;
;; 2018/10/11
;;      * Reset `color-rg-temp-visit-buffers' to avoid deleting the buffer being browsed after multiple searches.
;;      * Delete files that throw "error parsing glob" error when search.
;;
;; 2018/10/03
;;      * Use `color-rg-update-header-line-hits' update keywoard hits after filter operation.
;;
;; 2018/09/26
;;      * Make the save window configuration more robust when user do `color-rg-*' multiple times.
;;
;; 2018/09/23
;;      * Add `--heading' option force to make group matches work always to support Windows.
;;
;; 2018/09/22
;;      * Add `color-rg-unfilter'
;;
;; 2018/09/21
;;      * Add `color-rg-delete-all-lines'
;;
;; 2018/09/20
;;      * Display search hit in header line.
;;      * Fix `color-rg-replace-all-matches' void variable bug cause by refactor of `color-rg-cur-search'
;;      * Clean unused local variables.
;;      * Get string around point if point in string area and string not include space character.
;;
;; 2018/09/18
;;      * add `color-rg-cur-search' to store parameters of last search.
;;      * rewrite `color-rg-rerun-toggle-ignore', `color-rg-rerun-literal',
;;        `color-rg-rerun-toggle-case', `color-rg-rerun-regexp' and
;;        `color-rg-rerun-change-dir'
;;      * use `grep-expand-template' to expand keyword, we do not to care about
;;        escaping special characters now.
;;      * remove `color-rg-default-argument'
;;
;; 2018/09/17
;;      * Use `ido-completing-read' instead `completing-read' to provide fuzz match.
;;
;; 2018/09/15
;;      * Transferred keyword if use default `color-rg-default-argument'
;;        Make below transferred for regexp search in ripgrep.
;;
;;         "`foo" => "\`foo"
;;         ""foo" => "\"foo"
;;
;; 2018/09/11
;;      * Switch to literal search automaticity when parsing keyword regexp failed.
;;      * Adjust regex to match "Error parsing regex near".
;;
;; 2018/09/04
;;      * Use `color-rg-process-setup' monitor process finished, then output search hit in minibuffer.
;;      * Avoid function `move-to-column' change search file content.
;;      * Add `color-rg-filter-match-files' and `color-rg-filter-mismatch-files'
;;      * Flash match line after open search file.
;;
;; 2018/08/31
;;      * Fix `color-rg-window-configuration-before-search' override if user multiple search.
;;
;; 2018/08/30
;;      * Add color-rg-recover-buffer
;;      * Enhance rerun function
;;      * Add new functions: color-rg-filter-match-results, color-rg-filter-mismatch-results, color-rg-remove-line-from-results
;;      * Add function color-rg-filter-results
;;      * Add smart-case to color-rg-rerun-literal and color-rg-rerun-no-ignore
;;      * Use color-rg-get-row-column-position remove duplicate code.
;;      * Add customize option color-rg-default-argument
;;      * Add some research functions
;;      * Add color-rg-replace-all-matches
;;      * Add new functions: color-rg-change-search-keyword and color-rg-change-search-directory
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
(require 'cl-lib)
(require 'subr-x)
(require 'grep)

;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OS Config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar color-rg-mac-load-path-from-shell t
  "Some framework like Doom doens't use `exec-path-from-shell'.
You you make this option to nil if you don't want use `exec-path-from-shell'.")

(when (and color-rg-mac-load-path-from-shell
           (featurep 'cocoa))
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
  :group 'color-rg)

(defcustom color-rg-custom-type-aliases
  '(("gn" .    "*.gn *.gni")
    ("gyp" .    "*.gyp *.gypi"))
  "A list of file type aliases that are added to the 'rg' built in aliases.
Each list element may be a (string . string) cons containing the name of the
type alias and the file patterns, or a lambda returning a similar cons cell.
A lambda should return nil if it currently has no type aliases to contribute.")

(defcustom color-rg-flash-line-delay .3
  "How many seconds to flash `color-rg-font-lock-flash' after navigation.

Setting this to nil or 0 will turn off the indicator."
  :type 'number
  :group 'color-rg)

(defcustom color-rg-kill-temp-buffer-p t
  "Default this option is true, it will kill temp buffer when quit color-rg buffer.

A buffer will killed if it is open by color-rg and not edit by color-rg.

A buffer won't kill is it open before color-rg command start.
A buffer won't kill if buffer content is change by color-rg.

Anyway, you can set this option with nil if you don't like color-rg kill any buffer."
  :type 'boolean
  :group 'color-rg)

(defcustom color-rg-show-function-name-p t
  "View the function name when navigate in match line.

Default is enable, set this variable to nil if you don't like this feature."
  :type 'boolean
  :group 'color-rg)

(defcustom color-rg-max-column 3000
  "When searching for JS library files, the long JS library file will cause color-rg navigation to be very slow.
By default, there are 3000 columns of restrictions to avoid long file problems."
  :type 'integer
  :group 'color-rg)

(defcustom color-rg-search-compressed-file nil
  "Search compressed files when read the emacs source code.

Default is disabled, set this variable to true if you found it's useful"
  :type 'boolean
  :group 'color-rg)

(defcustom color-rg-search-no-ignore-file t
  "Don’t respect ignore files.

Default is enable, set this variable to nil if you want search files match gitignore rule."
  :type 'boolean
  :group 'color-rg)

(defcustom color-rg-search-ignore-rules "-g \"!node_modules\" -g \"!dist\""
  "When `color-rg-search-no-ignore-file' is non-nil, color-rg will search any file.
Include file match gitignore rule.

Default rule is search any file but except `node_modules' and `dist' directory,
you can customize ignore rules with your like."
  :type 'string
  :group 'color-rg)

(defface color-rg-font-lock-header-line-text
  '((t (:foreground "Green3" :bold t)))
  "Face for header line text."
  :group 'color-rg)

(defface color-rg-font-lock-header-line-keyword
  '((t (:foreground "Gold" :bold t)))
  "Face for header line keyword."
  :group 'color-rg)

(defface color-rg-font-lock-header-line-directory
  '((t (:foreground "DodgerBlue" :bold t)))
  "Face for header line directory."
  :group 'color-rg)

(defface color-rg-font-lock-header-line-edit-mode
  '((t (:foreground "Gold" :bold t)))
  "Face for header line edit mode."
  :group 'color-rg)

(defface color-rg-font-lock-command
  '((t (:foreground "Gray30" :bold t)))
  "Face for filepath."
  :group 'color-rg)

(defface color-rg-font-lock-file
  '((t (:foreground "DodgerBlue" :bold t)))
  "Face for filepath."
  :group 'color-rg)

(defface color-rg-font-lock-line-number
  '((t (:foreground "gray35")))
  "Face for line number."
  :group 'color-rg)

(defface color-rg-font-lock-column-number
  '((t (:foreground "gray35")))
  "Face for column number."
  :group 'color-rg)

(defface color-rg-font-lock-position-splitter
  '((t (:foreground "gray25")))
  "Face for position splitter."
  :group 'color-rg)

(defface color-rg-font-lock-match
  '((t (:foreground "Gold3" :bold t)))
  "Face for keyword match."
  :group 'color-rg)

(defface color-rg-font-lock-mark-changed
  '((t (:foreground "White" :background "#007aff" :bold t)))
  "Face for keyword match."
  :group 'color-rg)

(defface color-rg-font-lock-mark-deleted
  '((t (:foreground "#ff3b30" :bold t)))
  "Face for keyword match."
  :group 'color-rg)

(defface color-rg-font-lock-flash
  '((t (:inherit highlight)))
  "Face to flash the current line."
  :group 'color-rg)

(defface color-rg-font-lock-function-location
  '((t (:foreground "Gold" :bold t)))
  "Face for show function location."
  :group 'color-rg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar color-rg-temp-visit-buffers nil
  "The temp visit buffers use to kill temp buffer after quit color-rg.")

(defvar color-rg-window-configuration-before-search nil
  "Save window configuration before search,
used to restore window configuration after finish search.")

(defvar color-rg-buffer-point-before-search nil
  "Save buffer point before search,
used to restore buffer point after finish search.")

(defvar color-rg-window-configuration-before-apply nil
  "Save window configuration before apply changed,
used to restore window configuration after apply changed.")

(defvar color-rg-window-configuration-before-open nil
  "Save window configuration before open file,
used to restore window configuration after file content changed.")

(defvar color-rg-hit-count 0
  "Search keyword hit counter.")

(defvar color-rg-regexp-file "^[/\\~].*\\|^[a-z]:.*"
  "Regexp to match filename.")

(defvar color-rg-regexp-split-line "\n\n"
  "Regexp to match empty line between two files.")

(defvar color-rg-regexp-position "^\\([1-9][0-9]*\\):\\([1-9][0-9]*\\):"
  "Regexp to match line/column string.")

(defvar color-rg-changed-lines nil
  "The list that record the changed lines.")

(defvar color-rg-read-input-history nil)

(defvar color-rg-files-history nil "History for files args.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; color-rg mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar color-rg-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-a") 'color-rg-beginning-of-line)
    (define-key map (kbd "<tab>") 'color-rg-jump-next-keyword)
    (define-key map (kbd "<backtab>") 'color-rg-jump-prev-keyword)

    (define-key map (kbd "j") 'color-rg-jump-next-keyword)
    (define-key map (kbd "k") 'color-rg-jump-prev-keyword)
    (define-key map (kbd "h") 'color-rg-jump-next-file)
    (define-key map (kbd "l") 'color-rg-jump-prev-file)
    (define-key map (kbd "i") 'color-rg-insert-current-line)

    (define-key map (kbd "SPC") 'color-rg-open-file)
    (define-key map (kbd "RET") 'color-rg-open-file-and-stay)
    (define-key map (kbd "C-m") 'color-rg-open-file-and-stay)

    (define-key map (kbd "e") 'color-rg-switch-to-edit-mode)

    (define-key map (kbd "r") 'color-rg-replace-all-matches)
    (define-key map (kbd "f") 'color-rg-filter-match-results)
    (define-key map (kbd "F") 'color-rg-filter-mismatch-results)

    (define-key map (kbd "x") 'color-rg-filter-match-files)
    (define-key map (kbd "X") 'color-rg-filter-mismatch-files)
    (define-key map (kbd "u") 'color-rg-unfilter)

    (define-key map (kbd "D") 'color-rg-remove-line-from-results)

    (define-key map (kbd "I") 'color-rg-rerun-toggle-ignore)
    (define-key map (kbd "N") 'color-rg-rerun-toggle-node)
    (define-key map (kbd "C") 'color-rg-rerun-toggle-case)
    (define-key map (kbd "L") 'color-rg-rerun-literal)
    (define-key map (kbd "R") 'color-rg-rerun-regexp)
    (define-key map (kbd "G") 'color-rg-rerun-change-globs)
    (define-key map (kbd "E") 'color-rg-rerun-change-exclude-files)

    (define-key map (kbd "o") 'color-rg-rerun-parent-dir)
    (define-key map (kbd "O") 'color-rg-rerun-change-dir)

    (define-key map (kbd "S") 'color-rg-customized-search)

    (define-key map (kbd "q") 'color-rg-quit)
    map)
  "Keymap used by `color-rg-mode'.")

(defvar color-rg-mode-edit-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-a") 'color-rg-beginning-of-line)

    (define-key map (kbd "C-c C-j") 'color-rg-jump-next-keyword)
    (define-key map (kbd "C-c C-k") 'color-rg-jump-prev-keyword)
    (define-key map (kbd "C-c C-h") 'color-rg-jump-next-file)
    (define-key map (kbd "C-c C-l") 'color-rg-jump-prev-file)
    (define-key map (kbd "C-c <C-return>") 'color-rg-open-file)
    (define-key map (kbd "C-c C-v") 'color-rg-switch-to-view-mode)

    (define-key map (kbd "C-c C-d") 'color-rg-delete-line)
    (define-key map (kbd "C-c C-D") 'color-rg-delete-all-lines)
    (define-key map (kbd "C-c C-r") 'color-rg-recover-line)
    (define-key map (kbd "C-c C-R") 'color-rg-recover-buffer)
    (define-key map (kbd "C-c C-q") 'color-rg-quit)
    (define-key map (kbd "C-c C-c") 'color-rg-apply-changed)
    map)
  "Edit keymap used by `color-rg-mode'.")

(define-derived-mode color-rg-mode text-mode "color-rg"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'color-rg-mode)
  (setq mode-name "color-rg")
  (read-only-mode 1)
  (color-rg-highlight-keywords)
  (use-local-map color-rg-mode-map)
  (add-hook 'compilation-filter-hook 'color-rg-filter nil t)
  (set (make-local-variable 'compilation-process-setup-function) 'color-rg-process-setup)
  (run-hooks 'color-rg-mode-hook)
  )

(defun color-rg-highlight-keywords ()
  "Highlight keywords."
  ;; Add keywords for highlight.
  (font-lock-add-keywords
   nil
   '(
     ("^rg\\s-.*" . 'color-rg-font-lock-command)
     ("^\\([1-9][0-9]*\\)\\(:\\)\\([1-9][0-9]*\\)\\(:\\)" 1 'color-rg-font-lock-line-number)
     ("^\\([1-9][0-9]*\\)\\(:\\)\\([1-9][0-9]*\\)\\(:\\)" 2 'color-rg-font-lock-position-splitter)
     ("^\\([1-9][0-9]*\\)\\(:\\)\\([1-9][0-9]*\\)\\(:\\)" 3 'color-rg-font-lock-column-number)
     ("^\\([1-9][0-9]*\\)\\(:\\)\\([1-9][0-9]*\\)\\(:\\)" 4 'color-rg-font-lock-position-splitter)
     ("^[/\\~].*\\|^[a-z]:.*" . 'color-rg-font-lock-file)
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
        ;; Delete files that throw "error parsing glob" error when search.
        (while (re-search-forward "/.*:\\s-error\\s-parsing\\s-glob\\s-.*" end 1)
          (replace-match "" t t))

        ;; Highlight filename.
        (goto-char beg)
        (while (re-search-forward "^\033\\[[0]*m\033\\[35m\\(.*?\\)\033\\[[0]*m$" end 1)
          (replace-match (concat (propertize (match-string 1)
                                             'face nil 'font-lock-face 'color-rg-font-lock-file))
                         t t))

        ;; Highlight rg matches and delete marking sequences.
        (goto-char beg)
        (while (re-search-forward "\033\\[[0]*m\033\\[[3]*1m\033\\[[3]*1m\\(.*?\\)\033\\[[0]*m" end 1)
          (replace-match (propertize (match-string 1)
                                     'face nil 'font-lock-face 'color-rg-font-lock-match)
                         t t)
          (setq color-rg-hit-count (+ color-rg-hit-count 1))
          (color-rg-update-header-line)
          )

        ;; Delete all remaining escape sequences
        (goto-char beg)
        (while (re-search-forward "\033\\[[0-9;]*[0mK]" end 1)
          (replace-match "" t t))))
    ))

(defun color-rg-process-setup ()
  "Setup compilation variables and buffer for `color-rg'."
  (set (make-local-variable 'compilation-exit-message-function)
       (lambda (status code msg)
         (if (eq status 'exit)
             ;; This relies on the fact that `compilation-start'
             ;; sets buffer-modified to nil before running the command,
             ;; so the buffer is still unmodified if there is no output.
             (cond ((and (zerop code) (buffer-modified-p))
                    ;; Clone to temp buffer and we restore by command, such as `color-rg-unfilter'.
                    (run-at-time "1sec" nil
                                 (lambda ()
                                   (color-rg-clone-to-temp-buffer)))
                    `(,(format "finished (%d matches found)\n" color-rg-hit-count) . "matched"))
                   ((not (buffer-modified-p))
                    '("finished with no matches found\n" . "no match"))
                   ((string-prefix-p "exited abnormally with code" msg)
                    ;; Switch to literal search automaticity when parsing keyword regexp failed.
                    (with-current-buffer color-rg-buffer
                      (cond ((or (search-forward-regexp "^Error parsing regex near" nil t) (search-forward-regexp "^regex parse error" nil t))
                             (run-at-time "2sec" nil
                                          (lambda ()
                                            (message "COLOR-RG: parsing keyword regexp failed, switch to literal search automaticity.")))
                             (color-rg-rerun-literal t))
                            )))
                   (t (cons msg code)))
           (cons msg code)))))

(defun color-rg-update-header-line ()
  (setq header-line-format (concat
                            (propertize (format "%s mode" (color-rg-search-mode color-rg-cur-search)) 'font-lock-face 'color-rg-font-lock-match)
                            (propertize (format " %s matches" color-rg-hit-count) 'font-lock-face 'color-rg-font-lock-header-line-text)
                            (propertize " [ " 'font-lock-face 'color-rg-font-lock-line-number)
                            (propertize "Nav " 'font-lock-face 'color-rg-font-lock-header-line-text)
                            (propertize "j / k" 'font-lock-face 'color-rg-font-lock-header-line-edit-mode)
                            (propertize "  Replace " 'font-lock-face 'color-rg-font-lock-header-line-text)
                            (propertize "r" 'font-lock-face 'color-rg-font-lock-header-line-edit-mode)
                            (propertize "  Edit " 'font-lock-face 'color-rg-font-lock-header-line-text)
                            (propertize "e" 'font-lock-face 'color-rg-font-lock-header-line-edit-mode)
                            (propertize "  Filter files: " 'font-lock-face 'color-rg-font-lock-header-line-text)
                            (propertize "x / X / u" 'font-lock-face 'color-rg-font-lock-header-line-edit-mode)
                            (propertize "  Filter regex: " 'font-lock-face 'color-rg-font-lock-header-line-text)
                            (propertize "f / F" 'font-lock-face 'color-rg-font-lock-header-line-edit-mode)
                            (propertize "  Customize " 'font-lock-face 'color-rg-font-lock-header-line-text)
                            (propertize "C" 'font-lock-face 'color-rg-font-lock-header-line-edit-mode)
                            (propertize " ]" 'font-lock-face 'color-rg-font-lock-line-number)
                            )))

(cl-defstruct (color-rg-search (:constructor color-rg-search-create)
                               (:constructor color-rg-search-new (pattern dir))
                               (:copier nil))
  keyword           ; search keyword
  dir               ; base directory
  globs             ; filename only match these globs will be searched
  file-exclude ; toggle exclude files, t means filename NOT match the globs will be searched
  literal      ; literal patterh (t or nil)
  case-sensitive                        ; case-sensitive (t or nil)
  no-ignore                             ; toggle no-ignore (t or nil)
  no-node                               ; toggle no-node (t or nil)
  mode                                  ; view or edit mode
  )

(defvar color-rg-cur-search (color-rg-search-create)
  "Stores parameters of last search.
Becomes buffer local in `color-rg-mode' buffers.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utils functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar color-rg-builtin-type-aliases nil
  "Cache for 'rg --type-list'.")

(defconst color-rg-internal-type-aliases
  '(("all" . "all defined type aliases") ; rg --type all
    ("everything" . "*"))                ; rg without '--type' arg
  "Internal type aliases for special purposes.
These are not produced by 'rg --type-list' but we need them anyway.")

(defun color-rg-get-custom-type-aliases ()
  "Get alist of custom type aliases.
Any lambda elements will be evaluated, and nil results will be
filtered out."
  (delq nil (mapcar
             (lambda (ct) (if (functionp ct) (funcall ct) ct))
             color-rg-custom-type-aliases)))

(defun color-rg-list-builtin-type-aliases ()
  "Invokes rg --type-list and puts the result in an alist."
  (unless (executable-find "rg")
    (error "'rg' is not in path"))
  (let ((type-list (nbutlast (split-string
                              (shell-command-to-string
                               (concat (executable-find "rg") " --type-list"))
                              "\n") 1)))
    (mapcar
     (lambda (type-alias)
       (setq type-alias (split-string type-alias ":" t))
       (cons (string-trim (car type-alias))
             (string-trim
              (mapconcat 'identity
                         (split-string (cadr type-alias) "," t )
                         " "))))
     type-list)))

(defun color-rg-get-type-aliases (&optional skip-internal)
  "Return supported type aliases.
If SKIP-INTERNAL is non nil the `color-rg-internal-type-aliases' will be
excluded."
  (unless color-rg-builtin-type-aliases
    (setq color-rg-builtin-type-aliases (color-rg-list-builtin-type-aliases)))
  (append (color-rg-get-custom-type-aliases) color-rg-builtin-type-aliases
          (unless skip-internal color-rg-internal-type-aliases)))

(defun color-rg-is-custom-file-pattern (globs)
  "Return non nil if FILES is a custom file pattern."
  (not (assoc globs (color-rg-get-type-aliases))))

(defun color-rg-build-command (keyword dir globs &optional literal no-ignore no-node case-sensitive file-exclude)
  "Create the command line for KEYWORD.
LITERAL determines if search will be literal or regexp based.
NO-IGNORE determinies if search not ignore the ignored files.
CASE-SENSITIVE determinies if search is case-sensitive."
  (let ((command-line
         (append

          (list "--column --color=always -H")

          ;; NOTE:                      ;
          ;;
          ;; ripgrep is default use heading option (group matches by each file) in all OS's terminal.
          ;; But not greoup matches on Windows/Emacs.
          ;; So we add this option force to make group matches work always.
          ;;
          (list "--heading")

          (list "--max-columns" (number-to-string color-rg-max-column))

          (when (or color-rg-search-no-ignore-file no-ignore)
            (list "--no-ignore"))

          (unless no-node
            (list color-rg-search-ignore-rules))

          (when color-rg-search-compressed-file
            (list "-z"))

          (when (color-rg-is-custom-file-pattern globs)
            (list (concat "--type-add " (shell-quote-argument (concat "custom:" globs)))))

          (if case-sensitive
              (list "--case-sensitive")
            (list "--smart-case"))

          (when literal
            (list "--fixed-strings"))

          (when (not (equal globs "everything"))
            (if file-exclude
                (list "--type-not <F>")
              (list "--type <F>")))

          (list "-e <R>" (color-rg-filter-tramp-path dir)))))

    (setq command-line
          (grep-expand-template
           (mapconcat 'identity (cons "rg" (delete-dups command-line)) " ")
           keyword
           (if (color-rg-is-custom-file-pattern globs) "custom" globs)))
    (when (memq system-type '(cygwin windows-nt ms-dos))
      (setq command-line (encode-coding-string command-line locale-coding-system)))
    command-line))

(defun color-rg-filter-tramp-path (x)
  "Remove sudo from path.  Argument X is path."
  (if (and (boundp 'tramp-tramp-file-p)
           (tramp-tramp-file-p x))
      (let ((tx (tramp-dissect-file-name x)))
        (if (string-equal "sudo" (tramp-file-name-method tx))
            (tramp-file-name-localname tx)
          x))
    x))

(defun color-rg-search (keyword directory globs &optional literal no-ignore no-node case-sensitive file-exclude)
  (let* ((command (color-rg-build-command keyword directory globs literal no-ignore no-node case-sensitive file-exclude)))
    ;; Reset visit temp buffers.
    (setq color-rg-temp-visit-buffers nil)
    ;; Reset hit count.
    (setq color-rg-hit-count 0)
    ;; Erase or create search result.
    (if (get-buffer color-rg-buffer)
        (with-current-buffer color-rg-buffer
          (let ((inhibit-read-only t))
            ;; Switch to `color-rg-mode' first, otherwise `erase-buffer' will cause "save-excursion: end of buffer" error.
            (color-rg-mode)
            ;; Erase buffer content.
            (read-only-mode -1)
            (erase-buffer)))
      (generate-new-buffer color-rg-buffer))
    (setq color-rg-changed-lines nil)

    ;; Run search command.
    (with-current-buffer color-rg-buffer
      ;; Fix compatibility issues with doom-emacs, because it changed the value of compilation-buffer-name-function.
      (setq-local compilation-buffer-name-function #'compilation--default-buffer-name)
      ;; Start command.
      (compilation-start command 'color-rg-mode)

      ;; save last search
      (setq-default color-rg-cur-search
                    (color-rg-search-create
                     :keyword keyword
                     :dir directory
                     :globs globs
                     :file-exclude file-exclude
                     :no-ignore no-ignore
                     :no-node no-node
                     :literal literal
                     :case-sensitive case-sensitive
                     :mode "View"))
      (color-rg-update-header-line))

    ;; Pop search buffer.
    (pop-to-buffer color-rg-buffer)
    (goto-char (point-min))
    ))

(defun color-rg-customized-search ()
  "Rerun rg with customized arguments. This function will give
user more freedom to use rg with special arguments."
  (interactive)
  (let* ((command (read-from-minibuffer "Customized search: " (car compilation-arguments))))
    ;; Reset visit temp buffers.
    (setq color-rg-temp-visit-buffers nil)
    ;; Reset hit count.
    (setq color-rg-hit-count 0)
    ;; Erase or create search result.
    (if (get-buffer color-rg-buffer)
        (with-current-buffer color-rg-buffer
          (let ((inhibit-read-only t))
            ;; Switch to `color-rg-mode' first, otherwise `erase-buffer' will cause "save-excursion: end of buffer" error.
            (color-rg-mode)
            ;; Erase buffer content.
            (read-only-mode -1)
            (erase-buffer)))
      (generate-new-buffer color-rg-buffer))
    (setq color-rg-changed-lines nil)

    ;; Run search command.
    (with-current-buffer color-rg-buffer
      ;; Fix compatibility issues with doom-emacs, because it changed the value of compilation-buffer-name-function.
      (setq-local compilation-buffer-name-function #'compilation--default-buffer-name)
      ;; Start command.
      (compilation-start command 'color-rg-mode)

      (color-rg-update-header-line))

    ;; Pop search buffer.
    (pop-to-buffer color-rg-buffer)
    (goto-char (point-min))))

(defun color-rg-read-input ()
  (let* ((current-symbol (color-rg-pointer-string))
         (input-string
          (string-trim
           (read-string
            (format "COLOR-RG Search (%s): " current-symbol)
            nil
            'color-rg-read-input-history
            ))))
    (when (string-blank-p input-string)
      (setq input-string current-symbol))
    input-string))

(defun color-rg-current-parse-state ()
  "Return parse state of point from beginning of defun."
  (ignore-errors
    (save-excursion
      (let ((point (point)))
        (beginning-of-defun)
        (parse-partial-sexp (point) point)))))

(defun color-rg-in-string-p (&optional state)
  (or (nth 3 (or state (color-rg-current-parse-state)))
      (and
       (eq (get-text-property (point) 'face) 'font-lock-string-face)
       (eq (get-text-property (- (point) 1) 'face) 'font-lock-string-face))
      (and
       (eq (get-text-property (point) 'face) 'font-lock-doc-face)
       (eq (get-text-property (- (point) 1) 'face) 'font-lock-doc-face))
      ))

(defun color-rg-string-start+end-points (&optional state)
  "Return a cons of the points of open and close quotes of the string.
The string is determined from the parse state STATE, or the parse state
  from the beginning of the defun to the point.
This assumes that `color-rg-in-string-p' has already returned true, i.e.
  that the point is already within a string."
  (save-excursion
    (let ((start (nth 8 (or state (color-rg-current-parse-state)))))
      (goto-char start)
      (forward-sexp 1)
      (cons start (1- (point))))))

(defun color-rg-pointer-string ()
  (if (use-region-p)
      ;; Get region string if mark is set.
      (buffer-substring-no-properties (region-beginning) (region-end))
    ;; Get current symbol or string, and remove prefix char before return.
    (let* ((current-string (if (color-rg-in-string-p)
                               (buffer-substring-no-properties
                                (1+ (car (color-rg-string-start+end-points)))
                                (cdr (color-rg-string-start+end-points)))
                             ""))
           (current-symbol (if (or (string-empty-p current-string)
                                   (string-match-p "[[:space:]]" current-string))
                               ;; Get symbol around point if string around point is empty or include spaces.
                               (thing-at-point 'symbol)
                             ;; Otherwise, get string around point.
                             current-string)))
      (cond ((string-prefix-p "." current-symbol)
             (string-remove-prefix "." current-symbol))
            ((string-prefix-p "#" current-symbol)
             (string-remove-prefix "#" current-symbol))
            (t current-symbol)))
    ))

(defun color-rg-find-next-position (regexp)
  (save-excursion
    (end-of-line)
    (search-forward-regexp regexp nil t)))

(defun color-rg-get-match-file ()
  (save-excursion
    (search-backward-regexp color-rg-regexp-file nil t)
    (string-remove-suffix "\n" (thing-at-point 'line))))

(defun color-rg-get-match-line ()
  (beginning-of-line)
  (string-to-number (thing-at-point 'symbol)))

(defun color-rg-get-match-column ()
  (search-forward ":")
  (string-to-number (thing-at-point 'symbol)))

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

(defun color-rg-get-line-content (buffer line)
  (with-current-buffer buffer
    (save-excursion
      (goto-line line)
      (beginning-of-line)
      (search-forward-regexp color-rg-regexp-position nil t)
      (setq start (point))
      (end-of-line)
      (setq end (point))
      (buffer-substring start end))
    ))

(defun color-rg-get-row-column-position ()
  (let* ((search-bound
          (save-excursion
            (end-of-line)
            (point)))
         (row-column-position
          (save-excursion
            (beginning-of-line)
            (search-forward-regexp color-rg-regexp-position search-bound t))))
    row-column-position))

(defun color-rg-after-change-function (beg end leng-before)
  ;; NOTE:
  ;; We should use `save-match-data' wrap function that hook in `after-change-functions'.
  ;; Otherwise will got error: "replace-match-maybe-edit: Match data clobbered by buffer modification hooks"
  (save-match-data
    (let* ((change-line (save-excursion
                          (goto-char beg)
                          (line-number-at-pos)))
           change-line-content
           original-line-content)
      (setq changed-line-content (color-rg-get-line-content color-rg-buffer change-line))
      (setq original-line-content (color-rg-get-line-content color-rg-temp-buffer change-line))
      (if (string-equal changed-line-content original-line-content)
          (progn
            (setq color-rg-changed-lines (remove change-line color-rg-changed-lines))
            (color-rg-mark-position-clear change-line))
        (add-to-list 'color-rg-changed-lines change-line)
        (if (string-equal changed-line-content "")
            (color-rg-mark-position-deleted change-line)
          (color-rg-mark-position-changed change-line)))
      )))

(defun color-rg-mark-position-clear (line)
  (save-excursion
    (goto-line line)
    (beginning-of-line)
    (forward-char)
    (dolist (overlay (overlays-at (point)))
      (when (or (string-equal (overlay-get overlay 'overlay-type) "changed")
                (string-equal (overlay-get overlay 'overlay-type) "deleted"))
        (delete-overlay overlay)
        ))))

(defun color-rg-mark-position (line type face)
  (save-excursion
    (color-rg-mark-position-clear line)
    ;; Create mark changed overlay if not exists.
    (let (start end)
      (save-excursion
        (beginning-of-line)
        (setq start (point))
        (end-of-line)
        (setq end (point))
        (setq changed-overlay (make-overlay start end))
        (overlay-put changed-overlay 'overlay-type type)
        (overlay-put changed-overlay 'face face)
        ))))

(defun color-rg-mark-position-changed (line)
  (color-rg-mark-position line "changed" 'color-rg-font-lock-mark-changed))

(defun color-rg-mark-position-deleted (line)
  (color-rg-mark-position line "deleted" 'color-rg-font-lock-mark-deleted))

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

(defun color-rg-switch-to-view-mode ()
  (interactive)
  (with-current-buffer color-rg-buffer
    ;; Do clean work.
    (dolist (line color-rg-changed-lines)
      (color-rg-mark-position-clear line))
    (setq color-rg-changed-lines nil)
    (color-rg-kill-temp-buffer)
    (remove-hook 'after-change-functions 'color-rg-after-change-function t)
    ;; Switch to view mode.
    (read-only-mode 1)
    (use-local-map color-rg-mode-map)
    (kill-local-variable 'query-replace-skip-read-only)
    (setf (color-rg-search-mode color-rg-cur-search) "View")
    (color-rg-update-header-line)
    ))

(defun color-rg-filter-results (match-regexp)
  (let ((filter-regexp (read-string
                        (format (if match-regexp
                                    "Filter result match regexp: "
                                  "Filter result not match regexp: ")))))
    (save-excursion
      (with-current-buffer color-rg-buffer
        (setq remove-counter 0)
        (goto-char (point-min))
        (while (setq start (search-forward-regexp color-rg-regexp-position nil t))
          (setq line-content (color-rg-get-line-content color-rg-buffer (line-number-at-pos)))
          (if match-regexp
              (unless (string-match filter-regexp line-content)
                (color-rg-remove-line-from-results)
                (setq remove-counter (+ 1 remove-counter))
                )
            (when (string-match filter-regexp line-content)
              (color-rg-remove-line-from-results)
              (setq remove-counter (+ 1 remove-counter))
              )))
        (if match-regexp
            (message (format "Remove %s lines not match regexp '%s'." remove-counter filter-regexp))
          (message (format "Remove %s lines match regexp '%s'." remove-counter filter-regexp)))
        )))
  ;; Update hit number in header line.
  (color-rg-update-header-line-hits))

(defun color-rg-file-extension (file)
  (string-join (cdr (split-string (file-name-nondirectory file) "\\.")) "."))

(defun color-rg-filter-files (match-files)
  (let (file-extensions start end)
    (save-excursion
      (goto-char (point-min))
      (while (setq end (search-forward-regexp color-rg-regexp-file nil t))
        (beginning-of-line)
        (setq start (point))
        (setq filename (buffer-substring-no-properties start end))
        (end-of-line)
        (add-to-list 'file-extensions (color-rg-file-extension filename))))
    (if (< (length file-extensions) 2)
        (message (format "Has one type files now."))
      (setq filter-extension (ido-completing-read (if match-files
                                                      "Only display file suffix with: "
                                                    "Remove file suffix with: ")
                                                  file-extensions))
      (save-excursion
        (with-current-buffer color-rg-buffer
          (setq remove-counter 0)
          (goto-char (point-min))
          (while (setq end (search-forward-regexp color-rg-regexp-file nil t))
            (beginning-of-line)
            (setq start (point))
            (setq file-extension (color-rg-file-extension (buffer-substring-no-properties start end)))
            (if match-files
                (if (string-equal file-extension filter-extension)
                    (end-of-line)
                  (color-rg-remove-lines-under-file))
              (if (string-equal file-extension filter-extension)
                  (color-rg-remove-lines-under-file)
                (end-of-line))))
          ))))
  ;; Update hit number in header line.
  (color-rg-update-header-line-hits))

(defun color-rg-remove-lines-under-file ()
  (let (start end)
    (save-excursion
      (with-current-buffer color-rg-buffer
        (read-only-mode -1)
        (beginning-of-line)
        (setq start (point))
        (when (search-forward-regexp color-rg-regexp-split-line nil t)
          (setq end (point))
          (kill-region start end))
        (read-only-mode 1)))))

(defun color-rg-update-header-line-hits ()
  (setq color-rg-hit-count (color-rg-stat-hits))
  (color-rg-update-header-line))

(defun color-rg-stat-hits ()
  (let ((hit-count 0))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((plist (text-properties-at (point)))
              (next-change
               (or (next-property-change (point) (current-buffer))
                   (point-max))))
          (dolist (property plist)
            (when (string-equal (format "%s" property) "color-rg-font-lock-match")
              (setq hit-count (+ hit-count 1))))
          (goto-char next-change)))
      hit-count)))

(defun color-rg-read-file-type (format-string)
  (let* ((globs (color-rg-search-globs color-rg-cur-search))
         (default-files (if (and (color-rg-search-file-exclude color-rg-cur-search)
                                 (equal globs "everything"))
                            "nothing"
                          globs)))
    (completing-read
     (format format-string default-files)
     (color-rg-get-type-aliases)
     nil nil nil 'color-rg-files-history
     globs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun color-rg-search-input (&optional keyword directory globs)
  (interactive)
  ;; Save window configuration before do search.
  ;; Just save when `color-rg-window-configuration-before-search' is nil
  ;; Or current buffer is not `color-rg-buffer' (that mean user not quit color-rg and search again in other place).
  (when (or (not color-rg-window-configuration-before-search)
            (not (string-equal (buffer-name) color-rg-buffer)))
    (setq color-rg-window-configuration-before-search (current-window-configuration))
    (setq color-rg-buffer-point-before-search (point)))
  ;; Set `enable-local-variables' to :safe, avoid emacs ask annoyingly question when open file by color-rg.
  (setq enable-local-variables :safe)
  ;; Search.
  (let* ((search-keyboard
          (or keyword
              (color-rg-read-input)))
         (search-directory
          (or directory
              default-directory))
         (search-globs
          (or globs
              "everything")))
    (color-rg-search search-keyboard
                     (if (string-equal system-type "windows-nt")
                         (format "\"%s\"" search-directory)
                       search-directory)
                     search-globs)))

(defun color-rg-search-symbol ()
  (interactive)
  (color-rg-search-input (color-rg-pointer-string) default-directory))

(defun color-rg-search-symbol-with-type ()
  (interactive)
  (color-rg-search-input (color-rg-pointer-string) default-directory (color-rg-read-file-type "Filter file by type (default: [ %s ]): ")))

(defun color-rg-search-input-in-current-file ()
  (interactive)
  (color-rg-search-input (color-rg-read-input) (expand-file-name (buffer-file-name))))

(defun color-rg-search-symbol-in-current-file ()
  (interactive)
  (color-rg-search-input (color-rg-pointer-string) (expand-file-name (buffer-file-name))))

(defun color-rg-project-root-dir ()
  (let ((project (project-current)))
    (if project
        (progn
          (setq project (cdr project))

          (when (listp project)
            (setq project (nth (- (length project) 1) project)))

          (expand-file-name project))
      default-directory)))

(defalias 'color-rg-search-input-in-project 'color-rg-search-project)

(defun color-rg-search-project ()
  (interactive)
  (color-rg-search-input (color-rg-read-input) (color-rg-project-root-dir)))

(defun color-rg-search-symbol-in-project ()
  (interactive)
  (color-rg-search-input (color-rg-pointer-string) (color-rg-project-root-dir)))

(defun color-rg-search-project-with-type ()
  (interactive)
  (color-rg-search-input (color-rg-read-input) (color-rg-project-root-dir) (color-rg-read-file-type "Filter file by type (default: [ %s ]): ")))

(defun color-rg-search-project-rails ()
  (interactive)
  (color-rg-search-input (color-rg-read-input) (concat (color-rg-project-root-dir) "app")))

(defun color-rg-search-project-rails-with-type ()
  (interactive)
  (color-rg-search-input (color-rg-read-input) (concat (color-rg-project-root-dir) "app") (color-rg-read-file-type "Filter file by type (default: [ %s ]): ")))

(defun color-rg-replace-all-matches ()
  "Replace all matched results."
  (interactive)
  (save-excursion
    (let (changed-line-number)
      (let ((inhibit-message t)) ; don't flush to echo area when apply changed, optimise for color-rg
        (with-current-buffer color-rg-buffer
          (let* ((search-keyword (color-rg-search-keyword color-rg-cur-search))
                 (replace-text (read-string (format "Replace '%s' all matches with: " search-keyword) search-keyword)))
            (color-rg-switch-to-edit-mode)
            (if (color-rg-search-literal color-rg-cur-search)
                (query-replace search-keyword replace-text nil (point-min) (point-max))
              (query-replace-regexp search-keyword replace-text nil (point-min) (point-max)))
            (setq changed-line-number (length color-rg-changed-lines))
            (color-rg-apply-changed)
            (color-rg-switch-to-view-mode)
            (when (> changed-line-number 0)
              (setf (color-rg-search-keyword color-rg-cur-search) replace-text)))))
      (message "Replace %s lines" changed-line-number))))

(defun color-rg-filter-match-results ()
  (interactive)
  (color-rg-filter-results t))

(defun color-rg-filter-mismatch-results ()
  (interactive)
  (color-rg-filter-results nil))

(defun color-rg-filter-match-files ()
  (interactive)
  (color-rg-filter-files t))

(defun color-rg-filter-mismatch-files ()
  (interactive)
  (color-rg-filter-files nil))

(defun color-rg-unfilter ()
  (interactive)
  (save-excursion
    (with-current-buffer color-rg-buffer
      (let ((inhibit-read-only t)
            (old-compilation-arguments compilation-arguments))
        (color-rg-mode) ; switch to `color-rg-mode' first, otherwise `erase-buffer' will cause "save-excursion: end of buffer" error.
        (read-only-mode -1)
        (erase-buffer)
        (insert (with-current-buffer color-rg-temp-buffer
                  (buffer-substring (point-min) (point-max))))
        (read-only-mode 1)
        (setq-local compilation-arguments old-compilation-arguments)
        )
      ;; Update hit number in header line.
      (color-rg-update-header-line-hits))))

(defun color-rg-remove-line-from-results ()
  (interactive)
  (save-excursion
    (with-current-buffer color-rg-buffer
      (when (color-rg-get-row-column-position)
        (read-only-mode -1)
        (beginning-of-line)
        (kill-line)
        (kill-line)
        (read-only-mode 1)
        ))))

(defun color-rg-recompile ()
  "Run `recompile' while preserving some buffer local variables."
  (interactive)
  ;; Buffer locals will be reset in recompile so we need save them
  ;; here.
  (let ((cur-search color-rg-cur-search)
        ;; Fix compatibility issues with doom-emacs, because it changed the value of compilation-buffer-name-function.
        (compilation-buffer-name-function #'compilation--default-buffer-name))
    (recompile)
    (setq color-rg-cur-search cur-search)))

(defun color-rg-rerun ()
  "Run `color-rg-recompile' with `compilation-arguments' taken
from `color-rg-cur-search'."
  (interactive)
  (let ((keyword (color-rg-search-keyword color-rg-cur-search))
        (dir (color-rg-search-dir color-rg-cur-search))
        (globs (color-rg-search-globs color-rg-cur-search))
        (file-exclude (color-rg-search-file-exclude color-rg-cur-search))
        (literal (color-rg-search-literal color-rg-cur-search))
        (case-sensitive (color-rg-search-case-sensitive color-rg-cur-search))
        (no-ignore (color-rg-search-no-ignore color-rg-cur-search))
        (no-node (color-rg-search-no-node color-rg-cur-search)))
    (setcar compilation-arguments
            (color-rg-build-command keyword dir globs literal no-ignore no-node case-sensitive file-exclude))
    ;; Reset hit count.
    (setq color-rg-hit-count 0)

    ;; compilation-directory is used as search dir and
    ;; default-directory is used as the base for file paths.
    (setq compilation-directory (file-name-directory dir))
    (setq default-directory compilation-directory)
    (color-rg-recompile)
    (color-rg-update-header-line)
    (pop-to-buffer color-rg-buffer)
    (goto-char (point-min))
    ))

(defun color-rg-rerun-regexp (&optional keyword)
  "Re-search as regexp."
  (interactive)
  (setf (color-rg-search-keyword color-rg-cur-search)
        (read-string (format "Re-search with new keyword: ")
                     (color-rg-search-keyword color-rg-cur-search)))
  (setf (color-rg-search-literal color-rg-cur-search) nil)
  (color-rg-rerun))

(defun color-rg-rerun-change-globs ()
  "Rerun last search but prompt for new files."
  (interactive)
  (setf (color-rg-search-file-exclude color-rg-cur-search) nil)
  (setf (color-rg-search-globs color-rg-cur-search) (color-rg-read-file-type "Repeat search in files (default: [ %s ]): "))
  (color-rg-rerun))

(defun color-rg-rerun-change-exclude-files ()
  "Rerun last search but prompt for new files which will NOT be searched.
This function is the opposite of `color-rg-rerun-change-globs'"
  (interactive)
  (setf (color-rg-search-file-exclude color-rg-cur-search) t)
  (setf (color-rg-search-globs color-rg-cur-search) (color-rg-read-file-type "Repeat search exclude files (default: [ %s ]): "))
  (color-rg-rerun))

(defun color-rg-rerun-parent-dir ()
  "Rerun last command on parent dir."
  (interactive)
  (setf (color-rg-search-dir color-rg-cur-search)
        (file-name-directory (directory-file-name (color-rg-search-dir color-rg-cur-search))))
  (color-rg-rerun))

(defun color-rg-rerun-change-dir ()
  "Rerun last command but prompt for new dir."
  (interactive)
  (setf (color-rg-search-dir color-rg-cur-search)
        (read-file-name "In directory: "
                        (file-name-directory (color-rg-search-dir color-rg-cur-search)) nil))
  (color-rg-rerun))

(defun color-rg-rerun-literal (&optional nointeractive)
  "Re-search as literal."
  (interactive)
  (setf (color-rg-search-literal color-rg-cur-search)
        t)
  (if nointeractive
      (color-rg-rerun)
    (progn
      (setf (color-rg-search-keyword color-rg-cur-search)
            (read-string "Re-search with-literal: "
                         (color-rg-search-keyword color-rg-cur-search)))
      (color-rg-rerun))))

(defun color-rg-rerun-toggle-case ()
  "Rerun last search with toggled case sensitivity setting."
  (interactive)
  (let ((case-sensitive (not (color-rg-search-case-sensitive color-rg-cur-search))))

    (setf (color-rg-search-case-sensitive color-rg-cur-search)
          case-sensitive)
    (color-rg-rerun)))

(defun color-rg-rerun-toggle-ignore ()
  "Rerun last search with toggled '--no-ignore' flag."
  (interactive)
  (let ((ignore (not (color-rg-search-no-ignore color-rg-cur-search))))
    (setf (color-rg-search-no-ignore color-rg-cur-search)
          ignore)
    (color-rg-rerun)))

(defun color-rg-rerun-toggle-node ()
  "Rerun last search with toggled '--no-node' flag."
  (interactive)
  (let ((node (not (color-rg-search-no-node color-rg-cur-search))))
    (setf (color-rg-search-no-node color-rg-cur-search)
          node)
    (color-rg-rerun)))

(defun isearch-toggle-color-rg ()
  "toggle `color-rg' in isearch-mode."
  (interactive)
  (color-rg-search-input isearch-string)
  (isearch-exit)
  )

(defun color-rg-jump-next-keyword ()
  (interactive)
  (let* ((next-position (color-rg-find-next-position color-rg-regexp-position)))
    (if next-position
        (progn
          (goto-char next-position)
          (color-rg-open-file))
      (progn
        (message "Reach to the last line.")
        (goto-char (point-min))
        (call-interactively #'color-rg-jump-next-keyword)))))

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

(defun color-rg-insert-current-line ()
  (interactive)
  (let ((current-line (save-excursion
                        (beginning-of-line)
                        (search-forward-regexp color-rg-regexp-position nil t)
                        (setq start (point))
                        (end-of-line)
                        (setq end (point))
                        (buffer-substring start end)
                        )))
    (color-rg-quit)
    (insert current-line)))

(defun color-rg-open-file (&optional stay)
  (interactive)
  (setq color-rg-window-configuration-before-open (current-window-configuration))
  (let* ((match-file (color-rg-get-match-file))
         (match-line (color-rg-get-match-line))
         (match-column (color-rg-get-match-column))
         (match-buffer (color-rg-get-match-buffer match-file))
         in-org-link-content-p
         color-buffer-window)
    (save-excursion
      (let ((inhibit-message t))
        ;; Try fill variables when in org file.
        (color-rg-move-to-column match-column)
        (setq in-org-link-content-p
              (and (color-rg-is-org-file match-file)
                   (color-rg-in-org-link-content-p)))
        ;; Open file in other window.
        ;; Note, don't use `find-file-other-window', it will failed if path is tramp path that start with /sudo:root
        (other-window 1)
        (find-file match-file)
        ;; Add to temp list if file's buffer is not exist.
        (unless match-buffer
          (add-to-list 'color-rg-temp-visit-buffers (current-buffer)))
        ;; Jump to match point.
        ;; We use `ignore-errors' to make sure cursor will back to color-rg buffer
        ;; even target line is not exists in search file (such as delete by user).
        (ignore-errors
          (cond ((color-rg-is-org-file match-file)
                 ;; Jump to match position.
                 (color-rg-move-to-point match-line match-column)
                 ;; Expand org block if current file is *.org file.
                 (org-reveal)
                 ;; Jump to link beginning if keyword in content area.
                 (when in-org-link-content-p
                   (search-backward-regexp "\\[\\[" (line-beginning-position) t)))
                (t
                 (color-rg-move-to-point match-line match-column)))))
      ;; Flash match line.
      (color-rg-flash-line))
    (unless stay
      ;; Keep cursor in search buffer's window.
      (setq color-buffer-window (get-buffer-window color-rg-buffer))
      (if color-buffer-window
          (select-window color-buffer-window)
        ;; Split window and select if color-buffer is not exist in windows.
        (delete-other-windows)
        (split-window)
        (other-window 1)
        (switch-to-buffer color-rg-buffer)))
    ;; Ajust column position.
    (color-rg-move-to-column match-column)
    ))

(defun color-rg-back ()
  (interactive)
  (when color-rg-window-configuration-before-open
    (set-window-configuration color-rg-window-configuration-before-open)))

(defun color-rg-open-file-and-stay ()
  (interactive)
  (color-rg-open-file t))

(defun color-rg-flash-line ()
  (let ((pulse-iterations 1)
        (pulse-delay color-rg-flash-line-delay))
    ;; Flash match line.
    (pulse-momentary-highlight-one-line (point) 'color-rg-font-lock-flash)
    ;; View the function name when navigate in match line.
    (when color-rg-show-function-name-p
      (require 'which-func)
      (let ((function-name (which-function)))
        (when function-name
          (message "Located in function: %s"
                   (propertize
                    function-name
                    'face 'color-rg-font-lock-function-location
                    )))))))

(defun color-rg-in-org-link-content-p ()
  (and (looking-back "\\[\\[.*" (line-beginning-position))
       (looking-at ".*\\]\\[")
       (looking-at ".*\\]\\]")))

(defun color-rg-is-org-file (file)
  (string-equal (color-rg-file-extension file) "org"))

(defun color-rg-move-to-point (line column)
  ;; Jump to match position.
  (goto-line line)
  (beginning-of-line)

  (color-rg-jump-to-column column))

(defun color-rg-move-to-column (column)
  (beginning-of-line)
  (search-forward-regexp color-rg-regexp-position)

  ;; Forward to column if current line is not empty line (delete by `color-rg-delete-line').
  (unless (looking-at "[[:space:]]*$")
    (color-rg-jump-to-column column)))

(defun color-rg-jump-to-column (column)
  "This function use for jump to correct column positions in multi-byte strings.
Such as, mixed string of Chinese and English.

Function `move-to-column' can't handle mixed string of Chinese and English correctly."
  (let ((scan-column 0)
        (first-char-point (point)))

    (while (> column scan-column)
      (forward-char 1)
      (setq scan-column (string-bytes (buffer-substring first-char-point (point)))))

    (backward-char 1)))

(defun color-rg-switch-to-edit-mode ()
  (interactive)
  ;; Clone content to temp buffer.
  (color-rg-clone-to-temp-buffer)
  ;; Update header-line.
  (setf (color-rg-search-mode color-rg-cur-search) "Edit")

  ;; Set `query-replace-skip-read-only' to avoid read-only error when do `query-replace'.
  (set (make-local-variable 'query-replace-skip-read-only) t)
  (color-rg-update-header-line)
  ;; Turn off readonly mode.
  (read-only-mode -1)
  ;; Load edit keymap.
  (use-local-map color-rg-mode-edit-map)
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
  ;; Message to user.
  (message "Switch to edit mode: press C-c C-c to apply change, press C-c C-q cancel edit"))

(defun color-rg-quit ()
  (interactive)
  ;; Kill temp buffer open by color-rg.
  (when color-rg-kill-temp-buffer-p
    (dolist (temp-buffer color-rg-temp-visit-buffers)
      (kill-buffer temp-buffer)))
  (setq color-rg-temp-visit-buffers nil)
  ;; Kill search buffer.
  (kill-buffer color-rg-buffer)
  ;; Restore window configuration before search.
  (when color-rg-window-configuration-before-search
    (set-window-configuration color-rg-window-configuration-before-search)
    (goto-char color-rg-buffer-point-before-search)
    (setq color-rg-window-configuration-before-search nil)
    (setq color-rg-buffer-point-before-search nil)))

(defun color-rg-beginning-of-line ()
  (interactive)
  (let* ((row-column-position (color-rg-get-row-column-position)))
    (if row-column-position
        (goto-char row-column-position)
      (move-beginning-of-line 1))))

(defun color-rg-delete-line ()
  (interactive)
  (let* ((row-column-position (color-rg-get-row-column-position)))
    (when row-column-position
      (setq start row-column-position)
      (end-of-line)
      (setq end (point))
      (kill-region start end)
      )))

(defun color-rg-delete-all-lines ()
  (interactive)
  (save-excursion
    (with-current-buffer color-rg-buffer
      (goto-char (point-min))
      (while (search-forward-regexp color-rg-regexp-position nil t)
        (color-rg-delete-line)))))

(defun color-rg-recover-line ()
  (interactive)
  (color-rg-delete-line)
  (insert (color-rg-get-line-content color-rg-temp-buffer (line-number-at-pos))))

(defun color-rg-recover-buffer ()
  (interactive)
  (save-excursion
    (with-current-buffer color-rg-buffer
      (let ((inhibit-read-only t))
        ;; Recover buffer content from temp buffer.
        (color-rg-mode) ; switch to `color-rg-mode' first, otherwise `erase-buffer' will cause "save-excursion: end of buffer" error.
        (read-only-mode -1)
        (erase-buffer)
        (insert (with-current-buffer color-rg-temp-buffer
                  (buffer-substring (point-min) (point-max))))
        ;; Switch to edit mode.
        (color-rg-switch-to-edit-mode)
        ))))

(defun color-rg-apply-changed ()
  (interactive)
  (if (equal (length color-rg-changed-lines) 0)
      (message "Nothing need change.")
    ;; Save window configuration before do apply.
    (setq color-rg-window-configuration-before-apply (current-window-configuration))
    ;; Apply changed.
    (let ((inhibit-message t) ;don't flush to echo area when apply changed, optimise for color-rg
          (apply-files '()))
      (save-excursion
        (dolist (line color-rg-changed-lines)
          (let (match-file match-line changed-line-content)
            (setq changed-line-content (color-rg-get-line-content color-rg-buffer line))
            (with-current-buffer color-rg-buffer
              ;; Get match file and line.
              (goto-line line)
              (setq match-file (color-rg-get-match-file))
              (setq match-line (color-rg-get-match-line)))
            ;; Open file in other window.
            (find-file match-file)
            (add-to-list 'apply-files match-file)
            ;; Remove from temp list if file's buffer is exist.
            (setq color-rg-temp-visit-buffers (remove (current-buffer) color-rg-temp-visit-buffers))
            ;; Kill target line.
            (goto-line match-line)
            (kill-line)
            ;; Insert change line.
            (if (string-equal changed-line-content "")
                ;; Kill empty line if line mark as deleted.
                (kill-line)
              ;; Otherwise insert new line into file.
              (insert changed-line-content))))
        ;; Save files after change.
        (dolist (apply-file apply-files)
          (find-file apply-file)
          (basic-save-buffer))))
    ;; Restore window configuration before apply changed.
    (when color-rg-window-configuration-before-apply
      (set-window-configuration color-rg-window-configuration-before-apply)
      (setq color-rg-window-configuration-before-apply nil))
    ;; Message to user.
    (message (format "Apply %s lines" (length color-rg-changed-lines))))
  (color-rg-switch-to-view-mode))

(provide 'color-rg)

;;; color-rg.el ends here
