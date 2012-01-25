;;; git-logn.el --- Minor mode for incremental blame for Git  -*- coding: utf-8 -*-
;;
;; Copyright (C) 2007  David Kågedal
;;
;; Authors:    David Kågedal <davidk@lysator.liu.se>
;; Created:    31 Jan 2007
;; Message-ID: <87iren2vqx.fsf@morpheus.local>
;; License:    GPL
;; Keywords:   git, version control, release management
;;
;; Compatibility: Emacs21, Emacs22 and EmacsCVS
;;                Git 1.5 and up

;; This file is *NOT* part of GNU Emacs.
;; This file is distributed under the same terms as GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;; http://www.fsf.org/copyleft/gpl.html


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Here is an Emacs implementation of incremental git-logn.  When you
;; turn it on while viewing a file, the editor buffer will be updated by
;; setting the background of individual lines to a color that reflects
;; which commit it comes from.  And when you move around the buffer, a
;; one-line summary will be shown in the echo area.

;;; Installation:
;;
;; To use this package, put it somewhere in `load-path' (or add
;; directory with git-logn.el to `load-path'), and add the following
;; line to your .emacs:
;;
;;    (require 'git-logn)
;;
;; If you do not want to load this package before it is necessary, you
;; can make use of the `autoload' feature, e.g. by adding to your .emacs
;; the following lines
;;
;;    (autoload 'git-logn-mode "git-logn"
;;              "Minor mode for incremental blame for Git." t)
;;
;; Then first use of `M-x git-logn-mode' would load the package.

;;; Compatibility:
;;
;; It requires GNU Emacs 21 or later and Git 1.5.0 and up
;;
;; If you'are using Emacs 20, try changing this:
;;
;;            (overlay-put ovl 'face (list :background
;;                                         (cdr (assq 'color (cddddr info)))))
;;
;; to
;;
;;            (overlay-put ovl 'face (cons 'background-color
;;                                         (cdr (assq 'color (cddddr info)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile (require 'cl))			      ; to use `push', `pop'
(require 'format-spec)

(defface git-logn-prefix-face
  '((((background dark)) (:foreground "gray"
                          :background "black"))
    (((background light)) (:foreground "gray"
                           :background "white"))
    (t (:weight bold)))
  "The face used for the hash prefix."
  :group 'git-logn)

(defgroup git-logn nil
  "A minor mode showing Git blame information."
  :group 'git
  :link '(function-link git-logn-mode))


(defcustom git-logn-use-colors t
  "Use colors to indicate commits in `git-logn-mode'."
  :type 'boolean
  :group 'git-logn)

(defcustom git-logn-mouseover-format
  "%h %a %A: %s"
  "The format of the description shown when pointing at a line in
`git-logn' mode. The format string is passed to `format-spec'
with the following format keys:

  %h - the abbreviated hash
  %H - the full hash
  %a - the author name
  %A - the author email
  %c - the committer name
  %C - the committer email
  %s - the commit summary
"
  :group 'git-logn)


(defun git-logn-color-scale (&rest elements)
  "Given a list, returns a list of triples formed with each
elements of the list.

a b => bbb bba bab baa abb aba aaa aab"
  (let (result)
    (dolist (a elements)
      (dolist (b elements)
        (dolist (c elements)
          (setq result (cons (format "#%s%s%s" a b c) result)))))
    result))

;; (git-logn-color-scale "0c" "04" "24" "1c" "2c" "34" "14" "3c") =>
;; ("#3c3c3c" "#3c3c14" "#3c3c34" "#3c3c2c" "#3c3c1c" "#3c3c24"
;; "#3c3c04" "#3c3c0c" "#3c143c" "#3c1414" "#3c1434" "#3c142c" ...)

(defvar git-logn-log-oneline-format
  "format:[%cr] %cn: %s"
  "*Formatting option used for describing current line in the minibuffer.

This option is used to pass to git log --pretty= command-line option,
and describe which commit the current line was made.")

(defvar git-logn-dark-colors
  ;; (git-logn-color-scale "0c" "04" "24" "1c" "2c" "34" "14" "3c")
  (list "#57575a" "#505050"
        "#474747" "#404040"
        "#373737" "#303030"
        "#272727" "#202020"
        "#171717" "#101010")
  "*List of colors (format #RGB) to use in a dark environment.

To check out the list, evaluate (list-colors-display git-logn-dark-colors).")

(defvar git-logn-light-colors
  (git-logn-color-scale "c4" "d4" "cc" "dc" "f4" "e4" "fc" "ec")
  "*List of colors (format #RGB) to use in a light environment.

To check out the list, evaluate (list-colors-display git-logn-light-colors).")

(defvar git-logn-colors '()
  "Colors used by git-logn. The list is built once when activating git-logn
minor mode.")

(defvar git-logn-ancient-color "#000000"
  "*Color to be used for ancient commit.")

(defvar git-logn-autoupdate t
  "*Automatically update the blame display while editing")

(defvar git-logn-proc nil
  "The running git-logn process")
(make-variable-buffer-local 'git-logn-proc)

(defvar git-logn-overlays nil
  "The git-logn overlays used in the current buffer.")
(make-variable-buffer-local 'git-logn-overlays)

(defvar git-logn-cache nil
  "A cache of git-logn information for the current buffer")
(make-variable-buffer-local 'git-logn-cache)

(defvar git-logn-idle-timer nil
  "An idle timer that updates the blame")
(make-variable-buffer-local 'git-logn-cache)

(defvar git-logn-update-queue nil
  "A queue of update requests")
(make-variable-buffer-local 'git-logn-update-queue)

;; FIXME: docstrings
(defvar git-logn-file nil)
(defvar git-logn-current nil)

(defvar git-logn-mode nil)
(make-variable-buffer-local 'git-logn-mode)

(defvar git-logn-mode-line-string " logn"
  "String to display on the mode line when git-logn is active.")

(or (assq 'git-logn-mode minor-mode-alist)
    (setq minor-mode-alist
          (cons '(git-logn-mode git-logn-mode-line-string) minor-mode-alist)))

;;;###autoload
(defun git-logn-mode (&optional arg)
  "Toggle minor mode for displaying Git blame

With prefix ARG, turn the mode on if ARG is positive."
  (interactive "P")
  (cond
   ((null arg)
    (if git-logn-mode (git-logn-mode-off) (git-logn-mode-on)))
   ((> (prefix-numeric-value arg) 0) (git-logn-mode-on))
   (t (git-logn-mode-off))))

(defun git-logn-mode-on ()
  "Turn on git-logn mode.

See also function `git-logn-mode'."
  (make-local-variable 'git-logn-colors)
  (if git-logn-autoupdate
      (add-hook 'after-change-functions 'git-logn-after-change nil t)
    (remove-hook 'after-change-functions 'git-logn-after-change t))
  (git-logn-cleanup)
  (let ((bgmode (cdr (assoc 'background-mode (frame-parameters)))))
    (if (eq bgmode 'dark)
        (setq git-logn-colors git-logn-dark-colors)
      (setq git-logn-colors git-logn-light-colors)))
  (setq git-logn-cache (make-hash-table :test 'equal))
  (setq git-logn-mode t)
  (git-logn-run))

(defun git-logn-mode-off ()
  "Turn off git-logn mode.

See also function `git-logn-mode'."
  (git-logn-cleanup)
  (if git-logn-idle-timer (cancel-timer git-logn-idle-timer))
  (setq git-logn-mode nil))

;;;###autoload
(defun git-reblame ()
  "Recalculate all blame information in the current buffer"
  (interactive)
  (unless git-logn-mode
    (error "git-logn is not active"))

  (git-logn-cleanup)
  (git-logn-run))

(defun git-logn-run (&optional startline endline)
  (if git-logn-proc
      ;; Should maybe queue up a new run here
      (message "Already running git blame")
    (let ((display-buf (current-buffer))
          (blame-buf (get-buffer-create
                      (concat " git blame for " (buffer-name))))
          (args '("--incremental" "--since=2.days" "--contents" "-")))
      (if startline
          (setq args (append args
                             (list "-L" (format "%d,%d" startline endline)))))
      (setq args (append args
                         (list (file-name-nondirectory buffer-file-name))))
      (setq git-logn-proc
            (apply 'start-process
                   "git-logn" blame-buf
                   "git" "blame"
                   args))
      (with-current-buffer blame-buf
        (erase-buffer)
        (make-local-variable 'git-logn-file)
        (make-local-variable 'git-logn-current)
        (setq git-logn-file display-buf)
        (setq git-logn-current nil))
      (set-process-filter git-logn-proc 'git-logn-filter)
      (set-process-sentinel git-logn-proc 'git-logn-sentinel)
      (process-send-region git-logn-proc (point-min) (point-max))
      (process-send-eof git-logn-proc))))

(defun remove-git-logn-text-properties (start end)
  (let ((modified (buffer-modified-p))
        (inhibit-read-only t))
    (remove-text-properties start end '(point-entered nil))
    (set-buffer-modified-p modified)))

(defun git-logn-cleanup ()
  "Remove all blame properties"
    (mapcar 'delete-overlay git-logn-overlays)
    (setq git-logn-overlays nil)
    (remove-git-logn-text-properties (point-min) (point-max)))

(defun git-logn-update-region (start end)
  "Rerun blame to get updates between START and END"
  (let ((overlays (overlays-in start end)))
    (while overlays
      (let ((overlay (pop overlays)))
        (if (< (overlay-start overlay) start)
            (setq start (overlay-start overlay)))
        (if (> (overlay-end overlay) end)
            (setq end (overlay-end overlay)))
        (setq git-logn-overlays (delete overlay git-logn-overlays))
        (delete-overlay overlay))))
  (remove-git-logn-text-properties start end)
  ;; We can be sure that start and end are at line breaks
  (git-logn-run (1+ (count-lines (point-min) start))
                 (count-lines (point-min) end)))

(defun git-logn-sentinel (proc status)
  (with-current-buffer (process-buffer proc)
    (with-current-buffer git-logn-file
      (setq git-logn-proc nil)
      (if git-logn-update-queue
          (git-logn-delayed-update))))
  ;;(kill-buffer (process-buffer proc))
  ;;(message "git blame finished")
  )

(defvar in-blame-filter nil)

(defun git-logn-filter (proc str)
  (save-excursion
    (set-buffer (process-buffer proc))
    (goto-char (process-mark proc))
    (insert-before-markers str)
    (goto-char 0)
    (unless in-blame-filter
      (let ((more t)
            (in-blame-filter t))
        (while more
          (setq more (git-logn-parse)))))))

(defun git-logn-parse ()
  (cond ((looking-at "\\([0-9a-f]\\{40\\}\\) \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\)\n")
         (let ((hash (match-string 1))
               (src-line (string-to-number (match-string 2)))
               (res-line (string-to-number (match-string 3)))
               (num-lines (string-to-number (match-string 4))))
           (delete-region (point) (match-end 0))
           (setq git-logn-current (list (git-logn-new-commit hash)
                                         src-line res-line num-lines)))
         t)
        ((looking-at "\\([a-z-]+\\) \\(.+\\)\n")
         (let ((key (match-string 1))
               (value (match-string 2)))
           (delete-region (point) (match-end 0))
           (git-logn-add-info (car git-logn-current) key value)
           (when (string= key "filename")
             (git-logn-create-overlay (car git-logn-current)
                                       (caddr git-logn-current)
                                       (cadddr git-logn-current))
             (setq git-logn-current nil)))
         t)
        (t
         nil)))

(defun git-logn-new-commit (hash)
  (with-current-buffer git-logn-file
    (or (gethash hash git-logn-cache)
        ;; Assign a random color to each new commit info
        ;; Take care not to select the same color multiple times
        (let* ((color (if git-logn-colors
                          (pop git-logn-colors)
                        git-logn-ancient-color))
               (info `(,hash (color . ,color))))
          (puthash hash info git-logn-cache)
          info))))

(defun git-logn-create-overlay (info start-line num-lines)
  (save-excursion
    (set-buffer git-logn-file)
    (let ((inhibit-point-motion-hooks t)
          (inhibit-modification-hooks t))
      (goto-line start-line)
      (let* ((start (point))
             (end (progn (forward-line num-lines) (point)))
             (ovl (make-overlay start end))
             (hash (car info))
             (spec `((?h . ,(substring hash 0 6))
                     (?H . ,hash)
                     (?a . ,(git-logn-get-info info 'author))
                     (?A . ,(git-logn-get-info info 'author-mail))
                     (?c . ,(git-logn-get-info info 'committer))
                     (?C . ,(git-logn-get-info info 'committer-mail))
                     (?s . ,(git-logn-get-info info 'summary)))))
        (push ovl git-logn-overlays)
        (overlay-put ovl 'git-logn info)
        (overlay-put ovl 'help-echo
                     (format-spec git-logn-mouseover-format spec))
        (if git-logn-use-colors
            (overlay-put ovl 'face (list :background
                                         (cdr (assq 'color (cdr info))))))
        ))))

(defun git-logn-add-info (info key value)
  (nconc info (list (cons (intern key) value))))

(defun git-logn-get-info (info key)
  (cdr (assq key (cdr info))))

(defun git-logn-current-commit ()
  (let ((info (get-char-property (point) 'git-logn)))
    (if info
        (car info)
      (error "No commit info"))))

(defun git-describe-commit (hash)
  (with-temp-buffer
    (call-process "git" nil t nil
                  "log" "-1"
                  (concat "--pretty=" git-logn-log-oneline-format)
                  hash)
    (buffer-substring (point-min) (point-max))))

(defvar git-logn-last-identification nil)
(make-variable-buffer-local 'git-logn-last-identification)
(defun git-logn-identify (&optional hash)
  (interactive)
  (let ((info (gethash (or hash (git-logn-current-commit)) git-logn-cache)))
    (when (and info (not (eq info git-logn-last-identification)))
      (message "%s" (nth 4 info))
      (setq git-logn-last-identification info))))

;; (defun git-logn-after-save ()
;;   (when git-logn-mode
;;     (git-logn-cleanup)
;;     (git-logn-run)))
;; (add-hook 'after-save-hook 'git-logn-after-save)

(defun git-logn-after-change (start end length)
  (when git-logn-mode
    (git-logn-enq-update start end)))

(defvar git-logn-last-update nil)
(make-variable-buffer-local 'git-logn-last-update)
(defun git-logn-enq-update (start end)
  "Mark the region between START and END as needing blame update"
  ;; Try to be smart and avoid multiple callouts for sequential
  ;; editing
  (cond ((and git-logn-last-update
              (= start (cdr git-logn-last-update)))
         (setcdr git-logn-last-update end))
        ((and git-logn-last-update
              (= end (car git-logn-last-update)))
         (setcar git-logn-last-update start))
        (t
         (setq git-logn-last-update (cons start end))
         (setq git-logn-update-queue (nconc git-logn-update-queue
                                             (list git-logn-last-update)))))
  (unless (or git-logn-proc git-logn-idle-timer)
    (setq git-logn-idle-timer
          (run-with-idle-timer 0.5 nil 'git-logn-delayed-update))))

(defun git-logn-delayed-update ()
  (setq git-logn-idle-timer nil)
  (if git-logn-update-queue
      (let ((first (pop git-logn-update-queue))
            (inhibit-point-motion-hooks t))
        (git-logn-update-region (car first) (cdr first)))))

(provide 'git-logn)

;;; git-logn.el ends here
