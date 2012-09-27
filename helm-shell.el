;;; helm-shell.el --- pcomplete and shell completion for helm.

;; Created: 2012-09-28 01:54
;; Last Updated: Joseph 2012-09-28 01:56:34 星期五
;; Version: 0.1
;; Author: Joseph(纪秀峰)  jixiuf@gmail.com
;; Keywords: helm emacs shell complete pcomplete
;; URL: https://github.com/jixiuf/helm-shell

;; Copyright (C) 2012 Joseph(纪秀峰) all rights reserved.

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

;; Just put ctags-update.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (add-hook 'shell-mode-hook 'pcomplete-shell-setup)
;; (add-hook 'shell-mode-hook
;;           #'(lambda ()
;;               (define-key shell-mode-map [remap pcomplete] 'helm-shell-pcomplete)))

(eval-when-compile (require 'cl))
(require 'helm)
(require 'helm-elisp)
(require 'helm-regexp)
(require 'pcomplete)
(defvar helm-shell-history-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-p") 'helm-next-line)
    map)
  "Keymap for `helm-shell-history'.")

(defvar helm-c-source-shell
  '((name . "shell completions")
    (init . (lambda ()
              (setq pcomplete-current-completions nil
                    pcomplete-last-completion-raw nil)
              ;; Eshell-command add this hook in all minibuffers
              ;; Remove it for the helm one. (Fixed in Emacs24)
              ;; (remove-hook 'minibuffer-setup-hook 'eshell-mode)
              ))
    (candidates . helm-shell-get-candidates)
    (filtered-candidate-transformer
     (lambda (candidates _sources)
       (loop for i in candidates collect
             (cons (abbreviate-file-name i) i))))
    (action . helm-ec-insert))
  "Helm source for shell completion.")

;; Internal.
(defvar helm-ec-target "")
(defun helm-ec-insert (candidate)
  "Replace text at point with CANDIDATE.
The function that call this should set `helm-ec-target' to thing at point."
  (let ((pt (point)))
    (when (and helm-ec-target
               (search-backward helm-ec-target nil t)
               (string= (buffer-substring (point) pt) helm-ec-target))
      (delete-region (point) pt)))
  (if (string-match "\\`~/" helm-ec-target)
      (insert (helm-quote-whitespace (abbreviate-file-name candidate)))
      (insert (helm-quote-whitespace candidate))))

(defun helm-shell-get-candidates ()
  "Get candidates for shell completion using `pcomplete'."
  (catch 'pcompleted
    (with-helm-current-buffer
      (let* ((pcomplete-stub)
             pcomplete-seen pcomplete-norm-func
             pcomplete-args pcomplete-last pcomplete-index
             (pcomplete-autolist pcomplete-autolist)
             (pcomplete-suffix-list pcomplete-suffix-list)
             (table (pcomplete-completions))
             (entry (condition-case nil
                        ;; On Emacs24 `try-completion' return
                        ;; pattern when more than one result.
                        ;; Otherwise Emacs23 return nil, which
                        ;; is wrong, in this case use pattern
                        ;; to behave like Emacs24.
                        (or (try-completion helm-pattern
                                            (pcomplete-entries))
                            helm-pattern)
                      ;; In Emacs23 `pcomplete-entries' may fail
                      ;; with error, so try this instead.
                      (error
                       nil
                       (let ((fc (car (last
                                       (pcomplete-parse-arguments)))))
                         ;; Check if last arg require fname completion.
                         (and (file-name-directory fc) fc))))))
        (loop for i in (all-completions pcomplete-stub table)
              for file-cand = (and entry
                                   (if (file-remote-p i) i
                                       (expand-file-name
                                        i (file-name-directory entry))))
              if (and file-cand (or (file-remote-p file-cand)
                                    (file-exists-p file-cand)))
              collect file-cand into ls
              else collect i into ls
              finally return
              (if (and entry (not (string= entry "")) (file-exists-p entry))
                  (append (list (expand-file-name entry default-directory)) ls)
                  ls))))))
;;;###autoload
(defun helm-shell-pcomplete ()
  "Preconfigured helm to provide helm completion in shell."
  (interactive)
  (let* ((helm-quit-if-no-candidate t)
         (helm-execute-action-at-once-if-one t)
         ;; (end (point-marker))
         ;; (beg (save-excursion (move-beginning-of-line) (point)))
         (args (pcomplete-parse-arguments))
         ;; (args (catch 'eshell-incomplete
         ;;         (eshell-parse-arguments beg end)))
         ;; Use thing-at-point instead of last args value
         ;; to exclude possible delimiters e.g "(".
         ;;(target  (thing-at-point 'symbol))
         (target  (car  (last args)))
         (first (car args)) ; Maybe lisp delimiter "(".
         last) ; Will be the last but parsed by pcomplete.
    (setq helm-ec-target (or target " ")
          end (point)
          ;; Reset beg for `with-helm-show-completion'.
          beg (or (and target (- end (length target)))
                  ;; Nothing at point.
                  (progn (insert " ") (point))))
    (cond ((eq first ?\()
           (helm-lisp-completion-or-file-name-at-point))
          ;; In eshell `pcomplete-parse-arguments' is called
          ;; with `pcomplete-parse-arguments-function'
          ;; locally bound to `eshell-complete-parse-arguments'
          ;; which is calling `lisp-complete-symbol',
          ;; calling it before would popup the
          ;; *completions* buffer.
          (t (setq last (car (last (ignore-errors
                                     (pcomplete-parse-arguments)))))
             (with-helm-show-completion beg end
               (helm :sources 'helm-c-source-shell
                     :buffer "*helm pcomplete*"
                     :resume 'noresume
                     :input (and (stringp last)
                                 (helm-ff-set-pattern last))))))))
