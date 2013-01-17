;;; helm-ls-git.el --- list git files.

;; Copyright (C) 2012 Thierry Volpiatto <thierry.volpiatto@gmail.com>

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

;;; Code

(require 'helm-locate)
(require 'helm-files)


(defgroup helm-ls-git nil
  "Helm completion for git repos."
  :group 'helm)

(defcustom helm-ls-git-show-abs-or-relative 'absolute
  "Show full path or relative path to repo when using `helm-ff-toggle-basename'.
Valid values are symbol 'abs (default) or 'relative."
  :group 'helm-ls-git
  :type  '(radio :tag "Show full path or relative path to Git repo when toggling"
           (const :tag "Show full path" absolute)
           (const :tag "Show relative path" relative)))

(defcustom helm-ls-git-status-command 'vc-dir
  "Favorite git-status command for emacs."
  :group 'helm-ls-git
  :type 'symbol)

;; Append visited files from `helm-c-source-ls-git' to `file-name-history'.
(add-to-list 'helm-file-completion-sources "Git files")


(defvar helm-ls-git-log-file nil) ; Set it for debugging.


(defun helm-ls-git-list-files ()
  (when (and helm-ls-git-log-file
             (file-exists-p helm-ls-git-log-file))
    (delete-file helm-ls-git-log-file))
  ;; `helm-resume' will use the value of `helm-default-directory'
  ;; as value for `default-directory'.
  (helm-aif (helm-ls-git-root-dir)
      (with-helm-default-directory it
          (with-output-to-string
              (with-current-buffer standard-output
                (apply #'process-file
                       "git"
                       nil (list t helm-ls-git-log-file) nil
                       (list "ls-files" "--full-name" "--")))))))

(defun* helm-ls-git-root-dir (&optional (directory default-directory))
  (let ((root (locate-dominating-file directory ".git")))
    (and root (file-name-as-directory root))))

(defun helm-ls-git-not-inside-git-repo ()
  (not (helm-ls-git-root-dir)))

(defun helm-ls-git-transformer (candidates source)
  (loop with root = (helm-ls-git-root-dir helm-default-directory)
        for i in candidates
        for abs = (expand-file-name i root)
        for disp = (if (and helm-ff-transformer-show-only-basename
                            (not (string-match "[.]\\{1,2\\}$" i)))
                       (helm-c-basename i) (case helm-ls-git-show-abs-or-relative
                                             (absolute abs)
                                             (relative i)))
        collect
        (cons (propertize disp 'face 'helm-ff-file) abs)))

(defun helm-ls-git-init ()
  (let ((data (helm-ls-git-list-files)))
    (when (string= data "")
      (setq data
            (if helm-ls-git-log-file
                (with-current-buffer
                    (find-file-noselect helm-ls-git-log-file)
                  (prog1
                      (buffer-substring-no-properties
                       (point-min) (point-max))
                    (kill-buffer)))
                data)))
    (helm-init-candidates-in-buffer
     "*lsgit*" data)))

(defvar helm-c-source-ls-git
  `((name . "Git files")
    (init . helm-ls-git-init)
    (candidates-in-buffer)
    (keymap . ,helm-generic-files-map)
    (filtered-candidate-transformer . helm-ls-git-transformer)
    (action-transformer helm-c-transform-file-load-el)
    (action . ,(cdr (helm-get-actions-from-type helm-c-source-locate)))))


(defun helm-ls-git-grep (candidate)
  (let* ((helm-c-grep-default-command "git grep -n%cH --full-name -e %p %f")
         helm-c-grep-default-recurse-command
         (exts (helm-c-grep-guess-extensions (helm-marked-candidates)))
         (globs (format "'%s'" (mapconcat 'identity exts " ")))
         (files (cond ((equal helm-current-prefix-arg '(4))
                       (list "--" (read-string "OnlyExt(*.[ext]): " globs)))
                      ((equal helm-current-prefix-arg '(16))
                       '("--"))
                      (t (helm-marked-candidates))))
         ;; Expand filename of each candidate with the git root dir.
         ;; The filename will be in the help-echo prop.
         (helm-c-grep-default-directory-fn 'helm-ls-git-root-dir)
         ;; `helm-c-grep-init' initialize `default-directory' to this value,
         ;; So set this value (i.e `helm-ff-default-directory') to
         ;; something else.
         (helm-ff-default-directory (file-name-directory candidate)))
    (helm-do-grep-1 files)))

(helm-add-action-to-source
 "Git grep files (`C-u' only, `C-u C-u' all)"
 'helm-ls-git-grep helm-c-source-ls-git 3)

(helm-add-action-to-source
 "Search in Git log (C-u show patch)"
 'helm-ls-git-search-log
 helm-c-source-ls-git 4)


(defun helm-ls-git-search-log (_candidate)
  (let* ((query (read-string "Search log: "))
         (coms (if helm-current-prefix-arg
                   (list "log" "-p" "--grep" query)
                   (list "log" "--grep" query))))
    (with-current-buffer (get-buffer-create "*helm ls log*")
      (set (make-local-variable 'buffer-read-only) nil)
      (erase-buffer)
      (apply #'process-file "git" nil (list t nil) nil coms)))
  (pop-to-buffer "*helm ls log*")
  (goto-char (point-min))
  (diff-mode)
  (set (make-local-variable 'buffer-read-only) t))


(defun helm-ls-git-status ()
  (when (and helm-ls-git-log-file
             (file-exists-p helm-ls-git-log-file))
    (delete-file helm-ls-git-log-file))
  (with-output-to-string
      (with-current-buffer standard-output
        (apply #'process-file
               "git"
               nil (list t helm-ls-git-log-file) nil
               (list "status" "--porcelain")))))

(defun helm-ls-git-status-transformer (candidates source)
  (loop with root = (helm-ls-git-root-dir helm-default-directory)
        for i in candidates
        collect
        (cond ((string-match "^\\( M \\)\\(.*\\)" i) ; modified.
               (cons (propertize i 'face '((:foreground "yellow")))
                     (expand-file-name (match-string 2 i) root)))
              ((string-match "^\\(M+ *\\)\\(.*\\)" i) ; modified and staged.
               (cons (propertize i 'face '((:foreground "Gold")))
                     (expand-file-name (match-string 2 i) root)))
               ((string-match "^\\([?]\\{2\\} \\)\\(.*\\)" i)
                (cons (propertize i 'face '((:foreground "red")))
                      (expand-file-name (match-string 2 i) root)))
               ((string-match "^\\([ARC] +\\)\\(.*\\)" i)
                (cons (propertize i 'face '((:foreground "green")))
                      (expand-file-name (match-string 2 i) root)))
               ((string-match "^\\( [D] \\)\\(.*\\)" i)
                (cons (propertize i 'face '((:foreground "Darkgoldenrod3")))
                      (expand-file-name (match-string 2 i) root)))
               ((string-match "^\\([D] \\)\\(.*\\)" i)
                (cons (propertize i 'face '((:foreground "DimGray")))
                      (expand-file-name (match-string 2 i) root)))
               (t i))))

(defvar helm-c-source-ls-git-status
  '((name . "Git status")
    (init . (lambda ()
              (helm-init-candidates-in-buffer
               "*hgit status*"
               (helm-ls-git-status))))
    (candidates-in-buffer)
    (filtered-candidate-transformer . helm-ls-git-status-transformer)
    (persistent-action . helm-ls-git-diff)
    (persistent-help . "Diff")
    (action-transformer . helm-ls-git-status-action-transformer)
    (action . (("Find file" . helm-find-many-files)
               ("Git status" . (lambda (_candidate)
                                 (with-current-buffer helm-buffer
                                   (funcall helm-ls-git-status-command
                                            helm-default-directory))))))))

(defun helm-ls-git-status-action-transformer (actions candidate)
  (let ((disp (helm-get-selection nil t)))
    (cond ((string-match "^[?]\\{2\\}" disp)
           (append actions
                   (list '("Add file(s)"
                           . (lambda (candidate)
                               (let ((default-directory
                                      (file-name-directory candidate))
                                     (marked (helm-marked-candidates)))
                                 (vc-call-backend 'Git 'register marked))))
                         '("Copy bnames to .gitignore"
                           . (lambda (candidate)
                               (let ((default-directory
                                      (file-name-directory candidate))
                                     (marked (helm-marked-candidates)))
                                 (with-current-buffer (find-file-noselect
                                                       (expand-file-name
                                                        ".gitignore"
                                                        (helm-ls-git-root-dir)))
                                   (goto-char (point-max))
                                   (loop with last-bname 
                                         for f in marked
                                         for bname = (helm-c-basename f)
                                         unless (string= bname last-bname)
                                         do (insert (concat bname "\n"))
                                         do (setq last-bname bname))
                                   (save-buffer))))))))
          ((string-match "^ ?M+ *" disp)
           (append actions (list '("Diff file" . helm-ls-git-diff)
                                 '("Commit file(s)"
                                   . (lambda (candidate)
                                       (let* ((marked (helm-marked-candidates))
                                              (default-directory
                                               (file-name-directory (car marked))))
                                         (vc-checkin marked 'Git))))
                                 '("Revert file" . vc-git-revert))))
          ((string-match "^ D " disp)
           (append actions (list '("Git delete" . vc-git-delete-file))))
          (t actions))))

(defun helm-ls-git-diff (candidate)
  (with-current-buffer (find-file-noselect candidate)
    (call-interactively #'vc-diff)))


;;;###autoload
(defun helm-ls-git-ls ()
  (interactive)
  (helm :sources '(helm-c-source-ls-git-status
                   helm-c-source-ls-git)
        ;; When `helm-ls-git-ls' is called from lisp
        ;; `default-directory' is normally let-bounded,
        ;; to some other value;
        ;; we now set this new let-bounded value local
        ;; to `helm-default-directory'.
        :default-directory default-directory
        :buffer "*helm lsgit*"))


;;; Helm-find-files integration.
;;
(defun helm-ff-ls-git-find-files (candidate)
  (let ((default-directory helm-ff-default-directory))
    (helm-run-after-quit
     #'(lambda (d)
         (let ((default-directory d))
           (helm-ls-git-ls)))
     default-directory)))

(defun helm-ls-git-ff-dir-git-p (file)
  (when (or (file-exists-p file)
            (file-directory-p file))
    (stringp (condition-case nil
                 (helm-ls-git-root-dir
                  helm-ff-default-directory)
               (error nil)))))

(when (require 'helm-files)
  (helm-add-action-to-source-if
   "Git ls-files"
   'helm-ff-ls-git-find-files
   helm-c-source-find-files
   'helm-ls-git-ff-dir-git-p
   4))

(provide 'helm-ls-git)

;;; helm-ls-git.el ends here
