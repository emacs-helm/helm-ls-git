;;; helm-ls-git.el --- list git files. -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2015 Thierry Volpiatto <thierry.volpiatto@gmail.com>

;; Package-Requires: ((helm "1.7.8"))

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

(require 'cl-lib)
(require 'vc)
(require 'vc-git)
(require 'helm-files) ; helm-grep is required in helm-files.
(require 'helm-types)

(defvaralias 'helm-c-source-ls-git 'helm-source-ls-git)
(make-obsolete-variable 'helm-c-source-ls-git 'helm-source-ls-git "1.5.1")
(defvaralias 'helm-c-source-ls-git-status 'helm-source-ls-git-status)
(make-obsolete-variable 'helm-c-source-ls-git-status 'helm-source-ls-git-status "1.5.1")

(defvar inhibit-magit-refresh)
(declare-function magit-stage-file "ext:magit-apply")
(declare-function magit-unstage-file "ext:magit-apply")
(declare-function magit-commit "ext:magit-commit")

;; Define the sources.
(defvar helm-source-ls-git-status nil
  "This source will built at runtime.
It can be build explicitely with function
`helm-ls-git-build-git-status-source'.")
(defvar helm-source-ls-git nil
  "This source will built at runtime.
It can be build explicitely with function
`helm-ls-git-build-ls-git-source'.")
(defvar helm-source-ls-git-buffers nil
  "This source will built at runtime.
It can be build explicitely with function
`helm-ls-git-build-buffers-source'.")



(defgroup helm-ls-git nil
  "Helm completion for git repos."
  :group 'helm)

(defcustom helm-ls-git-show-abs-or-relative 'relative
  "Show full path or relative path to repo when using `helm-ff-toggle-basename'.
Valid values are symbol 'absolute or 'relative (default)."
  :group 'helm-ls-git
  :type  '(radio :tag "Show full path or relative path to Git repo when toggling"
           (const :tag "Show full path" absolute)
           (const :tag "Show relative path" relative)))

(defcustom helm-ls-git-status-command 'vc-dir
  "Favorite git-status command for emacs.

If you want to use magit use `magit-status-internal' and not
`magit-status' which is working only interactively."
  :group 'helm-ls-git
  :type 'symbol)

(defcustom helm-ls-git-fuzzy-match nil
  "Enable fuzzy matching in `helm-source-ls-git-status' and `helm-source-ls-git'."
  :group 'helm-ls-git
  :set (lambda (var val)
         (set var val)
         (setq helm-source-ls-git nil
               helm-source-ls-git-status nil
               helm-source-ls-git-buffers nil))
  :type 'boolean)

;; Now the git-grep command is defined in helm-grep.el,
;; alias it for backward compatibility.
(defvar helm-ls-git-grep-command)
(defvaralias 'helm-ls-git-grep-command 'helm-grep-git-grep-command)
(make-obsolete-variable 'helm-ls-git-grep-command 'helm-grep-git-grep-command "1.8.0")

(defcustom helm-ls-git-default-sources '(helm-source-ls-git-status
                                         helm-source-ls-git-buffers
                                         helm-source-ls-git)
  "Default sources for `helm-ls-git-ls'."
  :group 'helm-ls-git
  :type '(repeat symbol))

(defcustom helm-ls-git-format-glob-string "'%s'"
  "String to format globs in `helm-grep-get-file-extensions'.
Glob are enclosed in single quotes by default."
  :group 'helm-ls-git
  :type 'string)

(defcustom helm-ls-git-ls-switches '("ls-files" "--full-name" "--")
  "A list of arguments to pass to `git-ls-files'.
To see files in submodules add the option \"--recurse-submodules\"."
  :type '(repeat string)
  :group 'helm-ls-git)

(defface helm-ls-git-modified-not-staged-face
    '((t :foreground "yellow"))
  "Files which are modified but not yet staged."
  :group 'helm-ls-git)

(defface helm-ls-git-modified-and-staged-face
    '((t :foreground "Goldenrod"))
  "Files which are modified and already staged."
  :group 'helm-ls-git)

(defface helm-ls-git-renamed-modified-face
    '((t :foreground "Goldenrod"))
  "Files which are renamed or renamed and modified."
  :group 'helm-ls-git)

(defface helm-ls-git-untracked-face
    '((t :foreground "red"))
  "Files which are not yet tracked by git."
  :group 'helm-ls-git)

(defface helm-ls-git-added-copied-face
    '((t :foreground "green"))
  "Files which are newly added or copied."
  :group 'helm-ls-git)

(defface helm-ls-git-added-modified-face
    '((t :foreground "blue"))
  "Files which are newly added and have unstaged modifications."
  :group 'helm-ls-git)

(defface helm-ls-git-deleted-not-staged-face
    '((t :foreground "Darkgoldenrod3"))
  "Files which are deleted but not staged."
  :group 'helm-ls-git)

(defface helm-ls-git-deleted-and-staged-face
    '((t :foreground "DimGray"))
  "Files which are deleted and staged."
  :group 'helm-ls-git)

(defface helm-ls-git-conflict-face
    '((t :foreground "MediumVioletRed"))
  "Files which contain rebase/merge conflicts."
  :group 'helm-ls-git)


(defvar helm-ls-git-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-generic-files-map)
    (define-key map (kbd "C-s")   'helm-ff-run-grep)
    (define-key map (kbd "M-g g") 'helm-ls-git-run-grep)
    (define-key map (kbd "C-c g") 'helm-ff-run-gid)
    (define-key map (kbd "C-c i") 'helm-ls-git-ls-files-show-others)
    map))

(defvar helm-ls-git-buffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-buffer-map)
    (define-key map (kbd "C-c i") 'helm-ls-git-ls-files-show-others)
    map))

(defvar helm-ls-git-help-message
  "* Helm ls git

** Tips

*** Start helm-ls-git

You can start with `helm-ls-git-ls' but you can also use the generic
`helm-browse-project' which will use `helm-ls-git' if you are in a git
project (actually supported backends are git, hg and svn). 

*** You may want to use magit as git status command

By default helm-ls-git is using emacs `vc-dir' as `helm-ls-git-status-command',
perhaps you want to use something better like `magit-status' ?
If it's the case use `magit-status-internal' as value for `helm-ls-git-status-command'
as `magit-status' is working only interactively (it will not work from helm-ls-git).

*** Git grep usage

The behavior is not exactly the same as what you have when you
launch git-grep from `helm-find-files', here in what it differ:

1) The prefix arg allow to grep only the `default-directory' whereas
with `helm-find-files' the prefix arg allow browsing the whole repo.
So with `helm-ls-git' the default is to grep the whole repo.

2) With `helm-ls-git', because you have the whole list of files of the repo
you can mark some of the files to grep only those, if no files are marked grep
the whole repo or the files under current directory depending of prefix arg.

NOTE: The previous behavior was prompting user for the file
extensions to grep, this is non sense because we have here the
whole list of files (recursive) of current repo and not only the
file under current directory, so we have better time
selectionning the files we want to grep.

**** With no prefix arg.

Git grep all files in current repository.

**** With one prefix arg.

Git grep all files in current directory i.e. `default-directory'.
It may be the `default-directory' from the buffer you started
from or the directory from where you launched `helm-ls-git' from
`helm-find-files'.

**** Grep a subdirectory of current repository.

Switch to `helm-find-files' with `C-x C-f', navigate to your directory
and launch git-grep from there.

** Commands
\\<helm-ls-git-map>
\\[helm-ls-git-run-grep]\t\tRun git-grep.
\\[helm-ff-run-gid]\t\tRun Gid.
\\[helm-ls-git-ls-files-show-others]\t\tToggle tracked/non tracked files view.
\\<helm-generic-files-map>
\\[helm-ff-run-toggle-basename]\t\tToggle basename.
\\[helm-ff-run-zgrep]\t\tRun zgrep.
\\[helm-ff-run-pdfgrep]\t\tRun Pdfgrep on marked files.
\\[helm-ff-run-copy-file]\t\tCopy file(s)
\\[helm-ff-run-rename-file]\t\tRename file(s).
\\[helm-ff-run-symlink-file]\t\tSymlink file(s).
\\[helm-ff-run-hardlink-file]\t\tHardlink file(s).
\\[helm-ff-run-delete-file]\t\tDelete file(s).
\\[helm-ff-run-byte-compile-file]\t\tByte compile file(s) (C-u load) (elisp).
\\[helm-ff-run-load-file]\t\tLoad file(s) (elisp).
\\[helm-ff-run-ediff-file]\t\tEdiff file.
\\[helm-ff-run-ediff-merge-file]\t\tEdiff merge file.
\\[helm-ff-run-switch-other-window]\t\tSwitch other window.
\\[helm-ff-properties-persistent]\t\tShow file properties.
\\[helm-ff-run-etags]\t\tRun etags (C-u use tap, C-u C-u reload DB).
\\[helm-yank-text-at-point]\t\tYank text at point.
\\[helm-ff-run-open-file-externally]\t\tOpen file with external program (C-u to choose).
\\[helm-ff-run-open-file-with-default-tool]\t\tOpen file externally with default tool.
\\[helm-ff-run-insert-org-link]\t\tInsert org link.")



;; Append visited files from `helm-source-ls-git' to `file-name-history'.
(add-to-list 'helm-files-save-history-extra-sources "Git files")


(defvar helm-ls-git-log-file nil) ; Set it for debugging.


(defun helm-ls-git-list-files ()
  (when (and helm-ls-git-log-file
             (file-exists-p helm-ls-git-log-file))
    (delete-file helm-ls-git-log-file))
  ;; `helm-resume' will use the local value of `default-directory'
  ;; in `helm-buffer' as value for `default-directory'.
  (helm-aif (helm-ls-git-root-dir)
      (with-helm-default-directory it
          (with-output-to-string
              (with-current-buffer standard-output
                (apply #'process-file
                       "git"
                       nil (list t helm-ls-git-log-file) nil
                       helm-ls-git-ls-switches))))
    ;; Return empty string to give to `split-string'
    ;; in `helm-ls-git-init'.
    ""))

(defun helm-ls-git-ls-files-show-others ()
  "Toggle view of tracked/non tracked files."
  (interactive)
  (with-helm-alive-p
    (setq helm-ls-git-ls-switches
          (if (member "-o" helm-ls-git-ls-switches)
              (remove "-o" helm-ls-git-ls-switches)
              (helm-append-at-nth helm-ls-git-ls-switches "-o" 1)))
    (helm-force-update)))
(put 'helm-ls-git-ls-switches 'helm-only t)

(cl-defun helm-ls-git-root-dir (&optional (directory default-directory))
  (let ((root (locate-dominating-file directory ".git")))
    (and root (file-name-as-directory root))))

(defun helm-ls-git-not-inside-git-repo ()
  (not (helm-ls-git-root-dir)))

(defun helm-ls-git-transformer (candidates _source)
   (cl-loop with root = (helm-ls-git-root-dir)
            with untracking = (member "-o" helm-ls-git-ls-switches)
            for file in candidates
            for abs = (expand-file-name file root)
            for disp = (if (and helm-ff-transformer-show-only-basename
                                (not (string-match "[.]\\{1,2\\}\\'" file)))
                           (helm-basename file) file)
            collect
            (cons (propertize (if untracking (concat "? " disp) disp)
                              'face (if untracking
                                        'helm-ls-git-untracked-face
                                        'helm-ff-file))
                  abs)))

(defun helm-ls-git-sort-fn (candidates _source)
  "Transformer for sorting candidates."
  (helm-ff-sort-candidates candidates nil))

(defun helm-ls-git-init ()
  (let ((data (cl-loop with root = (helm-ls-git-root-dir)
                       for c in (split-string (helm-ls-git-list-files) "\n" t)
                       collect (if (eq helm-ls-git-show-abs-or-relative 'relative)
                                   c (expand-file-name c root)))))
    (when (null data)
      (setq data
            (if helm-ls-git-log-file
                (with-current-buffer
                    (find-file-noselect helm-ls-git-log-file)
                  (prog1
                      (buffer-substring-no-properties
                       (point-min) (point-max))
                    (kill-buffer)))
              data)))
    (helm-init-candidates-in-buffer 'global data)))

(defvar helm-ls-git--current-branch nil)
(defun helm-ls-git--branch ()
  (or helm-ls-git--current-branch
      (with-temp-buffer
        (let ((ret (call-process "git" nil t nil "symbolic-ref" "--short" "HEAD")))
          ;; Use sha of HEAD when branch name is missing.
          (unless (zerop ret)
            (erase-buffer)
            (call-process "git" nil t nil "rev-parse" "--short" "HEAD")))
        (buffer-substring-no-properties (goto-char (point-min))
                                        (line-end-position)))))

(defun helm-ls-git-header-name (name)
  (format "%s (%s)" name (helm-ls-git--branch)))

(defun helm-ls-git-actions-list (&optional actions)
  (helm-append-at-nth
   actions
   (helm-make-actions "Git status"
                      (lambda (_candidate)
                        (funcall helm-ls-git-status-command
                                 (helm-default-directory)))
                      "Git grep files (`C-u' only current directory)"
                      'helm-ls-git-grep
                      "Gid" 'helm-ff-gid
                      "Search in Git log (C-u show patch)"
                      'helm-ls-git-search-log)
   1))

(defun helm-ls-git-match-part (candidate)
  (if (with-helm-buffer helm-ff-transformer-show-only-basename)
      (helm-basename candidate)
      candidate))

;;;###autoload
(defclass helm-ls-git-source (helm-source-in-buffer)
  ((header-name :initform 'helm-ls-git-header-name)
   (init :initform 'helm-ls-git-init)
   (cleanup :initform (lambda ()
                        (setq helm-ls-git-ls-switches (remove "-o" helm-ls-git-ls-switches))))
   (update :initform (lambda ()
                       (helm-set-local-variable
                        'helm-ls-git--current-branch nil)))
   (keymap :initform helm-ls-git-map)
   (help-message :initform helm-ls-git-help-message)
   (match-part :initform 'helm-ls-git-match-part)
   (filtered-candidate-transformer
    :initform '(helm-ls-git-transformer
                helm-ls-git-sort-fn))
   (action-transformer :initform 'helm-transform-file-load-el)
   (action :initform (helm-ls-git-actions-list helm-type-file-actions))))

;;;###autoload
(defclass helm-ls-git-status-source (helm-source-in-buffer)
  ((header-name :initform 'helm-ls-git-header-name)
   (init :initform
         (lambda ()
           (helm-init-candidates-in-buffer 'global
             (helm-ls-git-status))))
   (keymap :initform helm-ls-git-map)
   (filtered-candidate-transformer :initform 'helm-ls-git-status-transformer)
   (persistent-action :initform 'helm-ls-git-diff)
   (persistent-help :initform "Diff")
   (action-transformer :initform 'helm-ls-git-status-action-transformer)
   (action :initform
           (helm-make-actions
            "Find file" 'helm-find-many-files
            "Git status" (lambda (_candidate)
                           (funcall helm-ls-git-status-command
                                    (helm-default-directory)))))))


(defun helm-ls-git-grep (_candidate)
  (let* ((helm-grep-default-command helm-ls-git-grep-command)
         helm-grep-default-recurse-command
         (mkd (helm-marked-candidates))
         (files (if (cdr mkd) mkd '("")))
         ;; Expand filename of each candidate with the git root dir.
         ;; The filename will be in the help-echo prop.
         (helm-grep-default-directory-fn 'helm-ls-git-root-dir)
         ;; set `helm-ff-default-directory' to the root of project.
         (helm-ff-default-directory (if helm-current-prefix-arg
                                        default-directory
                                      (helm-ls-git-root-dir))))
    (helm-do-grep-1 files)))

(defun helm-ls-git-run-grep ()
  "Run Git Grep action from helm-ls-git."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-ls-git-grep)))


(defun helm-ls-git-search-log (_candidate)
  (let* ((query (helm-read-string "Search log: "))
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
  (helm-aif (helm-ls-git-root-dir)
      (with-helm-default-directory it
          (with-output-to-string
              (with-current-buffer standard-output
                (apply #'process-file
                       "git"
                       nil (list t helm-ls-git-log-file) nil
                       (list "status" "--porcelain")))))))

(defun helm-ls-git-status-transformer (candidates _source)
  (cl-loop with root = (helm-ls-git-root-dir)
        for i in candidates
        collect
        (cond ((string-match "^\\( M \\)\\(.*\\)" i) ; modified.
               (cons (propertize i 'face 'helm-ls-git-modified-not-staged-face)
                     (expand-file-name (match-string 2 i) root)))
              ((string-match "^\\(M+ *\\)\\(.*\\)" i) ; modified and staged.
               (cons (propertize i 'face 'helm-ls-git-modified-and-staged-face)
                     (expand-file-name (match-string 2 i) root)))
              ((string-match "^\\([?]\\{2\\} \\)\\(.*\\)" i)
               (cons (propertize i 'face 'helm-ls-git-untracked-face)
                     (expand-file-name (match-string 2 i) root)))
              ((string-match "^\\([AC] +\\)\\(.*\\)" i)
               (cons (propertize i 'face 'helm-ls-git-added-copied-face)
                     (expand-file-name (match-string 2 i) root)))
              ((string-match "^\\( [D] \\)\\(.*\\)" i)
               (cons (propertize i 'face 'helm-ls-git-deleted-not-staged-face)
                     (expand-file-name (match-string 2 i) root)))
              ((string-match "^\\(RM?\\).* -> \\(.*\\)" i)
               (cons (propertize i 'face 'helm-ls-git-renamed-modified-face)
                     (expand-file-name (match-string 2 i) root)))
              ((string-match "^\\([D] \\)\\(.*\\)" i)
               (cons (propertize i 'face 'helm-ls-git-deleted-and-staged-face)
                     (expand-file-name (match-string 2 i) root)))
              ((string-match "^\\(UU \\)\\(.*\\)" i)
               (cons (propertize i 'face 'helm-ls-git-conflict-face)
                     (expand-file-name (match-string 2 i) root)))
              ((string-match "^\\(AM \\)\\(.*\\)" i)
               (cons (propertize i 'face 'helm-ls-git-added-modified-face)
                     (expand-file-name (match-string 2 i) root)))
              (t i))))

(defun helm-ls-git-status-action-transformer (actions _candidate)
  (let ((disp (helm-get-selection nil t))
        (mofified-actions
         (helm-make-actions "Diff file" 'helm-ls-git-diff
                            "Revert file(s)"
                            (lambda (_candidate)
                              (let ((marked (helm-marked-candidates)))
                                (cl-loop for f in marked do
                                         (progn
                                           (vc-git-revert f)
                                           (helm-aif (get-file-buffer f)
                                               (with-current-buffer it
                                                 (revert-buffer t t)))))))
                            "Copy file(s) `C-u to follow'" 'helm-find-files-copy
                            "Rename file(s) `C-u to follow'" 'helm-find-files-rename)))
    ;; Unregistered files
    (cond ((string-match "^[?]\\{2\\}" disp)
           (append actions
                   (list '("Add file(s)"
                           . (lambda (candidate)
                               (let ((default-directory
                                      (file-name-directory candidate))
                                     (marked (helm-marked-candidates)))
                                 (vc-call-backend 'Git 'register marked))))
                         '("Delete file(s)" . helm-ff-delete-files)
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
                                   (cl-loop with last-bname 
                                         for f in marked
                                         for bname = (helm-basename f)
                                         unless (string= bname last-bname)
                                         do (insert (concat bname "\n"))
                                         do (setq last-bname bname))
                                   (save-buffer))))))))
          ((string-match "^A " disp)
           (append actions '(("Commit staged file(s)"
                              . helm-ls-git-commit)
                             ("Extend commit"
                              . helm-ls-git-extend-commit)
                             ("Amend commit"
                              . helm-ls-git-amend-commit)
                             ("Unstage file(s)"
                              . helm-ls-git-unstage-files))))
          ;; Modified but not staged
          ((string-match "^ M+ *" disp)
           (append actions (helm-append-at-nth
                            mofified-actions
                            '(("Stage file(s)"
                               . helm-ls-git-stage-files)
                              ("Stage marked file(s) and commit"
                               . helm-ls-git-stage-marked-and-commit)
                              ("Stage marked file(s) and extend commit"
                               . helm-ls-git-stage-marked-and-extend-commit)
                              ("Stage marked file(s) and amend commit"
                               . helm-ls-git-stage-marked-and-amend-commit))
                            1)))
          ;; Modified and staged
          ((string-match "^M+ *" disp)
           (append actions (helm-append-at-nth
                            mofified-actions
                            '(("Commit staged file(s)"
                               . helm-ls-git-commit)
                              ("Extend commit"
                               . helm-ls-git-extend-commit)
                              ("Amend commit"
                               . helm-ls-git-amend-commit)
                              ("Unstage file(s)"
                               . helm-ls-git-unstage-files))
                            1)))
          ;; Deleted
          ((string-match "^ D " disp)
           (append actions (list '("Git delete" . vc-git-delete-file)
                                 '("Stage file(s)"
                                   . helm-ls-git-stage-files))))
          (t actions))))

(defun helm-ls-git-stage-files (_candidate)
  "Stage marked files."
  (require 'magit-apply nil t)
  (let* ((files (helm-marked-candidates))
         (default-directory
          (file-name-directory (car files))))
    (if (fboundp 'magit-stage-file)
        (helm-ls-git-magit-stage-files files)
      (apply #'process-file "git" nil nil nil "stage" files))))

(defun helm-ls-git-unstage-files (_candidate)
  "Unstage marked files."
  (require 'magit-apply nil t)
  (let* ((files (helm-marked-candidates))
         (default-directory (file-name-directory (car files))))
    (if (fboundp 'magit-unstage-file)
        (helm-ls-git-magit-unstage-files files)
      (apply #'process-file "git" nil nil nil "reset" "HEAD" "--" files))))

(defun helm-ls-git-stage-marked-and-commit (candidate)
  "Stage marked files and commit."
  (helm-ls-git-stage-files nil)
  (helm-ls-git-commit candidate))

(defun helm-ls-git-stage-marked-and-extend-commit (candidate)
  (helm-ls-git-stage-files nil)
  (helm-ls-git-extend-commit candidate))

(defun helm-ls-git-stage-marked-and-amend-commit (candidate)
  (helm-ls-git-stage-files nil)
  (helm-ls-git-amend-commit candidate))

(defun helm-ls-git-extend-commit (candidate)
  (require 'magit-commit nil t)
  (let ((default-directory (file-name-directory candidate)))
    (if (fboundp 'magit-commit-extend)
        (let ((inhibit-magit-refresh t))
          (magit-commit-extend))
      (process-file "git" nil nil nil "commit" "--amend" "--no-edit"))))

(defun helm-ls-git-amend-commit (candidate)
  (require 'magit-commit nil t)
  (let ((default-directory (file-name-directory candidate)))
    (if (fboundp 'magit-commit-amend)
        (let ((inhibit-magit-refresh t))
          (magit-commit-amend))
      (process-file "git" nil nil nil "commit" "--amend"))))

(defun helm-ls-git-commit (candidate)
  "Commit all staged files."
  (require 'magit-commit nil t)
  (let ((default-directory (file-name-directory candidate)))
    (if (fboundp 'magit-commit)
        (let ((inhibit-magit-refresh t))
          (magit-commit))
      (helm-ls-git-commit-files))))

(defun helm-ls-git-commit-files ()
  "Default function to commit files."
  (let* ((marked (helm-marked-candidates)))
    (vc-checkin marked 'Git)))

(defun helm-ls-git-magit-stage-files (files)
  (cl-loop for f in files
           do (magit-stage-file f)))

(defun helm-ls-git-magit-unstage-files (files)
  (cl-loop for f in files
           do (magit-unstage-file f)))

(defun helm-ls-git-diff (candidate)
  (let ((default-directory
         (expand-file-name (file-name-directory candidate)))
        (win (get-buffer-window "*vc-diff*" 'visible)))
    (if (and win
             (eq last-command 'helm-execute-persistent-action))
        (with-helm-window
          (kill-buffer "*vc-diff*")
          (if (and helm-persistent-action-display-window
                   (window-dedicated-p (next-window win 1)))
              (delete-window helm-persistent-action-display-window)
            (set-window-buffer win helm-current-buffer)))
      (when (buffer-live-p (get-buffer "*vc-diff*"))
        (kill-buffer "*vc-diff*"))
      (vc-git-diff (helm-marked-candidates))
      (pop-to-buffer "*vc-diff*")
      (diff-mode))))

;; Overhide the actions of helm-type-buffer.
(defmethod helm--setup-source :after ((source helm-source-buffers))
  (let ((name (slot-value source 'name)))
    (when (string= name "Buffers in git project")
      (setf (slot-value source 'action)
            (helm-append-at-nth
             helm-type-buffer-actions
             (helm-make-actions "Git status"
                                (lambda (_candidate)
                                  (funcall helm-ls-git-status-command
                                           (helm-default-directory))))
             1)))))

(defun helm-ls-git-build-git-status-source ()
  "Build `helm-source-ls-git-status'.

Do nothing when `helm-source-ls-git-status' is not member of
`helm-ls-git-default-sources'."
  (and (memq 'helm-source-ls-git-status helm-ls-git-default-sources)
       (helm-make-source "Git status" 'helm-ls-git-status-source
         :fuzzy-match helm-ls-git-fuzzy-match
         :group 'helm-ls-git)))

(defun helm-ls-git-build-ls-git-source ()
  "Build `helm-source-ls-git'.

Do nothing when `helm-source-ls-git' is not member of
`helm-ls-git-default-sources'."
  (and (memq 'helm-source-ls-git helm-ls-git-default-sources)
       (helm-make-source "Git files" 'helm-ls-git-source
         :fuzzy-match helm-ls-git-fuzzy-match
         :group 'helm-ls-git)))

(defun helm-ls-git-build-buffers-source ()
  "Build `helm-source-ls-git-buffers'.

Do nothing when `helm-source-ls-git-buffers' is not member of
`helm-ls-git-default-sources'."
  (and (memq 'helm-source-ls-git-buffers helm-ls-git-default-sources)
       (helm-make-source "Buffers in git project" 'helm-source-buffers
         :header-name #'helm-ls-git-header-name
         :buffer-list (lambda () (helm-browse-project-get-buffers
                                  (helm-ls-git-root-dir)))
         :keymap helm-ls-git-buffer-map)))


;;;###autoload
(defun helm-ls-git-ls (&optional arg)
  (interactive "p")
  (let ((helm-ff-default-directory
         (or helm-ff-default-directory
             default-directory)))
    (when (and arg (helm-ls-git-not-inside-git-repo))
      (error "Not inside a Git repository"))
    (unless (cl-loop for s in helm-ls-git-default-sources
                     always (symbol-value s))
      (setq helm-source-ls-git-status
            (helm-ls-git-build-git-status-source)
            helm-source-ls-git
            (helm-ls-git-build-ls-git-source)
            helm-source-ls-git-buffers
            (helm-ls-git-build-buffers-source)))
    (helm-set-local-variable 'helm-ls-git--current-branch (helm-ls-git--branch))
    (helm :sources helm-ls-git-default-sources
          :ff-transformer-show-only-basename nil
          :buffer "*helm lsgit*")))


(provide 'helm-ls-git)

;;; helm-ls-git.el ends here
