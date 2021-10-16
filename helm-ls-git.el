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

(defvar server-clients)
(declare-function helm-comp-read "ext:helm-mode.el")
(declare-function server-running-p "server.el")
(declare-function server-edit        "server.el")
(declare-function server-send-string "server.el")
(declare-function server-quote-arg   "server.el")
;; Define the sources.
(defvar helm-source-ls-git-status nil
  "This source will built at runtime.
It can be build explicitly with function
`helm-ls-git-build-git-status-source'.")
(defvar helm-source-ls-git nil
  "This source will built at runtime.
It can be build explicitly with function
`helm-ls-git-build-ls-git-source'.")
(defvar helm-source-ls-git-buffers nil
  "This source will built at runtime.
It can be build explicitly with function
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

If you want to use magit use `magit-status-setup-buffer' and not
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
                                         helm-ls-git-branches-source
                                         helm-source-ls-git-buffers
                                         helm-source-ls-git
                                         helm-ls-git-stashes-source
                                         helm-ls-git-create-branch-source)
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
To see files in submodules add the option \"--recurse-submodules\".
If you have problems displaying  unicode filenames use
\'(\"-c\" \"core.quotePath=false\" \"ls-files\" \"--full-name\" \"--\").
See Issue #52."
  :type '(repeat string)
  :group 'helm-ls-git)

(defcustom helm-ls-git-auto-checkout nil
  "Stash automatically uncommited changes before checking out a branch."
  :type 'boolean
  :group 'helm-ls-git)

(defcustom helm-ls-git-log-max-commits "100"
  "Max number of commits to show in git log (git log -n option)."
  :type 'string
  :group 'helm-ls-git)

(defcustom helm-ls-git-delete-branch-on-remote nil
  "Delete remote branch without asking when non nil.
This happen only when deleting a remote branch e.g. remotes/origin/foo."
  :type 'boolean
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

(defface helm-ls-git-branches-current
  '((t :foreground "gold"))
  "Color of the start prefixing current branch."
  :group 'helm-ls-git)

(defface helm-ls-git-branches-name
  '((t :foreground "red"))
  "Color of branches names."
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

(defvar helm-ls-git-branches-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c b") 'helm-ls-git-branches-toggle-show-all)
    (define-key map (kbd "M-L") 'helm-ls-git-run-show-log)
    (define-key map (kbd "C-c P") 'helm-ls-git-run-push)
    (define-key map (kbd "C-c F") 'helm-ls-git-run-pull)
    (define-key map (kbd "C-c f") 'helm-ls-git-run-fetch)
    map))

(defvar helm-ls-git-status-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-ls-git-map)
    (define-key map (kbd "C-c c") 'helm-ls-git-run-stage-marked-and-commit)
    (define-key map (kbd "C-c a") 'helm-ls-git-run-stage-marked-and-amend-commit)
    (define-key map (kbd "C-c s") 'helm-ls-git-run-stage-files)
    (define-key map (kbd "C-c e") 'helm-ls-git-run-stage-marked-and-extend-commit)
    (define-key map (kbd "C-c z") 'helm-ls-git-run-stash)
    (define-key map (kbd "C-c Z") 'helm-ls-git-run-stash-snapshot)
    map))

(defvar helm-ls-git-help-message
  "* Helm ls git

** Tips

*** Start helm-ls-git

You can start with `helm-ls-git' but you can also use the generic
`helm-browse-project' which will use `helm-ls-git' if you are in
a git project (actually supported backends are git and hg though
helm-ls-hg is no more maintained).

*** You may want to use magit as git status command

By default helm-ls-git is using emacs `vc-dir' as `helm-ls-git-status-command',
perhaps you want to use something better like `magit-status' ?

*** Git log

From branches source, you can launch git log.  With a numeric
prefix arg specify the number of commits to show.  Once you are
in Git log you can specify with 2 marked candidates range of
commits, specifying more than two marked candidate for actions
accepting ranges will fail.  When specifying a range of commits,
the top commit will be included in range whereas the bottom
commit will not be included, e.g. if you mark commit-2 and
commit-5, and use the format-patch action, git will make
01-commit-4.patch, 02-commit-3.patch, and 03-commit-2.patch files
taking care of naming files in the reverse order for applying
patches later, commit-5 beeing excluded.

Persistent action in git log is to show diff of commits, if you
want to always show diff while moving from one commit to the
other use follow-mode (C-c C-f).

*** Git commit

Commits will be done using emacsclient as GIT_EDITOR, with
major-mode `helm-ls-git-commmit-mode' which provide following commands:

\\<helm-ls-git-commit-mode-map>
|Keys|Description
|-------------+--------------|
|\\[helm-ls-git-server-edit]|Exit when done
|\\[helm-ls-git-server-edit-abort]|Abort

NOTE: This mode is based on diff-mode, this to show a colorized
diff of your commit, you can use any regular emacs editing
commands from there.

*** Git rebase

helm-ls-git provide two rebase actions, one that run
interactively from git log source and one that work
non-interactively from branches source.  With the former you can
rebase interactively from a given commit you selected from git log
and this ONLY for current branch, once done you can rebase one
branch into the other from branches source.  This is one workflow
that helm-ls-git provide, other workflows may not work, so for
more complex usage switch to command line or a more enhaced tool
like Magit.  For editing the first file git rebase use for
rebasing (\"git-rebase-todo\") helm-ls-git use a major-mode
called `helm-ls-git-rebase-todo-mode' which provide several commands:

\\<helm-ls-git-rebase-todo-mode-map>
|Keys|Description
|-------------+--------------|
|p|pick
|r|reword
|e|edit
|s|squash
|f|fixup 
|x|exec
|d|drop
|\\[helm-ls-git-rebase-todo-move-down]|Move line down
|\\[helm-ls-git-rebase-todo-move-up]|Move line up
|\\[helm-ls-git-server-edit]|Exit when done
|\\[helm-ls-git-server-edit-abort]|Abort

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

**** Grep a subdirectory of current repository.

Switch to `helm-find-files' with `C-x C-f', navigate to your directory
and launch git-grep from there.

*** Problem with unicode filenames (chinese etc...)

See docstring of `helm-ls-git-ls-switches'.

** Commands
\\<helm-ls-git-map>
|Keys|Description
|-----------+----------|
|\\[helm-ls-git-run-grep]|Run git-grep.
|\\[helm-ff-run-gid]|Run Gid.
|\\[helm-ls-git-ls-files-show-others]|Toggle tracked/non tracked files view.
|\\<helm-generic-files-map>
|\\[helm-ff-run-toggle-basename]|Toggle basename.
|\\[helm-ff-run-zgrep]|Run zgrep.
|\\[helm-ff-run-pdfgrep]|Run Pdfgrep on marked files.
|\\[helm-ff-run-copy-file]|Copy file(s)
|\\[helm-ff-run-rename-file]|Rename file(s).
|\\[helm-ff-run-symlink-file]|Symlink file(s).
|\\[helm-ff-run-hardlink-file]|Hardlink file(s).
|\\[helm-ff-run-delete-file]|Delete file(s).
|\\[helm-ff-run-byte-compile-file]|Byte compile file(s) (C-u load) (elisp).
|\\[helm-ff-run-load-file]|Load file(s) (elisp).
|\\[helm-ff-run-ediff-file]|Ediff file.
|\\[helm-ff-run-ediff-merge-file]|Ediff merge file.
|\\[helm-ff-run-switch-other-window]|Switch other window.
|\\[helm-ff-properties-persistent]|Show file properties.
|\\[helm-ff-run-etags]|Run etags (C-u use tap, C-u C-u reload DB).
|\\[helm-yank-text-at-point]|Yank text at point.
|\\[helm-ff-run-open-file-externally]|Open file with external program (C-u to choose).
|\\[helm-ff-run-open-file-with-default-tool]|Open file externally with default tool.
|\\[helm-ff-run-insert-org-link]|Insert org link.")



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
(put 'helm-ls-git-ls-files-show-others 'no-helm-mx t)

(cl-defun helm-ls-git-root-dir (&optional (directory default-directory))
  (locate-dominating-file directory ".git"))

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
        (let ((ret (process-file "git" nil t nil "symbolic-ref" "--short" "HEAD")))
          ;; Use sha of HEAD when branch name is missing.
          (unless (zerop ret)
            (erase-buffer)
            (process-file "git" nil t nil "rev-parse" "--short" "HEAD")))
        ;; We use here (goto-char (point-min)) instead of (point-min)
        ;; to not endup with a ^J control char at end of branch name.
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

(defclass helm-ls-git-source (helm-source-in-buffer)
  ((header-name :initform 'helm-ls-git-header-name)
   (init :initform 'helm-ls-git-init)
   (cleanup :initform (lambda ()
                        (setq helm-ls-git-ls-switches (remove "-o" helm-ls-git-ls-switches))))
   (update :initform (lambda ()
                       (helm-set-local-variable
                        'helm-ls-git--current-branch nil)))
   (keymap :initform 'helm-ls-git-map)
   (help-message :initform helm-ls-git-help-message)
   (match-part :initform 'helm-ls-git-match-part)
   (filtered-candidate-transformer
    :initform '(helm-ls-git-transformer
                helm-ls-git-sort-fn))
   (action-transformer :initform 'helm-transform-file-load-el)
   (action :initform (helm-ls-git-actions-list helm-type-file-actions))))

(defclass helm-ls-git-status-source (helm-source-in-buffer)
  ((header-name :initform 'helm-ls-git-header-name)
   (init :initform
         (lambda ()
           (helm-init-candidates-in-buffer 'global
             (helm-ls-git-status))))
   (keymap :initform 'helm-ls-git-status-map)
   (filtered-candidate-transformer :initform 'helm-ls-git-status-transformer)
   (persistent-action :initform 'helm-ls-git-diff)
   (persistent-help :initform "Diff")
   (help-message :initform helm-ls-git-help-message)
   (action-transformer :initform 'helm-ls-git-status-action-transformer)
   (action :initform
           (helm-make-actions
            "Find file" 'helm-find-many-files
            "Git status" (lambda (_candidate)
                           (funcall helm-ls-git-status-command
                                    (helm-default-directory)))))))

(defun helm-ls-git-revert-buffers-in-project ()
  (cl-loop for buf in (helm-browse-project-get-buffers (helm-ls-git-root-dir))
           when (buffer-file-name (get-buffer buf))
           do (with-current-buffer buf (revert-buffer nil t))))

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

;;; Git grep
;;
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
(put 'helm-ls-git-run-grep 'no-helm-mx t)

;;; Git log
;;
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
  (diff-mode))

(defun helm-ls-git-log (&optional branch num)
  (when (string-match "->" branch)
    (setq branch (car (last (split-string branch "->")))))
  (let* ((commits-number (if num
                             (number-to-string num)
                           helm-ls-git-log-max-commits))
         (switches `("log" "--color"
                     "--date=local"
                     "--pretty=format:%C(yellow)%h%Creset \
 %C(green)%ad%Creset %<(60,trunc)%s %Cred%an%Creset %C(auto)%d%Creset"
                     "-n" ,commits-number
                     ,(or branch ""))))
    (with-helm-default-directory (helm-ls-git-root-dir)
      (with-output-to-string
        (with-current-buffer standard-output
          (apply #'process-file "git" nil t nil switches))))))

(defun helm-ls-git-show-log (branch)
  (let* ((name (replace-regexp-in-string "[ *]" "" branch))
         (str (helm-ls-git-log name (helm-aif helm-current-prefix-arg
                                        (prefix-numeric-value it)))))
    (when (buffer-live-p "*git log diff*")
      (kill-buffer "*git log diff*"))
    (helm :sources (helm-build-in-buffer-source "Git log"
                     :header-name (lambda (sname) (format "%s (%s)" sname name))
                     :data str
                     :get-line 'buffer-substring
                     :marked-with-props 'withprop
                     :help-message 'helm-ls-git-help-message
                     :action '(("Show commit" . helm-ls-git-log-show-commit)
                               ("Find file at rev" . helm-ls-git-log-find-file)
                               ("Kill rev as short hash" .
                                helm-ls-git-log-kill-short-hash)
                               ("Kill rev as long hash" .
                                helm-ls-git-log-kill-long-hash)
                               ("Kill rev as <branch~n>" .
                                helm-ls-git-log-kill-rev)
                               ("Cherry-pick" . helm-ls-git-log-cherry-pick)
                               ("Format patches" . helm-ls-git-log-format-patch)
                               ("Git am" . helm-ls-git-log-am)
                               ("Git interactive rebase" . helm-ls-git-log-interactive-rebase)
                               ("Hard reset" . helm-ls-git-log-hard-reset)
                               ("Soft reset" . helm-ls-git-log-soft-reset)
                               ("Git revert" . helm-ls-git-log-revert))
                     :candidate-transformer
                     (lambda (candidates)
                       (cl-loop for c in candidates
                                for count from 0
                                for cand = (ansi-color-apply c)
                                collect (propertize
                                         cand 'rev (if (zerop count)
                                                       name
                                                     (format "%s~%s" name count))))))
          :buffer "*helm-ls-git log*")))

(defun helm-ls-git-log-show-commit-1 (candidate)
  (let ((sha (car (split-string candidate))))
    (with-current-buffer (get-buffer-create "*git log diff*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (with-helm-default-directory (helm-ls-git-root-dir
                                              (helm-default-directory))
                  (with-output-to-string
                    (with-current-buffer standard-output
                      (process-file
                       "git" nil (list t helm-ls-git-log-file) nil
                       "show" "-p" sha)))))
        (goto-char (point-min))
        (diff-mode))
      (display-buffer (current-buffer)))))

(defun helm-ls-git-log-kill-short-hash (candidate)
  (kill-new (car (split-string candidate))))

(defun helm-ls-git-log-kill-long-hash (_candidate)
  (helm-ls-git-log-get-long-hash 'kill))

(defun helm-ls-git-log-get-long-hash (&optional kill)
  (with-helm-buffer
    (let (str)
      (helm-aif (get-text-property
                 2 'rev
                 (helm-get-selection nil 'withprop))
          (setq str
                (replace-regexp-in-string
                 "\n" ""
                 (shell-command-to-string
                  (format "git rev-parse --default %s %s"
                          (replace-regexp-in-string
                           "~[0-9]+" "" it)
                          it)))))
      (when str
        (if kill (kill-new str) str)))))

(defun helm-ls-git-log-kill-rev (_candidate)
  (helm-aif (get-text-property
             2 'rev
             (helm-get-selection nil 'withprop))
      (kill-new it)))

(defun helm-ls-git-log-format-patch (_candidate)
  (helm-ls-git-log-format-patch-1))

(defun helm-ls-git-log-am (_candidate)
  (helm-ls-git-log-format-patch-1 'am))

(defun helm-ls-git-log-format-patch-1 (&optional am)
  (let ((commits (cl-loop for c in (helm-marked-candidates)
                          collect (get-text-property 1 'rev c)))
        range switches)
    (cond ((= 2 (length commits))
           ;; Using "..." makes a range from top marked (included) to
           ;; bottom marked (not included) e.g. when we have commit-2
           ;; marked and commit-5 marked the serie of patches will be
           ;; 01-commit-4.patch, 02-commit-3.patch, 03-commit-2.patch,
           ;; git taking care of numering the patch in reversed order
           ;; for further apply.
           (setq range (mapconcat 'identity (sort commits #'string-lessp) "...")
                 switches `("format-patch" ,range)))
          ((not (cdr commits))
           (setq range (car commits)
                 switches `("format-patch" "-1" ,range)))
          ((> (length commits) 2)
           (error "Specify either a single commit or a range with only two marked commits")))
    (with-helm-default-directory (helm-ls-git-root-dir
                                  (helm-default-directory))
      (if am
          (with-current-buffer-window "*git am*" '(display-buffer-below-selected
                                                   (window-height . fit-window-to-buffer)
                                                   (preserve-size . (nil . t)))
              nil
            (process-file-shell-command
             (format "git %s | git am -3 -k"
                     (mapconcat 'identity (helm-append-at-nth switches '("-k --stdout") 1) " "))
             nil t t))
        (apply #'process-file "git" nil "*git format-patch*" nil switches)))))

(defun helm-ls-git-log-reset-1 (hard-or-soft)
  (let ((rev (get-text-property 1 'rev (helm-get-selection nil 'withprop)))
        (arg (cl-case hard-or-soft
               (hard "--hard")
               (soft "--soft"))))
    (with-helm-default-directory (helm-ls-git-root-dir
                                  (helm-default-directory))
      (when (and (y-or-n-p (format "%s reset to <%s>?"
                                   (capitalize (symbol-name hard-or-soft)) rev))
                 (= (process-file "git" nil nil nil "reset" arg rev) 0))
        (message "Now at `%s'" (helm-ls-git-oneline-log
                                (helm-ls-git--branch)))))))

(defun helm-ls-git-log-hard-reset (_candidate)
  (helm-ls-git-log-reset-1 'hard))

(defun helm-ls-git-log-soft-reset (_candidate)
  (helm-ls-git-log-reset-1 'soft))

(defun helm-ls-git-log-revert (_candidate)
  (let ((rev (get-text-property 1 'rev (helm-get-selection nil 'withprop))))
    (helm-ls-git-with-editor "revert" rev)))

(defun helm-ls-git-log-revert-continue (_candidate)
  (helm-ls-git-with-editor "revert" "--continue"))

(defun helm-ls-git-revert-abort (_candidate)
  (with-helm-default-directory (helm-default-directory)
    (process-file "git" nil nil nil "revert" "--abort")))

(defun helm-ls-git-log-show-commit (candidate)
  (if (and (eq last-command 'helm-execute-persistent-action)
           (get-buffer-window "*git log diff*" 'visible))
      (kill-buffer "*git log diff*")
    (helm-ls-git-log-show-commit-1 candidate)))

(defun helm-ls-git-log-find-file (_candidate)
  (with-helm-default-directory (helm-default-directory)
    (let* ((rev (get-text-property 1 'rev (helm-get-selection nil 'withprop)))
           (file (helm :sources (helm-build-in-buffer-source "Git cat-file"
                                  :data (helm-ls-git-list-files))
                       :buffer "*helm-ls-git cat-file*"))
           (fname (concat rev ":" file))
           (path (expand-file-name fname))
           str status)
      (setq str (with-output-to-string
                  (with-current-buffer standard-output
                    (setq status (process-file "git" nil t nil "cat-file" "-p" fname)))))
      (if (zerop status)
          (progn
            (with-current-buffer (find-file-noselect path)
              (insert str)
              (save-buffer))
            (find-file path))
        (error "No such file %s at %s" file rev)))))

(defun helm-ls-git-log-cherry-pick (_candidate)
  (let* ((commits (cl-loop for c in (helm-marked-candidates)
                           collect (get-text-property 1 'rev c) into revs
                           finally return (sort revs #'string-greaterp))))
    (with-helm-default-directory (helm-ls-git-root-dir
                                  (helm-default-directory))
      (with-current-buffer-window "*git cherry-pick*" '(display-buffer-below-selected
                                                        (window-height . fit-window-to-buffer)
                                                        (preserve-size . (nil . t)))
          nil
        (apply #'process-file "git" nil "*git cherry-pick*" nil "cherry-pick" commits)))))

(defun helm-ls-git-cherry-pick-abort (_candidate)
  (with-helm-default-directory (helm-default-directory)
    (process-file "git" nil nil nil "cherry-pick" "--abort")))

(defun helm-ls-git-rebase-abort (_candidate)
  (with-helm-default-directory (helm-default-directory)
    (process-file "git" nil nil nil "rebase" "--abort")))

(defun helm-ls-git-merge-abort (_candidate)
  (with-helm-default-directory (helm-default-directory)
    (process-file "git" nil nil nil "merge" "--abort")))

(defun helm-ls-git-rebase-continue (_candidate)
  (helm-ls-git-with-editor "rebase" "--continue"))

(defun helm-ls-git-cherry-pick-continue (_candidate)
  (helm-ls-git-with-editor "cherry-pick" "--continue"))

(defun helm-ls-git-am-continue (_candidate)
  (helm-ls-git-with-editor "am" "--continue"))

(defun helm-ls-git-merge-continue (_candidate)
  (helm-ls-git-with-editor "merge" "--continue"))

(defun helm-ls-git-rebase-running-p ()
  (with-helm-buffer
    (with-helm-default-directory (helm-ls-git-root-dir)
      (let ((git-dir (expand-file-name ".git" default-directory)))
        (or (file-exists-p (expand-file-name "rebase-merge" git-dir))
            (file-exists-p (expand-file-name "rebase-apply/onto" git-dir)))))))

(defun helm-ls-git-log-interactive-rebase (_candidate)
  "Rebase interactively current branch from CANDIDATE.
Where CANDIDATE is a candidate from git log source and its commit
object will be passed git rebase i.e. git rebase -i <hash>."
  (if (helm-ls-git-rebase-running-p)
      (if (y-or-n-p "A rebase is already running, continue ?")
          (helm-ls-git-rebase-continue nil)
        (helm-ls-git-rebase-abort nil))
  (let ((hash (helm-ls-git-log-get-long-hash)))
    (helm-ls-git-with-editor "rebase" "-i" hash))))

(defun helm-ls-git-run-show-log ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action #'helm-ls-git-show-log)))
(put 'helm-ls-git-run-show-log 'no-helm-mx t)


;;; Git branch basic management
;;
(defvar helm-ls-git-branches-show-all nil)

(defun helm-ls-git-collect-branches (&optional arg)
  (helm-aif (helm-ls-git-root-dir)
      (with-helm-default-directory it
        (with-output-to-string
          (with-current-buffer standard-output
            (cond ((null arg)
                   ;; Only local branches.
                   (apply #'process-file "git" nil t nil '("branch")))
                  (t
                   (apply #'process-file "git" nil t nil '("branch" "-a")))))))
    ""))

(defun helm-ls-git-branches-toggle-show-all ()
  (interactive)
  (setq helm-ls-git-branches-show-all (not helm-ls-git-branches-show-all))
  (helm-force-update))
(put 'helm-ls-git-branches-toggle-show-all 'no-helm-mx t)

(defun helm-ls-git-checkout (candidate)
  (let ((default-directory (helm-default-directory)))
    (if (and helm-ls-git-auto-checkout
             (helm-ls-git-modified-p t))
        (helm-ls-git-stash-1 "")
      (cl-assert (not (helm-ls-git-modified-p t))
                 nil "Please commit or stash your changes before proceeding"))
    (with-helm-default-directory (helm-ls-git-root-dir)
      (let* ((branch (replace-regexp-in-string "[ ]" "" candidate))
             (real (replace-regexp-in-string "\\`\\*" "" branch)))
        (if (string-match "\\`[*]" candidate)
            (message "Already on %s branch" real)
          (let* ((switches (if (string-match "\\`[Rr]emotes" real)
                               `("checkout" "-b"
                                 ,(car (last (split-string real "/" t)))
                                 "-t" ,real)
                             `("checkout" ,real)))
                 (status (apply #'process-file "git"
                                nil nil nil
                                switches)))
            (if (= status 0)
                (progn (message "Switched to %s branch" real)
                       (helm-ls-git-revert-buffers-in-project))
              (error "Process exit with non zero status"))))))))

(defun helm-ls-git-branches-create (candidate)
  (with-helm-default-directory (helm-ls-git-root-dir)
    (process-file "git" nil nil nil
                  "checkout" "-B" candidate "-t" (helm-ls-git--branch))))

(defun helm-ls-git-branches-delete (candidate)
  (with-helm-default-directory (helm-ls-git-root-dir)
    (let* ((branch (helm-ls-git-normalize-branch-name candidate))
           (remote (string-match "remotes/" candidate))
           (switches (if remote
                         `("-D" "-r" ,branch)
                       `("-D" ,branch))))
      (cl-assert (not (string-match "\\`[*]" candidate))
                 nil "Can't delete current branch")
      (if (= (apply #'process-file "git" nil nil nil "branch" switches) 0)
          (progn
            (when (and remote
                       (or helm-ls-git-delete-branch-on-remote
                           (y-or-n-p (format "Delete `%s' branch on remote as well ?" branch))))
              (let ((proc (start-file-process
                           "git" "*helm-ls-git branch delete*"
                           "git" "push" "origin" "--delete"
                           (car (last (split-string branch "/" t))))))
                (set-process-sentinel
                 proc
                 (lambda (_process event)
                   (if (string= event "finished\n")
                       (message "Remote branch %s deleted successfully" branch)
                     (message "Failed to delete remote branch %s" branch))))))
            (message "Local branch %s deleted successfully" branch))
        (message "failed to delete branch %s" branch)))))

(defun helm-ls-git-normalize-branch-names (names)
  (cl-loop for name in names collect
           (helm-ls-git-normalize-branch-name name)))

(defun helm-ls-git-normalize-branch-name (name)
  (helm-aand name
             (replace-regexp-in-string " " "" it)
             (replace-regexp-in-string "[*]" "" it)
             (replace-regexp-in-string "remotes/" "" it)))

(defun helm-ls-git-delete-marked-branches (_candidate)
  (let* ((branches (helm-marked-candidates))
         (bnames (helm-ls-git-normalize-branch-names branches))
         (display-buf "*helm-ls-git deleted branches*"))
    (with-helm-display-marked-candidates
      display-buf bnames                                   
      (when (y-or-n-p "Really delete branche(s) ?")
        (cl-loop for b in branches
                 do (helm-ls-git-branches-delete b))))))

(defun helm-ls-git-modified-p (&optional ignore-untracked)
  (with-helm-default-directory (helm-ls-git-root-dir)
    (not (string= (helm-ls-git-status ignore-untracked) ""))))

(defun helm-ls-git-branches-merge (candidate)
  (with-helm-default-directory (helm-ls-git-root-dir)
    (let ((branch (replace-regexp-in-string "[ ]" "" candidate))
          (current (helm-ls-git--branch)))
      (when (y-or-n-p (format "Merge branch %s into %s?" branch current))
        (if (= (process-file "git" nil nil nil "merge" branch) 0)
            (progn (message "Branch %s merged successfully into %s" branch current)
                   (helm-ls-git-revert-buffers-in-project))
          (message "failed to merge branch %s" branch))))))

(defvar helm-ls-git-create-branch-source
  (helm-build-dummy-source "Create branch"
    :filtered-candidate-transformer
    (lambda (_candidates _source)
      (list (or (and (not (string= helm-pattern ""))
                     helm-pattern)
                "Enter new branch name")))
    :action 'helm-ls-git-branches-create))

(defun helm-ls-git-oneline-log (branch)
  (let ((output (with-output-to-string
                  (with-current-buffer standard-output
                    (process-file
                     "git" nil t nil
                     "log" (car (split-string branch "->"))
                     "-n" "1" "--oneline")))))
    (replace-regexp-in-string "\n" "" output)))

(defun helm-ls-git-branches-transformer (candidates)
  (cl-loop for c in candidates
           for maxlen = (cl-loop for i in candidates maximize (length i))
           for name = (replace-regexp-in-string "[ *]" "" c)
           for log = (helm-ls-git-oneline-log name)
           for disp = (if (string-match "\\`\\([*]\\)\\(.*\\)" c)
                          (format "%s%s: %s%s"
                                  (propertize (match-string 1 c)
                                              'face 'helm-ls-git-branches-current)
                                  (propertize (match-string 2 c)
                                              'face 'helm-ls-git-branches-name)
                                  (make-string (- maxlen (length c)) ? )
                                  log)
                        (format "%s: %s%s"
                                (propertize c 'face 'helm-ls-git-branches-name)
                                (make-string (- maxlen (length c)) ? )
                                log))
           collect (cons disp c)))

(defun helm-ls-git-push (_candidate)
  (with-helm-default-directory (helm-default-directory)
    (message "Pushing changes on remote...")
    (let ((proc (start-file-process
                 "git" "*helm-ls-git push*" "git" "push" "origin" "HEAD")))
      (set-process-sentinel
       proc (lambda (_process event)
              (if (string= event "finished\n")
                  (message "Pushing changes on remote done")
                (error "Failed to push on remote")))))))

(defun helm-ls-git-run-push ()
  (interactive)
  (with-helm-alive-p
    (when (y-or-n-p "Push on remote ?")
      (helm-exit-and-execute-action #'helm-ls-git-push))))
(put 'helm-ls-git-run-push 'no-helm-mx t)

(defun helm-ls-git-remotes ()
  (with-helm-default-directory (helm-default-directory)
    (with-output-to-string
      (with-current-buffer standard-output
        (process-file "git" nil t nil "remote")))))

(defun helm-ls-git--pull-or-fetch (command &rest args)
  (with-helm-default-directory (helm-default-directory)
    (let* ((remote "origin")
           (pcommand (capitalize command))
           ;; A `C-g' in helm-comp-read will quit function as well.
           (switches (if current-prefix-arg
                         (append (list command)
                                 args
                                 (list (setq remote
                                             (helm-comp-read
                                              (format "%s from: " pcommand)
                                              (split-string
                                               (helm-ls-git-remotes)
                                               "\n")
                                              :allow-nest t)))
                                 (list (helm-ls-git--branch)))
                       (append (list command) args)))
           process-connection-type
           proc)
      (setq proc (apply #'helm-ls-git-with-editor switches))
      (with-current-buffer (process-buffer proc) (erase-buffer))
      (message "%sing from `%s'..." pcommand remote)
      (set-process-filter proc 'helm-ls-git--filter-process)
      (save-selected-window
        (display-buffer (process-buffer proc)))
      (set-process-sentinel
       proc (lambda (_process event)
              (if (string= event "finished\n")
                  (progn (message "%sing from %s done" pcommand remote)
                         (when helm-alive-p
                           (with-helm-window (helm-force-update "^\\*"))))
                (error "Failed %sing from %s" command remote)))))))

(defun helm-ls-git--filter-process (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
        (save-excursion
          ;; Insert the text, advancing the process marker.
          (goto-char (process-mark proc))
          ;; Ignore git progress reporter lines.
          (unless (string-match-p "\\'" string)
            (insert string))
          (set-marker (process-mark proc) (point)))
        (when moving
          (goto-char (process-mark proc)))))))

(defun helm-ls-git-pull (_candidate)
  (helm-ls-git--pull-or-fetch "pull" "--stat"))

(defun helm-ls-git-fetch (_candidate)
  (helm-ls-git--pull-or-fetch "fetch"))

(defun helm-ls-git-run-pull ()
  (interactive)
  (with-helm-alive-p
    (helm-set-attr 'pull 'helm-ls-git-pull)
    (helm-execute-persistent-action 'pull)))
(put 'helm-ls-git-run-pull 'no-helm-mx t)

(defun helm-ls-git-run-fetch ()
  (interactive)
  (with-helm-alive-p
    (helm-set-attr 'fetch '(helm-ls-git-fetch . never-split))
    (helm-execute-persistent-action 'fetch)))
(put 'helm-ls-git-run-fetch 'no-helm-mx t)

(defun helm-ls-git-branch-rebase (candidate)
  "Rebase CANDIDATE branch on current branch."
  (if (helm-ls-git-rebase-running-p)
      (if (y-or-n-p "A rebase is already running, continue ?")
          (helm-ls-git-rebase-continue nil)
        (helm-ls-git-rebase-abort nil))
    (let ((branch (helm-ls-git-normalize-branch-name candidate))
          (current (helm-ls-git--branch)))
      (when (y-or-n-p (format "Rebase branch %s from %s?" current branch))
        (if (= (process-file "git" nil nil nil "rebase" branch) 0)
            (progn (message "Branch %s rebased successfully from %s" current branch)
                   (helm-ls-git-revert-buffers-in-project))
          (message "failed to rebase from branch %s, try to abort rebasing or resolve conflicts" branch))))))

(defvar helm-ls-git-branches-source
  (helm-build-in-buffer-source "Git branches"
    :init (lambda ()
            (let ((data (helm-ls-git-collect-branches
                         helm-ls-git-branches-show-all)))
              (helm-init-candidates-in-buffer 'global data)))
    :candidate-transformer 'helm-ls-git-branches-transformer
    :action-transformer (lambda (actions candidate)
                          (if (not (string-match "\\`[*]" candidate))
                              (append
                               '(("Checkout" . helm-ls-git-checkout)
                                 ("Delete branche(s)" . helm-ls-git-delete-marked-branches)
                                 ("Merge in current" .
                                  helm-ls-git-branches-merge)
                                 ("Rebase in current" .
                                  helm-ls-git-branch-rebase))
                               actions)
                            (helm-append-at-nth
                             actions
                             '(("Git amend" . helm-ls-git-amend-commit)
                               ("Git push (C-c P)" . helm-ls-git-push))
                             2)))
    :help-message 'helm-ls-git-help-message
    :cleanup (lambda () (setq helm-ls-git-branches-show-all nil))
    :persistent-help "Checkout"
    :persistent-action (lambda (candidate)
                         (helm-ls-git-checkout candidate)
                         (helm-force-update "^\\*"))
    :action '(("Git status" . (lambda (_candidate)
                                (funcall helm-ls-git-status-command
                                         (helm-default-directory))))
              ("Git log (M-L)" . helm-ls-git-show-log))
    :keymap 'helm-ls-git-branches-map))


;;; Stashing
;;
(defun helm-ls-git-list-stashes ()
  (helm-aif (helm-ls-git-root-dir)
      (with-helm-default-directory it
        (with-output-to-string
          (with-current-buffer standard-output
            (apply #'process-file
                   "git"
                   nil (list t helm-ls-git-log-file) nil
                   (list "stash" "list")))))))

(defun helm-ls-git-get-stash-name (candidate)
  (when (string-match "stash@[{][0-9]+[}]" candidate)
    (match-string 0 candidate)))

(defun helm-ls-git-stash-show (candidate)
  (if (and (eq last-command 'helm-execute-persistent-action)
           (get-buffer-window "*stash diff*" 'visible))
      (kill-buffer "*stash diff*")
    (let ((stash (helm-ls-git-get-stash-name candidate)))
      (with-current-buffer (get-buffer-create "*stash diff*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (with-helm-default-directory (helm-ls-git-root-dir)
                    (with-output-to-string
                      (with-current-buffer standard-output
                        (process-file
                         "git" nil (list t helm-ls-git-log-file) nil
                         "stash" "show" "-p" stash)))))
          (diff-mode))
        (display-buffer (current-buffer))))))

(defun helm-ls-git-stash-apply (candidate)
  (let ((num (helm-ls-git-get-stash-name candidate)))
    (if (eq (process-file "git" nil nil nil "stash" "apply" num) 0)
        (progn
          (helm-ls-git-revert-buffers-in-project)
          (message "Stash <%s> applied" candidate))
      (error "Couldn't apply stash <%s>" candidate))))

(defun helm-ls-git-stash-pop (candidate)
  (let ((num (helm-ls-git-get-stash-name candidate)))
    (if (eq (process-file "git" nil nil nil "stash" "pop" num) 0)
        (progn
          (helm-ls-git-revert-buffers-in-project)
          (message "Stashed pop <%s>" candidate))
      (error "Couldn't stash pop <%s>" candidate))))

(defun helm-ls-git-stash-1 (name)
  (with-helm-default-directory (helm-ls-git-root-dir)
    (apply #'process-file "git" nil nil nil `("stash" "push" "-m" ,name))
    (helm-ls-git-revert-buffers-in-project)))

(defun helm-ls-git-stash (_candidate)
  (let ((name (read-string "Stash name: ")))
    (helm-ls-git-stash-1 name)))

(defun helm-ls-git-run-stash ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-ls-git-stash)))
(put 'helm-ls-git-run-stash 'no-helm-mx t)

(defun helm-ls-git-stash-snapshot (_candidate)
  (vc-git-stash-snapshot))

(defun helm-ls-git-run-stash-snapshot ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-ls-git-stash-snapshot)))
(put 'helm-ls-git-run-stash-snapshot 'no-helm-mx t)

(defun helm-ls-git-stash-drop (candidate)
  (let ((num (helm-ls-git-get-stash-name candidate)))
    (if (eq (process-file "git" nil nil nil "stash" "drop" num) 0)
        (message "Stash <%s> deleted" candidate)
      (error "Couldn't delete <%s>" candidate))))

(defun helm-ls-git-stash-drop-marked (_candidate)
  (let ((mkd (helm-marked-candidates)))
    (cl-loop with sorted =
             (sort mkd (lambda (s1 s2)
                         (let ((n1 (and (string-match
                                         "^stash@[{]\\([0-9]+\\)[}]" s1)
                                        (match-string 1 s1)))
                               (n2 (and (string-match
                                         "^stash@[{]\\([0-9]+\\)[}]" s2)
                                        (match-string 1 s2))))
                           (string-greaterp n1 n2))))
             for c in sorted do (helm-ls-git-stash-drop c))))

(defun helm-ls-git-apply-patch (_candidate)
  (with-helm-default-directory (helm-default-directory)
    (let ((patchs (helm-marked-candidates)))
      (with-current-buffer-window "*git apply*" '(display-buffer-below-selected
                                                  (window-height . fit-window-to-buffer)
                                                  (preserve-size . (nil . t)))
          nil
        (apply #'process-file "git" nil t t "apply" patchs)
        (helm-ls-git-revert-buffers-in-project)))))

(defvar helm-ls-git-stashes-source
  (helm-build-in-buffer-source "Stashes"
    :data 'helm-ls-git-list-stashes
    :persistent-action 'helm-ls-git-stash-show
    :action '(("Apply stash" . helm-ls-git-stash-apply)
              ("Pop stash" . helm-ls-git-stash-pop)
              ("Drop stashe(s)" . helm-ls-git-stash-drop-marked))))

;;; Git status
(defun helm-ls-git-status (&optional ignore-untracked)
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
                   (if ignore-untracked
                       (list "status" "-uno" "--porcelain")
                     (list "status" "--porcelain"))))))))

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
                 ((string-match "^\\([D] +\\)\\(.*\\)" i)
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
                   (helm-make-actions "Add file(s)"
                                      (lambda (candidate)
                                        (let ((default-directory
                                                (file-name-directory candidate))
                                              (marked (helm-marked-candidates)))
                                          (vc-call-backend 'Git 'register marked)))
                                      "Delete file(s)"
                                      'helm-ff-delete-files
                                      (lambda ()
                                        (and (string-match "\\`[?]\\{2\\}.*\\.patch\\|diff" disp)
                                             "Apply patch"))
                                      'helm-ls-git-apply-patch
                                      (lambda ()
                                        (and (string-match "\\`[?]\\{2\\}.*\\.patch" disp)
                                             "Git AM patches"))
                                      'helm-ls-git-am-files
                                      "Copy bnames to .gitignore"
                                      (lambda (candidate)
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
                                            (save-buffer)))))))
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
                            '(("Stage file(s) (C-c s)"
                               . helm-ls-git-stage-files)
                              ("Stage marked file(s) and commit (C-c c)"
                               . helm-ls-git-stage-marked-and-commit)
                              ("Stage marked file(s) and extend commit (C-c e)"
                               . helm-ls-git-stage-marked-and-extend-commit)
                              ("Stage marked file(s) and amend commit (C-c a)"
                               . helm-ls-git-stage-marked-and-amend-commit)
                              ("Stash (C-c z)" . helm-ls-git-stash)
                              ("Stash snapshot (C-c Z)" . helm-ls-git-stash-snapshot))
                            1)))
          ;; Modified and staged
          ((string-match "^M+ +" disp)
           (append actions (helm-append-at-nth
                            mofified-actions
                            '(("Commit staged file(s)"
                               . helm-ls-git-commit)
                              ("Extend commit"
                               . helm-ls-git-extend-commit)
                              ("Amend commit"
                               . helm-ls-git-amend-commit)
                              ("Unstage file(s)"
                               . helm-ls-git-unstage-files)
                              ("Git rebase continue" .
                               helm-ls-git-rebase-continue)
                              ("Git cherry-pick continue" .
                               helm-ls-git-cherry-pick-continue)
                              ("Git AM continue" .
                               helm-ls-git-am-continue)
                              ("Git merge continue" .
                               helm-ls-git-merge-continue)
                              ("Git revert continue" .
                               helm-ls-git-log-revert-continue))
                            1)))
          ;; Deleted
          ((string-match "^ D " disp)
           (append actions (list '("Git delete" . (lambda (_candidate)
                                                    (let ((mkd (helm-marked-candidates)))
                                                      (cl-loop for c in mkd
                                                               do (helm-ls-git-rm c)))))
                                 '("Stage file(s)"
                                   . helm-ls-git-stage-files))))
          ;; Deleted and staged
          ((string-match "^D +" disp)
           (append actions (list '("Commit staged file(s)"
                                   . helm-ls-git-commit)
                                 '("Stage marked file(s) and commit"
                                   . helm-ls-git-stage-marked-and-commit))))
          ;; Conflict
          ((string-match "^U+ +" disp)
           (append actions (list '("Git cherry-pick abort" . helm-ls-git-cherry-pick-abort)
                                 '("Git rebase abort" . helm-ls-git-rebase-abort)
                                 '("Git AM abort" . helm-ls-git-am-abort)
                                 '("Git merge abort" . helm-ls-git-merge-abort)
                                 '("Git revert abort" . helm-ls-git-log-revert-abort))))
          (t actions))))

(defun helm-ls-git-am-files (_candidate)
  (let ((files (helm-marked-candidates)))
    (cl-assert (cl-loop for f in files
                        for ext = (file-name-extension f)
                        always (and ext (string= ext "patch"))))
    (with-current-buffer-window "*git am*" '(display-buffer-below-selected
                                             (window-height . fit-window-to-buffer)
                                             (preserve-size . (nil . t)))
        nil
      (apply #'process-file "git" nil t nil "am" files))))

(defun helm-ls-git-am-abort (_candidate)
  (with-helm-default-directory (helm-default-directory)
    (process-file "git" nil nil nil "am" "--abort")))

(defun helm-ls-git-rm (_candidate)
  (with-helm-default-directory (helm-default-directory)
    (let ((files (helm-marked-candidates)))
    (apply #'process-file "git" nil nil nil "rm" files))))

;;; Stage and commit
;;
(defun helm-ls-git-stage-files (_candidate)
  "Stage marked files."
  (let* ((files (helm-marked-candidates))
         (default-directory (helm-default-directory)))
    (apply #'process-file "git" nil nil nil "stage" files)))

(defun helm-ls-git-run-stage-files (arg)
  (interactive "P")
  (with-helm-alive-p
    (helm-exit-and-execute-action (if arg
                                      'helm-ls-git-unstage-files
                                    'helm-ls-git-stage-files))))
(put 'helm-ls-git-run-stage-files 'no-helm-mx t)

(defun helm-ls-git-unstage-files (_candidate)
  "Unstage marked files."
  (let* ((files (helm-marked-candidates))
         (default-directory (file-name-directory (car files))))
    (apply #'process-file "git" nil nil nil "reset" "HEAD" "--" files)))

(defun helm-ls-git-stage-marked-and-commit (_candidate)
  "Stage marked files and commit."
  (helm-ls-git-stage-files nil)
  (let ((proc (helm-ls-git-with-editor "commit" "-v")))
    (set-process-sentinel proc 'helm-ls-git-commit-sentinel)))

(defun helm-ls-git-commit-sentinel (process event)
  (let ((default-directory (with-current-buffer (process-buffer process)
                             default-directory)))
    (when (string= event "finished\n")
      (let ((commit (helm-ls-git-oneline-log (helm-ls-git--branch))))
        (when (string-match "\\`\\([^ ]+\\)+ +\\(.*\\)" commit)
          (add-face-text-property 0 (match-end 1)
                                  'font-lock-type-face nil commit)
          (add-face-text-property (1+ (match-end 1))
                                  (match-end 2)
                                  'font-lock-function-name-face nil commit))
        (message "Commit done, now at `%s'" commit)))))

(defun helm-ls-git-run-stage-marked-and-commit ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-ls-git-stage-marked-and-commit)))
(put 'helm-ls-git-run-stage-marked-and-commit 'no-helm-mx t)

(defun helm-ls-git-stage-marked-and-extend-commit (candidate)
  "Stage marked files and extend these changes to last commit"
  (helm-ls-git-stage-files nil)
  (helm-ls-git-extend-commit candidate))

(defun helm-ls-git-run-stage-marked-and-extend-commit ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-ls-git-stage-marked-and-extend-commit)))
(put 'helm-ls-git-run-stage-marked-and-extend-commit 'no-helm-mx t)

(defun helm-ls-git-stage-marked-and-amend-commit (candidate)
  "Stage marked files and amend last commit."
  (helm-ls-git-stage-files nil)
  (helm-ls-git-amend-commit candidate))

(defun helm-ls-git-run-stage-marked-and-amend-commit ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-ls-git-stage-marked-and-amend-commit)))
(put 'helm-ls-git-run-stage-marked-and-amend-commit 'no-helm-mx t)

(defun helm-ls-git-extend-commit (candidate)
  (let ((default-directory (file-name-directory candidate)))
    (process-file "git" nil nil nil "commit" "--amend" "--no-edit")))

(defun helm-ls-git-amend-commit (_candidate)
  "Amend last commit."
  (let ((proc (helm-ls-git-with-editor "commit" "-v" "--amend")))
    (set-process-sentinel proc 'helm-ls-git-commit-sentinel)))

(defun helm-ls-git-commit (_candidate)
  "Commit already staged files."
  (let ((proc (helm-ls-git-with-editor "commit" "-v")))
    (set-process-sentinel proc 'helm-ls-git-commit-sentinel)))


;;; Emacsclient as git editor
;;
;;;###autoload
(add-to-list 'auto-mode-alist '("/COMMIT_EDITMSG$" . helm-ls-git-commit-mode))

(defun helm-ls-git-with-editor (&rest args)
  "Binds GIT_EDITOR env var to emacsclient and run git with ARGS.
Bound `default-directory' to the root dir of project determining value
from the helm-buffer, so don't use this function outside of helm
context i.e. use it in helm actions."
  (require 'server)
  (let ((default-directory (expand-file-name
                            (helm-ls-git-root-dir
                             (helm-default-directory))))
        (process-environment process-environment)
        (bname (format "*helm-ls-git %s*" (car args))))
    ;; It seems git once it knows GIT_EDITOR reuse the same value
    ;; along its whole process e.g. when squashing in a rebase
    ;; process, so even if the env setting goes away after initial
    ;; process, git should reuse same GIT_EDITOR in subsequent
    ;; commits.
    (when (get-buffer bname) (kill-buffer bname))
    (push "GIT_EDITOR=emacsclient $@" process-environment)
    (unless (server-running-p)
      (server-start))
    (apply #'start-file-process "git" bname "git" args)))

(defun helm-ls-git-server-edit ()
  (interactive)
  (cl-assert server-clients nil "No server editing buffers exist")
  ;; Prevent server asking to save file when done.
  (helm-aif buffer-file-name
      (save-buffer it))
  (server-edit))

;; Same as `server-edit-abort' from emacs-28 but kill edit buffer as well.
(defun helm-ls-git-server-edit-abort ()
  "Abort editing the current client buffer."
  (interactive)
  (if server-clients
      (progn
        (mapc (lambda (proc)
                (server-send-string
                 proc (concat "-error "
                              (server-quote-arg "Aborted by the user"))))
              server-clients)
        (kill-buffer))
    (message "This buffer has no clients")))

(defvar helm-ls-git-commit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'helm-ls-git-server-edit)
    (define-key map (kbd "C-c C-k") 'helm-ls-git-server-edit-abort)
    map))

;;;###autoload
(define-derived-mode helm-ls-git-commit-mode diff-mode "helm-ls-git-commit"
  "Mode to edit COMMIT_EDITMSG files.

Commands:
\\{helm-ls-git-commit-mode-map}
"
  (helm-ls-git-with-editor-setup))

(defun helm-ls-git-with-editor-setup ()
  (setq fill-column 70)
  (setq buffer-read-only nil)
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-end) "")
  (auto-fill-mode 1)
  (run-at-time
   0.1 nil
   (lambda ()
     (message
      "When done with a buffer, type `C-c C-c', to abort type `C-c C-k'"))))

;;; Git rebase
;;
;;;###autoload
(add-to-list 'auto-mode-alist '("/git-rebase-todo$" . helm-ls-git-rebase-todo-mode))

(defconst helm-ls-git-rebase-actions
  '(("p" . "pick")
    ("r" . "reword")
    ("e" . "edit")
    ("s" . "squash")
    ("f" . "fixup")
    ("x" . "exec")
    ("d" . "drop")))

(defvar helm-ls-git-rebase-todo-font-lock-keywords
  '(("^\\([a-z]+\\) \\([0-9a-f]+\\) \\(.*\\)$"
     (1 'font-lock-keyword-face)
     (2 'font-lock-function-name-face))
    ("^#.*$" . 'font-lock-comment-face))
  "Keywords in `helm-ls-git-rebase-todo' mode.")

(defvar helm-ls-git-rebase-todo-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-n") 'helm-ls-git-rebase-todo-move-down)
    (define-key map (kbd "M-p") 'helm-ls-git-rebase-todo-move-up)
    (define-key map (kbd "C-c C-c") 'helm-ls-git-server-edit)
    (define-key map (kbd "C-c C-k") 'helm-ls-git-server-edit-abort)
    map)
  "Keymap used in `helm-ls-git-rebase-todo-mode' buffers.")

(defun helm-ls-git-rebase-todo-move-down ()
  "Move commit line one line down."
  (interactive)
  (beginning-of-line)
  (let* ((next (+ 1 (line-end-position)))
         (line (buffer-substring (point) next)))
    (delete-region (point) next)
    (forward-line 1)
    (insert line)
    (forward-line -1)))

(defun helm-ls-git-rebase-todo-move-up ()
  "Move commit line on line up."
  (interactive)
  (beginning-of-line)
  (let* ((next (+ 1 (line-end-position)))
         (line (buffer-substring (point) next)))
    (delete-region (point) next)
    (forward-line -1)
    (insert line)
    (forward-line -1)))

(defun helm-ls-git-rebase-action (action)
  "Replace the current rebase command at bol by ACTION.
ACTION is the cdr entry of one of `helm-ls-git-rebase-actions'."
  (let* ((assocs helm-ls-git-rebase-actions)
         (regexp (cl-loop with len = (length assocs)
                          for (_k . v) in assocs
                          for count from 1 to len
                          concat (concat v (if (= count len) "" "\\|")) into str
                          finally return (concat "^\\(" str "\\) +")))
         (inhibit-read-only t))
    (goto-char (point-at-bol))
    (save-excursion
      (when (re-search-forward regexp (point-at-eol) t)
        (delete-region (point-at-bol) (match-end 1))))
    (insert (cdr (rassoc action assocs)))
    (forward-line 1)))

(cl-defun helm-ls-git-rebase-build-commands ()
  "build a function for each `helm-ls-git-rebase-actions' entry.
Bind it to the car of each entry of `helm-ls-git-rebase-actions'."
  (cl-loop for (k . v) in helm-ls-git-rebase-actions
           for sym = (intern (concat "helm-ls-git-rebase-" v))
           for doc = (format "Replace current rebase command at bol by `%s'." v)
           do (progn
                (defalias sym `(lambda () (interactive)
                                 (helm-ls-git-rebase-action ,v))
                  doc)
                (define-key helm-ls-git-rebase-todo-mode-map (kbd k) sym))))

;;;###autoload
(define-derived-mode helm-ls-git-rebase-todo-mode fundamental-mode "helm-ls-git-rebase-todo"
  "Major Mode to edit git-rebase-todo files when using git rebase -i.

Commands:
\\{helm-ls-git-rebase-todo-mode-map}
"
  (set (make-local-variable 'font-lock-defaults)
       '(helm-ls-git-rebase-todo-font-lock-keywords t))
  (helm-ls-git-rebase-build-commands)
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-end) "")
  (run-at-time
   0.1 nil
   (lambda ()
     (message
      "When done with a buffer, type `C-c C-c', to abort type `C-c C-k'"))))


;;; Build sources
;;
;; Overhide the actions of helm-type-buffer.
(cl-defmethod helm--setup-source :after ((source helm-source-buffers))
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
         :keymap 'helm-ls-git-buffer-map)))


;;;###autoload
(defun helm-ls-git (&optional arg)
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
          :truncate-lines helm-buffers-truncate-lines
          :buffer "*helm lsgit*")))

(defalias 'helm-ls-git-ls 'helm-ls-git)
(make-obsolete 'helm-ls-git-ls 'helm-ls-git "1.9.2")
(put 'helm-ls-git-ls 'no-helm-mx t)


(provide 'helm-ls-git)

;;; helm-ls-git.el ends here
