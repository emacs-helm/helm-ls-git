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

(defvar helm-ls-git-log-file "/tmp/ls-git.log")
;; Internal flag
(defvar helm-ls-git-root-directory nil)

(defun helm-ls-git-list-files ()
  (when (file-exists-p helm-ls-git-log-file)
    (delete-file helm-ls-git-log-file))
  (with-output-to-string
      (with-current-buffer standard-output
        (apply #'process-file
               "git"
               nil (list t helm-ls-git-log-file) nil
               (list "ls-files" "--full-name" "--")))))

(defun helm-ls-git-root-dir ()
  (let ((result
         (with-output-to-string
             (with-current-buffer standard-output
               (process-file "git" nil (list t nil) nil
                             "rev-parse" "--git-dir")))))
    (unless (or (string= result "") (not result))
      (file-name-as-directory
       (expand-file-name
        ".." (replace-regexp-in-string "\n" "" result))))))

(defun helm-ls-git-transformer (candidates source)
  (loop with root = (let ((default-directory
                           (or helm-ls-git-root-directory
                               (with-helm-current-buffer
                                 default-directory))))
                      (helm-ls-git-root-dir))
        for i in candidates
        for abs = (expand-file-name i root)
        for disp = (if (and helm-ff-transformer-show-only-basename
                            (not (string-match "[.]\\{1,2\\}$" i)))
                       (helm-c-basename i) abs)
        collect
        (cons (propertize disp 'face 'helm-ff-file) abs)))

(defun helm-ls-git-init ()
  (let ((data (helm-ls-git-list-files)))
    (when (string= data "")
      (setq data
            (with-current-buffer
                (find-file-noselect helm-ls-git-log-file)
              (prog1
                  (buffer-substring-no-properties
                   (point-min) (point-max))
                (kill-buffer)))))
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

;;;###autoload
(defun helm-ls-git-ls ()
  (interactive)
  (unwind-protect
       (helm :sources 'helm-c-source-ls-git 
             :buffer "*helm lsgit*")
    (setq helm-ls-git-root-directory nil)))

;;; Helm-find-files integration.
;;
(defun helm-ff-ls-git-find-files (candidate)
  (let ((default-directory helm-ff-default-directory))
    (setq helm-ls-git-root-directory default-directory)
    (helm-run-after-quit
     #'(lambda (d)
         (let ((default-directory d))
           (helm-ls-git-ls)))
     default-directory)))

(defun helm-ls-git-root-p (file)
  (when (or (file-exists-p file)
            (file-directory-p file))
    (let ((default-directory helm-ff-default-directory))
      (stringp (condition-case nil
                   (helm-ls-git-root-dir)
                 (error nil))))))

(when (require 'helm-files)
  (helm-add-action-to-source-if
   "Git ls-files"
   'helm-ff-ls-git-find-files
   helm-c-source-find-files
   'helm-ls-git-root-p
   4))

(provide 'helm-ls-git)

;;; helm-ls-git.el ends here
