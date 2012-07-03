helm-ls-git
===========

Yet another helm to list git file.

Features:

Similar to helm-git.el but with no dependency on magit.

Allow toggling full path of files with C-]

Inherit actions from helm locate.

Signal error in helm-buffer when trying to use on a non git based repo.

Action popup in action buffer of helm-find-files only when current directory is git based.
