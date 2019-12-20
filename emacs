(server-start)
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(defun prepare-extensions()
  (package-install 'htmlize)
  (package-install 'elnode))

(unless package-archive-contents
  (package-refresh-contents))