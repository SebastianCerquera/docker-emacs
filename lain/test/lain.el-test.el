;;; lain-tests.el --- Tests for lain.el

;; Author: Sebastian Gonzalez <sebastian2zen@gmail.com>
;; Maintainer: sebastian2zen@gmail.com

;; Tests for lain.el

;;; Code:

(require 'ert)
(require 'lain)

(defun test-look-and-feel ())

(defalias 'lain-look-and-feel 'test-look-and-feel)

(ert-deftest check-todo-agenda ()
  (org-base-view '("/home/sebastian/sources/lain/docker-emacs/lain/test/AGENDA/PROJECT.org") 'org-todo-list)
  (re-search-forward "PROJECTS, 1"))


(provide 'lain-tests)
;;; lain-tests.el ends here
