;;; lain-tests.el --- Tests for lain.el

;; Author: Sebastian Gonzalez <sebastian2zen@gmail.com>
;; Maintainer: sebastian2zen@gmail.com

;; Tests for lain.el

;;; Code:

(require 'ert)
(require 'lain)

(defun test-look-and-feel ())

(defalias 'lain-look-and-feel 'test-look-and-feel)

(defvar lain-agenda-buffer-name "*Org Agenda*")

(defvar lain-agenda-buffer-name "TASKS.html")

(defvar my-test-location (file-name-directory (or default-directory buffer-file-name)))

(ert-deftest check-org-base-view ()
  (org-base-view (list (concat my-test-location "test/AGENDA/PROJECT.org")) 'org-todo-list)
  (let ((buffer (get-buffer (buffer-name))))
	(switch-to-buffer buffer)
	(set-buffer buffer)
	(re-search-forward "PROJECTS, 1")))

(ert-deftest check-base-view ()
  (let ((name "TEST1"))
    (base-view name (list (concat my-test-location "test/AGENDA/PROJECT.org")) 'org-todo-list)
    (should (file-exists-p (concat "/tmp/org/" name ".html")))))

(provide 'lain-tests)
;;; lain-tests.el ends here
