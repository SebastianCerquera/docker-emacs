;;; lain-tests.el --- Tests for lain.el

;; Author: Sebastian Gonzalez <sebastian2zen@gmail.com>
;; Maintainer: sebastian2zen@gmail.com

;; Tests for lain.el

;;; Code:

(require 'ert)
(require 'lain)

(defun test-look-and-feel ())

(defalias 'lain-look-and-feel 'test-look-and-feel)

;; Enviromental variables
(setq lain-agenda-buffer-name "*Org Agenda*")

(defvar my-test-location (file-name-directory (or default-directory buffer-file-name)))

(ert-deftest check-org-base-view ()
  (org-base-view (list (concat my-test-location "test/AGENDA/PROJECT.org")) 'org-todo-list)
  (re-search-forward "PROJECTS, 1"))

(ert-deftest check-base-view ()
  (let ((name "TEST1"))
    (base-view name (list (concat my-test-location "test/AGENDA/PROJECT.org")) 'org-todo-list)
    (should (file-exists-p (concat "/tmp/org/" name ".html")))))

(defun extrac-periodic-scheduling-test (text result)
  (org-base-view (list (concat my-test-location "test/AGENDA/PROJECT.org")) 'org-todo-list)
  (lain-goto-to-task text)
  (should (string-equal (lain-extract-periodic-scheduling) result)))

(ert-deftest extrac-periodic-scheduling ()
  (extrac-periodic-scheduling-test "PROJECTS, 1 weekly" "+1w")
  (extrac-periodic-scheduling-test "PROJECTS, 1 monthly" "+1m"))

(provide 'lain-tests)
;;; lain-tests.el ends here
