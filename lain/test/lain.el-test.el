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

(defun test-rescheduling-task(text date)
  (org-base-view (list (concat my-test-location "test/AGENDA/PROJECT.org")) 'org-todo-list)
  (lain-reschedule-task text date)
  (lain-goto-to-task text)
  (let ((result (lain-extract-date-scheduling)))
	(should (string-equal date result))))

(ert-deftest rescheduling-task ()
  (test-rescheduling-task "PROJECTS, 2" "2020-12-05"))

(ert-deftest rescheduling-task-periodic ()
  (test-rescheduling-task "PROJECTS, 1 monthly" "2020-02-05")
  (extrac-periodic-scheduling-test "PROJECTS, 1 monthly" "+1m"))



(ert-deftest lain-task-done-periodic ()
  (lain-done-task "PROJECTS, 1 weekly" "2020-12-05" "00:00" nil)
  (let ((result (lain-extract-date-scheduling)))
    (should (string-equal "2020-01-12" result))))

;; No se el regex para obtener el primer item de la lista de notas de registro
;; (ert-deftest lain-task-done-link ())

(ert-deftest lain-task-done ()
  (lain-done-task "PROJECTS, TO COMPLETE" "2020-12-05" "00:00" nil))

(provide 'lain-tests)
;;; lain-tests.el ends here
