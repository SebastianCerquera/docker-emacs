;;; lain-tests.el --- Tests for lain.el

;; Author: Sebastian Gonzalez <sebastian2zen@gmail.com>
;; Maintainer: sebastian2zen@gmail.com

;; Tests for lain.el

;;; Code:

(require 'ert)
(require 'lain)

(ert-deftest test-dummy ()
  (lain-goto-to-task "hola")
  (should (= 1 1)))


(provide 'lain-tests)
;;; lain-tests.el ends here
