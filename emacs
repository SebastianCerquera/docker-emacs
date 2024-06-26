(ido-mode t)

(global-set-key (kbd "C-x C-m") 'execute-extended-command)

(require 'iso-transl)
(setq-default indent-tabs-mode nil)

(setq-default standard-indent 2)

(show-paren-mode t)
(put 'narrow-to-region 'disabled nil)

(put 'scroll-left 'disabled nil)

(setq x-select-enable-clipboard t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(setq org-log-done 'time)

(setq org-clock-idle-time 15)
(setq org-clock-persist 'history)

(setq org-use-tag-inheritance nil)

(server-start)

(put 'downcase-region 'disabled nil)

(require 'package)
; add MELPA to repository list
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

(package-initialize)

;;(require 'elnode)
(require 'htmlize)

(add-to-list 'load-path "~/.emacs.d/lain/")
(require 'lain)

(require 'evil)
(evil-mode 1)

(defun high-bright-look-and-feel ()
  (interactive)
  (set-background-color "black")
  (set-foreground-color "orange"))


(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (emacs-lisp . t)))

(setq org-babel-python-command "python3")

(setq org-src-fontify-natively t)
(setq org-html-htmlize-output-type 'css)

(defface org-block-begin-line
  '((t (:underline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF")))
  "Face used for the line delimiting the begin of source blocks.")

(defface org-block-background
  '((t (:background "#FFFFEA")))
  "Face used for the source block background.")

(defface org-block-end-line
  '((t (:overline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF")))
  "Face used for the line delimiting the end of source blocks.")
