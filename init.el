;; -*-lisp-*-
;;
;; init.el



;;
;; -- Startup --
;;

;; hide welcome screen
(setq inhibit-startup-message t)

;; custom scratch buffer message
(setq initial-scratch-message
      (with-temp-buffer
        (insert-file-contents "~/.emacs.d/logo.txt")
        (buffer-string)))


;;
;; -- UI Layout --
;;

;; hide menu bar
(menu-bar-mode -1)

;; hide toolbar
(tool-bar-mode -1)

;; display line and column numbers
(line-number-mode)
(column-number-mode)


;;
;; -- package management --
;;

(require 'package)
;(setq package-enable-at-startup nil)

(setf package-archives
  '(("gnu"       . "http://elpa.gnu.org/packages/")
    ("melpa"     . "http://melpa.org/packages/")
    ("marmalade" . "http://marmalade-repo.org/packages/")))

(package-initialize)

;; bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package pabbrev
  :ensure t)

(use-package tex-site
  :ensure auctex)

(use-package clojure-mode
  :ensure t)

(use-package cider
  :ensure t)

(use-package gitignore-mode
  :ensure t)

(use-package magit
  :ensure t
  :config
  ;; hide magit warnings up to 1.4.0
  (setq magit-last-seen-setup-instructions "1.4.0"))

(use-package markdown-mode
  :ensure t)

(use-package paredit
  :ensure t
  :init
  (autoload 'enable-paredit-mode "paredit"
    "Turn on pseudo-structural editing of Lisp code." t)
  (dolist (hook '(emacs-lisp-mode-hook
                  eval-expression-minibuffer-setup-hook
                  ielm-mode-hook
                  lisp-mode-hook
                  lisp-interaction-mode-hook
                  scheme-mode-hook))
    (add-hook hook #'enable-paredit-mode)))

(use-package python-mode
  :ensure t)



;;
;; -- doc-view-mode settings --
;;

; makes scrolling past the end of a page jump to the next page
(setf doc-view-continuous t)

;;
;; -- formatting --
;;

;; tabs are evil
(setq-default indent-tabs-mode nil)

;; set tab width to 4
(setq-default tab-width 4)

;; show trailing whitespace
(setq-default show-trailing-whitespace t)

;; whitespace-mode
(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)
