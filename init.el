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

;; hide magit warnings up to 1.4.0
(setq magit-last-seen-setup-instructions "1.4.0")


;;
;; -- UI Layout --
;;

;; hide menu bar
(menu-bar-mode -1)

;; hide toolbar
(tool-bar-mode -1)


;;
;; -- package management --
;;

;; list of packages to download
(setf *package-list*
  '(clojure-mode
    cider
    gitignore-mode
    magit
    markdown-mode
    paredit
    python-mode))

;; list of packages to require
(setf *package-require-list*
  *package-list*)



;; load package management system (Emacs 24+ only)
(when (>= emacs-major-version 24)
  ;; load package
  (require 'package)

  ;; define package repository locations
  (setf package-archives
    '(("gnu"       . "http://elpa.gnu.org/packages/")
      ("melpa"     . "http://melpa.org/packages/")
      ("marmalade" . "http://marmalade-repo.org/packages/")))

  ;; initialize packages
  (package-initialize)

  ;; download repository information if not present
  (unless (and (file-exists-p "~/.emacs.d/elpa/archives/gnu")
	       (file-exists-p "~/.emacs.d/elpa/archives/melpa")
	       (file-exists-p "~/.emacs.d/elpa/archives/marmalade"))
    (package-refresh-contents))

  ;; install packages
  (dolist (package *package-list*)
    (unless (package-installed-p package)
      (package-install package)))

  ;; require packages
  (dolist (package *package-require-list*)
    (require package)))


;;
;; -- doc-view-mode settings --
;;

; makes scrolling past the end of a page jump to the next page
(setf doc-view-continuous t)


;;
;; -- paredit mode --
;;

(autoload 'enable-paredit-mode
  "paredit"
  "Turn on pseudo-structural editing of Lisp code."
  t)
(dolist (hook '(emacs-lisp-mode-hook
		eval-expression-minibuffer-setup-hook
		ielm-mode-hook
		lisp-mode-hook
		lisp-interaction-mode-hook
		scheme-mode-hook))
  (add-hook hook #'enable-paredit-mode))
