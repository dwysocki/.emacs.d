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
(and (boundp 'tool-bar-mode)
     (tool-bar-mode -1))

;; hide scrollbar
(and (boundp 'scroll-bar-mode)
     (scroll-bar-mode -1))


;; display line and column numbers
(line-number-mode)
(column-number-mode)


;;
;; -- package management --
;;

;; bootstrap `package'
(require 'package)
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
  :ensure t
  :init
  (add-hook 'markdown-mode-hook
    (lambda ()
      (define-key markdown-mode-map
        (kbd "C-c C-c C-c") 'Rmarkdown-compile-silent)
      (define-key markdown-mode-map
        (kbd "C-c C-c C-v") 'Rmarkdown-compile-verbose))))

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
  :ensure t
  :init
  (add-hook 'python-mode-hook
    (lambda () (whitespace-mode t))))


;; load local files
(add-to-list 'custom-theme-load-path
             (expand-file-name "~/.emacs.d/themes/"))

;; load theme
(load-theme 'base16-eighties-dark t)


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

(defun enable-whitespace-mode ()
  (whitespace-mode t))

;; enable whitespace-mode only in certain modes
(dolist (hook '(c-mode-hook
                clojure-mode-hook
                java-mode-hook
                lisp-mode-hook
                lisp-interaction-mode-hook
                python-mode-hook))
  (add-hook hook #'enable-whitespace-mode))


;;
;; -- OS X specific --
;;

(when (eq system-type 'darwin)
  (setf ispell-program-name "/usr/local/bin/ispell")
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/Library/TeX/texbin/"))
  (setf exec-path (append exec-path '("/usr/local/bin" "/Library/TeX/texbin/"))))

;;
;; -- keybindings --
;;

(global-set-key (kbd "C-;")
  'other-window)
(global-set-key (kbd "C-:")
  'prev-window)
(global-set-key (kbd "C-x 4 4")
  'split-window-4-way)

(defun prev-window ()
  (interactive)
  (other-window -1))

(defun split-window-4-way ()
  (interactive)
  (let ((starting-window (selected-window)))
    (split-window-below)
    (split-window-right)
    (other-window 2)
    (split-window-right)
    (balance-windows)
    ;; return to initial window
    (select-window starting-window)))

;;
;; -- Rmarkdown
;;

(defun Rmarkdown-compile-silent ()
  (interactive)
  (shell-command
    (format "echo 'rmarkdown::render(\"%s\", \"all\")' | R --no-save --silent > /dev/null"
            (buffer-file-name))))

(defun Rmarkdown-compile-verbose ()
  (interactive)
  (shell-command
    (format "echo 'rmarkdown::render(\"%s\", \"all\")' | R --no-save --silent"
            (buffer-file-name))))
