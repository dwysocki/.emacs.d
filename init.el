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
;; -- global keybindings --
;;

;; switch focus to the next window
(global-set-key (kbd "C-;")
  'other-window)
;; switch focus to the previous window
(global-set-key (kbd "C-:")
  'prev-window)
;; split the focused window into 4
(global-set-key (kbd "C-x 4 4")
  'split-window-4-way)



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

;; load AucTeX
(use-package tex-site
  :ensure auctex)

;; load clojure mode, for hacking at clojure code
(use-package clojure-mode
  :ensure t)

;; load cider, for interactive clojure hacking
(use-package cider
  :ensure t)

;; load gitignore mode, for editing .gitignore files
(use-package gitignore-mode
  :ensure t)

;; load magit, the Emacs TUI for Git
(use-package magit
  :ensure t
  :config
  ;; hide magit warnings up to 1.4.0
  (setq magit-last-seen-setup-instructions "1.4.0"))

;; load markdown mode
(use-package markdown-mode
  :ensure t
  :init
  (add-hook 'markdown-mode-hook
    (lambda ()
      ;; add keyboard shortcuts for compiling
      (define-key markdown-mode-map
        (kbd "C-c C-c C-c") 'Rmarkdown-compile-silent)
      (define-key markdown-mode-map
        (kbd "C-c C-c C-v") 'Rmarkdown-compile-verbose))))

;; load paredit mode, which keeps parentheses balanced, and allows for easy
;; manipulation of S-expressions
(use-package paredit
  :ensure t
  :init
  (autoload 'enable-paredit-mode "paredit"
    "Turn on pseudo-structural editing of Lisp code." t)
  ;; add hook for all lisp dialect modes
  (dolist (hook '(clojure-mode-hook
                  emacs-lisp-mode-hook
                  eval-expression-minibuffer-setup-hook
                  ielm-mode-hook
                  lisp-mode-hook
                  lisp-interaction-mode-hook
                  scheme-mode-hook))
    (add-hook hook #'enable-paredit-mode)))

;; load python mode, for hacking at python code
(use-package python-mode
  :ensure t
  :init
  (add-hook 'python-mode-hook
    ;; turn on whitespace-mode
    #'enable-whitespace-mode))


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

;; set up options for whitespace mode
(setq whitespace-style '(face empty tabs lines-tail trailing))

(defun enable-whitespace-mode ()
  "Enables whitespace-mode."
  (whitespace-mode t))

;; enable whitespace-mode only in certain modes
(dolist (hook '(c-mode-hook
                clojure-mode-hook
                java-mode-hook
                lisp-mode-hook
                lisp-interaction-mode-hook
                python-mode-hook))
  (add-hook hook #'enable-whitespace-mode))




(defun prev-window (&optional (count 1))
  "Switches focus to the previous window. Opposite of `other-window'."
  (interactive)
  (other-window (- count)))

(defun split-window-4-way ()
  "Splits the current window into 4."
  (interactive)
  (let (;; remember the window we started at, so we can return to it
        (starting-window (selected-window)))
    ;; split the original window into top & bottom halves
    (split-window-below)
    ;; split the top half into a left & right half
    (split-window-right)
    ;; switch focus to the bottom half
    (other-window 2)
    ;; split the bottom half into a left & right half
    ;; we have now split the initial window into 4
    (split-window-right)
    ;; ensure that the 4 windows are the same size
    (balance-windows starting-window)
    ;; return to initial window
    (select-window starting-window)))

;;
;; -- Rmarkdown
;;

(defun Rmarkdown-compile-silent ()
  "Compile file being visited with Rmarkdown, suppressing its output."
  (interactive)
  (shell-command
    (format (concat "echo 'rmarkdown::render(\"%s\", \"all\")'"
                    "|"
                    "R --no-save --silent > /dev/null")
            (buffer-file-name))))

(defun Rmarkdown-compile-verbose ()
  "Compile file being visited with Rmarkdown, writing its output to the
   minibuffer."
  (interactive)
  (shell-command
    (format (concat "echo 'rmarkdown::render(\"%s\", \"all\")'"
                    "|"
                    "R --no-save --silent")
            (buffer-file-name))))

;;
;; -- OS X specific --
;;

(when (eq system-type 'darwin)
  ;; provide the path to ispell
  (setf ispell-program-name "/usr/local/bin/ispell")
  ;; add TeXbin to the PATH environment variable
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/Library/TeX/texbin/"))
  ;; add /usr/local/bin and /Library/TeX/texbin to the exec-path variable
  (setf exec-path (append exec-path '("/usr/local/bin" "/Library/TeX/texbin/"))))




