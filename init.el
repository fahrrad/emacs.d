;; Loader - set home dir
(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         (concat user-emacs-directory "elisp/" ))
        ((boundp 'user-init-directory)
         (concat user-init-directory "elisp/" ))
        (t "~/.emacs.d/")))


(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

(load-user-file "local.el")

;;-------------------------------------------------------------
;;-- Packages
;; ------------------------------------------------------------
;; Packages
(require 'package)
(package-initialize)
(setq my-packages '(paredit
		    ;; cider installed via git
		    cider
		    clojure-mode ;; cider depends on these
		    dash ;; cider depends on these
		    pkg-info ;; cider depends on these
		      color-theme
		      ;;icicles
		      rainbow-delimiters))

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ;;("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/"))
;;(add-to-list 'package-archives
;;             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(setq package-enable-at-startup t)
;; See if some packages needs reinstalling
;; (if (some (lambda (x) (not (package-installed-p x))) my-packages))
(when (not package-archive-contents) (package-refresh-contents))

(dolist (p my-packages)
   (when (not (package-installed-p p))
     (package-install p)))





;; ------------------------------------------------------------
;; -- CONFIG MODES
;; ------------------------------------------------------------
;;; turn on syntax hilighting
(global-font-lock-mode 1)

;; Expand region
(require 'expand-region)
(global-set-key (kbd "C-@") 'er/expand-region)

;; emacs and Python
(elpy-enable)

;; to many rope bugs...
(setq elpy-rpc-backend "jedi")

;; projectile
(projectile-global-mode)

;; flx-ido
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; sqlplus mode
(require 'sqlplus)

;; ORG mode
(setq org-directory "~/host/archive/org")

;;; Global keys for org mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)


;; capture mode
(setq org-default-notes-file (concat org-directory "/notes.org"))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline (concat org-directory "/wardgtd.org") "Tasks")
	 "* TODO %?\n  %i")))

;; Add this file to the agenda
(setq org-agenda-files "")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(org-agenda-files (quote ("~/host/archive/org/wardgtd.org")))
 '(python-shell-exec-path (quote ("C:/Python33")))
 '(sqlplus-command "sqlplus64")
 '(sqlplus-html-output-header "Fri Jan 15 13:43:37 2016<br><br>")
 '(w32-symlinks-handle-shortcuts t))

;; Markdown Mode
(add-to-list 'load-path "~/.emacs.d/elisp/ext/markdown-mode")
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(add-hook 'slime-mode-hook 'show-paren-mode)
(add-hook 'slime-mode-hook 'paredit-mode)
;;;; (load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq slime-contribs '(slime-fancy))

;; CIDER
(add-to-list 'load-path "~/.emacs.d/elisp/ext/cider")
(require 'cider)
(add-hook 'cider-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'show-paren-mode)
(add-hook 'cider-repl-mode-hook 'show-paren-mode)
(add-hook 'cider-repl-mode-hook 'subword-mode)
;; (add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
;; (add-hook 'cider-repl-mode-hook 'company-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
;; (add-hook 'cider-repl-mode-hook 'company-mode)


;; Windows shortcuts support
;; (add-to-list 'load-path "~/.emacs.d/elisp/ext")
;; (require 'w32-symlinks)

;; color-scheme
(require 'color-theme)
(require 'color-theme-solarized)
(color-theme-initialize)
(color-theme-solarized-light)

;; No toolbar please
(tool-bar-mode -1)
;; No menubar please
(menu-bar-mode -1)
(scroll-bar-mode -1)
 
;; ------------------------------------------------------------------------
;; Keyboard Shortcuts  
;; ------------------------------------------------------------------------

;; Run current file
(global-set-key (kbd "<f11>") 'run-current-file)
(global-set-key (kbd "C-c s") 'slime-selector)


;; Winner mode saves the configuration of windws. I can toggle previous 
;; configurations with C-c left|right
(when (fboundp 'winner-mode)
	       (winner-mode 1))


;; This cool mode lets met jump buffers with shift-arrows!
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; function to add dir to execute path
(defun add-path-to-exec-dir (dir)
  "Adds the dirs to the exec-path and PATH env var"
  (push dir exec-path)
  (setenv "PATH"
          (concat (getenv "PATH") ";"
                  dir)))

(setq exec-dirs '("c:/Users/coessewa/apps" 
                  "c:/Users/coessewa/apps/graphviz/bin/"
		  "C:/Program Files (x86)/clisp-2.49"
		  "C:/Program Files/Haskell Platform/2014.2.0.0/bin"
		  "C:/Program Files (x86)/GnuWin32/bin"
		  "C:/Program Files (x86)/svn-win32-1.8.10/bin"))

(mapc 'add-path-to-exec-dir exec-dirs )

;; Bypass proxy for following hosts
(setenv "http_no_proxy" "*.cropdesign.local")
(setenv "no_proxy" (getenv "http_no_proxy"))

(defmacro after (mode &rest body)
  `(eval-after-load ,mode
     '(progn ,@body)))

(after 'auto-complete
       (add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
       (setq ac-use-menu-map t)
       (define-key ac-menu-map "\C-n" 'ac-next)
       (define-key ac-menu-map "\C-p" 'ac-previous))

(after 'auto-complete-autoloads
       (autoload 'auto-complete-mode "auto-complete" "enable auto-complete-mode" t nil)
       (add-hook 'python-mode-hook
                 (lambda ()
                   (require 'auto-complete-config)
                   (add-to-list 'ac-sources 'ac-source-ropemacs)
                   (auto-complete-mode))))

(setenv "PYTHONUNBUFFERED" "x") ;; Solves an issue with Python 3 on
;; windows, where the output is lagging behind...

(setq inhibit-startup-message t)


;; Done, start listening
(server-start)

(require 'virtualenvwrapper)
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(setq venv-location "/home/ward/.virtualenvs/")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(find-file "~/.emacs.d/init.el")

;; project specific settings
(load-user-file "project.el")
