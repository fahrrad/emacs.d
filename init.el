;; Loader
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

;; Load other files
(load-user-file "functions.el")
(load-user-file "lively.el")


;; Packages
(require 'package)

;; Set proxy
(setq url-using-proxy t)
(setq url-proxy-services  '(("http" . "http-proxy.zx.basf-ag.de:8080")))

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(setq package-enable-at-startup nil)

(unless (package-installed-p 'markdown-mode)
  (package-refresh-contents)
  (package-install 'markdown-mode))

(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))


;; SLIME
(add-to-list 'load-path "~/.emacs.d/elisp/ext/slime")
(require 'slime-autoloads)
(setq slime-contribs '(slime-fancy slime-banner))
(load (expand-file-name "~/quicklisp\\slime-helper.el"))
(setq inferior-lisp-program "clisp")
(slime-setup)
(add-to-list 'slime-mode-hook 'show-paren-mode)
(add-to-list 'slime-mode-hook 'paredit-mode)
(add-hook 'slime-mode-hook
	  (lambda ()
	    (unless (slime-connected-p)
	      (save-excursion 
		(slime)))))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inferior-R-program-name "C:/Program Files/R/R-3.0.3/bin/x64/Rterm.exe")
 '(initial-buffer-choice t)
 '(initial-scratch-message "
")
 '(org-agenda-files (quote ("c:/Users/coessew/org/CD/todo.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;e:/tools/GSTools/gs8.14/fonts")
(setenv "GS_LIB" "C:/Users/coessew/apps/gs9.05/lib")
   (setq ps-lpr-command "C:/Users/coessew/apps/gs9.05/bin/gswin64c.exe")
   (setq ps-lpr-switches '("-q" "-dNOPAUSE" "-dBATCH" "-sDEVICE=mswinpr2"))
   (setq ps-printer-name t)

;; Org Mode
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(require 'org-install)


;; Message hooks 
(add-hook 'message-mode-hook 'orgstruct++-mode 'append)
(add-hook 'message-mode-hook 'turn-on-auto-fill 'append)
(add-hook 'message-mode-hook 'bbdb-define-all-aliases 'append)
(add-hook 'message-mode-hook 'orgtbl-mode 'append)
(add-hook 'message-mode-hook 'turn-on-flyspell 'append)
(add-hook 'message-mode-hook
          '(lambda () (setq fill-column 72))
          'append)
(add-hook 'message-mode-hook
          '(lambda () (local-set-key (kbd "C-c M-o") 'org-mime-htmlize))
          'append)
;; Custom Key Bindings
(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key (kbd "<f5>") 'bh/org-todo)
(global-set-key (kbd "<S-f5>") 'bh/widen)
(global-set-key (kbd "<f7>") 'bh/set-truncate-lines)
(global-set-key (kbd "<f8>") 'org-cycle-agenda-files)
(global-set-key (kbd "<f9> <f9>") 'bh/show-org-agenda)
(global-set-key (kbd "<f9> b") 'bbdb)
(global-set-key (kbd "<f9> c") 'calendar)
(global-set-key (kbd "<f9> f") 'boxquote-insert-file)
(global-set-key (kbd "<f9> g") 'gnus)
(global-set-key (kbd "<f9> h") 'bh/hide-other)
(global-set-key (kbd "<f9> n") 'org-narrow-to-subtree)
(global-set-key (kbd "<f9> w") 'widen)
(global-set-key (kbd "<f9> u") 'bh/narrow-up-one-level)

(global-set-key (kbd "<f9> I") 'bh/punch-in)
(global-set-key (kbd "<f9> O") 'bh/punch-out)

(global-set-key (kbd "<f9> o") 'bh/make-org-scratch)

(global-set-key (kbd "<f9> r") 'boxquote-region)
(global-set-key (kbd "<f9> s") 'bh/switch-to-scratch)

(global-set-key (kbd "<f9> t") 'bh/insert-inactive-timestamp)
(global-set-key (kbd "<f9> T") 'tabify)
(global-set-key (kbd "<f9> U") 'untabify)

(global-set-key (kbd "<f9> v") 'visible-mode)
(global-set-key (kbd "<f9> SPC") 'bh/clock-in-last-task)
(global-set-key (kbd "C-<f9>") 'previous-buffer)
(global-set-key (kbd "M-<f9>") 'org-toggle-inline-images)
(global-set-key (kbd "C-x n r") 'narrow-to-region)
(global-set-key (kbd "C-<f10>") 'next-buffer)
;; will use for run current file 
;; (global-set-key (kbd "<f11>") 'org-clock-goto)
(global-set-key (kbd "C-<f11>") 'org-clock-in)
(global-set-key (kbd "C-s-<f12>") 'bh/save-then-publish)
(global-set-key (kbd "C-M-r") 'org-capture)
(global-set-key (kbd "C-c r") 'org-capture)



(defun bh/hide-other ()
  (interactive)
  (save-excursion
    (org-back-to-heading 'invisible-ok)
    (hide-other)
    (org-cycle)
    (org-cycle)
    (org-cycle)))

(defun bh/set-truncate-lines ()
  "Toggle value of truncate-lines and refresh window display."
  (interactive)
  (setq truncate-lines (not truncate-lines))
  ;; now refresh window display (an idiom from simple.el):
  (save-excursion
    (set-window-start (selected-window)
                      (window-start (selected-window)))))

(defun bh/make-org-scratch ()
  (interactive)
  (find-file "/tmp/publish/scratch.org")
  (gnus-make-directory "/tmp/publish"))

(defun bh/switch-to-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))


(global-set-key (kbd "<f5>") 'bh/org-todo)

(defun bh/org-todo (arg)
  (interactive "p")
  (if (equal arg 4)
      (save-restriction
        (bh/narrow-to-org-subtree)
        (org-show-todo-tree nil))
    (bh/narrow-to-org-subtree)
    (org-show-todo-tree nil)))

(global-set-key (kbd "<S-f5>") 'bh/widen)

(defun bh/widen ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (org-agenda-remove-restriction-lock)
    (widen)
    (org-agenda-remove-restriction-lock)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "W" 'bh/widen))
          'append)

(defun bh/restrict-to-file-or-follow (arg)
  "Set agenda restriction to 'file or with argument invoke follow mode.
I don't use follow mode very often but I restrict to file all the time
so change the default 'F' binding in the agenda to allow both"
  (interactive "p")
  (if (equal arg 4)
      (org-agenda-follow-mode)
    (if (equal major-mode 'org-agenda-mode)
        (bh/set-agenda-restriction-lock 4)
      (widen))))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "F" 'bh/restrict-to-file-or-follow))
          'append)

(defun bh/narrow-to-org-subtree ()
  (widen)
  (org-narrow-to-subtree)
  (save-restriction
    (org-agenda-set-restriction-lock)))

(defun bh/narrow-to-subtree ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (org-with-point-at (org-get-at-bol 'org-hd-marker)
        (bh/narrow-to-org-subtree))
    (bh/narrow-to-org-subtree)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "N" 'bh/narrow-to-subtree))
          'append)

(defun bh/narrow-up-one-org-level ()
  (widen)
  (save-excursion
    (outline-up-heading 1 'invisible-ok)
    (bh/narrow-to-org-subtree)))

(defun bh/get-pom-from-agenda-restriction-or-point ()
  (or (org-get-at-bol 'org-hd-marker)
      (and (marker-position org-agenda-restrict-begin) org-agenda-restrict-begin)
      (and (equal major-mode 'org-mode) (point))
      org-clock-marker))

(defun bh/narrow-up-one-level ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
        (bh/narrow-up-one-org-level))
    (bh/narrow-up-one-org-level)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "U" 'bh/narrow-up-one-level))
          'append)

(defun bh/narrow-to-org-project ()
  (widen)
  (save-excursion
    (bh/find-project-task)
    (bh/narrow-to-org-subtree)))

(defun bh/narrow-to-project ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
        (bh/narrow-to-org-project))
    (bh/narrow-to-org-project)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "P" 'bh/narrow-to-project))
          'append)

(defvar bh/current-view-project nil)

(defun bh/view-next-project ()
  (interactive)
  (unless (marker-position org-agenda-restrict-begin)
    (goto-char (point-min))
    (re-search-forward "^Projects$")
    (setq bh/current-view-project (point)))
  (bh/widen)
  (goto-char bh/current-view-project)
  (forward-visible-line 1)
  (setq bh/current-view-project (point))
  (if (org-get-at-bol 'org-hd-marker)
      (bh/narrow-to-project)
    (message "All projects viewed.")
    (ding)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "V" 'bh/view-next-project))
          'append) 


;; TODO states
(setq org-todo-keywords
      '((sequence "TODO" "WORKING" "|" "DONE" "WAITING FOR")))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t))) ; this line activates dot



;; emacs  groovy
(add-to-list 'load-path "~/sources/emacs/groovy/")
(add-to-list 'load-path "~/sources/emacs/dash.el/")

(add-to-list 'load-path "~/sources/emacs/nrepl/")



;;; turn on syntax hilighting
(global-font-lock-mode 1)

;;; use groovy-mode when file ends in .groovy or has #!/bin/groovy at start
(autoload 'groovy-mode "groovy-mode" "Groovy editing mode." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

;; python-emacs 
;; (load-file "C:/Users/coessew/apps/emacs4python/epy-init.el")
;; clojure 
;; (require 'paredit) if you didn't install it via package.el
(defvar my-packages '(cider
                      color-theme
                      paredit))

(dolist (p my-packages)
   (when (not (package-installed-p p))
     (package-install p)))

(add-hook 'clojure-mode-hook 'paredit-mode)

;; Icicles : very good completion of mini buffers
(unless (package-installed-p 'icicles)
  (package-install 'icicles))

(icy-mode 1)

;; color-scheme
(require 'color-theme)
(color-theme-initialize)
(color-theme-standard)

;; No toolbar please
(tool-bar-mode -1)
;; No menubar please
(menu-bar-mode -1)
 
; +----------------------+
; |  Keyboard Shortcuts  |
; +----------------------+
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

;; reverts files when changed on disc
(global-auto-revert-mode t)

;; 
;; Hippy expand
;; (global-set-key (kbd "M-/") 'hippie-expand)


;; function to add dir to execute path
(defun add-path-to-exec-dir (dir)
  "Adds the dirs to the exec-path and PATH env var"
  (push dir exec-path)
  (setenv "PATH"
          (concat (getenv "PATH") ";"
                  dir)))

(setq exec-dirs '("c:/Users/coessew/apps" "c:/Users/coessew/Envs/emacs/Scripts"
                  "C:/RDE/cygwin_x64/bin" "c:/Users/coessew/Envs/emacs/SCRIPTS"
                  "~/apps/graphviz/bin/"))

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

(after 'auto-complete-config
       (ac-config-default)
       (when (file-exists-p (expand-file-name "/Users/dcurtis/.emacs.d/elisp/Pymacs"))
         (ac-ropemacs-initialize)
         (ac-ropemacs-setup)))

(after 'auto-complete-autoloads
       (autoload 'auto-complete-mode "auto-complete" "enable auto-complete-mode" t nil)
       (add-hook 'python-mode-hook
                 (lambda ()
                   (require 'auto-complete-config)
                   (add-to-list 'ac-sources 'ac-source-ropemacs)
                   (auto-complete-mode))))

(setenv "PYTHONUNBUFFERED" "x") ;; Solves an issue with Python 3 on
;; windows, where the output is lagging behind...

;; Done, start listening
(server-start)



