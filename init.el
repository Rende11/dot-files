;; Global Emacs config
(tool-bar-mode -1)
(setq ring-tell-function 'ignore)
(menu-bar-mode -1)

;; Packages: Melpa Elpla
(require 'package)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
(add-to-list 'package-archives (cons "gnu" "https://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

(setq use-package-always-ensure t)


;; - Evil mode
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil) 
(evil-mode 1)


;; Evil escape
(use-package evil
 :ensure t
 :config
 (evil-mode 1)
 (define-key evil-insert-state-map "§" 'evil-normal-state))

;; Custom Railscasts
;;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;;(load-theme 'railscasts-reloaded-custom t nil)

;; Custom packages
;;(add-to-list 'load-path "~/.emacs.d/escreen")

;;(exec-path-from-shell-initialize)
(setenv "PGPASSWORD" "postgres")

(define-key evil-normal-state-map (kbd "RET") 'run-sql)

;;(add-to-list 'load-path "~/.emacs.d/packages/psql")
;;(load "psql")

;; Modules
;; - Show parent mode
(show-paren-mode 1)
;; - Popwin

;;(require 'popwin)


;; - Helm 
(use-package helm)
(recentf-mode 1)
(setq-default recent-save-file "~/.emacs.d/recentf")  
(run-at-time nil (* 5 60) 'recentf-save-list)
(setq recentf-max-saved-items 2500)

(setq-default helm-M-x-fuzzy-match t)
(setq-default helm-buffers-fuzzy-matching t)
(setq-default helm-recentf-fuzzy-match t)

(add-to-list
 'display-buffer-alist
 `(,(rx bos "*helm" (* not-newline) "*" eos)
   (display-buffer-in-side-window)
   (inhibit-same-window . t)
   (window-height . 0.3)))

;; - Company
;;(require 'company)
;;(company-mode 1)
;; - SmartParens 
;;(require 'smartparens)
;;(smartparens-mode 1)
;; - ParEdit
;;(require 'paredit)
;;(paredit-mode 1)
;;(projectile-mode +1)
;; - Dashboard
;;(require 'dashboard)
;;(dashboard-setup-startup-hook)
;;(setq dashboard-startup-banner "~/splogo.png")
;;(setq dashboard-items '((projects . 5)
;;			(recents  . 5)))
;; - Cider
;;(global-eldoc-mode 1)
(setq-default cider-repl-display-help-banner nil)
;;(add-hook 'cider-repl-mode-hook #'company-mode)
;;(add-hook 'cider-mode-hook #'company-mode)

;;(add-hook 'after-init-hook 'global-company-mode)
;;(add-hook 'clojure-mode-hook (lambda () (paredit-mode 1) ))
;;(add-hook 'cider-repl-mode-hook (lambda () (paredit-mode 1) ))
;;(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode 1)))
;;--------------------------------------------
;; Bindings 
;;--------------------------------------------

;; Prefix <SPC>
(define-prefix-command 'my-macs)
(define-key evil-normal-state-map (kbd "SPC") 'my-macs)
;; Prefix <,>
(define-prefix-command 'my-cider)
(define-key evil-normal-state-map (kbd ",") 'my-cider)


;; Escreen
;;(load "escreen")
;;(escreen-install)
;;(define-key my-macs (kbd "l") 'escreen-prefix) 


(defun connect-to-alkona-repl ()
  (interactive)
  (cider-connect '(:host "localhost" :port "55555"))
  (message "Connected"))

;; Cider
(setq cider-prompt-for-symbol nil)
(setq cider-eldoc-display-for-symbol-at-point t)
(define-key my-cider (kbd "e b") 'cider-eval-buffer)
(define-key my-cider (kbd "t n") 'cider-test-run-ns-tests)
(define-key my-cider (kbd "s c") 'cider-connect)
(define-key my-cider (kbd "c c") 'cider-repl-clear-buffer)
(define-key my-cider (kbd "g g") 'cider-find-var )
(define-key my-cider (kbd "d d") 'cider-doc )
(define-key my-cider (kbd "d c") 'cider-clojuredocs )
(define-key my-cider (kbd "c a") 'connect-to-alkona-repl )
(define-key my-cider (kbd "c a") '(cider-connect '(:host "localhost" :port "55555")) )

;; SmartParens
(define-key my-macs (kbd "k s") 'sp-forward-slurp-sexp )
(define-key my-macs (kbd "k w") 'sp-wrap-round )
(define-key my-macs (kbd "k W") 'sp-unwrap-sexp )
(define-key my-macs (kbd "k e") 'sp-wrap-square )
;;(define-key my-macs (kbd "k q") 'sp-wrap- )

;; Evil
(define-key my-macs (kbd "w") 'evil-window-map) 

;; Magit
(define-key my-macs (kbd "m") #'magit)

;; Helm
(define-key evil-visual-state-map (kbd "SPC SPC") 'helm-M-x)
(define-key my-macs (kbd "SPC") 'helm-M-x)
(define-key my-macs (kbd "f f") 'helm-find-files)
(define-key my-macs (kbd "b b") 'helm-mini)

;;(define-key helm-read-file-map  "\M-DEL" #'backward-kill-word)

(add-hook
 'helm-after-initialize-hook
 (lambda()
   (define-key helm-buffer-map (kbd "§") 'helm-keyboard-quit)
   ;;(define-key helm-M-x-map (kbd "§") 'helm-keyboard-quit)
   (define-key helm-map (kbd "§") 'helm-keyboard-quit)))
(helm-mode 1)

;; Colors
;;(set-background-color "#252525")
;;(set-face-attribute 'fringe nil
;;		    :foreground (face-foreground 'default)
	;;	    :background (face-background 'default))
;;(set-face-foreground 'vertical-border "#252525")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("6343f4d41b209fe8990e3c5f4d2040b359612ef9cd8682f1e1e2a836beba8107" default)))
 '(helm-M-x-fuzzy-match t t)
 '(helm-buffer-max-length 80)
 '(helm-buffers-fuzzy-matching t)
 '(helm-display-header-line nil)
 '(helm-follow-mode-persistent t)
 '(helm-recentf-fuzzy-match t t)
 '(package-selected-packages
   (quote
    (clj-refactor helm-ag ag exec-path-from-shell popwin evil-magit dashboard auto-complete-auctex auto-complete ivy window-layout nlinum evil-paredit paredit company-web helm-descbinds projectile magit cider clojure-mode 4clojure company evil-escape helm evil)))
 '(scroll-bar-mode (quote right)))


;;(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;; '(default ((t (:inherit nil :stipple nil :background "#252525" :foreground "#E6E1DC" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 135 :width normal :foundry "nil" :family "Iosevka"))))
;; '(alkona-header ((t (:foreground "tan2" :weight bold :height 1.3))))
;; '(evil-ex-search ((t (:background "dark cyan"))))
;; '(evil-ex-substitute-matches ((t (:background "SpringGreen4"))))
;; '(isearch ((t (:background "chocolate1" :foreground "gray100"))))
;; '(mode-line ((t (:background "#505050" :foreground "#E6E1DC"))))
;; '(mode-line-inactive ((t (:background "#353535"))))
;; '(scroll-bar ((t (:background "firebrick2"))))
 ;;'(show-paren-match ((t (:background "red" :weight bold))))
;; '(vertical-border ((t (:foreground "dark gray")))))

(defun simple-mode-line-render (left right)
  (let* ((available-width (- (window-width) (length left) 2)))
    (format (format " %%s %%%ds " available-width) left right)))

(setq-default
 mode-line-format
 '((:eval (simple-mode-line-render
	   (format-mode-line "%f")
	   (format-mode-line "L:%l:%c")))))


(setq explicit-shell-file-name "/bin/bash")
(setq-default explicit-shell-file-name "/bin/bash")
(setq-default shell-file-name "/bin/bash")

;; Global Emacs config 
(toggle-frame-fullscreen)
(scroll-bar-mode -1)
;;(hs-minor-mode 1)
(setq ring-bell-function 'ignore)

;;(setq cider-clojure-cli-parameters "-A:dev:test:nrepl")
;;(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))

(add-hook 'view-mode-hook 'evil-emacs-state)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)

(load-theme 'tsdh-light)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . light))

(set-face-attribute 'default nil :font "Inconsolata LGC 14")
;; (setq default-frame-alist '((font . "Inconsolata LGC 14")))
(setq-default line-spacing 0)
(setq initial-frame-alist '((width . 135) (height . 55)))
(tool-bar-mode -1)

(set-face-background 'show-paren-match "wheat")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)
(show-paren-mode)
(setq org-startup-truncated nil)
(setq column-number-mode t) ;; show columns in addition to rows in mode line
(setq-default frame-title-format "%b (%f)")
(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(setq js-indent-level 2)
(setq css-indent-offset 2)
(setq-default c-basic-offset 2)
(setq c-basic-offset 2)
(setq-default tab-width 2)
(setq-default c-basic-indent 2)
(blink-cursor-mode 0)
(global-visual-line-mode t)

(use-package clojure-mode)
(use-package cider)
(use-package clj-refactor)

(defun my-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1) ; for adding require/use/import statements
    ;; This choice of keybinding leaves cider-macroexpand-1 unbound
    (cljr-add-keybindings-with-prefix "C-c C-m"))
(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
