;; Packages
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                          ("org" . "https://orgmode.org/elpa/")
			  ("elpa" . "https://elpa.gnu.org/packages/"))) 

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)
(setq use-package-always-ensure t)

;; User interface
(setq inhibit-startup-message t) ;; Disable startup message
(setq visible-bell t)  ;; Disables the bell and inverts top and bottom of screen instead

(scroll-bar-mode -1)   ;; Disable scroll bar
(menu-bar-mode -1)     ;; Disable menu bar
(tool-bar-mode -1)     ;; Disable tool bar
(tooltip-mode -1)      ;; Disable tooltips
(set-fringe-mode 10)   ;; Give some breathing room (spacing?)



;; Theme
(use-package doom-themes)
(load-theme 'doom-laserwave t)



;; User Experience
(setq default-directory "~/Github")
(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ;; Make ESC quit prompts
(cua-mode 1)  ;; Enable CUA mode (natural copy, paste...)

(column-number-mode)
(global-display-line-numbers-mode t)
;; Disable line number for some modes
(dolist ( mode '(org-mode-hook
		 term-mode-hook
		 shell-mode-hook
		 eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.1))



;; Autocompletion with Ivy
(use-package ivy
  :diminish
  :bind (("C-f" . swiper)
	 ("C-r" . eval-buffer)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil))  ; Don't start searches with ^
	 
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))


;; Doom modeline configuration
(use-package all-the-icons)
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 35)))

;; Projectile

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)

  :init
  (when (file-directory-p "~/Github")
    (setq projectile-project-search-path '("~/Github")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))


;; Magit

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))


(use-package general)

;; French keeb layout
(general-define-key
 "C-é" 'split-window-vertically
 "C-\"" 'split-window-horizontally
 "C-à" 'delete-window)

;; Move window
(general-define-key
 "M-<up>" 'windmove-up
 "M-<right>" 'windmove-right
 "M-<down>" 'windmove-down
 "M-<left>" 'windmove-left)


