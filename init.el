(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(ace-jump-mode
                      ag
                      autopair
                      cider
                      company
                      clj-refactor
                      clojure-mode
                      elisp-slime-nav
                      evil
                      evil-exchange
                      evil-iedit-state
                      evil-leader
                      evil-nerd-commenter
                      evil-numbers
                      evil-surround
                      exec-path-from-shell
                      flx-ido
                      jedi
                      geiser
                      ggtags
                      helm
                      helm-gtags
                      helm-swoop
                      ido-vertical-mode
                      iedit
                      magit
                      monokai-theme
                      org
                      paredit
                      projectile
                      rainbow-delimiters
                      rust-mode
                      smex
                      solarized-theme
                      yasnippet
                      zenburn-theme)
  "A list of packages to ensure are installed at launch")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'extensions)

(evil-mode 1)
(global-evil-surround-mode)
(global-evil-leader-mode)
(evil-exchange-install)

(global-company-mode)
(setq company-idle-delay 0)

(smex-initialize)

(projectile-global-mode 1)
(setq projectile-indexing-method 'alien)

(ido-mode 1)
(ido-everywhere t)
(ido-vertical-mode)
(flx-ido-mode)

(setq compilation-scroll-output t)
(setq compilation-ask-about-save nil)

(setq helm-exit-idle-delay 0)
(setq helm-buffers-fuzzy-matching t)
(setq helm-semantic-fuzzy-match t)
(setq helm-M-x-fuzzy-match t)
(setq helm-exit-idle-delay 0)

(setq-default fill-column 120)

(autopair-global-mode)

(setq jedi:complete-on-dot t)

(global-ede-mode t)

(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
(ad-activate 'split-window-horizontally)

(setq ring-bell-function 'ignore)

(setq inhibit-startup-message t)

(show-paren-mode t)

(global-hl-line-mode)

(set-default 'truncate-lines t)

(setq-default indent-tabs-mode nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(recentf-mode 1)

(setq backup-directory-alist '(("." . "~/.emacs.d/backups" )))

(global-auto-revert-mode t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(load-theme 'monokai t)

(set-default-font "Consolas-10")
(add-to-list 'default-frame-alist '(font . "-outline-Consolas-normal-normal-normal-mono-13-*-*-*-c-*-iso8859-1"))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (setq mac-command-modifier 'control)
  (setq mac-option-modifier 'meta))

(define-key key-translation-map [?\C-h] [?\C-?])

(global-set-key (kbd "<f1>") 'help-command)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-k") 'kill-this-buffer)
(global-set-key (kbd "C-j") 'newline-and-indent)

(global-set-key [f12] 'stro/locate-current-file-in-explorer)

(define-key company-active-map (kbd "<tab>") 'company-complete)

(define-key evil-normal-state-map (kbd "C-;") 'iedit-mode)
(define-key evil-normal-state-map (kbd "M-0") 'delete-window)
(define-key evil-normal-state-map (kbd "M-1") 'delete-other-windows)
(define-key evil-normal-state-map (kbd "M-2") 'split-window-vertically)
(define-key evil-normal-state-map (kbd "M-3") 'split-window-horizontally)

(define-key evil-visual-state-map (kbd "M-q") 'fill-region)
(define-key evil-visual-state-map (kbd "x") 'er/expand-region)
(define-key evil-visual-state-map (kbd "X") 'er/contract-region)
(evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)

(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "<SPC>" 'ace-jump-word-mode
  "TAB" 'stro/alternate-buffer
  "/" 'projectile-ag
  ";" 'evilnc-comment-or-uncomment-lines
  "ase" 'eshell
  "aP" 'package-list-packages
  "cc" 'projectile-compile-project
  "d" 'dired-jump
  "e" 'eval-last-sexp
  "fed" (lambda () (interactive) (find-file-existing "~/.emacs.d/init.el"))
  "ff" 'ido-find-file
  "fs" 'save-buffer
  "g" 'magit-status
  "o" 'helm-mini
  "pd" 'projectile-dired
  "pf" 'projectile-find-file
  "ps" 'projectile-switch-project
  "sa" 'ag
  "sl" 'helm-semantic-or-imenu
  "ss" 'helm-swoop
  (kbd "C-s s") 'helm-multi-swoop-all
  "X" 'delete-trailing-whitespace
  "x" 'smex)

(evil-leader/set-key-for-mode 'clojure-mode
  "cj" 'cider-jack-in
  "e" 'cider-eval-last-sexp
  "cb" 'cider-eval-buffer
  "k" 'cider-jump-to-var
  "t" 'cider-jump-back)

(evil-leader/set-key-for-mode 'c++-mode
  "k" 'helm-gtags-dwim
  "t" 'helm-gtags-pop-stack
  (kbd "C-k") 'helm-gtags-find-tag-other-window
  "K" 'stro/semantic-goto-definition
  "T" 'stro/semantic-pop-tag-mark)

(evil-leader/set-key-for-mode 'c-mode
  "k" 'helm-gtags-dwim
  "t" 'helm-gtags-pop-stack
  (kbd "C-k") 'helm-gtags-find-tag-other-window
  "K" 'semantic-goto-definition
  "T" 'semantic-pop-tag-mark)

(evil-leader/set-key-for-mode 'emacs-lisp-mode
  "k" 'elisp-slime-nav-find-elisp-thing-at-point
  "t" 'pop-tag-mark)

(evil-leader/set-key-for-mode 'python-mode
  "k" 'jedi:goto-definition
  "t" 'jedi:goto-definition-pop-marker)

(add-hook 'prog-mode-hook
          (lambda ()
            (hs-minor-mode)
            (rainbow-delimiters-mode)))

(add-hook 'c-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)
            (setq tab-width 4)
            (setq c-basic-offset 4)
            (setq evil-shift-width c-basic-offset)
            (semantic-mode)
            (setq-local company-backends '(company-gtags company-dabbrev-code))))

(add-hook 'c++-mode-hook
          (lambda ()
            (semantic-mode)
            (setq-local company-backends '(company-gtags company-dabbrev-code))))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (elisp-slime-nav-mode 1)
            (paredit-mode 1)))

(add-hook 'clojure-mode
          (lambda ()
            (paredit-mode 1)
            (cider-mode 1)
            (clj-refactor-mode 1)))

(add-hook 'python-mode-hook
          (lambda ()
            (jedi:setup)))

;; Custom set variables
