(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

(setq gc-cons-threshold 100000000)
(setq inhibit-startup-message t)

(defalias 'yes-or-no-p 'y-or-n-p)

(defconst my-packages '(ace-jump-mode
                        ag
                        anaconda-mode
                        auctex
                        cider
                        company
                        company-jedi
                        counsel
                        elisp-slime-nav
                        evil
                        evil-exchange
                        evil-iedit-state
                        evil-leader
                        evil-matchit
                        evil-magit
                        evil-mc
                        evil-nerd-commenter
                        evil-numbers
                        evil-surround
                        exec-path-from-shell
                        expand-region
                        function-args
                        iedit
                        jedi
                        geiser
                        ggtags
                        helm
                        helm-swoop
                        iedit
                        julia-mode
                        julia-shell
                        magit
                        markdown-mode
                        monokai-theme
                        org
                        projectile
                        rainbow-delimiters
                        rust-mode
                        smartparens
                        swiper
                        solarized-theme
                        tao-theme
                        yaml-mode
                        yasnippet
                        ws-butler
                        zenburn-theme)
  "A list of packages to ensure are installed at launch")
(defun install-packages ()
  "Install all required packages."
  (interactive)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package my-packages)
    (unless (package-installed-p package)
      (package-install package))))

(install-packages)

(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'extensions)

(setq backup-directory-alist '(("." . "~/.emacs.d/backups" )))

(evil-mode 1)
(global-evil-surround-mode)
(global-evil-leader-mode)
(global-evil-matchit-mode)
(global-evil-mc-mode)
(evil-exchange-install)
(evil-magit-init)
(eval-after-load "evil-maps"
  (dolist (map '(evil-motion-state-map
                 evil-insert-state-map
                 evil-emacs-state-map))
    (define-key (eval map) "\C-w" nil)))
(require 'evil-nerd-commenter)

(blink-cursor-mode 0)

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(delete 'company-semantic company-backends)
;;(define-key c-mode-base-map [(tab)] 'company-complete)
;; (setq company-minimum-prefix-length 2)
;; (setq company-idle-delay 0)
;; (setq company-dabbrev-downcase nil)

(setq org-src-fontify-natively t)

(setq require-final-newline t)

(yas-global-mode)
(yas-reload-all)

(projectile-global-mode 1)
(setq projectile-completion-system 'ivy)

(setq ivy-height 20)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(advice-add
 'swiper--action
 :after
 (defun move-to-match-beginning* (_)
   (goto-char (match-beginning 0))))

(setq compilation-scroll-output t)
(setq compilation-ask-about-save nil)

(setq-default TeX-engine 'xetex)
(setq-default TeX-PDF-mode t)

(setq-default fill-column 80)

(setq jedi:complete-on-dot t)

(setq dired-dwim-target t)

(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)

(setq ring-bell-function 'ignore)

(show-paren-mode t)

(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(sp-use-paredit-bindings)

(global-hl-line-mode)

(set-default 'truncate-lines t)

(setq-default indent-tabs-mode nil)

(recentf-mode 1)

(global-auto-revert-mode t)

(load-theme 'tao-yang t)
(setq evil-visual-state-cursor '(box "salmon"))
(setq evil-normal-state-cursor '(box "black"))

(set-default-font "Menlo-12")

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none))

(setq scheme-program-name "csi -:c")

(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-7") 'swiper-mc)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-c w") 'whitespace-mode)
(global-set-key (kbd "C-a") 'prelude-move-beginning-of-line)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-j") 'newline-and-indent)
(global-set-key (kbd "C-;") 'iedit-mode)
(global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)
(global-set-key (kbd "M-l") 'other-window)
(global-set-key (kbd "M-k") 'kill-this-buffer)
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-3") 'split-window-horizontally)

(define-key company-active-map (kbd "<tab>") 'company-complete)

(define-key evil-visual-state-map (kbd "M-q") 'fill-region)
(define-key evil-visual-state-map (kbd "x") 'er/expand-region)
(define-key evil-visual-state-map (kbd "X") 'er/contract-region)
(evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)

(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "+" 'text-scale-increase
  "-" 'text-scale-decrease
  "/" 'counsel-git-grep
  ":" 'evilnc-comment-or-uncomment-lines
  ";" 'evilnc-comment-operator
  "<SPC>" 'avy-goto-word-1
  "TAB" 'alternate-buffer
  "A" 'package-list-packages
  "W" 'helm-multi-swoop-all
  "X" 'delete-trailing-whitespace
  "b" 'evil-scroll-page-up
  "c" 'projectile-compile-project
  "d" 'dired-jump
  "e" 'eval-last-sexp
  "f" 'counsel-find-file
  "g" 'magit-status
  "h" 'help-command
  "j" 'counsel-git
  "l" 'counsel-imenu
  "o" 'ivy-switch-buffer
  "p" 'projectile-switch-project
  "q" (lambda () (interactive) (find-file-existing "~/.emacs.d/init.el"))
  "s" 'save-buffer
  "v" 'evil-scroll-page-down
  "v" 'toggle-truncate-lines
  "w" 'balance-windows
  "x" 'counsel-M-x
  "z" 'eshell
  )

(evil-leader/set-key-for-mode 'c++-mode
  "k" 'helm-gtags-dwim
  "t" 'helm-gtags-pop-stack
  (kbd "C-k") 'helm-gtags-find-tag-other-window
  "K" 'semantic-goto-definition
  "T" 'semantic-pop-tag-mark)

(evil-leader/set-key-for-mode 'c-mode
  "k" 'helm-gtags-dwim
  "t" 'helm-gtags-pop-stack
  (kbd "C-k") 'helm-gtags-find-tag-other-window
  "K" 'semantic-goto-definition
  "T" 'semantic-pop-tag-mark
  "u" (lambda () (interactive) (let ((current-prefix-arg '(4))) (call-interactively 'helm-gtags-update-tags))))

(evil-leader/set-key-for-mode 'emacs-lisp-mode
  "k" 'elisp-slime-nav-find-elisp-thing-at-point
  "K" 'elisp-slime-nav-describe-elisp-thing-at-point
  "t" 'pop-tag-mark)

(evil-leader/set-key-for-mode 'python-mode
  "k" 'jedi:goto-definition
  "t" 'jedi:goto-definition-pop-marker)

(add-hook 'prog-mode-hook
          (lambda ()
            (interactive)
            (hs-minor-mode)
            (rainbow-delimiters-mode)
            (ws-butler-mode)))

(add-hook 'c-mode-common-hook
          (lambda ()
            (setq indent-tabs-mode t)
            (setq tab-width 4)
            (setq c-basic-offset 4)
            (setq evil-shift-width c-basic-offset)
            (c-set-offset 'arglist-intro '+)
            (c-set-offset 'arglist-cont-nonempty '+)
            (c-set-offset 'case-label 0)
            (c-set-offset 'substatement-open 0)
            (c-set-offset 'brace-list-open 0)
            (electric-pair-mode)
            (ws-butler-mode)
            (yas-minor-mode)
            (visual-line-mode)
            (adaptive-wrap-prefix-mode)
            (add-hook 'after-save-hook 'helm-gtags-update-tags nil 'local)
            (setq-local company-backends '(company-gtags company-dabbrev-code))))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (elisp-slime-nav-mode 1)))

(add-hook 'python-mode-hook
          (lambda ()
            (jedi:setup)))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook
          (lambda ()
            (setq js2-basic-offset 2)
            (setq-local evil-shift-width js2-basic-offset)
            (setq js2-strict-trailing-comma-warning nil)
            (tern-mode)))

;; Custom set variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-type-face ((t (:foreground "#161616" :underline nil)))))
