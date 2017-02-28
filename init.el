(require 'package)
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(setq inhibit-startup-message t)

(defalias 'yes-or-no-p 'y-or-n-p)

;; turn of GUI stuff
(scroll-bar-mode 0)
(tool-bar-mode 0)

(setq backup-directory-alist '(("." . "~/.emacs.d/backups" )))

(setq use-package-always-ensure t)

(use-package evil
  :config
  (evil-mode 1)

  (use-package evil-surround
    :config
    (global-evil-surround-mode)
    (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region))

  (use-package evil-matchit
    :config
    (global-evil-matchit-mode))

  (use-package evil-exchange
    :config
    (evil-exchange-install))

  (use-package evil-magit
    :config
    (evil-magit-init))

  (use-package evil-ediff)
  (use-package evil-iedit-state)

  (use-package evil-leader
    :config
    (global-evil-leader-mode)
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key
      "+" 'text-scale-increase
      "-" 'text-scale-decrease
      "." 'ffap
      "TAB" 'alternate-buffer
      "`" 'ff-find-other-file
      "A" 'package-list-packages
      "X" 'delete-trailing-whitespace
      "F" 'open-finder
      "d" 'dired-jump
      "e" 'eval-last-sexp
      "o" 'help-command
      "q" (lambda () (interactive) (find-file-existing "~/.emacs.d/init.el"))
      "s" 'save-buffer
      "v" 'toggle-truncate-lines
      "w" 'balance-windows
      "z" 'eshell))

  (use-package evil-nerd-commenter
    :bind ("M-;" . evilnc-comment-or-uncomment-lines)
    :config
    (evil-leader/set-key
      ":" 'evilnc-comment-or-uncomment-lines
      "p" 'evilnc-comment-operator))

  (use-package evil-escape
    :config
    (evil-escape-mode)
    (setq-default evil-escape-key-sequence "fj")))

(use-package expand-region
  :bind (:map evil-visual-state-map
              ("x" . er/expand-region)
              ("X" . er/contract-region)))

(use-package ace-window
  :bind ("C-l" . ace-delete-window)
  :init
  (evil-leader/set-key "a" 'ace-window)
  :config
  (setq aw-keys '(?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package company
  :config
  (define-key company-active-map "\C-w" nil)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (add-hook 'prog-mode-hook (lambda () (company-mode))))

;; ignore case for completion
(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

(use-package yasnippet
  :config
  (yas-global-mode))

(use-package projectile
  :config
  (projectile-global-mode 1)
  (setq projectile-completion-system 'ivy)
  (evil-leader/set-key "c" 'projectile-compile-project)
  
  (use-package counsel-projectile
    :config
    (evil-leader/set-key ";" 'counsel-projectile-switch-project)
    (ivy-add-actions
     'counsel-projectile
     '(("/" (lambda (dir)
              (let ((projectile-switch-project-action 'counsel-git-grep))
                (projectile-switch-project-by-name dir arg)))
        "counsel git grep")))))

(use-package hydra)

(use-package ivy
  :bind ("C-c C-r" . ivy-resume)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (evil-leader/set-key
    "i" 'ivy-switch-buffer
    "v" 'ivy-push-view
    "V" 'ivy-pop-view)
  (use-package ivy-hydra))

(use-package avy
  :config
  (evil-leader/set-key "<SPC>" 'avy-goto-word-1))

(use-package swiper
  :init
  (global-unset-key "\M-s")
  :bind ("M-s" . swiper-all)
  :config
  (advice-add
   'swiper--action
   :after
   (defun move-to-match-beginning* (_)
     (goto-char (match-beginning 0)))))

(use-package smex)

(use-package counsel
  :bind (("C-s" . counsel-grep-or-swiper)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-ag)
         ("C-x l" . counsel-locate))
  :init
  (evil-leader/set-key
    "/" 'counsel-git-grep
    "f" 'counsel-find-file
    "j" 'counsel-git
    "l" 'counsel-imenu
    "x" 'counsel-M-x)
  (use-package counsel-gtags
    :config
    (mapcar '(lambda (mode)  
               (evil-leader/set-key-for-mode mode
                 "K" 'counsel-gtags-dwim
                 "T" 'counsel-gtags-pop
                 "C" 'counsel-gtags-create-tags
                 "U" 'counsel-gtags-update-tags))
            '('c-mode 'c++-mode 'objc-mode))))

(use-package tiny
  :config
  (global-set-key (kbd "C-'") 'tiny-expand))

(use-package iedit
  :bind ("C-;" . iedit-mode))

(use-package quickrun
  :config
  (evil-leader/set-key "Q" 'quickrun))

(use-package magit
  :config
  (evil-leader/set-key "g" 'magit-status))

(use-package company-jedi
  :config
  (add-hook 'python-mode-hook (lambda () (add-to-list 'company-backends 'company-jedi)
                                (jedi-mode t)))
  (evil-leader/set-key-for-mode 'python-mode
    "k" 'jedi:goto-definition
    "t" 'jedi:goto-definition-pop-marker))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package julia-mode
  :config
  (use-package julia-shell))

(use-package elisp-slime-nav
  :config
  (add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode 1)))
  (evil-leader/set-key-for-mode 'emacs-lisp-mode
    "k" 'elisp-slime-nav-find-elisp-thing-at-point
    "K" 'elisp-slime-nav-describe-elisp-thing-at-point
    "t" 'pop-tag-mark))

(use-package paredit
  :config
  (add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode))))

(use-package color-theme-sanityinc-tomorrow
  :config
  (load-theme 'sanityinc-tomorrow-eighties t))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook (lambda () (rainbow-delimiters-mode))))

;; (use-package relative-line-numbers
;;   :config
;;   (add-hook 'prog-mode-hook (lambda () (relative-line-numbers-mode))))

(use-package flycheck)

;; (use-package rtags
;;   :config
;;   (rtags-enable-standard-keybindings)
;;   (setq rtags-rc-log-enabled t)
;;   (setq rtags-autostart-diagnostics t)
;;   (rtags-diagnostics)
;;   (setq rtags-completions-enabled t)
;;   (push 'company-rtags company-backends)
;;   (add-hook 'c-mode-common-hook 'rtags-start-process-unless-running)
;;   (mapcar (lambda (mode)
;;             (evil-leader/set-key-for-mode mode
;;               "k" 'rtags-find-symbol-at-point
;;               "t" 'rtags-location-stack-back
;;               "r n" 'rtags-next-match
;;               "r p" 'rtags-previous-match
;;               "r r" 'rtags-find-references-at-point
;;               "r R" 'rtags-rename-symbol))
;;           (list 'c-mode 'c++-mode 'objc-mode)))

;; (use-package irony
;;   :config
;;   (add-hook 'c++-mode-hook 'irony-mode)
;;   (add-hook 'c-mode-hook 'irony-mode)
;;   (add-hook 'objc-mode-hook 'irony-mode)

;;   ;; replace the `completion-at-point' and `complete-symbol' bindings in
;;   ;; irony-mode's buffers by irony-mode's function
;;   (defun my-irony-mode-hook ()
;;     (define-key irony-mode-map [remap completion-at-point]
;;       'irony-completion-at-point-async)
;;     (define-key irony-mode-map [remap complete-symbol]
;;       'irony-completion-at-point-async))
;;   (add-hook 'irony-mode-hook 'my-irony-mode-hook)
;;   (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package markdown-mode)
(use-package org)
(use-package wgrep)
(use-package adaptive-wrap)
(use-package clean-aindent-mode)
(use-package cmake-mode)
(use-package protobuf-mode)

;; compilation
(setq compilation-scroll-output t)
(setq compilation-ask-about-save nil)

;; misc
(blink-cursor-mode 0)
(global-auto-revert-mode t)
(global-hl-line-mode)
(recentf-mode 1)
(set-default 'truncate-lines t)
(setq dired-dwim-target t)
(setq git-commit-finish-query-functions nil)
(setq org-src-fontify-natively t)
(setq require-final-newline t)
(setq ring-bell-function 'ignore)
(setq-default fill-column 80)
(setq-default indent-tabs-mode nil)
(show-paren-mode t)

;; font
(set-default-font "Menlo-12")

;; use command as meta under OS X
(when (memq window-system '(mac ns))
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none))

;; keybindings
(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key (kbd "C-c w") 'whitespace-mode)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-j") 'newline-and-indent)
(global-set-key (kbd "M-k") 'kill-this-buffer)
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-3") 'split-window-horizontally)

(define-key evil-visual-state-map (kbd "M-q") 'fill-region)
(define-key evil-insert-state-map (kbd "C-j") 'newline-and-indent)

(defconst my-cc-style
  '("cc-mode"
    (c-offsets-alist . ((innamespace . [0])))))

(c-add-style "my-cc-mode" my-cc-style)

(add-hook 'prog-mode-hook
          (lambda ()
            (interactive)
            (hs-minor-mode)
            (electric-pair-mode)))

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
            (yas-minor-mode)
            (adaptive-wrap-prefix-mode)
            (cwarn-mode)))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook
          (lambda ()
            (setq js2-basic-offset 2)
            (setq-local evil-shift-width js2-basic-offset)
            (setq js2-strict-trailing-comma-warning nil)
            (tern-mode)))

(defun alternate-buffer ()
  "Switch back and forth between current and last buffer"
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) t)))

(defun open-finder-1 (dir file)
  (let ((script
         (if file
             (concat
              "tell application \"Finder\"\n"
              "    set frontmost to true\n"
              "    make new Finder window to (POSIX file \"" dir "\")\n"
              "    select file \"" file "\"\n"
              "end tell\n")
           (concat
            "tell application \"Finder\"\n"
            "    set frontmost to true\n"
            "    make new Finder window to {path to desktop folder}\n"
            "end tell\n"))))
    (start-process "osascript-getinfo" nil "osascript" "-e" script)))

(defun open-finder ()
  (interactive)
  (let ((path (buffer-file-name))
        dir file)
    (when path
      (setq dir (file-name-directory path))
      (setq file (file-name-nondirectory path)))
    (open-finder-1 dir file)))

(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

;; Custom set variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (function-args linum-relative speed-type counsel-gtags protobuf-mode cmake-mode clean-aindent-mode adaptive-wrap wgrep markdown-mode irony yasnippet use-package tiny smex rtags rainbow-delimiters quickrun paredit julia-shell ivy-hydra iedit helm-swoop helm-gtags flycheck expand-region exec-path-from-shell evil-surround evil-nerd-commenter evil-matchit evil-magit evil-leader evil-exchange evil-escape elisp-slime-nav counsel-projectile company-jedi color-theme-sanityinc-tomorrow ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
