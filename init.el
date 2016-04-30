(require 'package)
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(setq gc-cons-threshold 100000000)
(setq inhibit-startup-message t)

(defalias 'yes-or-no-p 'y-or-n-p)

(defconst my-packages '(ace-window
                        adaptive-wrap
                        ag
                        anaconda-mode
                        auctex
                        cider
                        company
                        company-jedi
                        counsel
                        delight
                        dtrt-indent
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
                        helm-gtags
                        helm-swoop
                        iedit
                        ivy
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
                        quickrun
                        yaml-mode
                        yasnippet
                        wgrep
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

;; turn of GUI stuff
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)

(setq backup-directory-alist '(("." . "~/.emacs.d/backups" )))

;; evil
(evil-mode 1)
(global-evil-surround-mode)
(global-evil-leader-mode)
(global-evil-matchit-mode)
(evil-exchange-install)
(evil-magit-init)
(eval-after-load "evil-maps"
  (dolist (map '(evil-motion-state-map
                 evil-insert-state-map
                 evil-emacs-state-map))
    (define-key (eval map) "\C-w" nil)))
(require 'evil-nerd-commenter)

;; ace window
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

;; company
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(delete 'company-semantic company-backends)
;;(define-key c-mode-base-map [(tab)] 'company-complete)
(setq company-minimum-prefix-length 2)
(setq company-idle-delay 0.1)
(setq company-dabbrev-downcase nil)

;; ignore case for completion
(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; yas
(yas-global-mode)
(yas-reload-all)

;; projectile
(projectile-global-mode 1)
(setq projectile-completion-system 'ivy)

;; ivy
(setq ivy-height 20)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(advice-add
 'swiper--action
 :after
 (defun move-to-match-beginning* (_)
   (goto-char (match-beginning 0))))

;; compilation
(setq compilation-scroll-output t)
(setq compilation-ask-about-save nil)

;; tex
(setq-default TeX-engine 'xetex)
(setq-default TeX-PDF-mode t)

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

;; theme
(load-theme 'leuven t)

;; font
(set-default-font "Inconsolata-16")

;; use command as meta under OS X
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none))

;; keybindings
(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-s" 'counsel-grep-or-swiper)
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
(global-set-key (kbd "C-l") 'ace-delete-window)
(global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)
(global-set-key (kbd "M-k") 'kill-this-buffer)
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-3") 'split-window-horizontally)

(define-key company-active-map (kbd "<tab>") 'company-complete)

(define-key evil-visual-state-map (kbd "M-q") 'fill-region)
(define-key evil-visual-state-map (kbd "x") 'er/expand-region)
(define-key evil-visual-state-map (kbd "X") 'er/contract-region)
(define-key evil-insert-state-map (kbd "C-j") 'newline-and-indent)
(evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)

(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "+" 'text-scale-increase
  "-" 'text-scale-decrease
  "/" 'counsel-git-grep
  ":" 'evilnc-comment-or-uncomment-lines
  ";" 'evilnc-comment-operator
  "." 'ffap
  "<SPC>" 'avy-goto-word-1
  "TAB" 'alternate-buffer
  "A" 'package-list-packages
  "C" 'helm-gtags-create-tags
  "W" 'helm-multi-swoop-all
  "X" 'delete-trailing-whitespace
  "F" 'open-finder
  "Q" 'quickrun
  "a" 'ace-window
  "b" 'evil-scroll-page-up
  "c" 'projectile-compile-project
  "d" 'dired-jump
  "e" 'eval-last-sexp
  "f" 'counsel-find-file
  "g" 'magit-status
  "h" 'help-command
  "j" 'counsel-git
  "l" 'counsel-imenu
  "m" 'mu4e
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
(setq jedi:complete-on-dot t)

(add-hook 'prog-mode-hook
          (lambda ()
            (interactive)
            (hs-minor-mode)
            (rainbow-delimiters-mode)
            (ws-butler-mode)
            (clean-aindent-mode)))

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

(defun prelude-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.
If ARG is not nil or 1, move forward ARG - 1 lines first. If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; Custom set variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
