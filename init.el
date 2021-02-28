(add-to-list 'load-path (concat user-emacs-directory "lisp"))

;; Custom

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;; Packages

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(setq package-archive-priorities '(("melpa" . -10)))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package aggressive-indent)
(use-package company
  :config (global-company-mode))
(use-package go-mode)
(use-package indent-tools)
(use-package markdown-mode)
(use-package rust-mode)
(use-package tuareg)
(use-package yaml-mode)

;; Calendar

(require 'calendar)

(calendar-set-date-style 'iso)
(setq calendar-week-start-day 1)

;; Email

(setq user-full-name "Anton Yabchinskiy")

(require 'mu4e)

(setq mu4e-maildir (concat "~/mail/" user-mail-address))

(setq mu4e-compose-signature-auto-include nil)
(setq mu4e-confirm-quit nil)
(setq mu4e-drafts-folder "/Drafts")
(setq mu4e-headers-date-format "%Y-%m-%d")
(setq mu4e-mu-home (expand-file-name "~/mail/.mu"))
(setq mu4e-refile-folder "/Archive")
(setq mu4e-sent-folder "/Sent")
(setq mu4e-trash-folder "/Trash")
(setq mu4e-update-interval 150)
(setq mu4e-view-show-addresses t)

(setq mail-user-agent 'mu4e-user-agent)

(require 'smtpmail)

(setq message-kill-buffer-on-exit t)
(setq message-send-mail-function 'smtpmail-send-it)
(setq smtpmail-smtp-server
      (replace-regexp-in-string "^\\w+@" "mail." user-mail-address))
(setq smtpmail-smtp-service "submission")
(setq smtpmail-smtp-user user-mail-address)
(setq smtpmail-stream-type 'starttls)

(global-set-key (kbd "C-c m") 'mu4e)

;; Evil

(use-package evil
  :config (evil-mode 1)
  )

;; Ido, Smex, avy

(require 'ido)

(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

(use-package smex
  :bind
  ("M-x" . smex)
  ("M-X" . smex-major-mode-commands)
  )

(use-package avy
  :bind ("M-g" . avy-goto-word-1)
  )

;; Buffers

(require 'midnight)
(require 'uniquify)

(midnight-delay-set 'midnight-delay (* 5 60 60))
(setq uniquify-buffer-name-style 'post-forward)

;; EDiff

(require 'ediff)

(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; YASnippet

(use-package yasnippet
  :config (yas-global-mode 1)
  )

;; Org and remember

(require 'org)

(setq org-directory "~/org/")
(setq org-default-notes-file (concat org-directory "notes.org"))
(setq org-clock-idle-time 10)
(setq org-clock-persist t)
(setq org-log-into-drawer t)
(setq org-modules '(org-habit))

(org-clock-persistence-insinuate)

(add-hook 'org-mode-hook 'turn-on-auto-fill)

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; Maxima

(setq imaxima-fnt-size "LARGE")

;; Ada

(setq ada-case-identifier 'ada-no-auto-case)
(setq ada-indent-is-separate nil)
(setq ada-language-version 'ada2005)

;; C, C++

(require 'include-guard)

(c-add-style "arn/cc-style"
             '("stroustrup"
               (c-offsets-alist (inline-open . 0)
                                (innamespace . -))))
(c-add-style "arn/java-style"
             '("bsd"
               (c-basic-offset . 3)))
(setq c-tab-always-indent nil)
(setq c-default-style
      '((c++-mode . "arn/cc-style")
        (c-mode . "arn/cc-style")
        (java-mode . "arn/java-style")))

(add-hook 'c-mode-common-hook (lambda () (setq show-trailing-whitespace t)))

(use-package rtags
  :config
  (rtags-enable-standard-keybindings)
  (rtags-start-process-unless-running))

;; Haskell

(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;; OCaml

(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))))

(require 'dune)
(require 'merlin)
(require 'merlin-company)

(add-hook 'tuareg-mode-hook 'merlin-mode)
(add-hook 'merlin-mode-hook 'company-mode)

(require 'ocp-indent)

(defun opam-config-env ()
  (interactive nil)
  (dolist (var (car (read-from-string
                     (shell-command-to-string
                      "opam config env --sexp"))))
    (setenv (car var) (cadr var))))

;; Erlang and Elixir

(use-package elixir-mode)

(let ((path "/usr/local/lib/erlang18/"))
  (when (file-accessible-directory-p path)
    (setq erlang-root-dir path)
    ;; FIXME: Handle tools version
    (add-to-list 'load-path (concat erlang-root-dir "lib/tools-2.8/emacs/"))
    (add-to-list 'exec-path (concat erlang-root-dir "bin/"))
    (require 'erlang-start)))

(add-to-list 'auto-mode-alist '("elvis\\.config" . erlang-mode))
(add-to-list 'auto-mode-alist '("rebar\\.config" . erlang-mode))
(add-to-list 'auto-mode-alist '("sys\\.config" . erlang-mode))

;; Lua

(setq lua-indent-level 4)

;; flymake and flycheck

(use-package flycheck
  :init (setq flycheck-display-errors-delay 0.25)
  :config (global-flycheck-mode)
  )

;; Backup

(setq backup-by-copying t)
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backup/"))))
(setq delete-old-versions t)
(setq kept-new-versions 2)
(setq kept-old-versions 5)
(setq make-backup-files t)
(setq version-control t)

;; Whitespace

(require 'whitespace)

(setq whitespace-style '(face empty tabs lines trailing))

(add-hook 'prog-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'whitespace-cleanup nil t)))

(use-package shrink-whitespace
  :bind ("M-S-SPC" . shrink-whitespace)
  )

;; Appearance and behaviour

(defalias 'yes-or-no-p 'y-or-n-p)

(setq comment-empty-lines t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq mouse-yank-at-point t)
(setq scroll-margin 4)
(setq show-paren-style 'mixed)
(setq tab-always-indent t)
(setq x-select-enable-primary t)

(set-default 'indicate-empty-lines t)

(auto-compression-mode 1)
(blink-cursor-mode -1)
(column-number-mode 1)
(desktop-save-mode -1)
(global-hl-line-mode 1)
(menu-bar-mode -1)
(mouse-avoidance-mode 'animate)
(scroll-bar-mode -1)
(show-paren-mode 1)
(size-indication-mode 1)
(tool-bar-mode -1)
(tooltip-mode -1)

(use-package gruvbox-theme
  :config (load-theme 'gruvbox-dark-medium)
  )

(server-start)

;; Bind keys, enable functions

(global-set-key (kbd "C-c =") 'align-regexp)
(global-set-key (kbd "C-c \\") 'sort-lines)
(global-set-key (kbd "C-c w") 'woman)
(global-set-key (kbd "C-x C-b") 'ibuffer-other-window)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
