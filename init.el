(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(if (not (fboundp 'string-replace))
    (progn
      (defun string-replace (from to in)
        (replace-regexp-in-string (regexp-quote from) to in nil 'literal))
      (declare-function string-replace "init")
      ))

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
  :config
  (setq company-minimum-prefix-length 1)
  (global-company-mode)
  )
(use-package indent-tools)
(use-package lsp-mode)
(use-package magit)
(use-package markdown-mode)
(use-package rainbow-mode)
(use-package reformatter)
(use-package yaml-mode)

;; Calendar

(require 'calendar)

(calendar-set-date-style 'iso)
(setq calendar-week-start-day 1)

;; Email

(setq user-full-name "Anton Yabchinskiy")

(require 'mu4e)

(setq mu4e-change-filenames-when-moving t)
(setq mu4e-compose-context-policy 'always-ask)
(setq mu4e-compose-signature-auto-include nil)
(setq mu4e-confirm-quit nil)
(setq mu4e-headers-date-format "%Y-%m-%d")
(setq mu4e-update-interval 150)
(setq mu4e-view-show-addresses t)

(setq mail-user-agent 'mu4e-user-agent)

(require 'smtpmail)

(setq message-kill-buffer-on-exit t)
(setq message-send-mail-function 'smtpmail-send-it)
(setq smtpmail-smtp-service "submission")
(setq smtpmail-stream-type 'starttls)

(global-set-key (kbd "C-c m") 'mu4e)

(setq mu4e-contexts
      (list (let* ((name "bestmx")
                   (host (concat name ".net")))
              (make-mu4e-context
               :name name
               :match-func `(lambda (msg)
                              (when msg
                                (mu4e-message-contact-field-matches msg '(:from :to) ,name)))
               :vars `((user-mail-address . ,(concat (user-login-name) "@" host))
                       (mu4e-drafts-folder . ,(concat "/" name "/Drafts"))
                       (mu4e-refile-folder . ,(concat "/" name "/Archive"))
                       (mu4e-sent-folder . ,(concat "/" name "/Sent"))
                       (mu4e-trash-folder . ,(concat "/" name "/Trash"))
                       (smtpmail-smtp-server . ,(concat "mail." host))
                       (smtpmail-smtp-user . ,user-mail-address)
                       )))
            (let* ((name "intabs")
                   (host (concat name ".net")))
              (make-mu4e-context
               :name name
               :match-func `(lambda (msg)
                              (when msg
                                (mu4e-message-contact-field-matches msg '(:from :to) ,name)))
               :vars `((user-mail-address . ,(concat (user-login-name) "@" host))
                       (mu4e-drafts-folder . ,(concat "/" name "/Drafts"))
                       (mu4e-refile-folder . ,(concat "/" name "/Archive"))
                       (mu4e-sent-folder . ,(concat "/" name "/Sent"))
                       (mu4e-trash-folder . ,(concat "/" name "/Trash"))
                       (smtpmail-smtp-server . ,host)
                       (smtpmail-smtp-user . ,(user-login-name))
                       (smtpmail-servers-requiring-authorization . ,(regexp-quote host))
                       )))
            (let* ((name "yandex")
                   (domain (concat name ".ru"))
                   (host (concat "smtp." domain))
                   (user (string-replace " " "." (downcase user-full-name))))
              (make-mu4e-context
               :name name
               :vars `((user-mail-address . ,(concat user "@" domain))
                       (mu4e-drafts-folder . ,(concat "/" name "/Drafts"))
                       (mu4e-refile-folder . ,(concat "/" name "/Archive"))
                       (mu4e-sent-folder . ,(concat "/" name "/Sent"))
                       (mu4e-trash-folder . ,(concat "/" name "/Trash"))
                       (smtpmail-smtp-server . ,host)
                       (smtpmail-smtp-service . 465)
                       (smtpmail-smtp-user . ,user)
                       (smtpmail-stream-type . ssl)
                       (smtpmail-servers-requiring-authorization . ,(regexp-quote host))
                       )))
            ))

;; Evil

(use-package evil
  :config
  (setq evil-undo-system 'undo-redo)
  (evil-mode 1)
  )

;; Ido, Smex

(require 'ido)

(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

(use-package smex
  :bind
  ("M-x" . smex)
  ("M-X" . smex-major-mode-commands)
  )

;; Buffers

(require 'uniquify)

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
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
(setq org-list-allow-alphabetical t)
(setq org-log-into-drawer t)
(setq org-modules '(org-habit))

(org-clock-persistence-insinuate)

(add-hook 'org-mode-hook 'turn-on-auto-fill)

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; C, C++

(use-package clang-format)
(use-package ggtags
  :config
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                (ggtags-mode t)))))

(require 'include-guard)

(c-add-style "arn/cc-style"
             '("stroustrup"
               (c-offsets-alist (inline-open . 0)
                                (innamespace . -))))
(setq c-tab-always-indent nil)
(setq c-default-style
      '((c++-mode . "arn/cc-style")
        (c-mode . "arn/cc-style")
        ))

;; Go

(use-package go-mode
  :config
  (add-hook 'go-mode-hook #'lsp-deferred)
  )

;; OCaml

(use-package dune)
(use-package merlin)
(use-package ocamlformat)
(use-package ocp-indent)
(use-package tuareg)
(use-package utop)

(add-hook 'tuareg-mode-hook 'merlin-mode)

;; Erlang and Elixir

(use-package elixir-mode)

(add-to-list 'auto-mode-alist '("elvis\\.config" . erlang-mode))
(add-to-list 'auto-mode-alist '("rebar\\.config" . erlang-mode))
(add-to-list 'auto-mode-alist '("sys\\.config" . erlang-mode))

(reformatter-define erlfmt
  :program "erlfmt"
  :args '("-"))

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

;; Appearance and behaviour

(defalias 'yes-or-no-p 'y-or-n-p)

(setq comment-empty-lines t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq mouse-yank-at-point t)
(setq scroll-margin 4)
(setq show-paren-style 'mixed)
(setq tab-always-indent t)
(setq warning-minimum-level :error)
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
  :config
  (load-theme 'gruvbox-dark-medium)
  :custom-face
  (diff-refine-added ((t (:background "#79740e" :foreground "#b8bb26"))))
  (diff-refine-removed ((t (:background "#9d0006" :foreground "#fb4933"))))
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
