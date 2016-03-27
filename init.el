(setq arn/at-home (string-match "^mithlond" (system-name)))

(add-to-list 'load-path (concat user-emacs-directory "lisp"))

;; Custom

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;; Easy PG

(require 'epa-file)

(setq epa-armor t)

;; Sensitive

(dolist (path `(,(concat user-emacs-directory "secret.el")
                ,(concat user-emacs-directory "secret.el.gpg")))
  (when (file-exists-p path)
    (load path)))

;; Packages

(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(unless (file-exists-p "~/.emacs.d/elpa/archives")
  (package-refresh-contents))

(dolist (pkg '(auto-complete
               cmake-mode
               color-theme-solarized
               evil
               evil-numbers
               go-mode
               jabber
               restclient
               rust-mode
               smex
               toml-mode
               tuareg
               yasnippet))
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; Calendar

(require 'calendar)

(calendar-set-date-style 'iso)
(setq calendar-week-start-day 1)

;; Email

(setq user-full-name "Anton Yabchinskiy")
(setq user-mail-address (getenv "EMAIL"))

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
(setq smtpmail-smtp-user user-mail-address)
(setq smtpmail-stream-type 'starttls)

;; Evil

(require 'evil)

(evil-mode 1)

(require 'evil-numbers)

(global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt)

;; Ido and Smex

(require 'ido)

(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

(require 'smex)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

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

(require 'yasnippet)

(yas-global-mode 1)

;; Jabber

(setq dired-bind-jump nil)
(require 'jabber)

(add-to-list 'gnutls-trustfiles "/etc/ssl/cert.pem")

(setq arn/jabber-resource
      (if arn/at-home
	  "Hejmloke"
	"Laborloke"))
(setq arn/jabber-priority
      (if arn/at-home
	  32
	64))

(setq fsm-debug nil)
(setq jabber-account-list
      `((,(concat arn/jabber-id "/" arn/jabber-resource)
         (:connection-type . starttls)
         (:password . ,arn/jabber-password))))
(setq jabber-use-sasl t)
(setq jabber-connection-ssl-program 'gnutls)
(setq jabber-activity-count-in-title t)
(setq jabber-auto-reconnect t)
(setq jabber-autoaway-method 'jabber-xprintidle-get-idle-time)
(setq jabber-autoaway-verbose t)
(setq jabber-backlog-days 7)
(setq jabber-backlog-number 128)
(setq jabber-chat-buffer-show-avatar nil)
(setq jabber-chat-delayed-time-format "%Y-%m-%d %H:%M:%S%z")
(setq jabber-chat-fill-long-lines nil)
(setq jabber-chat-time-format "%H:%M:%S")
(setq jabber-chatstates-confirm nil)
(setq jabber-default-priority arn/jabber-priority)
(setq jabber-events-confirm-composing nil)
(setq jabber-events-confirm-delivered nil)
(setq jabber-events-confirm-displayed nil)
(setq jabber-history-enabled t)
(setq jabber-history-size-limit 2048)
(setq jabber-history-dir (concat user-emacs-directory "xmpp/"))
(setq jabber-muc-completion-delimiter ": ")
(setq jabber-roster-show-empty-group t)
(setq jabber-roster-show-title nil)
(setq jabber-roster-sort-functions '(jabber-roster-sort-by-displayname))
(setq jabber-show-resources 'sometimes)
(setq jabber-use-global-history nil)
(setq jabber-vcard-avatars-retrieve nil)

(jabber-activity-mode 1)
(jabber-mode-line-mode 1)

(add-hook 'jabber-chat-mode-hook 'flyspell-mode)
(add-hook 'jabber-post-connect-hooks 'jabber-autoaway-start)

;; Org and remember

(require 'org)

(setq org-archive-tag "АРХИВ")
(setq org-clock-string "ВРЕМЯ:")
(setq org-closed-string "ВЫПОЛНЕНО:")
(setq org-deadline-string "СРОК:")
(setq org-default-notes-file (concat org-directory "default.org"))
(setq org-directory (concat user-emacs-directory "org/"))
(setq org-hide-leading-stars t)
(setq org-modules '(org-habit))
(setq org-quote-string "ЦИТАТА")
(setq org-scheduled-string "ПО ПЛАНУ:")

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

;; Haskell

(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;; OCaml

(let ((s (substring (shell-command-to-string "opam config var share") 0 -1)))
  (add-to-list 'load-path (concat s "/emacs/site-lisp")))

(require 'merlin)

(add-hook 'tuareg-mode-hook 'merlin-mode)
(setq merlin-command 'opam)
(setq merlin-use-auto-complete-mode 'easy)

(require 'ocp-indent)

;; Erlang

(let ((path "/usr/local/lib/erlang18/"))
  (when (file-accessible-directory-p path)
    (setq erlang-root-dir path)
    ;; FIXME: Handle tools version
    (add-to-list 'load-path (concat erlang-root-dir "lib/tools-2.8/emacs/"))
    (add-to-list 'exec-path (concat erlang-root-dir "bin/"))
    (require 'erlang-start)))

;; Lua

(setq lua-indent-level 4)

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
(global-whitespace-mode t)

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

(cond ((= emacs-major-version 24)
       (load-theme 'solarized t))
      ((= emacs-major-version 23)
       (color-theme-initialize)
       (color-theme-solarized-dark)))

(server-start)

;; Bind keys, enable functions

(global-set-key (kbd "C-c =") 'align-regexp)
(global-set-key (kbd "C-c \\") 'sort-lines)
(global-set-key (kbd "C-x C-b") 'ibuffer-other-window)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
