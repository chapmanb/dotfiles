;;; packages.el --- brad Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq brad-packages
  '(
    ;; anki-editor
    ;; epg
    paredit
    ;; offlineimap
    ;; org-gcal
    ;; notmuch
    ;; snakemake-mode
    writegood-mode
    ;; xclip
    ))

;; For each package, define a function brad/init-<package-brad>
;;
(defun brad/init-epg ()
  (require 'epa-file)
  (epa-file-enable)
  (load-library "~/.emacs.d/private/brad/secrets.el.gpg"))
(defun brad/init-paredit ()
  (require 'paredit)
  (eval-after-load 'paredit
    '(progn
       ;(define-key paredit-mode-map "\c-c0" 'paredit-forward-slurp-sexp)
       ;(define-key paredit-mode-map "\c-c9" 'paredit-forward-barf-sexp)
       (define-key paredit-mode-map (kbd "{") 'paredit-open-curly)
       (define-key paredit-mode-map (kbd "}") 'paredit-close-curly)
       (define-key paredit-mode-map (kbd "M-{") 'paredit-wrap-curly)
       (define-key paredit-mode-map (kbd "M-]") 'paredit-wrap-square)))
  (add-hook 'clojure-mode-hook 'enable-paredit-mode)
  (add-hook 'nrepl-mode-hook 'paredit-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode))

(defun brad/init-writegood-mode ()
  (require 'writegood-mode)
  (setq whitespace-line-column 100)
  (setq whitespace-style '(face tabs empty trailing lines-tail))
  (dolist (hook '(python-mode-hook
                  clojure-mode-hook
                  ruby-mode-hook))
    ;;(add-hook hook 'writegood-mode)
    (add-hook hook 'flyspell-prog-mode))

  (dolist (hook '(text-mode-hook
                  org-mode-hook
                  message-mode-hook
                  LaTeX-mode-hook
                  markdown-mode-hook
                  twittering-edit-mode-hook))
    (add-hook hook 'writegood-mode)
    (add-hook hook 'whitespace-mode)
    (add-hook hook 'turn-on-flyspell))
  (dolist (hook '(text-mode-hook
                  org-mode-hook
                  LaTeX-mode-hook
                  markdown-mode-hook))
    (add-hook hook '(lambda () (setq fill-column 80)))
    (add-hook hook 'turn-on-auto-fill))

  (defun unfill-paragraph ()
    "Takes a multi-line paragraph and makes it into a single line of text."
    (interactive)
    (let ((fill-column (point-max)))
      (fill-paragraph nil)))
  (define-key global-map "\M-Q" 'unfill-paragraph))

(defun brad/init-offlineimap ()
  (require 'offlineimap))

(defun brad/init-org-gcal ()
  (require 'org-gcal)
  (setq org-gcal-file-alist '(("bchapman@ginkgobioworks.com" .  "~/drive/org/calendar/ginkgo.org"))))

(defun brad/init-notmuch ()
  (require 'notmuch)
  ;(require 'hl-line)
  ;(global-hl-line-mode t)
  ;(set-face-background 'hl-line "#444")

  (setq message-kill-buffer-on-exit t)
  (setq notmuch-fcc-dirs "fastmail/INBOX.Sent")
  (setq send-mail-function 'sendmail-send-it)
  (setq sendmail-program "/usr/bin/msmtp")
  (setq mail-host-address "fastmail.com")
  (setq mail-specify-envelope-from t)
  (setq mail-envelope-from 'header)
  (setq message-sendmail-envelope-from 'header)
  (add-hook 'message-mode-hook '(lambda () (setq fill-column 78)) 'append)
  (setq mm-text-html-renderer 'w3m-standalone)
  (setq message-default-mail-headers "Cc: \n")
  (setq message-auto-save-directory "~/mail/drafts")
  (setq notmuch-hello-recent-searches-max 0)
  (define-key notmuch-search-mode-map "k" 'notmuch-search-previous-thread)
  (define-key notmuch-search-mode-map "j" 'notmuch-search-next-thread)
  (define-key notmuch-show-mode-map "k" 'notmuch-show-next-open-message)
  (define-key notmuch-show-mode-map "j" 'notmuch-show-next-open-message)
  (defun brad/show-remove-sort-tags ()
    (dolist (x '("good" "junk"))
      (if (member x (notmuch-show-get-tags))
          (notmuch-show-tag-message (format "-%s" x)))))
  (defun toggle-delete-tag-show ()
    (interactive)
    (if (member "deleted" (notmuch-show-get-tags))
        (notmuch-show-tag-message "-deleted")
      (notmuch-show-tag-message "+deleted")))
  (defun toggle-delete-tag-search ()
    (interactive)
    (if (member "deleted" (notmuch-search-get-tags))
        (notmuch-search-tag '("-deleted"))
      (notmuch-search-tag '("+deleted")))
    (notmuch-search-next-thread))
  (defun toggle-todo-tag-show ()
    (interactive)
    (if (member "todo" (notmuch-show-get-tags))
        (notmuch-show-tag-message "-todo")
      (notmuch-show-tag-message "+todo")))
  (defun toggle-todo-tag-search ()
    (interactive)
    (if (member "todo" (notmuch-search-get-tags))
        (notmuch-search-tag  '("-todo"))
      (notmuch-search-tag '("+todo")))
    (notmuch-search-next-thread))
  (define-key notmuch-show-mode-map "d" 'toggle-delete-tag-show)
  (define-key notmuch-show-mode-map "t" 'toggle-todo-tag-show)
  (define-key notmuch-search-mode-map "d" 'toggle-delete-tag-search)
  (define-key notmuch-search-mode-map "t" 'toggle-todo-tag-search)
  (setq notmuch-saved-searches '(("inbox" . "tag:inbox AND NOT tag:deleted")
                                 ("todo" . "tag:todo AND NOT tag:deleted")
                                 ("unread" . "tag:unread AND NOT tag:deleted")))
  (add-hook 'notmuch-show-hook 'brad/show-remove-sort-tags)
  (defun brad-mail ()
    (interactive)
    (offlineimap)
    (notmuch-hello))
  (defun brad-mail-refresh ()
    (interactive)
    (offlineimap-quit)
    (offlineimap)
    (notmuch-refresh-this-buffer))
  (defun brad-mail-quit ()
    (interactive)
    (offlineimap-quit)
    (shell-command "notmuch dump > ~/drive/mail/tag-dump.txt")
    (notmuch-bury-or-kill-this-buffer)
    (if (get-buffer "*Notmuch errors*")
        (kill-buffer "*Notmuch errors*")))
  (define-key notmuch-search-mode-map "g" 'brad-mail-refresh)
  (define-key notmuch-show-mode-map "g" 'brad-mail-refresh)
  (define-key notmuch-hello-mode-map "q" 'brad-mail-quit)
  ;; fixes: killing a notmuch buffer does not show the previous buffer
  ;; https://github.com/syl20bnr/spacemacs/issues/2163
  (push "\\*notmuch.+\\*" spacemacs-useful-buffers-regexp)
  (setq notmuch-search-line-faces `(("good" . '(:foreground "#268bd2"))
                                    ("junk" . '(:foreground "#cb4b16"))
                                    ("unread" . '(:foreground "#2aa198"))
                                    ("deleted" . '(:foreground "#6c71c4"))))
  (add-hook 'notmuch-search-hook
            (lambda ()
              (set-face-attribute 'notmuch-tag-face nil :foreground "OliveDrab1")))
  (add-hook 'notmuch-show-hook
            (lambda ()
              (set-face-attribute 'notmuch-message-summary-face nil :background "#303030"))))

(defun brad/init-snakemake-mode ()
  (require 'snakemake-mode))

(defun brad/init-xclip ()
  ;; Integrate with X copy buffer
  (xclip-mode +1)
  (setq
   x-select-enable-clipboard t
   x-select-enable-primary t)
  ;;(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
)
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package


(defun brad/init-anki-editor ()
  (require 'anki-editor))
