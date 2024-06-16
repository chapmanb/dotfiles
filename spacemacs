;; -*- mode: dotspacemacs -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.
;; Apply changes with SPC f e R

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (ie. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(rust
     javascript
     html
     csv
     lua
     ;; --------------------------------------------------------
     ;; Example of useful layers you may want to use right away
     ;; Uncomment a layer name and press C-c C-c to install it
     ;; --------------------------------------------------------
     (auto-completion :variables
                      auto-completion-enable-help-tooltip t
                      auto-completion-use-tab-instead-of-enter t
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-sort-by-usage t)
     better-defaults
     brad
     (clojure :variables
              clojure-backend 'cider
              clojure-toplevel-inside-comment-form t
              cider-overlays-use-font-lock t
              clojure-enable-linters 'clj-kondo
              cider-repl-buffer-size-limit 1000
              cider-preferred-build-tool 'clojure-cli)
     emacs-lisp
     (git :variables
          git-magit-status-fullscreen t
          magit-diff-refine-hunk 'all)
     (helm :variables
           helm-follow-mode-persistent t)
     (languagetool :variables
                   langtool-language-tool-jar "/home/bchapman/install/system/languagetool/languagetool-commandline.jar")
     (lsp :variables
          lsp-headerline-breadcrumb-enable nil
          )
     (python :variables
             ;; python-backend 'anaconda
             python-backend 'lsp
             python-lsp-server 'pyright
             python-formatter 'black
             python-fill-column 100
             python-test-runner '(pytest nose)
             python-format-on-save nil
             python-sort-imports-on-save nil)
     markdown
     ;; notmuch
     (org :variables
          org-enable-github-support t
          org-enable-roam-support t)
     spell-checking
     syntax-checking
     ;; version-control
     xclipboard
     yaml
     ;; Configuration: https://github.com/seagle0128/doom-modeline#customize
     (spacemacs-modeline :variables
                         doom-modeline-height 12
                         doom-modeline-major-mode-color-icon t
                         doom-modeline-buffer-file-name-style 'relative-to-project
                         doom-modeline-display-default-persp-name t
                         doom-modeline-minor-modes nil
                         doom-modeline-modal-icon nil)
     ;; confluence
     ;; ess
     ;; go
     ;; (shell :variables
     ;;        shell-default-shell 'ansi-term
     ;;        shell-default-position 'right
     ;;        shell-default-height 50
     ;;        shell-default-width 50)
     )
   dotspacemacs-additional-packages '(all-the-icons
                                      (evil-ediff :location (recipe :fetcher github :repo "emacs-evil/evil-ediff")))
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages
   '(helm-c-yasnippet
     rainbow-delimiters
     yasnippet)
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   dotspacemacs-enable-server t
   ;; Either `vim' or `emacs'. Evil is always enabled but if the variable
   ;; is `emacs' then the `holy-mode' is enabled at startup.
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progess in `*Messages*' buffer.
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to a .PNG file.
   ;; If the value is nil then no banner is displayed.
   ;; dotspacemacs-startup-banner 'official
   dotspacemacs-startup-banner 'official
   ;; t if you always want to see the changelog at startup
   dotspacemacs-always-show-changelog nil
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'."
   dotspacemacs-startup-lists '((recents . 20)
                                (projects . 10))
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(zenburn
                         base16-ocean
                         solarized-dark
                         monokai)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Hack"
                               :size 14
                               :powerline-scale 1.0)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it.
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil the paste micro-state is enabled. While enabled pressing `p`
   ;; several times cycle between the kill ring content.
   dotspacemacs-enable-paste-micro-state t
   ;; Guide-key delay in seconds. The Guide-key is the popup buffer listing
   ;; the commands bound to the current keystrokes.
   dotspacemacs-guide-key-delay 1.0
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil ;; to boost the loading time.
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up.
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX."
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-frame-title-format "%a %t"
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line.
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-mode-line-theme `doom
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen.
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   dotspacemacs-smartparens-strict-mode t
   ;; If non nil advises quit functions to keep server open when quitting.
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now.
   dotspacemacs-default-package-repository nil
   )
  ;; User initialization goes here
  (setq org-roam-v2-ack t)
  )

(defun dotspacemacs/user-config ()
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."
  (add-to-list 'exec-path "/mnt/work/ginkgo/env/bin/")
  ;; ;; notmuch mail
  ;; (require 'notmuch)
  ;; (setq notmuch-fcc-dirs "fastmail/INBOX.Sent")
  ;; (setq send-mail-function 'sendmail-send-it)
  ;; (setq sendmail-program "/usr/bin/msmtp")
  ;; (setq mail-host-address "fastmail.com")
  ;; (setq mail-specify-envelope-from t)
  ;; (setq mail-envelope-from 'header)
  ;; (setq message-sendmail-envelope-from 'header)

  ;; (defun notmuch-exec-offlineimap ()
  ;;   (interactive)
  ;;   (set-process-sentinel
  ;;    (start-process-shell-command "offlineimap" "*offlineimap*" "offlineimap -o")
  ;;    '(lambda (process event)
  ;;       (let ((w (get-buffer-window "*offlineimap*")))
  ;;         (when w
  ;;           (with-selected-window w (recenter (window-end)))))))
  ;;   (popwin:display-buffer "*offlineimap*"))
  ;; ;; (add-to-list 'popwin:special-display-config
  ;; ;;              '("*offlineimap*" :dedicated t :position bottom :stick t :height 0.3 :noselect t))
  ;; (define-key notmuch-search-mode-map "," 'notmuch-exec-offlineimap)
  ;; (define-key notmuch-show-mode-map "d" (lambda () (interactive)
  ;;                                         (notmuch-show-tag notmuch-message-deleted-tags)))

  ;; smartparens
  (define-key evil-normal-state-map "\C-c0" 'sp-forward-slurp-sexp)
  (define-key evil-normal-state-map "\C-c9" 'sp-forward-barf-sexp)
  (define-key evil-normal-state-map "\C-c1" 'sp-backword-slurp-sexp)
  (define-key evil-normal-state-map "\C-c2" 'sp-backword-barf-sexp)
  ;; Avoid highlighting long lines in org mode
  (setq whitespace-line-column 240)

  ;; ## org-mode
  ;; org-pomodoro -- avoid final sound
  (setq-default org-pomodoro-play-sounds nil)
  (require 'org)
  (add-to-list 'org-modules 'org-table)
  (add-to-list 'org-modules 'org-clock)
  (setq org-clock-into-drawer nil)
  (setq org-roam-directory "~/personal/org/roam")
  ;; beamer export
  ;; (require 'ox-beamer)
  ;; (setq org-export-backends '(beamer html latex md))
  ;; org calendar
  ;; (global-set-key "\C-cc" 'org-capture)
  ;; (setq org-directory (expand-file-name "~/drive/org/calendar"))
  ;; (setq org-agenda-files (list org-directory))
  ;; (setq org-default-notes-file (concat org-directory "/calendar.org"))
  ;; (setq org-capture-templates
  ;;       '(("c" "Calendar" entry (file+headline org-default-notes-file "Calendar")
  ;;          "* %?\n  %a")))
  ;; Fix annoying auto indent issues with electric-indent mode
  (defun fix-electric-indent-mode ()
    (setq electric-indent-inhibit t)
    ;(disable-electric-indent-mode)
    (setq electric-indent-chars '(?\n))
    (if (fboundp 'electric-indent-local-mode)
        (electric-indent-local-mode -1)))
  ;; (add-hook 'python-mode-hook 'fix-electric-indent-mode)
  ;; (add-hook 'yaml-mode-hook 'fix-electric-indent-mode)
  ;; Avoid doing vc checking on remote files
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))
  ;; (setq tramp-default-method "ssh")
  (setq tramp-use-ssh-controlmaster-options nil)
  ;; Remote nrepl support https://practicalli.github.io/spacemacs/clojure-repl/connect-to-remote-repl.html
  (setq nrepl-use-ssh-fallback-for-remote-hosts t)
  ;; Fix tramp issues with git-gutter
  ;; https://github.com/nonsequitur/git-gutter-plus/issues/42
  (with-eval-after-load 'git-gutter+
    (defun git-gutter+-remote-default-directory (dir file)
      (let* ((vec (tramp-dissect-file-name file))
             (method (tramp-file-name-method vec))
             (user (tramp-file-name-user vec))
             (domain (tramp-file-name-domain vec))
             (host (tramp-file-name-host vec))
             (port (tramp-file-name-port vec)))
        (tramp-make-tramp-file-name method user domain host port dir)))
    (defun git-gutter+-remote-file-path (dir file)
      (let ((file (tramp-file-name-localname (tramp-dissect-file-name file))))
        (replace-regexp-in-string (concat "\\`" dir) "" file))))

  ;; Make underscore part of word in python-mode for searching
  (add-hook 'python-mode-hook (function
                               (lambda () (modify-syntax-entry ?_ "w" python-mode-syntax-table))))
  ;; Clojure configurations
  ;;
  ;; Handle clojure words like this->that
  ;; (add-hook 'clojure-mode-hook (function
  ;;                               (lambda ()
  ;;                                 ;(modify-syntax-entry ?> "w" clojure-mode-syntax-table)
  ;;                                 ;(modify-syntax-entry ?- "w" clojure-mode-syntax-table)
  ;;                                 (modify-syntax-entry ?> "w")
  ;;                                 (modify-syntax-entry ?- "w"))))
  ;; add support for matching '<' and '>' in clojure mode
  (setq ahs-include '((clojure-mode . "^[<>0-9A-Za-z/_.,:;*+=&%|$#@!^?-]+$")))
  ;; CIDER 0.23 Lima release options
  ;; Configure the position of evaluation result
  ;; By default the result displays at the end of the current line
  ;; Set cider-result-overlay-position to `at-point' to display results right after the expression evaluated
  ;; Useful for evaluating nexsted expressions with `, e e'
  (setq cider-result-overlay-position 'at-point)
  ;; Pretty print in Clojure to use the Fast Idiomatic Pretty-Printer.
  (setq cider-pprint-fn 'fipp)
  ;; Indentation of function forms
  ;; https://github.com/clojure-emacs/clojure-mode#indentation-of-function-forms
  (setq clojure-indent-style 'align-arguments)
  ;; Vertically align s-expressions
  ;; https://github.com/clojure-emacs/clojure-mode#vertical-alignment
  (setq clojure-align-forms-automatically nil)
  ;; Auto-indent code automatically
  ;; https://emacsredux.com/blog/2016/02/07/auto-indent-your-code-with-aggressive-indent-mode/
  (add-hook 'clojure-mode-hook #'aggressive-indent-mode)

  ;; Magit support
  (setq-default git-commit-check-style-conventions nil)
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (xterm-color shell-pop org-gcal request-deferred deferred gntp multi-term lua-mode parent-mode fringe-helper flx highlight transient goto-chg ctable julia-mode eshell-z eshell-prompt-extras esh-help pos-tip paredit lv eval-sexp-fu sesman pkg-info parseedn parseclj a epl anki-editor pythonic f auto-complete popup avy go-mode git-commit async multiple-cursors clojure-mode packed with-editor hydra s dash evil-ediff dumb-jump column-enforce-mode clojure-snippets git-gutter+ diminish company cider anaconda-mode package-build bind-key bind-map evil base16-ocean-theme yasnippet ess helm helm-core yapfify winum uuidgen unfill thrift powerline py-isort ox-gfm metaweblog xml-rpc org-projectile org-category-capture alert log4e org-mime org-download mwim markdown-mode live-py-mode link-hint dash-functional projectile request go-guru gitignore-mode git-link git-gutter fuzzy flycheck eyebrowse evil-visual-mark-mode evil-unimpaired magit magit-popup ghub let-alist smartparens base16-ocean-dark-theme paradox company-quickhelp yaml-mode xclip ws-butler writegood-mode wolfram-mode window-numbering which-key volatile-highlights vi-tilde-fringe use-package twittering-mode toc-org stan-mode spacemacs-theme spaceline snakemake-mode smooth-scrolling smeargle scad-mode restart-emacs quelpa qml-mode pyvenv python pytest pyenv-mode popwin pip-requirements persp-mode pcre2el page-break-lines orgit org2blog org-repo-todo org-present org-pomodoro org-plus-contrib org-bullets open-junk-file offlineimap notmuch neotree move-text mmm-mode matlab-mode markdown-toc magit-gitflow macrostep lorem-ipsum linum-relative leuven-theme info+ indent-guide ido-vertical-mode hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation help-fns+ helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-company helm-ag google-translate golden-ratio go-eldoc gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-gutter-fringe git-gutter-fringe+ gh-md flycheck-pos-tip flx-ido fill-column-indicator fancy-battery expand-region exec-path-from-shell evil-visualstar evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-jumper evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-args evil-anzu ess-smart-equals ess-R-object-popup ess-R-data-view elisp-slime-nav diff-hl define-word cython-mode company-statistics company-go company-anaconda clj-refactor clean-aindent-mode cider-eval-sexp-fu buffer-move bracketed-paste base16-theme auto-yasnippet auto-highlight-symbol auto-compile arduino-mode aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-want-Y-yank-to-eol nil)
 '(package-selected-packages
   '(toml-mode ron-mode racer rust-mode helm-gtags ggtags flycheck-rust counsel-gtags counsel swiper ivy cargo xterm-color shell-pop org-gcal request-deferred deferred gntp multi-term lua-mode parent-mode fringe-helper flx highlight transient goto-chg ctable julia-mode eshell-z eshell-prompt-extras esh-help pos-tip paredit lv eval-sexp-fu sesman pkg-info parseedn parseclj a epl anki-editor pythonic f auto-complete popup avy go-mode git-commit async multiple-cursors clojure-mode packed with-editor hydra s dash evil-ediff dumb-jump column-enforce-mode clojure-snippets git-gutter+ diminish company cider anaconda-mode package-build bind-key bind-map evil base16-ocean-theme yasnippet ess helm helm-core yapfify winum uuidgen unfill thrift powerline py-isort ox-gfm metaweblog xml-rpc org-projectile org-category-capture alert log4e org-mime org-download mwim markdown-mode live-py-mode link-hint dash-functional projectile request go-guru gitignore-mode git-link git-gutter fuzzy flycheck eyebrowse evil-visual-mark-mode evil-unimpaired magit magit-popup ghub let-alist smartparens base16-ocean-dark-theme paradox company-quickhelp yaml-mode xclip ws-butler writegood-mode wolfram-mode window-numbering which-key volatile-highlights vi-tilde-fringe use-package twittering-mode toc-org stan-mode spacemacs-theme spaceline snakemake-mode smooth-scrolling smeargle scad-mode restart-emacs quelpa qml-mode pyvenv python pytest pyenv-mode popwin pip-requirements persp-mode pcre2el page-break-lines orgit org2blog org-repo-todo org-present org-pomodoro org-plus-contrib org-bullets open-junk-file offlineimap notmuch neotree move-text mmm-mode matlab-mode markdown-toc magit-gitflow macrostep lorem-ipsum linum-relative leuven-theme info+ indent-guide ido-vertical-mode hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation help-fns+ helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-company helm-ag google-translate golden-ratio go-eldoc gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-gutter-fringe git-gutter-fringe+ gh-md flycheck-pos-tip flx-ido fill-column-indicator fancy-battery expand-region exec-path-from-shell evil-visualstar evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-jumper evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-args evil-anzu ess-smart-equals ess-R-object-popup ess-R-data-view elisp-slime-nav diff-hl define-word cython-mode company-statistics company-go company-anaconda clj-refactor clean-aindent-mode cider-eval-sexp-fu buffer-move bracketed-paste base16-theme auto-yasnippet auto-highlight-symbol auto-compile arduino-mode aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell))
 '(paradox-github-token t)
 '(safe-local-variable-values
   '((python-sort-imports-on-save)
     (python-format-on-save)
     (javascript-backend . tide)
     (javascript-backend . tern)
     (javascript-backend . lsp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(highlight-parentheses-highlight ((nil (:weight ultra-bold))) t))
)
