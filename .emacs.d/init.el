;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/")))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Jump to init.el
(global-set-key (kbd "S-C-M-i")
                (lambda ()
                  (interactive)
                  (find-file "~/.emacs.d/init.el")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Incremental completion & narrowing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Helm
(require 'helm-config)
(helm-mode 1)

(helm-autoresize-mode 1)
(helm-descbinds-mode)

(setq helm-mode-fuzzy-match t)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-m") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-h f") 'helm-apropos)
(global-set-key (kbd "C-h r") 'helm-info-emacs)
(global-set-key (kbd "C-h C-l") 'helm-locate-library)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; History
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Don't keep dirty temp files
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Magit
(global-set-key (kbd "C-x g s") 'magit-status)

; ;; desktop-mode
; (setq desktop-dirname "~/.emacs.d/desktop"
;       desktop-base-file-name "emacs.desktop"
;       desktop-base-lock-name "lock"
;       desktop-path (list desktop-dirname)
;       desktop-save t
;       desktop-auto-save-timeout 5
;       desktop-load-locked-desktop nil)
; 
; (desktop-save-mode 1)

; ;; Bookmark+
; (setq bookmark-default-file "~/.emacs.d/bookmarks"
;       bmkp-auto-light-when-set 'all-in-buffer
;       bmkp-auto-light-when-jump 'all-in-buffer
;       bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks"
;       bmkp-bmenu-state-file "~/.emacs.d/.emacs-bmk-bmenu-state.el")

; ;; Recent Files
; (require 'recentf)
; (recentf-mode 1)
; (setq recentf-max-menu-items 25)
; (global-set-key "\C-x\ \C-r" 'recentf-open-files)

(setq abbrev-file-name "~/.emacs.d/abbrev_defs")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Mouse and keyboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use y/n in place of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Turn off mouse interface
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)

;; avy - jump to text
(global-set-key (kbd "C-:") 'avy-goto-char)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Mouse and keyboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Mark and delete a region, rather than inserting text
(delete-selection-mode 1)

;; Narrow to region hides the rest of the buffer.
;; Use C-x n n to narrow
;;     C-x n w to widen
(put 'narrow-to-region 'disabled nil)

;; Enable upcase and downcase commands
;; Usage:
;;   Upcase:   C-x C-u
;;   Downcase: C-x C-l
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Easily move from one window to another.
;; Usage: M-<left>|<right>|<up>|<down>
(windmove-default-keybindings 'meta)

;; Winner-mode allows you to restore window configurations.
;; Usage: C-c <left> | <right>
(when (fboundp 'winner-mode)
  (winner-mode 1))

;; zoom-frm
(require 'zoom-frm)
(define-key ctl-x-map [(control ?+)] 'zoom-in/out)
(define-key ctl-x-map [(control ?-)] 'zoom-in/out)
(define-key ctl-x-map [(control ?=)] 'zoom-in/out)
(define-key ctl-x-map [(control ?0)] 'zoom-in/out)

;; Resizing windows
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; Display line numbers next to the buffer.
(global-linum-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use empty *scratch* buffer
(setq initial-scratch-message nil)

;; Display tooltips in echo area
(setq tooltip-use-echo-area t)

;; iBuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;(add-to-list 'ibuffer-never-show-predicates "^\\*helm")

(setq ibuffer-formats
      '((mark modified read-only
              " "
              (name 25 25 :left :elide)
              " "
              (size 9 -1 :right)
              " "
              (mode 10 10 :left :elide)
              " " filename-and-process)
        (mark 
         " " (name 16 -1)
         " " filename)))

;; Dired-x
(load-library "dired-x")

;; Projectile
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

(use-package ag
  :pin melpa
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TabBar mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tabbar-mode
(tabbar-mode 1)

;; Groups tabs into one of two groups. The first, "meta" includes the scratch
;; buffer, messages, help, processes, etc. The second being "user" tabs, which
;; is everything else, i.e. text files will be in this group.
 (setq tabbar-buffer-groups-function
       (lambda ()
         (list (cond ((string-equal "*" (substring (buffer-name) 0 1)) "meta")
                     ((eq major-mode 'dired-mode) "meta")
                     ((projectile-project-p) (projectile-project-name))
                     (t "user")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SrSpeedbar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-x <") 'sr-speedbar-toggle)

(setq speedbar-frame-parameters
      '((minibuffer)
        (border-width . 0)
        (left-fringe . 0)
        (menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (unsplittable . t)))

(setq speedbar-hide-button-brackets-flag t
      speedbar-show-unknown-files t
      speedbar-smart-directory-expand-flag t
      speedbar-use-images t
      sr-speedbar-auto-refresh t
      sr-speedbar-delete-windows t
      sr-speedbar-skip-other-window-p t
      sr-speedbar-right-side nil
      sr-speedbar-width-x 180)

(add-hook 'speedbar-mode-hook
          (lambda ()
            (linum-mode 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; multiple-cursors (like IntelliJ)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Indentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use spaces instead of tabs, and set the default indent
;; width to be 4 spaces. Need to use setq-default because of
;; `web-mode`.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default indent-line-function 'insert-tab)

(setq-default truncate-lines t)
(setq-default scss-compile-at-save nil)

;; Auto-linting behavior
(setq require-final-newline t)
(setq auto-indent-on-visit-file t)

;; Shell scripts
(setq sh-basic-offset tab-width)
(setq sh-indentation tab-width)

;; CSS mode
(setq css-indent-offset tab-width)

;; Auto close pairs
(electric-pair-mode)

;; Align
(global-set-key (kbd "C-x /") 'align-regexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Font-locking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; highlight-symbol
;; TODO: Don't rebind f3, f4 - these keys are used for keyboard macros
(global-set-key (kbd "C-<f3>") 'highlight-symbol)
(global-set-key (kbd "<f3>") 'highlight-symbol-next)
(global-set-key (kbd "S-<f3>") 'highlight-symbol-prev)
(global-set-key (kbd "M-<f3>") 'highlight-symbol-query-replace)

;; Show matching parentheses immediately.
(show-paren-mode 1)
(setq show-paren-delay 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; flycheck
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package flycheck
;;   :pin melpa
;;   :ensure t
;;   :init (global-flycheck-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; company-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Setup company-mode
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0)

;; Use ssh for tramp by default
(setq tramp-default-method "ssh")

;; Use aspell for spell checking
(setq-default ispell-program-name "/usr/bin/aspell")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; yasnippet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"
        "~/.emacs.d/yasnippet-snippets"))
(yas-global-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Setup org-mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'load-path "~/.emacs.d/org-mode/contrib/lisp")

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-startup-indented t)
(setq org-agenda-default-appointment-duration nil)
(setq org-directory "~/Notes/org/")
(setq org-log-done 'time)

(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "|" "NOPE(n)" "SOME(m)" "DONE(d)")))

(setq org-todo-keyword-faces
      '(("TODO" . org-todo)
        ("STARTED" . "teal")
        ("NOPE" . "gray")
        ("SOME" . "yellow")
        ("DONE" . org-done)))

(org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
        (ruby . t)
        (js . t)
        (css . t)
        (sass . t)))

(add-hook 'org-mode-hook
          (lambda ()
            (linum-mode 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; mode-line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Display column number next to line number in the mode-line.
(column-number-mode t)

; ;; Smart Mode Line
; (setq sml/no-confirm-load-theme t)
; (setq sml/theme 'powerline)
; ;(setq sml/show-encoding nil)
; (setq sml/shorten-directory t)
; (sml/setup)

;; Powerline
(powerline-default-theme)

; (mode-icons-mode t)

;; GitGutter
(global-git-gutter-mode t)
(git-gutter:linum-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load up-to-date python-mode
(setq py-install-directory "~/.emacs.d/python-mode")
(add-to-list 'load-path py-install-directory)
(require 'python-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; scss-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.scss\\.erb\\'" . scss-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; web-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Setup web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.\\(text\\|html\\|hbs\\)\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(add-hook 'web-mode-hook
          (lambda ()
            "Make the offset throughout web-mode smaller."
            (setq web-mode-markup-indent-offset tab-width
                  web-mode-css-indent-offset    tab-width
                  web-mode-code-indent-offset   tab-width)))

(setq web-mode-content-types-alist
      '(("handlebars" . ".*merchant_dashboard/app/assets/javascripts/.*\\.hbs\\.erb\\'")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Ruby
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'hs-special-modes-alist
	     '(ruby-mode
	       "\\(def\\|do\\|{\\)" "\\(end\\|end\\|}\\)" "#"
	       (lambda (arg) (ruby-end-of-block)) nil))

;; Fix Ruby Indentation
; (defadvice ruby-indent-line (after unindent-closing-paren activate)
;   (let ((column (current-column))
;         indent offset)
;     (save-excursion
;       (back-to-indentation)
;       (let ((state (syntax-ppss)))
;         (setq offset (- column (current-column)))
;         (when (and (eq (char-after) ?\))
;                    (not (zerop (car state))))
;           (goto-char (cadr state))
;           (setq indent (current-indentation)))))
;     (when indent
;       (indent-line-to indent)
;       (when (> offset 0) (forward-char offset)))))

(setq ruby-insert-encoding-magic-comment nil)

(setq ruby-deep-indent-paren nil)
(setq ruby-deep-indent-paren-style nil)
(setq ruby-use-smie nil)

(add-to-list 'auto-mode-alist '("\\.\\(rabl\\|jbuilder\\)\\'" . ruby-mode))

;; Font-locking for pointy lambdas, i.e. `-> (x) { x + 1 }'
(add-hook 'ruby-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("\\(?:->\\)" . font-lock-keyword-face)))))

;; Enable robe (navigation, documentation, and completion)
(add-hook 'ruby-mode-hook 'robe-mode)
(eval-after-load 'company
  '(push 'company-robe company-backends))

(defun inf-ruby-console-rails-with-zeus (dir)
  "Run Rails console in DIR. Supports Zeus."
  (interactive "D")
  (let* ((default-directory (file-name-as-directory dir))
         (envs (inf-ruby-console-rails-envs))
         (env (completing-read "Rails environment: " envs nil t
                               nil nil (car (member "development" envs))))
         (cmd (if (and (file-exists-p "zeus.json")
                       (string-equal env "development"))
                  "zeus console"
                (concat (if (file-exists-p "Gemfile") "bundle exec " "")
                        "rails console " env))))
    (run-ruby cmd "rails")))

(advice-add 'inf-ruby-console-rails :override #'inf-ruby-console-rails-with-zeus)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; HAML
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'haml-mode-hook 'flymake-haml-load)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; YAML
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.yml\\.erb\\'" . yaml-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Coffeescript
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.coffee.*\\'" . coffee-mode))

(setq coffee-tab-width tab-width)

(add-hook 'coffee-mode-hook
          (lambda ()
            (auto-fill-mode 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Scala
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ensime
  :pin melpa-stable
  :ensure t)

(use-package dockerfile-mode
  :pin melpa
  :ensure t)

;; Ensime (Scala)
;; (require 'ensime)
;; (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
;; (add-hook 'ensime-scala-mode-hook
;;           (lambda ()
;;             (setq debug-on-error t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; JavaScript: js2-mode 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq js2-basic-offset tab-width)
(setq js2-strict-missing-semi-warning nil)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(add-to-list 'auto-mode-alist '("\\.es6\\'" . js2-jsx-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode))

(use-package json-mode
  :pin melpa
  :ensure t)

(setq js-indent-level tab-width)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Emacs Code Browser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/ecb")
(require 'ecb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Theming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load theme
(load-theme 'zenburn t)

(set-fringe-mode '(8 . 0))

(let ((bg   "#4F4F4F")
      (fg   "#DCDCCC")
      (bg-1 "#383838"))
  (set-face-attribute 'header-line nil
                      :box nil)
  (set-face-attribute 'tabbar-default nil
                      :background bg
                      :foreground bg
                      :box nil)
  (set-face-attribute 'tabbar-selected nil
                      :background bg-1
                      :foreground fg
                      :box (list :line-width 5 :color bg-1 :style nil))
  (set-face-attribute 'tabbar-unselected nil
                      :inherit 'shadow
                      :background bg
                      :box (list :line-width 5 :color bg :style nil))
  (set-face-attribute 'tabbar-separator nil
                      :background bg-1
                      :foreground bg-1
                      :box nil)
  (set-face-attribute 'tabbar-highlight nil
                      :inherit 'tabbar-selected
                      :underline nil)
  (set-face-attribute 'linum nil :background (face-attribute 'fringe :background))
  (set-face-attribute 'mode-line nil :box nil)
  (set-face-attribute 'mode-line-inactive nil :box nil)
  (set-face-attribute 'vertical-border nil :foreground bg-1))
