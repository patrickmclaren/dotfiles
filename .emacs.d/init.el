(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)

;; Empty scratch buffer
(setq initial-scratch-message nil)

;; Use y/n in place of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Turn off mouse interface
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)

;; Don't keep dirty temp files
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Use spaces instead of tabs, and set the default indent
;; width to be 4 spaces. Need to use setq-default because of
;; `web-mode`.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default indent-line-function 'insert-tab)

(setq sh-basic-offset tab-width)
(setq sh-indentation tab-width)

(setq-default truncate-lines t)
(setq-default scss-compile-at-save nil)

;; Use smaller spaces for HTML
(defvar html-offset 2)

;; Setup auto-complete
(add-to-list 'load-path "~/.emacs.d/auto-complete")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/ac-dict")
(ac-config-default)
(setq abbrev-file-name "~/.emacs.d/abbrev_defs")

;; Mark and delete a region, rather than inserting text
(delete-selection-mode 1)

;; Narrow to region hides the reset of the buffer.
;; Use C-x n n to narrow
;;     C-x n w to widen
(put 'narrow-to-region 'disabled nil)

;; Use ssh for tramp by default
(setq tramp-default-method "ssh")

;; Use aspell for spell checking
(setq-default ispell-program-name "/usr/bin/aspell")

;; Setup org-mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'load-path "~/.emacs.d/org-mode/contrib/lisp")

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-startup-indented t)
(setq org-agenda-default-appointment-duration nil)
(setq org-agenda-files (list "~/life/org/life.org"))
(setq org-log-done 'time)

(setq org-capture-templates
      '(("n" "Note" entry (file+headline "~/life/org/life.org" "Notes")
         "* %?\nEntered on %U\n")
        ("t" "Task" entry (file+headline "~/life/org/life.org" "Tasks")
         "* TODO [#C] %^{Enter task details} %^G\nSCHEDULED: %T")
        ("s" "Shopping" entry (file+headline "~/life/org/life.org" "Misc")
         "* %^{Enter item}")
        ("b" "BSC Task" entry (file+headline "~/life/org/life.org" "Bachelor of Science")
         "* TODO %^{Enter task details} %?%^G\nSCHEDULED: %T")))

(setq org-directory "~/life/org/")
(add-to-list 'org-agenda-files '"~/life/org/life.org")

;; Enable upcase and downcase commands
;; Usage:
;;   Upcase:   C-x C-u
;;   Downcase: C-x C-l
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Easily move from one window to another.
;; Usage: M-<left>|<right>|<up>|<down>
(windmove-default-keybindings 'meta)

;; Display column number next to line number in the mode-line.
(column-number-mode t)

;; Display line numbers next to the buffer.
(global-linum-mode t)

;; Load up-to-date python-mode
(setq py-install-directory "~/.emacs.d/python-mode")
(add-to-list 'load-path py-install-directory)
(require 'python-mode)

;; Setup web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(defun set-web-mode-offset ()
  "Make the offset throughout web-mode smaller."
  (setq web-mode-markup-indent-offset html-offset)
  (setq web-mode-css-indent-offset html-offset)
  (setq web-mode-code-indent-offset html-offset))

(add-hook 'web-mode-hook 'set-web-mode-offset)

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

(setq ruby-align-chained-calls t)
(setq ruby-align-to-stmt-keywords nil)
(setq ruby-deep-indent-paren nil)
(setq ruby-deep-indent-paren-style nil)
(setq ruby-use-smie nil)

(add-to-list 'auto-mode-alist '("\\.rabl\\'" . ruby-mode))

;; Robe (code navigation, documentation lookup,
;; and completion for ruby)
(add-hook 'ruby-mode-hook 'robe-mode)

(add-hook 'ruby-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("\\(?:->\\)" . font-lock-keyword-face)))))

; Company-mode
;(eval-after-load 'company
;  '(push 'company-robe company-backends))

; Auto-complete
(add-hook 'robe-mode-hook 'ac-robe-setup)

;; Setup multiple-cursors - just like IntelliJ.
(require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-next-previous-link-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Winner-mode allows you to restore window configurations.
;; Usage: C-c <left> | <right>
(when (fboundp 'winner-mode)
  (winner-mode 1))

;; Show matching parentheses immediately.
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Stuff for Privy
(add-to-list 'auto-mode-alist '("\\.coffee.*\\'" . coffee-mode))

(setq coffee-tab-width 2
      require-final-newline t)

;; Auto close pairs
(electric-pair-mode)

;; Sr-speedbar
(global-set-key (kbd "C-x <") 'sr-speedbar-toggle)

(setq speedbar-show-unknown-files t
      sr-speedbar-right-side nil
      sr-speedbar-width-x 200
      sr-speedbar-delete-windows t
      sr-speedbar-skip-other-window-p t)

;(make-face 'speedbar-face)
;(set-face-font 'speedbar-face "Inconsolata-9")
;(add-hook 'speedbar-mode-hook
;          (lambda () (buffer-set-face 'speedbar-face)))


;; Magit
(global-set-key (kbd "C-x g s") 'magit-status)

;; iBuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

(setq ibuffer-formats
      '((mark modified read-only
              " "
              (name 18 -1 :left :elide)
              " "
              (size 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " " filename-and-process)
        (mark 
         " " (name 16 -1)
         " " filename)))

;; Bookmark+
(setq bookmark-default-file "~/.emacs.d/bookmarks"
      bmkp-auto-light-when-set 'all-in-buffer
      bmkp-auto-light-when-jump 'all-in-buffer
      bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks"
      bmkp-bmenu-state-file "~/.emacs.d/.emacs-bmk-bmenu-state.el")

;; Ensime (Scala)
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(add-hook 'ensime-scala-mode-hook
          (lambda ()
            (setq debug-on-error t)))


; Disable auto-complete in favor of company-mode 
(add-hook 'scala-mode-hook
          (lambda ()
            (auto-complete-mode 0)))

;; Dired-x
(load-library "dired-x")

;; Load theme
(load-theme 'zenburn t)

;; Smart Mode Line
(sml/setup)
(setq sml/theme 'dark)
(setq sml/show-encoding nil)
(setq sml/shorten-directory t)

;; Recent Files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; GitGutter
(global-git-gutter-mode t)
(git-gutter:linum-setup)

;; Projectile
(projectile-global-mode)

;; ido-mode
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; Sublimity
(require 'sublimity)
(require 'sublimity-scroll)
(require 'sublimity-map)

(sublimity-map-set-delay nil)

(global-set-key (kbd "C-x m") 'sublimity-mode)

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

;; Auto-indent
(setq auto-indent-on-visit-file t)

;; desktop-mode
(setq desktop-dirname "~/.emacs.d/desktop"
      desktop-base-file-name "emacs.desktop"
      desktop-base-lock-name "lock"
      desktop-path (list desktop-dirname)
      desktop-save t
      desktop-load-locked-desktop nil)

(desktop-save-mode 1)

;; tabbar-mode
(tabbar-mode 1)

;; Jump to init.el
(global-set-key (kbd "S-C-M-i")
                (lambda ()
                  (interactive)
                  (find-file "~/.emacs.d/init.el")))
