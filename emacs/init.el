;;; init.el --- Emacs configuration file

;;; # leaf (package manager)

(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))

  (package-initialize)

  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)
    :config (leaf-keywords-init))

  (leaf leaf-convert
    :ensure t)
  
  (leaf leaf-tree
    :ensure t
    :custom ((imenu-list-size . 30)
             (imenu-list-position . 'left))))

;;; # general functions

(defun insert-current-date () (format-time-string "%Y-%m-%d" (current-time)))

(defun insert-current-scheduled-date () (format-time-string "%Y-%m-%d %a" (current-time)))

;;; # setting for each os

(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

;;; # built-in packages

(leaf cus-edit
  :tag "builtin" "faces" "help"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

(leaf cus-start
  :tag "builtin" "internal"
  :init
  (global-display-line-numbers-mode)
  (line-number-mode)
  (column-number-mode)
  :bind (([?¥] . [?\\])
         ("s-[" . indent-region)
         ("s-r" . replace-string)
         ("s-R" . replace-regexp)
         ("s-/" . comment-region))
  :custom ((user-login-name . "yuta")
	         (create-lockfiles . nil)
	         (tab-width . 2)
	         (debug-on-error . nil)
           (init-file-debug . t)
	         (ring-bell-function . 'ignore)
	         (indent-tabs-mode . nil)
           (menu-bar-mode . nil)
           (tool-bar-mode . nil)
           (scroll-bar-mode . nil))
  :config
  (defalias 'yes-or-no-p 'y-or-n-p))

(leaf autorevert
  :tag "builtin"
  :global-minor-mode global-auto-revert-mode)

(leaf paren
  :tag "builtin"
  :global-minor-mode show-paren-mode)

(leaf delsel
  :tag "builtin"
  :global-minor-mode delete-selection-mode)

(leaf startup
  :tag "builtin"
  :config
  (setq auto-save-list-file-prefix nil))

(leaf electric
  :tag "builtin"
  :init (electric-pair-mode t))

(leaf dired
  :doc "directory-browsing commands"
  :tag "builtin"
  :bind (("s-i" . find-name-dired))
  :config
  (setq dired-listing-switches "-lha")
  (setq dired-dwim-target t)
  (setq dired-recursive-copies 'always))

(leaf simple
  :tag "builtin"
  :bind (("M-z" . toggle-truncate-lines)))

(leaf files
  :tag "builtin"
  :config
  (setq make-backup-files nil)
  (setq auto-save-default nil)
  (setq auto-save-timeout 15)
  (setq auto-save-interval 120)
  (setq auto-save-visited-interval 30)
  (auto-save-visited-mode t)
  (setq auto-save-file-name-transforms
        `((".*" ,(concat user-emacs-directory "auto-save/") t)))
  (setq backup-directory-alist
        `(("." . ,(expand-file-name
                   (concat user-emacs-directory "backups"))))))

(leaf frame
  :tag "builtin"
  :bind (("s-@" . other-frame))
  :config
  (setq frame-title-format "%f"))

(leaf autoinsert
  :tag "builtin"
  :config
  (auto-insert-mode t)
  (define-auto-insert 'org-mode
    '(""
      "#+TITLE: " (file-name-base (buffer-file-name)) "\n"
      "#+AUTHOR: Yuta Natsui\n"
      "#+DATE: " (insert-current-date) "\n"
      "#+STARTUP: latexpreview\n"
      "#+TAGS: \n\n"
      "* :toc: \n\n"
      "* 概要\n"
      ":PROPERTIES:\n"
      ":CREATED: " (insert-current-date) "\n"
      ;; file path from home directory (replace char '/' to '-' and remove '.org')
      ":ID: " (replace-regexp-in-string "/" "-" (replace-regexp-in-string ".org" "" (file-relative-name (buffer-file-name) (expand-file-name "~/"))) t t) "\n"
      ":END:\n\n")))

;;; appearence

(leaf my/font
  :config
  (let* ((family "Cica")
         (fontspec (font-spec :family family)))
    (set-face-attribute 'default nil :family family :height 140)
    (set-fontset-font nil 'ascii fontspec nil 'append)
    (set-fontset-font nil 'japanese-jisx0208 fontspec nil 'append)
    (set-fontset-font nil '(#x01F000 . #x01FFFF) "Twitter Color Emoji" nil 'prepend)))

;;; ずれ確認用 半角40字、全角20字
;;; AIfUEaiueoAIUEOaiueoAIUEOaiueoAIUEOaiueo ASCII英字
;;; 0123456789012345678901234567890123456789 ASCII数字
;;; ｱｲｳｴｵｱｲｳｴｵｱｲｳｴｵｱｲｳｴｵｱｲｳｴｵｱｲｳｴｵｱｲｳｴｵｱｲｳｴｵ JIS X 0201ｶﾅ
;;; あいうえおあいうえおあいうえおあいうえお JIS X 0208ひらがな
;;; アイウエオアイウエオアイウエオアイウエオ 同カタカナ
;;; ＡＢＣＤＥＡＢＣＤＥＡＢＣＤＥＡＢＣＤＥ 同英字
;;; 亜唖娃阿哀亜唖娃阿哀亜唖娃阿哀亜唖娃阿哀 同漢字
;;; 𠀋𡈽𡌛𡑮𡢽𠀋𡈽𡌛𡑮𡢽𠀋𡈽𡌛𡑮𡢽𠀋𡈽𡌛𡑮𡢽 JIS X 0213漢字
;;; 🤔😱😊‼️🈲🤔😱😊‼️🈲🤔😱😊‼️🈲🤔😱😊‼️🈲
;;; 🀄🀅🀆🀇🀈🀄🀅🀆🀇🀈🀄🀅🀆🀇🀈🀄🀅🀆🀇🀈 絵文字

(leaf doom-themes
  :ensure t
  :config
  (load-theme 'doom-dracula t)
  (doom-themes-org-config))

(leaf rainbow-delimiters
  :ensure t
  :hook ((prog-mode-hook tuareg-mode-hook emacs-lisp-mode-hook) . rainbow-delimiters-mode))

(leaf doom-modeline
  :ensure t
  :init (doom-modeline-mode))

(leaf nerd-icons
  :ensure t)

;;; system settings

(leaf mozc
  :ensure t
  :config
  (set-language-environment "Japanese")
  (setq default-input-method "japanese-mozc")
  (prefer-coding-system 'utf-8))

(leaf auto-async-byte-compile
  :preface
  (defun auto-compile-inits ()
	  "Byte-compile Lisp files modified in the directory."
	  (interactive)
	  (byte-recompile-directory (expand-file-name "~/.emacs.d") 0))
  :ensure t
  :hook
  (emacs-lisp-mode-hook . enable-auto-async-byte-compile-mode)
  (kill-emacs-hook . auto-compile-inits)
  :config
  (setq load-prefer-newer t))

(leaf buffer-move
  :ensure t
  :bind (("C-c <up>" . buf-move-up)
	       ("C-c <down>" . buf-move-down)
	       ("C-c <left>" . buf-move-left)
	       ("C-c <right>" . buf-move-right)))

(leaf win-switch
  :ensure t
  :config
  (win-switch-setup-keys-ijkl "\C-xo"))

(setq delete-by-moving-to-trash t)

(setq inhibit-startup-message t)

(setq use-short-answers t)

(defalias 'quit 'kill-emacs)

(setq display-buffer-alist
      '(("*Warnings*"
         (display-buffer-reuse-window display-buffer-same-window)
         (inhibit-same-window . nil))))

(leaf saveplace
  :ensure t
  :config (save-place-mode t))

(leaf activities
  :ensure t
  :init
  (activities-mode)
  :bind (("C-x C-a C-n" . activities-new)
         ("C-x C-a C-d" . activities-define)
         ("C-x C-a C-a" . activities-resume)
         ("C-x C-a C-s" . activities-suspend)
         ("C-x C-a C-k" . activities-kill)
         ("C-x C-a RET" . activities-switch)
         ("C-x C-a b" . activities-switch-buffer)
         ("C-x C-a g" . activities-revert)
         ("C-x C-a l" . activities-list)))

(leaf undo-fu-session
  :ensure t
  :config
  (undo-fu-session-global-mode))

(leaf maxframe
  :ensure t
  :hook (window-setup-hook . maximize-frame)
  :config
  (leaf my-move-frame
    :doc
    "Move and maximize all frames to the next monitor in sequence."
    :preface
    (defun move-and-maximize-all-frames-to-monitor (monitor-index)
      "Move all Emacs frames to the specified monitor by index and maximize them."
      (let* ((frame-list (frame-list))
             (screen-attributes (display-monitor-attributes-list)))
        (if (and (>= monitor-index 0)
                 (< monitor-index (length screen-attributes)))
            (let ((target-monitor (nth monitor-index screen-attributes)))
              (let* ((geometry (cdr (assq 'geometry target-monitor)))
                     (x (nth 0 geometry))
                     (y (nth 1 geometry))
                     (width (nth 2 geometry))
                     (height (nth 3 geometry)))
                (dolist (frame frame-list)
                  ;; フレームを指定モニターに移動
                  (set-frame-position frame x y)
                  ;; フレームを最大化
                  (x-maximize-frame frame))))
          (error "Invalid monitor index"))))
    (defun move-and-maximize-all-frames-to-next-monitor ()
      "Move all Emacs frames to the next monitor in sequence and maximize them."
      (interactive)
      (let* ((screen-attributes (display-monitor-attributes-list))
             (num-monitors (length screen-attributes)))
        (if (= num-monitors 0)
          (error "No monitors detected")
        ;; Get the current monitor index
        (let* ((current-monitor (frame-monitor-attributes))
               (current-geometry (assq 'geometry current-monitor))
               (current-index (cl-position current-geometry screen-attributes
                                           :test (lambda (a b) (equal (cdr a) (cdr (assq 'geometry b)))))))
          (if (null current-index)
              (error "Unable to determine current monitor index")
            ;; Compute the next monitor index
            (let ((next-index (mod (1+ current-index) num-monitors)))
              ;; Move and maximize all frames to the next monitor
              (move-and-maximize-all-frames-to-monitor next-index)
              (message "Moved all frames to monitor %d" next-index)))))))
    :bind
    (("C-c m" . move-and-maximize-all-frames-to-next-monitor))))

(leaf which-key
  :ensure t
  :hook (after-init-hook . which-key-mode))

;;; edit

(leaf tempel
  :ensure t
  :bind (("M-+" . tempel-complete)
	       ("M-*" . temple-insert)))

(leaf counsel
  :ensure t
  :init (counsel-mode)
  :config
  (setq enable-recursive-minibuffers t))

(leaf ivy-rich
  :ensure t
  :hook (after-init-hook . ivy-rich-mode))

;;; tools

(leaf vterm
  :ensure t)

(leaf diff-hl
  :url "https://github.com/dgutov/diff-hl"
  :ensure t
  :init (global-diff-hl-mode))

(leaf magit
  :ensure t
  :bind ("C-x g" . magit-status))

;;; languages

(leaf macrostep
  :ensure t
  :bind (("C-c e" . macrostep-expand)))

(leaf tuareg
  :ensure t
  :custom ((tuareg-support-metaocaml . t)))

(leaf dune
  :url "https://github.com/ocaml/dune"
  :ensure t)

(leaf dune-format
  :ensure t)

(leaf utop
  :ensure t)

(leaf opam-switch-mode
  :ensure t
  :hook (tuareg-mode-hook . opam-switch-mode))

(leaf haskell-mode
  :ensure t)

(leaf auctex
  :ensure t
  :config
  (setq-default TeX-master nil)
  (setq exec-path (append '("/usr/local/bin" "/Library/TeX/texbin" "/Applications/Skim.app/Contents/SharedSupport") exec-path))
  (leaf auctex-latexmk
    :ensure t
    :config
    (auctex-latexmk-setup)))

(leaf ebib
  :ensure t)

(leaf bibtex
  :ensure t
  :config
  (setq bibtex-autokey-year-length 4
        bibtex-autokey-name-year-separator "-"
        bibtex-autokey-year-title-separator "-"
        bibtex-autokey-titleword-separator "-"
        bibtex-autokey-titlewords 2
        bibtex-autokey-titlewords-stretch 1
        bibtex-autokey-titleword-length 5))

(leaf markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode))

(leaf pdf-tools
  :ensure t
  :init (pdf-tools-install)
  :hook (pdf-view-mode-hook . (lambda () (display-line-numbers-mode -1)))
  :bind (:pdf-view-mode-map
         ("C-s" . isearch-forward))
  :config
  (setq pdf-view-resize-factor 1.0))

(leaf lsp-mode
  :ensure t
  :commands lsp
  :hook ((tuareg-mode-hook . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  (setq lsp-ocaml-lsp-server-command '("ocamllsp" "--fallback-read-dot-merlin")))

(leaf lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode))

(leaf lsp-ivy
  :ensure t)

(leaf company
  :ensure t
  :hook (prog-mode-hook latex-mode-hook emacs-lisp-mode-hook tuareg-mode-hook))

(leaf flycheck
  :ensure t
  :hook (text-mode-hook latex-mode-hook emacs-lisp-mode-hook lsp-mode)
  :bind (("C-c C-n" . flycheck-next-error)
	       ("C-c C-p" . flycheck-previous-error))
  :config
  (setq flycheck-grammarly-check-time 0.8))

(leaf projectile
  :ensure t
  :init (projectile-mode)
  :bind (:projectile-mode-map
         ("C-c p" . projectile-command-map)))

(leaf copilot
  :el-get (copilot
           :type github
           :pkgname "zerolfx/copilot.el")
  :hook ((tuareg-mode-hook) . copilot-mode)
  :bind (:copilot-completion-map
         ("<tab>" . my/copilot-tab)
         ("TAB" . my/copilot-tab)
         ("C-TAB" . copilot-accept-completion-by-word)
         ("C-<tab>" . copilot-accept-completion-by-word))
  :config
  (defun my/copilot-tab ()
    (interactive)
    (or (copilot-accept-completion)
        (indent-for-tab-command)))
  (setopt copilot-indent-offset-warning-disable t))



;;; org

(leaf org
  :ensure t
  :bind (("C-c a" . org-agenda)
	       ("C-c l" . org-store-link))
  :config
  (setq org-id-link-to-org-use-id t)
  (setq org-startup-with-inline-images t)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
  (setq org-log-done 'time)
  (setq org-todo-keywords '((sequence "TODO(t)" "WIP(w)" "|" "DONE(d)" "CANCEL(c)")))
  (setq org-agenda-files (directory-files-recursively "~/pro/" "org$"))
  (setq org-agenda-start-on-weekday 0)
  (setq org-agenda-use-time-grid nil)
  (setq org-agenda-skip-additional-timestamps-same-entry nil)
  (setq org-agenda-span 'day)
  (setq org-agenda-include-diary t)
  (setq org-agenda-prefix-format
	      '((agenda . " %i %-12:c%?-12t% s")
          (todo . " %i %-12:c")
          (tags . " %i %-12:c")
          (search . " %i %-12:c")))
  (setq org-agenda-date-format "%Y-%m-%d (%a)")
  (setq org-agenda-time-grid
	      '((daily today require-timed)
          (800 1000 1200 1400 1600 1800 2000)
          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (setq org-agenda-deadline-leaders
	      '("締切:今日!   " "締切:あと%2d日 " "締切:過ぎ%2d日"))
  (setq org-agenda-scheduled-leaders
	      '("予定:今日     " "予定:あと%2d日 "))
  (setq org-agenda-window-setup 'other-window)

  (leaf org-ref
    :ensure t
    :bind (:org-mode-map
           ("C-c ]" . org-ref-insert-link))
    :config
    (setq bibtex-completion-bibliography '("~/pro/notes/my.bib"))
    (setq bibtex-completion-pdf-open-function (lambda (fpath) (call-process "open" nil 0 nil fpath))))

  (leaf org-bullets
    :ensure t
    :hook (org-mode-hook . org-bullets-mode))

  (leaf toc-org
    :ensure t
    :hook (org-mode-hook . toc-org-mode))

  (leaf org-babel
    :config
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((ocaml . t)
       (haskell . t)
       (emacs-lisp . t))))

  (leaf org-super-agenda
    :preface
    (defun day-of-time (time)
      (nth 3 (decode-time time)))
    
    (defun month-of-time (time)
      (nth 4 (decode-time time)))

    (defun year-of-time (time)
      (nth 5 (decode-time time)))

    (defun time-of-days (day month year)
      (encode-time 0 0 0 day month year))

    (defun days-equal (time1 time2)
      (and
       (= (day-of-time time1) (day-of-time time2))
       (= (month-of-time time1) (month-of-time time2))
       (= (year-of-time time1) (year-of-time time2))))
    
    (defun begin-of-day-of-time (time)
      (encode-time 0 0 0 (day-of-time time) (month-of-time time) (year-of-time time)))
    
    (defun end-of-day-of-time (time)
      (encode-time 59 59 23 (day-of-time time) (month-of-time time) (year-of-time time)))

    (defun day-of-week-of-time (time)
      (calendar-day-of-week (list (month-of-time time) (day-of-time time) (year-of-time time))))
    
    (defun begin-of-weeks (time)
      (let* ((year (year-of-time time))
             (month (month-of-time time))
             (day (day-of-time time))
             (day-of-week (day-of-week-of-time time))
             (days-to-monday (mod (- day-of-week 1) 7))
             (monday-time (time-subtract time (days-to-time days-to-monday))))
        monday-time))

    (defun end-of-weeks (time)
      (let* ((year (year-of-time time))
             (month (month-of-time time))
             (day (day-of-time time))
             (day-of-week (day-of-week-of-time time))
             (days-to-monday (mod (- 7 day-of-week) 7))
             (monday-time (time-add time (days-to-time days-to-monday))))
        monday-time))

    (defun begin-of-month (time)
      (encode-time 0 0 0 1 (month-of-time time) (year-of-time time)))

    (defun end-of-month (time)
      (let* ((month (month-of-time time))
             (year (year-of-time time))
             (first-of-next-month
              (if (= month 12)
                  (encode-time 0 0 0 1 1 (1+ year))
                (encode-time 0 0 0 1 (1+ month) year))))
        (time-subtract first-of-next-month (days-to-time 1))))

    (defun skip-after-today ()
      (let* ((scheduled (org-entry-get nil "SCHEDULED"))
             (deadline (org-entry-get nil "DEADLINE"))
             (scheduled-time (and scheduled (org-time-string-to-time scheduled)))
             (deadline-time (and deadline (org-time-string-to-time deadline)))
             (today (begin-of-day-of-time (current-time))))
        (unless (or (and scheduled-time (time-less-p scheduled-time today))
                    (and deadline-time (time-less-p deadline-time today)))
          (save-excursion
            (or (outline-next-heading) (point-max))))))

    (defun skip-no-today ()
      (let* ((scheduled (org-entry-get nil "SCHEDULED"))
             (deadline (org-entry-get nil "DEADLINE"))
             (scheduled-time (and scheduled (org-time-string-to-time scheduled)))
             (deadline-time (and deadline (org-time-string-to-time deadline)))
             (today (org-time-string-to-time (format-time-string "%Y-%m-%d" (current-time)))))
        (unless (or (and scheduled-time (days-equal today scheduled-time))
                     (and deadline-time (days-equal today deadline-time)))
            (save-excursion
              (or (outline-next-heading) (point-max))))))

    (defun skip-before-today ()
      (let* ((scheduled (org-entry-get nil "SCHEDULED"))
             (deadline (org-entry-get nil "DEADLINE"))
             (scheduled-time (and scheduled (org-time-string-to-time scheduled)))
             (deadline-time (and deadline (org-time-string-to-time deadline)))
             (today (end-of-day-of-time (current-time))))
        (unless (or (and scheduled-time (time-less-p today scheduled-time))
                    (and deadline-time (time-less-p today deadline-time)))
          (save-excursion
            (or (outline-next-heading) (point-max))))))

    (defun skip-after-this-week ()
      (let* ((scheduled (org-entry-get nil "SCHEDULED"))
             (deadline (org-entry-get nil "DEADLINE"))
             (scheduled-time (and scheduled (org-time-string-to-time scheduled)))
             (deadline-time (and deadline (org-time-string-to-time deadline)))
             (today (current-time))
             (sow (begin-of-day-of-time (begin-of-weeks today))))
        (unless (or (and scheduled-time (time-less-p scheduled-time sow))
                    (and deadline-time (time-less-p deadline-time sow)))
          (save-excursion
            (or (outline-next-heading) (point-max))))))

    (defun skip-before-this-week ()
      (let* ((scheduled (org-entry-get nil "SCHEDULED"))
             (deadline (org-entry-get nil "DEADLINE"))
             (scheduled-time (and scheduled (org-time-string-to-time scheduled)))
             (deadline-time (and deadline (org-time-string-to-time deadline)))
             (today (current-time))
             (eow (end-of-day-of-time (end-of-weeks today))))
        (unless (or (and scheduled-time (time-less-p eow scheduled-time))
                    (and deadline-time (time-less-p eow deadline-time)))
          (save-excursion
            (or (outline-next-heading) (point-max))))))

    (defun skip-no-this-week ()
      (let* ((scheduled (org-entry-get nil "SCHEDULED"))
             (deadline (org-entry-get nil "DEADLINE"))
             (scheduled-time (and scheduled (org-time-string-to-time scheduled)))
             (deadline-time (and deadline (org-time-string-to-time deadline)))
             (today (current-time))
             (sow (begin-of-day-of-time (begin-of-weeks today)))
             (eow (end-of-day-of-time (end-of-weeks today))))
        (unless (or (and scheduled-time (time-less-p sow scheduled-time) (time-less-p scheduled-time eow))
                    (and deadline-time (time-less-p sow deadline-time) (time-less-p deadline-time eow)))
          (save-excursion
            (or (outline-next-heading) (point-max))))))

    (defun skip-scheduled ()
      (let* ((scheduled (org-entry-get nil "SCHEDULED"))
             (deadline (org-entry-get nil "DEADLINE"))
             (scheduled-time (and scheduled (org-time-string-to-time scheduled)))
             (deadline-time (and deadline (org-time-string-to-time deadline)))
             (today (org-time-string-to-time (format-time-string "%Y-%m-%d" (current-time)))))
        (if (or scheduled-time deadline-time)
            (save-excursion
              (or (outline-next-heading) (point-max))))))

    (defun skip-no-this-month ()
      (let* ((scheduled (org-entry-get nil "SCHEDULED"))
             (deadline (org-entry-get nil "DEADLINE"))
             (scheduled-time (and scheduled (org-time-string-to-time scheduled)))
             (deadline-time (and deadline (org-time-string-to-time deadline)))
             (today (current-time))
             (som (begin-of-day-of-time (begin-of-month today)))
             (eom (end-of-day-of-time (end-of-month today))))
        (unless (or (and scheduled-time (time-less-p som scheduled-time) (time-less-p scheduled-time eom))
                    (and deadline-time (time-less-p som deadline-time) (time-less-p deadline-time eom)))
          (save-excursion
            (or (outline-next-heading) (point-max))))))

    (defun skip-before-this-month ()
      (let* ((scheduled (org-entry-get nil "SCHEDULED"))
             (deadline (org-entry-get nil "DEADLINE"))
             (scheduled-time (and scheduled (org-time-string-to-time scheduled)))
             (deadline-time (and deadline (org-time-string-to-time deadline)))
             (today (current-time))
             (eom (end-of-day-of-time (end-of-month today))))
        (unless (or (and scheduled-time (time-less-p eom scheduled-time))
                    (and deadline-time (time-less-p eom deadline-time)))
          (save-excursion
            (or (outline-next-heading) (point-max))))))
    
    (defun skip-after-this-month ()
      (let* ((scheduled (org-entry-get nil "SCHEDULED"))
             (deadline (org-entry-get nil "DEADLINE"))
             (scheduled-time (and scheduled (org-time-string-to-time scheduled)))
             (deadline-time (and deadline (org-time-string-to-time deadline)))
             (today (current-time))
             (som (begin-of-day-of-time (begin-of-month today))))
        (unless (or (and scheduled-time (time-less-p scheduled-time som))
                    (and deadline-time (time-less-p deadline-time som)))
          (save-excursion
            (or (outline-next-heading) (point-max))))))
    
    :ensure t
    :init
    (org-super-agenda-mode)
    :config
    (setq org-agenda-custom-commands
          '(("ad" "Today's agenda"
             ((agenda "Today's agenda"
                      ((org-agenda-overriding-header "Today's agenda")
                       (org-agenda-span 'day)
                       (org-super-agenda-groups '((:name "Missed scheduled tasks"
                                                         :scheduled past
                                                         :order 1)
                                                  (:name "Missed deadline tasks"
                                                         :deadline past
                                                         :order 2)
                                                  (:name "Today's scheduled tasks"
                                                         :scheduled today
                                                         :order 3)
                                                  (:name "Today's deadline tasks"
                                                         :deadline today
                                                         :order 4)))))))

            ("aw" "This week's agenda"
             ((agenda "This week's agenda"
                      ((org-agenda-overriding-header "This week's agenda")
                       (org-agenda-span 'week)
                       (org-super-agenda-groups '((:anything t)))))))

            ("tt" "Today's task (Tag)"
             ((alltodo "Missed tasks"
                       ((org-agenda-overriding-header "Missed tasks")
                        (org-agenda-skip-function 'skip-after-today)
                        (org-super-agenda-groups
                         '((:auto-tags t)))))
              (alltodo "Today's tasks"
                       ((org-agenda-overriding-header "Today's task")
                        (org-agenda-skip-function 'skip-no-today)
                        (org-super-agenda-groups
			                   '((:auto-tags t)))))
              (alltodo "Unscheduled tasks"
                       ((org-agenda-overriding-header "Unscheduled tasks")
                        (org-agenda-skip-function 'skip-scheduled)
                        (org-super-agenda-groups
                         '((:auto-tags t)))))
              (alltodo "Future tasks"
                       ((org-agenda-overriding-header "Future tasks")
                        (org-agenda-skip-function 'skip-before-today)
                        (org-super-agenda-groups
                         '((:auto-tags t)))))))

            ("td" "Today's task (Day)"
             ((alltodo "Missed tasks"
                       ((org-agenda-overriding-header "Missed tasks")
                        (org-agenda-skip-function 'skip-after-today)
                        (org-super-agenda-groups
                         '((:auto-planning t)))))
              (alltodo "Today's tasks"
                       ((org-agenda-overriding-header "Today's task")
                        (org-agenda-skip-function 'skip-no-today)
                        (org-super-agenda-groups
			                   '((:auto-planning t)))))
              (alltodo "Unscheduled tasks"
                       ((org-agenda-overriding-header "Unscheduled tasks")
                        (org-agenda-skip-function 'skip-scheduled)
                        (org-super-agenda-groups
                         '((:auto-tags t)))))
              (alltodo "Future tasks"
                       ((org-agenda-overriding-header "Future tasks")
                        (org-agenda-skip-function 'skip-before-today)
                        (org-super-agenda-groups
                         '((:auto-planning t)))))))

            ("wt" "This week's task (Tag)"
             ((alltodo "Previous week's tasks"
                       ((org-agenda-overriding-header "Previous week's tasks")
                        (org-agenda-skip-function 'skip-after-this-week)
                        (org-super-agenda-groups
                         '((:auto-tags t)))))
              (alltodo "This week's tasks"
                       ((org-agenda-overriding-header "This week's tasks")
                        (org-agenda-skip-function 'skip-no-this-week)
                        (org-super-agenda-groups
                         '((:auto-tags t)))))
              (alltodo "Unscheduled tasks"
                       ((org-agenda-overriding-header "Unscheduled tasks")
                        (org-agenda-skip-function 'skip-scheduled)
                        (org-super-agenda-groups
                         '((:auto-tags t)))))
              (alltodo "Next week's task"
                       ((org-agenda-overriding-header "Next week's tasks")
                        (org-agenda-skip-function 'skip-before-this-week)
                        (org-super-agenda-groups
                         '((:auto-tags t)))))))

            ("wd" "This week's task (Day)"
             ((alltodo "Previous week's tasks"
                       ((org-agenda-overriding-header "Previous week's tasks")
                        (org-agenda-skip-function 'skip-after-this-week)
                        (org-super-agenda-groups
                         '((:auto-planning t)))))
              (alltodo "This week's tasks"
                       ((org-agenda-overriding-header "This week's tasks")
                        (org-agenda-skip-function 'skip-no-this-week)
                        (org-super-agenda-groups
                         '((:auto-planning t)))))
              (alltodo "Unscheduled tasks"
                       ((org-agenda-overriding-header "Unscheduled tasks")
                        (org-agenda-skip-function 'skip-scheduled)
                        (org-super-agenda-groups
                         '((:auto-tags t)))))
              (alltodo "Next week's task"
                       ((org-agenda-overriding-header "Next week's tasks")
                        (org-agenda-skip-function 'skip-before-this-week)
                        (org-super-agenda-groups
                         '((:auto-planning t)))))))

            ("mt" "This month's task (Tag)"
             ((alltodo "Previous month's tasks"
                       ((org-agenda-overriding-header "Previous month's tasks")
                        (org-agenda-skip-function 'skip-after-this-month)
                        (org-super-agenda-groups
                         '((:auto-tags t)))))
              (alltodo "This month's tasks"
                       ((org-agenda-overriding-header "This month's tasks")
                        (org-agenda-skip-function 'skip-no-this-month)
                        (org-super-agenda-groups
                         '((:auto-tags t)))))
              (alltodo "Unscheduled tasks"
                       ((org-agenda-overriding-header "Unscheduled tasks")
                        (org-agenda-skip-function 'skip-scheduled)
                        (org-super-agenda-groups
                         '((:auto-tags t)))))
              (alltodo "Next month's tasks"
                       ((org-agenda-overriding-header "Next month's tasks")
                        (org-agenda-skip-function 'skip-before-this-month)
                        (org-super-agenda-groups
                         '((:auto-tags t)))))))

            ("md" "This month's task (Day)"
             ((alltodo "Previous month's tasks"
                       ((org-agenda-overriding-header "Previous month's tasks")
                        (org-agenda-skip-function 'skip-after-this-month)
                        (org-super-agenda-groups
                         '((:auto-planning t)))))
              (alltodo "This month's tasks"
                       ((org-agenda-overriding-header "This month's tasks")
                        (org-agenda-skip-function 'skip-no-this-month)
                        (org-super-agenda-groups
                         '((:auto-planning t)))))
              (alltodo "Unscheduled tasks"
                       ((org-agenda-overriding-header "Unscheduled tasks")
                        (org-agenda-skip-function 'skip-scheduled)
                        (org-super-agenda-groups
                         '((:auto-tags t)))))
              (alltodo "Next month's tasks"
                       ((org-agenda-overriding-header "Next month's tasks")
                        (org-agenda-skip-function 'skip-before-this-month)
                        (org-super-agenda-groups
                         '((:auto-planning t)))))))

	          ("u" "Unscheduled task (Tag)"
	           ((alltodo "Unscheduled"
                       ((org-agenda-overriding-header "Unscheduled task")
                        (org-agenda-skip-function 'skip-scheduled)
                        (org-super-agenda-groups
                         '((:auto-tags t)))))))))))


;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

(provide 'init)
;;; init.el ends here
