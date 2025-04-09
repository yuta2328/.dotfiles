;;; init.el --- Emacs configuration file

;;; leaf (package manager)

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

(leaf top-level
  :preface
  (defvar is-mac (eq system-type 'darwin))
  (defvar is-linux (eq system-type 'gnu/linux))
  :config

  (leaf system-setting
    :custom ((user-login-name . "yuta")
	           (create-lockfiles . nil)
	           (debug-on-error . t)
             (init-file-debug . t)
	           (ring-bell-function . 'ignore))
    :config
    (setq custom-file (locate-user-emacs-file "custom.el"))
    (setq delete-by-moving-to-trash t)
    (setq inhibit-startup-message t)
    (setq use-short-answers t)
    (setq display-buffer-alist
          '(("*Warnings*"
             (display-buffer-reuse-window display-buffer-same-window)
             (inhibit-same-window . nil))))
    (setq special-display-buffer-names '("*compilation*"))

    (leaf dired
      :doc "directory-browsing commands"
      :tag "builtin"
      :bind (("s-i" . find-name-dired))
      :config
      (setq dired-listing-switches "-lha")
      (setq dired-dwim-target t)
      (setq dired-recursive-copies 'always))

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

    (leaf vterm
      :ensure t)

    (leaf macrostep
      :ensure t
      :bind (("C-c e" . macrostep-expand)))

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

    (leaf mozc
      :ensure t
      :config
      (set-language-environment "Japanese")
      (setq default-input-method "japanese-mozc")
      (prefer-coding-system 'utf-8))

    (leaf buffer-move
      :ensure t
      :bind (("C-c <up>" . buf-move-up)
	           ("C-c <down>" . buf-move-down)
	           ("C-c <left>" . buf-move-left)
	           ("C-c <right>" . buf-move-right)))

    (leaf win-switch
      :ensure t
      :config
      (win-switch-setup-keys-ijkl "\C-xo")))
  
  (leaf common
    :global-minor-mode (global-auto-revert-mode
                        show-paren-mode
                        delete-selection-mode)
    :bind (([?¥] . [?\\])
           ("s-z" . undo)
           ("s-c" . kill-ring-save)
           ("s-[" . indent-region)
           ("s-r" . replace-string)
           ("s-R" . replace-regexp)
           ("s-/" . comment-region)
           ("s-n" . make-frame)
           ("s-w" . delete-frame)
           ("C-x C-0" . global-text-scale-adjust)
           ("C-x C-M-0" . text-scale-adjust)
           ("M-z" . toggle-truncate-lines)
           ("s-@" . other-frame))
    :custom ((tab-width . 2)
	           (indent-tabs-mode . nil))
    :config
    (defalias 'yes-or-no-p 'y-or-n-p)
    (setq gc-cons-threshold (* 512 1024 1024))
    (setq auto-save-list-file-prefix nil))

  (leaf appearance
    :init
    (global-display-line-numbers-mode)
    (line-number-mode)
    (column-number-mode)
    (electric-pair-mode t)
    :when (display-graphic-p)
    :custom ((menu-bar-mode . nil)
             (tool-bar-mode . nil)
             (scroll-bar-mode . nil))
    :config
    (setq frame-title-format "%f")

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
    
    (leaf my/font
      :config
      (let* ((family "Source Han Code JP")
             (fontspec (font-spec :family family :size 13)))
        (set-face-attribute 'default nil :family family :height 130)
        (set-fontset-font nil 'ascii fontspec nil 'append)
        (set-fontset-font nil 'japanese-jisx0208 fontspec nil 'append))
      (setq line-spacing 0.05))
                                        ; ずれ確認用 半角40字、全角20字
                                        ; AIfUEaiueoAIUEOaiueoAIUEOaiueoAIUEOaiueo ASCII英字
                                        ; 0123456789012345678901234567890123456789 ASCII数字
                                        ; ｱｲｳｴｵｱｲｳｴｵｱｲｳｴｵｱｲｳｴｵｱｲｳｴｵｱｲｳｴｵｱｲｳｴｵｱｲｳｴｵ JIS X 0201ｶﾅ
                                        ; あいうえおあいうえおあいうえおあいうえお JIS X 0208ひらがな
                                        ; アイウエオアイウエオアイウエオアイウエオ 同カタカナ
                                        ; ＡＢＣＤＥＡＢＣＤＥＡＢＣＤＥＡＢＣＤＥ 同英字
                                        ; 亜唖娃阿哀亜唖娃阿哀亜唖娃阿哀亜唖娃阿哀 同漢字
                                        ; 𠀋𡈽𡌛𡑮𡢽𠀋𡈽𡌛𡑮𡢽𠀋𡈽𡌛𡑮𡢽𠀋𡈽𡌛𡑮𡢽 JIS X 0213漢字

    (leaf doom-themes
      :ensure t
      :config
      (load-theme 'doom-dracula :no-confirm)
      (doom-themes-org-config))

    (leaf rainbow-delimiters
      :ensure t
      :hook ((latex-mode-hook prog-mode-hook tuareg-mode-hook emacs-lisp-mode-hook) . rainbow-delimiters-mode))

    (leaf doom-modeline
      :ensure t
      :init (doom-modeline-mode))

    (leaf nerd-icons
      :ensure t)

    (leaf volatile-highlights
      :ensure t
      :init (volatile-highlights-mode))

    (leaf which-key
      :init
      (which-key-mode)))

  (leaf edit-setting
    :config
    (leaf undo-fu-session
      :ensure t
      :config
      (undo-fu-session-global-mode))

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

    (leaf anzu
      :init (global-anzu-mode +1)
      :ensure t)

    (leaf diff-hl
      :url "https://github.com/dgutov/diff-hl"
      :ensure t
      :init (global-diff-hl-mode))

    (leaf magit
      :ensure t
      :bind ("C-x g" . magit-status))

    (leaf lang-setting
      :config
      (leaf lang-common-setting
        :config
        (leaf lsp-mode
          :ensure t
          :commands lsp
          :hook (((tuareg-mode-hook latex-mode-hook) . lsp)
                 (lsp-mode . lsp-enable-which-key-integration))
          :config
          (setq lsp-ocaml-lsp-server-command '("ocamllsp" "--fallback-read-dot-merlin")))

        (leaf lsp-ui
          :ensure t
          :hook (lsp-mode . lsp-ui-mode))

        (leaf lsp-ivy
          :ensure t)

        (leaf eglot
          :ensure t)

        (leaf company
          :ensure t
          :hook (prog-mode-hook latex-mode-hook emacs-lisp-mode-hook tuareg-mode-hook))

        (leaf flycheck
          :ensure t
          :hook (text-mode-hook latex-mode-hook emacs-lisp-mode-hook lsp-mode)
          :bind (:flycheck-mode-map
                 ("C-c M-n" . flycheck-next-error)
	               ("C-c M-p" . flycheck-previous-error))
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
                 ("<tab>" . copilot-accept-completion)
                 ("TAB" . copilot-accept-completion))
          :config
          (setopt copilot-indent-offset-warning-disable t)))
      
      (leaf org
        :bind (("C-c a" . org-agenda)
	             ("C-c l" . org-store-link))
        :config
        (setq org-use-tag-inheritance "^@")
        (setq org-startup-with-inline-images t)
        (setq org-log-done 'time)
        (setq org-todo-keywords '((sequence "TODO(t)" "WIP(w)" "|" "DONE(d)" "CANCEL(c)")))
        (org-link-set-parameters
         "tag"
         :follow (lambda (tag) (org-tags-view nil tag))
         :complete (lambda () (concat "tag:" (read-string "Tag: "))))
        (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.0))
        
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

        (leaf org-ql
          :ensure t
          :url "alphapapa/org-ql")

        (leaf org-babel
          :config
          (org-babel-do-load-languages
           'org-babel-load-languages
           '((ocaml . t)
             (haskell . t)
             (emacs-lisp . t))))

        (leaf org-agenda
          :config
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
          (setq org-agenda-window-setup 'other-window)

          (leaf org-super-agenda
            :preface
            (defun day-of-time (time) (nth 3 (decode-time time)))
            (defun month-of-time (time) (nth 4 (decode-time time)))
            (defun year-of-time (time) (nth 5 (decode-time time)))
            (defun time-of-days (day month year) (encode-time 0 0 0 day month year))
            (defun days-equal (time1 time2) (and
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
              (time-subtract time (days-to-time (mod (- (day-of-week-of-time time) 1) 7))))
            (defun end-of-weeks (time)
              (time-add time (days-to-time (mod (- 7 (day-of-week-of-time time)) 7))))
            (defun begin-of-month (time) (encode-time 0 0 0 1 (month-of-time time) (year-of-time time)))
            (defun end-of-month (time)
              (let* ((month (month-of-time time))
                     (year (year-of-time time))
                     (first-of-next-month
                      (if (= month 12)
                          (encode-time 0 0 0 1 1 (1+ year))
                        (encode-time 0 0 0 1 (1+ month) year))))
                (time-subtract first-of-next-month (days-to-time 1))))
            (defun gen-skip (pred)
              (let* ((scheduled (org-entry-get nil "SCHEDULED"))
                     (deadline (org-entry-get nil "DEADLINE"))
                     (scheduled-time (and scheduled (org-time-string-to-time scheduled)))
                     (deadline-time (and deadline (org-time-string-to-time deadline))))
                (when (funcall pred scheduled-time deadline-time)
                  (save-excursion (or (outline-next-heading) (point-max))))))
            (defun gen-skip-with-cond-on-defined (day-pred)
              (gen-skip (lambda (scheduled deadline)
                          (let* ((today (begin-of-day-of-time (current-time))))
                            (and (if scheduled (not (funcall day-pred scheduled today)) t)
                                 (if deadline (not (funcall day-pred deadline today)) t))))))
            (defun get-not-scheduled () (gen-skip (lambda (scheduled deadline) (or (not (eq nil scheduled)) (not (eq nil deadline))))))
            (defun get-before-today () (gen-skip-with-cond-on-defined (lambda (ts td) (time-less-p ts td))))
            (defun get-today () (gen-skip-with-cond-on-defined (lambda (ts td) (days-equal ts td))))
            (defun get-after-today () (gen-skip-with-cond-on-defined (lambda (ts td) (time-less-p td ts))))
            (defun get-before-this-week ()
              (gen-skip-with-cond-on-defined (lambda (ts td) (time-less-p ts (begin-of-day-of-time (begin-of-weeks today))))))
            (defun get-after-this-week ()
              (gen-skip-with-cond-on-defined (lambda (ts td) (time-less-p (end-of-day-of-time (end-of-weeks today)) ts))))
            (defun get-this-week ()
              (gen-skip-with-cond-on-defined (lambda (ts td) (and (time-less-p (begin-of-day-of-time (begin-of-weeks td)) ts) (time-less-p ts (end-of-day-of-time (end-of-weeks td)))))))
            (defun get-before-this-month ()
              (gen-skip-with-cond-on-defined (lambda (ts td) (time-less-p ts (begin-of-day-of-time (begin-of-month today))))))
            (defun get-after-this-month ()
              (gen-skip-with-cond-on-defined (lambda (ts td) (time-less-p (end-of-day-of-time (end-of-month today)) ts))))
            (defun get-this-month ()
              (gen-skip-with-cond-on-defined (lambda (ts td) (and (time-less-p (begin-of-day-of-time (begin-of-month td)) ts) (time-less-p ts (end-of-day-of-time (end-of-month td)))))))
            ;; (defun gen-todo-agenda-def (get-before get-this get-after get-non)
            ;;   '((alltodo "Missed tasks"
            ;;              ((org-agenda-overriding-header "Missed tasks")
            ;;               (org-agenda-skip-function #'get-before)
            ;;               (org-super-agenda-groups
            ;;                '((:auto-tags t)))))
            ;;     (alltodo "Today's tasks"
            ;;              ((org-agenda-overriding-header "Today's tasks")
            ;;               (org-agenda-skip-function #'get-this)
            ;;               (org-super-agenda-groups
            ;;                '((:auto-tags t)))))
            ;;     (alltodo "Future tasks"
            ;;              ((org-agenda-overriding-header "Future tasks")
            ;;               (org-agenda-skip-function #'get-after)
            ;;               (org-super-agenda-groups
            ;;                '((:auto-tags t)))))
            ;;     (alltodo "Non-scheduled tasks"
            ;;              ((org-agenda-overriding-header "Non-scheduled tasks")
            ;;               (org-agenda-skip-function  #'get-non)
            ;;               (org-super-agenda-groups
            ;;                '((:auto-tags t)))))))
            :ensure t
            :init
            (org-super-agenda-mode)
            :config
            (setq org-agenda-custom-commands
                  `(("s" "Search" ((search "")))
                    ;; 各期間ごとのタスクを生成
                    ("t" "Today's task"
                     ((alltodo "Missed tasks"
                               ((org-agenda-overriding-header "Missed tasks")
                                (org-agenda-skip-function #'get-before-today)
                                (org-super-agenda-groups
                                 '((:auto-tags t)))))
                      (alltodo "Today's tasks"
                               ((org-agenda-overriding-header "Today's tasks")
                                (org-agenda-skip-function #'get-today)
                                (org-super-agenda-groups
                                 '((:auto-tags t)))))
                      (alltodo "Future tasks"
                               ((org-agenda-overriding-header "Future tasks")
                                (org-agenda-skip-function #'get-after-today)
                                (org-super-agenda-groups
                                 '((:auto-tags t)))))
                      (alltodo "Non-scheduled tasks"
                               ((org-agenda-overriding-header "Non-scheduled tasks")
                                (org-agenda-skip-function #'get-not-scheduled)
                                (org-super-agenda-groups
                                 '((:auto-tags t)))))))
                    ("w" "This week's task"
                     ((alltodo "Missed tasks"
                               ((org-agenda-overriding-header "Missed tasks")
                                (org-agenda-skip-function #'get-before-this-week)
                                (org-super-agenda-groups
                                 '((:auto-tags t)))))
                      (alltodo "Today's tasks"
                               ((org-agenda-overriding-header "Today's tasks")
                                (org-agenda-skip-function #'get-this-week)
                                (org-super-agenda-groups
                                 '((:auto-tags t)))))
                      (alltodo "Future tasks"
                               ((org-agenda-overriding-header "Future tasks")
                                (org-agenda-skip-function #'get-after-this-week)
                                (org-super-agenda-groups
                                 '((:auto-tags t)))))
                      (alltodo "Non-scheduled tasks"
                               ((org-agenda-overriding-header "Non-scheduled tasks")
                                (org-agenda-skip-function #'get-not-scheduled)
                                (org-super-agenda-groups
                                 '((:auto-tags t)))))))
                    ("m" "This month's task"
                     ((alltodo "Missed tasks"
                               ((org-agenda-overriding-header "Missed tasks")
                                (org-agenda-skip-function #'get-before-this-month)
                                (org-super-agenda-groups
                                 '((:auto-tags t)))))
                      (alltodo "Today's tasks"
                               ((org-agenda-overriding-header "Today's tasks")
                                (org-agenda-skip-function #'get-this-month)
                                (org-super-agenda-groups
                                 '((:auto-tags t)))))
                      (alltodo "Future tasks"
                               ((org-agenda-overriding-header "Future tasks")
                                (org-agenda-skip-function #'get-after-this-month)
                                (org-super-agenda-groups
                                 '((:auto-tags t)))))
                      (alltodo "Non-scheduled tasks"
                               ((org-agenda-overriding-header "Non-scheduled tasks")
                                (org-agenda-skip-function #'get-not-scheduled)
                                (org-super-agenda-groups
                                 '((:auto-tags t)))))))
                    ("u" "Unscheduled task"
                     ((alltodo "Unscheduled"
                               ((org-agenda-overriding-header "Unscheduled task")
                                (org-agenda-skip-function #'get-not-scheduled)
                                (org-super-agenda-groups
                                 '((:auto-tags t))))))))))))

      (leaf ocaml-setting
        :config
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
          :hook (tuareg-mode-hook . opam-switch-mode)))

      (leaf haskell-setting
        :config
        (leaf haskell-mode
          :ensure t
          :hook (haskell-mode-hook . lsp))
        
        (leaf lsp-haskell
          :ensure t))

      (leaf latex-setting
        :config
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

        (leaf pdf-tools
          :ensure t
          :init (pdf-tools-install)
          :hook (pdf-view-mode-hook . (lambda () (display-line-numbers-mode -1)))
          :bind (:pdf-view-mode-map
                 ("C-s" . isearch-forward))
          :config
          (setq pdf-view-resize-factor 1.0))

        
        ;; (leaf lsp-grammarly
        ;;   :ensure t
        ;;   :hook ((latex-mode-hook org-mode-hook) . lsp))
        )

      (leaf web-setting
        :config
        (leaf typescript-mode
          :ensure t)

        (leaf svelte-mode
          :ensure t)

        (leaf sass-mode
          :ensure t))

      (leaf fsharp-setting
        :config
        (leaf fsharp-mode
          :ensure t
          :config
          (leaf eglot-fsharp
            :ensure t)))

      (leaf markdown-setting
        :config
        (leaf markdown-mode
          :ensure t
          :mode ("\\.md\\'" . gfm-mode)))

      (leaf prolog-setting
        :config
        (leaf prolog-setting
          :config
          (add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode)))))))

;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

(provide 'init)
;;; init.el ends here
