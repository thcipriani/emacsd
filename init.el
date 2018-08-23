;; Packages
(require 'package)
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

(menu-bar-mode -1)

(setq my-packages
    '(
      deft
      evil
      evil-leader
      evil-tabs
      evil-surround
      flycheck
      flyspell
      helm
      magit
      markdown-mode
      monokai-theme
      leuven-theme
      neotree
      org
      puppet-mode
      use-package))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'use-package)

(setq inhibit-splash-screen t)

(when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(set-default 'line-spacing 1)
;; (setq visible-bell t)
;; no bell
(setq ring-bell-function 'ignore)

;; Display current column
(column-number-mode 1)

;; Show parentheses matching
(show-paren-mode 1)


(use-package evil)
(use-package evil-leader)
(use-package evil-surround)

(global-evil-tabs-mode t)
(global-evil-surround-mode 1)

;; Use C-u as scroll up in evil mode
(setq evil-want-C-u-scroll t)
(evil-leader/set-leader ",")
(setf sentence-end-double-space nil)
(global-evil-leader-mode)
(evil-mode 1)
(evil-leader/set-key "q" 'delete-window)

;; a buffer
(evil-leader/set-key "ab" 'helm-mini)
(global-set-key (kbd "C-x b") 'helm-mini)

(evil-leader/set-key "ho" 'helm-mini)
(global-set-key (kbd "C-c h o") 'helm-occur)

;; a file
(evil-leader/set-key "af" 'helm-find-files)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(evil-leader/set-key "hx" 'helm-mini)
(global-set-key (kbd "M-x") 'helm-M-x)

(evil-leader/set-key (kbd "gs") 'magit-status)
(global-set-key (kbd "C-x g") 'magit-status)

;; neotree
(use-package neotree)
(evil-leader/set-key "t" 'neotree-toggle)

(evil-leader/set-key "v" 'split-window-right)
(evil-leader/set-key "w" 'other-window)
(evil-leader/set-key "bn" 'next-buffer)
(evil-leader/set-key "bp" 'previous-buffer)

(add-hook 'neotree-mode-hook
    (lambda ()
        (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
        (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
        (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
        (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))


(use-package helm-config)
(setq helm-quick-update                   t ; do not display invisible candidates
    helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
    helm-buffers-fuzzy-matching           t) ; fuzzy matching buffer names when non--nil
;; (helm-mode 1)

;; Org mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; Deft - note taking
(use-package deft)
(setq deft-extension "org")
(setq deft-default-extension "org")
(setq deft-directory "~/Documents/notes")
(setq deft-text-mode 'org-mode)
(setq deft-use-filename-as-title t)
(setq deft-use-filter-string-for-filename t)
(setq deft-file-naming-rules '((noslash . "-")
                               (nospace . "-")))
;; a note
(evil-leader/set-key "an" 'deft)

;; Customize styles
(global-visual-line-mode 0)
(global-whitespace-mode 1)

(setq whitespace-line-column 80)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq whitespace-style
      (quote (face
              trailing
              tabs
              newline
              newline-mark)))

(setq whitespace-display-mappings
      '((newline-mark ?\n   [?\xAC ?\n] [?¬ ?\n])
        (tab-mark     ?\t   [?\x25B8 ?\t] [?▸ ?\t])))

(global-linum-mode 1)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Org capture to use in xmonad
;; <http://www.solasistim.net/posts/org_mode_with_capture_and_xmonad/>
(setq org-capture-templates
    '(("t" "Todo" entry (file (concat org-directory "/work-todo.org"))
       "* TODO %x %?")))

(defadvice org-capture-finalize
        (after delete-capture-frame activate)
    "Advise capture-finalize to close the frame"
    (if (equal "capture" (frame-parameter nil 'name))
        (delete-frame)))

(defadvice org-capture-destroy
        (after delete-capture-frame activate)
    "Advise capture-destroy to close the frame"
    (if (equal "capture" (frame-parameter nil 'name))
       (delete-frame)))

;; make the frame contain a single window. by default org-capture
;; splits the window.
(add-hook 'org-capture-mode-hook
    'delete-other-windows)

(defun make-capture-frame ()
    "Create a new frame and run org-capture."  
    (interactive)
    (make-frame '((name . "capture")
        (width . 120)
        (height . 15)))
    (select-frame-by-name "capture")
    (setq word-wrap 1)
    (setq truncate-lines nil)
    ;; Using the second argument to org-capture, we bypass interactive selection
    ;; and use the existing template defined above.
    (org-capture nil "t"))

(load "server")
(unless (server-running-p)
  (server-start))

(load-theme 'leuven t)

;; separate line numbers a bit
(setq linum-format "%4d \u2502 ")

(add-hook 'after-init-hook #'global-flycheck-mode)
(use-package puppet-mode)

;; puppet needs 4 spaces
(setq puppet-indent-level 4)

;; TODO: No recommended for some reason?
(setq-default truncate-lines t)

;; (setq browse-url-browser-function 'browse-url-generic
;;     browse-url-generic-program "google-chrome")
(setq browse-url-browser-function 'browse-url-generic
    browse-url-generic-program "firefox")
