(setq user-full-name "Luis Cabellos")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(compilation-message-face (quote default))
 '(custom-enabled-themes (quote (misterioso)))
 '(ecb-history-show-node-info (quote (never . name-path)))
 '(ecb-layout-name "leftright2")
 '(ecb-layout-window-sizes
   (quote
    (("leftright2"
      (ecb-directories-buffer-name 0.2126984126984127 . 0.6511627906976745)
      (ecb-sources-buffer-name 0.2126984126984127 . 0.3372093023255814)
      (ecb-methods-buffer-name 0.23809523809523808 . 0.3372093023255814)
      (ecb-history-buffer-name 0.23809523809523808 . 0.6511627906976745)))))
 '(ecb-options-version "2.50")
 '(ecb-source-path
   (quote
    (("d:\\workspace\\" "WORKSPACE")
     ("c:\\projects\\diesel\\engine\\" "DIESEL")
     ("e:\\projects\\crossfire\\UE4\\Crossfire" "CROSSFIRE GAME")
     ("e:\\projects\\crossfire\\UE4\\Engine" "CROSSFIRE ENGINE")
     ("d:\\projects\\OTWD_packaging\\Trunk" "OTWD TRUNK")
     ("c:\\Users\\root\\AppData\\Roaming\\" "HOME")
     ("c:" "c:"))))
 '(ecb-tip-of-the-day nil)
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(magit-diff-use-overlays nil)
 '(magit-git-executable "C:\\\\Program Files\\\\Git\\mingw64\\\\bin\\\\git")
 '(package-selected-packages
   (quote
    (flatui-theme twilight-theme moe-theme lenlen-theme solarized-theme plan9-theme leuven-theme minimap projectile multiple-cursors gotham-theme darkokai-theme monokai-theme p4 csharp-mode irony irony-eldoc vlf magit-svn guide-key rainbow-blocks htmlize yasnippet kotlin-mode helm-ls-git helm smart-tabs-mode python-mode magit lua-mode json-mode haskell-mode flycheck-irony flatland-black-theme find-file-in-repository find-file-in-project ecb cmake-mode bookmark+ autopair auto-complete-nxml auto-complete-c-headers ag)))
 '(safe-local-variable-values (quote ((projectile-project-name . CROSSFIRE-GAME)))))

;;(set-frame-font "ProggyCleanTT")
;;(set-frame-font "Ubuntu Mono")
;;(set-frame-font "Inconsolata-g")
(defun zhen/font-monaco ()
  (interactive)
  (set-frame-font "Monaco")
  (set-face-attribute 'default (selected-frame) :height 105)
  (set-face-attribute 'default (selected-frame) :foreground "white")
  (set-face-attribute 'default (selected-frame) :background "black")
  )

(defun zhen/font-hack ()
  (interactive)
  (set-frame-font "Hack")
  (set-face-attribute 'default (selected-frame) :height 105)
  (set-face-attribute 'default (selected-frame) :foreground "white")
  (set-face-attribute 'default (selected-frame) :background "black")
  )

(defun zhen/font-proggy ()
  (interactive)
  (set-frame-font "ProggyCleanTT")
  (set-face-attribute 'default (selected-frame) :height 155)
  (set-face-attribute 'default (selected-frame) :foreground "white")
  (set-face-attribute 'default (selected-frame) :background "black")
  )

(defun zhen/font-source ()
  (interactive)
  (set-frame-font "Source Code Pro Medium")
  (set-face-attribute 'default (selected-frame) :height 115)
  (set-face-attribute 'default (selected-frame) :foreground "white")
  (set-face-attribute 'default (selected-frame) :background "black")
  )

(defun zhen/font-consolas ()
  (interactive)
  (set-frame-font "Consolas")
  (set-face-attribute 'default (selected-frame) :height 125)
  (set-face-attribute 'default (selected-frame) :foreground "white")
  (set-face-attribute 'default (selected-frame) :background "black")
  )

(zhen/font-monaco)

;; =============================================================================
;; paquetes
;; =============================================================================
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives
	       '("gnu" . "http://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archives
	       '("melpa" . "http://melpa.org/packages/") t)
  ;;(add-to-list 'package-archives
  ;;	       '("marmalade" . "https://marmalade-repo.org/packages/") t)

  (package-initialize)
  ;;(unless package-archive-contents ;fetch the list of packages available
  ;;  (package-refresh-contents))
  )

(when (string-equal system-type "windows-nt")
  (setq find-program "\"C:\\Users\\root\\Programs\\UnxUtils\\usr\\local\\wbin\\find.exe\""))


(require 'yasnippet)
(setq yas-snippet-dirs (append yas-snippet-dirs '("~/.emacs.d/mysnippets/")))
(yas-global-mode 1)

(define-key yas-minor-mode-map [backtab]     'yas-expand)

;; All rebinds seem to be needed.
(define-key yas-minor-mode-map [(tab)]        nil)
(define-key yas-minor-mode-map (kbd "TAB")    nil)
(define-key yas-minor-mode-map (kbd "<tab>")  nil)

(defgroup zhen/yas-cc nil
  "Zhen YAS cc layout configuration"
  :prefix "zhen/yas-cc/"
  :group 'editing)

(defcustom zhen/yas-cc/control-space-1 t
  "Non-nil means insert a space before control condition."
  :type 'boolean)

(defcustom zhen/yas-cc/control-space-2 t
  "Non-nil means insert a space after control condition."
  :type 'boolean)

(defcustom zhen/yas-cc/control-space-within-pars nil
  "Non-nil means insert space within parenthesis of a control statement"
  :type 'boolean)

(defcustom zhen/yas-cc/control-nl t
  "Non-nil means insert new-line before a control statements block."
  :type 'boolean)

(setq zhen/yas-cc/control-space-1 t)
(setq zhen/yas-cc/control-space-2 t)
(setq zhen/yas-cc/control-space-within-pars nil)
(setq zhen/yas-cc/control-nl t)


(require 'popup)


(defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
  (when (featurep 'popup)
    (popup-menu*
     (mapcar
      (lambda (choice)
        (popup-make-item
         (or (and display-fn (funcall display-fn choice))
             choice)
         :value choice))
      choices)
     :prompt prompt
     ;; start isearch mode immediately
     :isearch t
     )))

(setq yas-prompt-functions '(yas-popup-isearch-prompt yas-ido-prompt yas-no-prompt))

;; =============================================================================
;; configurar ficheros de backup
;; =============================================================================
(setq version-control t never nil)
(setq delete-old-versions t)
(setq backup-directory-alist (quote (("." . "~/.emacs.d/backups/"))))
(setq auto-save-timeout 1200)

;; =============================================================================
;; Server MODE
;; =============================================================================
(require 'server)
(when (and (>= emacs-major-version 23)
           (equal window-system 'w32))
  (defun server-ensure-safe-dir (dir) "Noop" t)) ; Suppress error "directory
                                                 ; ~/.emacs.d/server is unsafe"
                                                 ; on windows.
(server-start)

;; =============================================================================
;; configuraciones varias
;; =============================================================================
; (server-start) ;; allow emacsclient
(setq frame-title-format "emacs - %b")
(setq inhibit-startup-message t)
(line-number-mode 1)
(column-number-mode 1)
(display-time)
(transient-mark-mode t)
;; hide bars
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq visible-bell 1)
;; scroll just one line when hitting the bottom of the window
(setq scroll-step 1)
(setq scroll-conservatively 1)
(setq-default fill-column 80)

;; THEME
;(load-theme 'gotham t)
;(load-theme 'darkokai t)
(load-theme 'misterioso t)

(setq default-directory "~/")

;; Bind M-F12 to recentf-open-files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 100)
(setq recentf-max-menu-items 60)
(global-set-key [(meta f11)] 'recentf-open-files)

(define-key global-map (read-kbd-macro "C-S-w") 'delete-region)

(require 'bookmark+)

(defun zhen/fix-buffer-tab ()
   "clean format of a buffer"
   (interactive)
   (whitespace-cleanup)
   (indent-region (point-min) (point-max) nil)
   (tabify (point-min) (point-max)))

;;(setq zhen/fix-modes-tab-list '(c++-mode))
(setq zhen/fix-modes-tab-list '())

(add-hook 'before-save-hook
	  (lambda ()
	    (when (member major-mode zhen/fix-modes-tab-list)
	      (zhen/fix-buffer-tab))))

(global-set-key [(meta f3)] 'whitespace-cleanup)
(define-key global-map (read-kbd-macro "C-c w") 'whitespace-cleanup)

(global-set-key (read-kbd-macro "C-+") 'text-scale-increase)
(global-set-key (read-kbd-macro "C--") 'text-scale-decrease)

(require 'helm)
;(helm-mode 1)
(setq helm-for-files-preferred-list
      '(helm-source-buffers-list helm-source-recentf helm-source-bookmarks helm-source-file-cache helm-source-files-in-current-dir))

(global-unset-key (kbd "C-z"))
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)
(global-set-key (read-kbd-macro "M-x") 'helm-M-x)
(global-set-key (read-kbd-macro "C-x C-f") 'helm-find-files)
(global-set-key (read-kbd-macro "C-x C-g") 'helm-for-files)
(global-set-key (kbd "s-h") 'global-hl-line-mode)

(defun zhen/kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer 
	(delq (current-buffer) 
	      (remove-if-not 'buffer-file-name (buffer-list)))))

(global-set-key (read-kbd-macro "C-x C-a") 'zhen/kill-other-buffers)

(defun sort-words (reverse beg end)
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))

(require 'htmlize)

;; =============================================================================
;; autocompletado (con diccionario sacado de los ficheros abiertos)
;; =============================================================================
(define-key global-map (read-kbd-macro "M-RET") 'hippie-expand)

;; Start autocomplete in buffer
(global-set-key [(meta f1)] 'auto-complete-mode)
(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
(set-cursor-color "white")
(require 'auto-complete-c-headers)
(add-to-list 'ac-sources 'ac-source-c-headers)

(require 'autopair)

;; (require 'prepaint)
;; (prepaint-global-mode 1)

;; use tabs instead of spaces
(setq-default indent-tabs-mode t)

(add-to-list 'auto-mode-alist '("\\.h$"  . c++-mode))
(add-to-list 'auto-mode-alist '("\\.vert$"  . c-mode))
(add-to-list 'auto-mode-alist '("\\.frag$"  . c-mode))
(add-to-list 'auto-mode-alist '("\\.dat$"  . hexl-mode))

(setq c-default-style "bsd")

(defun zhen/find-dired-regex (dir string)
  (interactive "DDirectory: \nsFile pattern: ")
  (let* ((buffer-name "find")
	 (cmd (concat find-program " \"" dir "\" ")))
    (with-current-buffer (get-buffer-create buffer-name)
      (switch-to-buffer (current-buffer))
      (widen)
      (setq default-directory dir)
      (shell-command cmd (current-buffer))
      (insert "  " dir ":\n")
      (insert "  " cmd "\n")
      )))

(defun zhen/dwim-at-point ()
  "If there's an active selection, return that.
Otherwise, get the symbol at point, as a string."
  (cond ((use-region-p)
	 (buffer-substring-no-properties (region-beginning) (region-end)))
	((symbol-at-point)
	 (substring-no-properties
	  (symbol-name (symbol-at-point))))))

(defun zhen/read-from-minibuffer (prompt)
  "Read a value from the minibuffer with PROMPT.
If there's a string at point, offer that as a default."
  (let* ((suggested (zhen/dwim-at-point))
	 (final-prompt
	  (if suggested
	      (format "%s (default %s): " prompt suggested)
	    (format "%s: " prompt)))
	 ;; Ask the user for input, but add `suggested' to the history
	 ;; so they can use M-n if they want to modify it.
	 (user-input (read-from-minibuffer
		      final-prompt
		      nil nil nil nil suggested)))
    ;; Return the input provided by the user, or use `suggested' if
    ;; the input was empty.
    (if (> (length user-input) 0)
	user-input
      suggested)))

(setq ag-highlight-search t)

(defun zhen/search-files (str path)
  (interactive (list (zhen/read-from-minibuffer "[filenames] Search string")
		     (read-directory-name "DDirectory: ")))
  (ag-dired path str)
  )

(defun zhen/search-cpp (str path)
  (interactive (list (zhen/read-from-minibuffer "[Cpp] Search string")
		     (read-directory-name "DDirectory: ")))
  (ag-files str '(:file-type "cpp") path)
  )

(defun zhen/search-csharp (str path)
  (interactive (list (zhen/read-from-minibuffer "[C#] Search string")
		     (read-directory-name "DDirectory: ")))
  (ag-files str '(:file-type "csharp") path)
  )

(defun zhen/search-lua (str path)
  (interactive (list (zhen/read-from-minibuffer "[Lua] Search string")
		     (read-directory-name "DDirectory: ")))
  (ag-files str '(:file-type "lua") path)
  )

(defun zhen/search-filetype (str ext path)
  (interactive (list (zhen/read-from-minibuffer "[extension] Search string")
		     (read-string "Extension: ")
		     (read-directory-name "DDirectory: ")))
  (ag-files str (cons :file-regex (list (concat "\\." ext))) path)
  )

(defun zhen/search-all (str path)
  (interactive (list (zhen/read-from-minibuffer "[All] Search string")
		     (read-directory-name "DDirectory: ")))
  (ag-files str '(:file-type "all-types") path)
  )

(defun zhen/search-all-regex (str path)
  (interactive (list (zhen/read-from-minibuffer "[Regex] Search string")
		     (read-directory-name "DDirectory: ")))
  (ag-regexp str path)
  )

(define-key global-map (read-kbd-macro "C-c C-f") 'zhen/search-filetype)
(define-key global-map (read-kbd-macro "C-c C-g") 'zhen/search-all-regex)

(define-key global-map (read-kbd-macro "C-c f s") 'ag-kill-buffers)
(define-key global-map (read-kbd-macro "C-c f a") 'zhen/search-all)
(define-key global-map (read-kbd-macro "C-c f f") 'zhen/search-files)
(define-key global-map (read-kbd-macro "C-c f c") 'zhen/search-cpp)
(define-key global-map (read-kbd-macro "C-c f l") 'zhen/search-lua)
(define-key global-map (read-kbd-macro "C-c f #") 'zhen/search-csharp)

(define-key global-map (read-kbd-macro "C-c <right>") 'zhen/search-cpp)
(define-key global-map (read-kbd-macro "C-c <down>") 'zhen/search-all)

(global-set-key (kbd "C-x C-b") 'buffer-menu)

(add-to-list 'exec-path "D:/programs/Aspell/bin/")
(setq ispell-program-name "aspell")



(require 'p4)

;; Multiple cursors for parallel edit
(require 'multiple-cursors)

(global-set-key (kbd "C-<") 'mc/mark-previous-like-this-word)
(global-set-key (kbd "C->") 'mc/mark-next-like-this-word)
(global-set-key (kbd "C-z C-<") 'mc/mark-all-like-this-dwim)
(global-set-key (kbd "C-z r") 'mc/mark-all-in-region)
(global-set-key (kbd "C-z a") 'mc/mark-all-dwim)


(add-hook 'org-mode-hook
	  (lambda()
	    (toggle-truncate-lines nil)
	    ))

(require 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)


(defun zhen/revert-buffer-no-confirm ()
  (interactive)
  (revert-buffer t t))

(global-set-key (kbd "<f5>") 'zhen/revert-buffer-no-confirm)

;; =============================================================================
;; LUA
;; =============================================================================
(defun zhen/reload_lua ()
  (interactive)
  (shell-command "e:\\projects\\payday2\\reload_lua.bat")
  )

(add-hook 'lua-mode-hook
	  (lambda ()
	    (autopair-mode t)
	    (setq show-trailing-whitespace t)
	    (setq lua-indent-level 8)
	    (setq indent-tabs-mode t)
	    (setq tab-width lua-indent-level)
	    (local-set-key  (kbd "C-c C-c")  'zhen/reload_lua)
	    ))


;; =============================================================================
;; C & C++
;; =============================================================================
(defface todo-face
  '((t (:foreground "yellow1" :weight bold :underline t)))
  "Used to mark todo comments.")

(defface bug-face
  '((t (:foreground "red1" :weight bold :underline t)))
  "Used to mark note comments.")

(defface note-face
  '((t (:foreground "pale green" :weight bold :underline t)))
  "Used to mark note comments.")

(defface operator-face
  '((t (:foreground "gray70")))
  "Used to mark operators.")

(font-lock-add-keywords 'c++-mode
			'(("<\\|>\\|(\\|)\\|{\\|}" 0 'operator-face)))

(add-hook 'c-mode-common-hook
	  (lambda()
	    (font-lock-add-keywords nil
				    '(("\\<\\(TODO\\)\\((\\w*)\\)?" 1 'todo-face t)
				      ("\\<\\(FIXME\\|BUG\\)\\((\\w*)\\)?" 1 'bug-face t)
				      ("\\<\\(NOTE\\)\\((\\w*)\\)?" 1 'note-face t)
				      ;; new c++11 keywords
				      ("\\<\\(alignof\\|alignas\\|constexpr\\|decltype\\|noexcept\\|nullptr\\|static_assert\\|thread_local\\|override\\|final\\)\\>" . font-lock-keyword-face)
				      ("\\<\\(char16_t\\|char32_t\\)\\>" . font-lock-keyword-face)
				      ))
	    ;;(set-face-foreground 'font-lock-string-face "red")
	    (setq show-trailing-whitespace t)
	    (which-function-mode 1)
	    (autopair-mode t)
	    (show-paren-mode t)
	    (local-set-key  (kbd "C-c RET") 'ff-find-other-file)
	    (local-set-key  (kbd "C-c C-c")  'comment-or-uncomment-region)
	    ))

(add-hook 'c++-mode-hook
	  (lambda ()
	    (local-set-key  (kbd "C-c C-l")  'compile)
	    (setq c-set-style "bsd")
	    (setq indent-tabs-mode t)
	    (setq tab-width 4)
	    (setq c-basic-offset 4)))

;; =============================================================================
;; C#
;; =============================================================================
(add-hook 'csharp-mode-hook
	  (lambda ()
	    (setq tab-width 4)))


;; =============================================================================
;; Python
;; =============================================================================
(add-hook 'python-mode-hook
	  (lambda()
	    (autopair-mode t)
	    ))

(setq python-shell-interpreter "C:\Python27\python.exe")

;; =============================================================================
;; XML
;; =============================================================================

(add-to-list 'auto-mode-alist '("\\.animation_states$"  . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.animation_subset$"  . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.package$"  . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.unit$"  . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.object$"  . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.material_config$"  . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.sequence_manager$"  . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.gui$"  . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.menu$"  . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.bundle_info$"  . nxml-mode))

(require 'ecb)
;;(require 'ecb-autoloads)

(global-set-key (kbd "C-x C-;") 'ecb-activate)
(global-set-key (kbd "C-x C-,") 'ecb-deactivate)

(global-set-key (kbd "M-1") 'ecb-goto-window-edit1)
(global-set-key (kbd "M-2") 'ecb-goto-window-methods)
(global-set-key (kbd "M-3") 'ecb-goto-window-history)
(global-set-key (kbd "C-1") 'ecb-goto-window-directories)
(global-set-key (kbd "C-2") 'ecb-goto-window-sources)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; =============================================================================
;; PAYDAY 2 SPECIFIC Configuration
;; =============================================================================

(eval-after-load 'rng-loc
  '(add-to-list 'rng-schema-locating-files "c:\\projects\\payday2\\tools\\schemas\\schemas.xml"))

(setq tags-table-list
      '("c:\\projects\\diesel\\engine"))
