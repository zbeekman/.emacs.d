;;add more package archives
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(setq package-enable-at-startup nil)
(package-initialize)

;; mac laptop stuff
(if (eq system-type 'darwin)
    (progn (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
	   (setenv "PATH" (concat "/opt/local/bin:/opt/local/sbin:" (getenv "PATH"))) ; macports
	   (setq vc-svn-program "/opt/local/bin/svn") ; due to macports
	   (setenv "PATH" (concat "/usr/local/ossh/bin:/usr/local/krb5/bin:" (getenv "PATH"))) ;DoD
	   (setq exec-path (append exec-path '("/usr/local/bin")))
	   (if (file-executable-p "/usr/local/bin/aspell")
	       (progn
		 (setq ispell-program-name "/usr/local/bin/aspell")
		 (setq ispell-extra-args '("-d" "/Library/Application Support/cocoAspell/aspell6-en-6.0-0/en.multi"))
		 ))))

;; Copy environment variables over if on Mac window system
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


;; Flymake general
(require 'flymake)
(add-to-list 'load-path "~/.emacs.d/elisp/emacs-flymake-cursor/")
(eval-after-load 'flymake '(require 'flymake-cursor))

;; Flymake for ELisp
(defun flymake-elisp-init ()
  (unless (string-match "^ " (buffer-name))
    (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
           (local-file  (file-relative-name
                         temp-file
                         (file-name-directory buffer-file-name))))
      (list
       (expand-file-name invocation-name invocation-directory)
       (list
        "-Q" "--batch" "--eval" 
        (prin1-to-string
         (quote
          (dolist (file command-line-args-left)
            (with-temp-buffer
              (insert-file-contents file)
              (condition-case data
                  (scan-sexps (point-min) (point-max))
                (scan-error
                 (goto-char(nth 2 data))
                 (princ (format "%s:%s: error: Unmatched bracket or quote\n"
                                file (line-number-at-pos)))))))
          )
         )
        local-file)))))
(push '("\\.el$" flymake-elisp-init) flymake-allowed-file-name-masks)
(add-hook 'emacs-lisp-mode-hook
          ;; workaround for (eq buffer-file-name nil)
          (function (lambda () (if buffer-file-name (flymake-mode)))))

;; Let's see if we can get .F90 files using flymake
(setq flymake-allowed-file-name-masks
      (cons '(".+\\.F90$"
	      flymake-simple-make-init
	      flymake-simple-cleanup
	      flymake-get-real-file-name)
	    flymake-allowed-file-name-masks))


;; Save the emacs buffer state to local directory
(desktop-save-mode 1)

;; ditch the iconified tool bar for more coding screen realestate.
(tool-bar-mode -1)

;; bind a sane compilation to \C-cm
(global-set-key "\C-cM" 'compile)
(global-set-key "\C-cm" 'recompile)

;; Org mode stuff 
(require 'org)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-tag-persistent-alist '((:startgroup . nil)
				 ("@work" . ?w) ("@home" . ?h) ("@lunch" . ?u)
				 ("errands" . ?e)
				 (:endgroup . nil)
				 ("laptop" . ?l) ("calls" . ?c) ("workstation" . ?k)))
;; (setq org-tag-alist '((:startgroup . nil)
;; 		      ("@work" . ?w) ("@home" . ?h)
;; 		      ("errands" . ?e)
;; 		      (:endgroup . nil)
;; 		      ("laptop" . ?l) ("calls" . ?c) ("workstation" . ?k)))
(setq org-todo-keywords
       '((sequence "TODO(t)" "TOVERIFY(v)" "WAITING(w@)" "|" "DONE(d!)" "DELEGATED(g@)" "CANCELED(c@)")
	 (sequence "TOPURCHASE(p!)" "|" "BOUGHT(b!)")
	 (sequence "EMAILED(e!)" "PHONED(p!)" "MAILED(m!)" "SUBMITTED(s!)" "|" 
		   "APPROVED(a!)" "DENIED(n!)" "RESOLVED(r!)")))
(setq org-directory "~/GTD")
(setq org-default-notes-file (concat org-directory "/InBox.org"))
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline (concat org-directory "/InBox.org") "Tasks")
             "* TODO %^{Brief Description} %^g\n%?\nAdded: %U")
	))
(add-to-list 'auto-mode-alist '("README$" . org-mode))

;; Work machine uses btsync to sync this folder with my laptop,
;; so as long as the laptop is online this should get coordinated with
;; mobile org
(setq org-mobile-directory "~/Dropbox/GTD")

(if (eq system-type 'darwin) ;; Sync for mobile org via dropbox when on laptop
    (progn (defvar org-mobile-push-timer nil
	     "Timer that `org-mobile-push-timer' used to reschedule itself, or nil.")
	   (defun org-mobile-push-with-delay (secs)
	     (when org-mobile-push-timer
	       (cancel-timer org-mobile-push-timer))
	     (setq org-mobile-push-timer
		   (run-with-idle-timer
		    (* 1 secs) nil 'org-mobile-push)))
	   (add-hook 'after-save-hook
		     (lambda ()
		       (when (eq major-mode 'org-mode)
			 (dolist (file (org-mobile-files-alist))
			   (if (string= (file-truename (expand-file-name (car file)))
					(file-truename (buffer-file-name)))
			       (org-mobile-push-with-delay 30)))
			 )))
	   (run-at-time "00:05" 86400 '(lambda () (org-mobile-push-with-delay 1))) ;; refreshes agenda file each day
	   (setq org-mobile-inbox-for-pull (concat org-directory "/InBox.org"))
	   (org-mobile-pull) ;; run org-mobile-pull at startup
	   (defun install-monitor (file secs)
	     (run-with-timer
	      0 secs
	      (lambda (f p)
		(unless (< p (second (time-since (elt (file-attributes f) 5))))
		  (org-mobile-pull)))
	      file secs))
	   (install-monitor (file-truename
			     (concat
			      (file-name-as-directory org-mobile-directory)
			      org-mobile-capture-file))
			    5)
	   ;; Do a pull every 5 minutes to circumvent problems with timestamping
	   ;; (ie. dropbox bugs)
	   (run-with-timer 0 (* 5 60) 'org-mobile-pull)
	   ))

;; highlight parentheses
(require 'highlight-parentheses)
(highlight-parentheses-mode 1)

;; macro-math
(autoload 'macro-math-eval-and-round-region "macro-math" t nil)
(autoload 'macro-math-eval-region "macro-math" t nil)

(global-set-key "\C-x~" 'macro-math-eval-and-round-region)
(global-set-key "\C-x=" 'macro-math-eval-region)

;; Recent file menu/opening from mastering emacs
(require 'recentf)
 
;; get rid of `find-file-read-only' and replace it with something
;; more useful.
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)
 
;; enable recent files mode.
(recentf-mode t)
 
; 100 files ought to be enough.
(setq recentf-max-saved-items 100)
 
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))
;; http://www.masteringemacs.org/articles/2011/01/27/find-files-faster-recent-files-package/

;; Give IDO mode a shot
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(setq ido-use-filename-at-point 'guess)
(setq ido-file-extensions-order '(".F90" ".f90" ".pbs" ".inp" ".sh" ".el" ".py" ".cmd" ".txt"))
(setq ido-create-new-buffer 'prompt)
;(setq ido-ignore-extensions t)
;http://www.masteringemacs.org/articles/2010/10/10/introduction-to-ido-mode/

;; better performance, maybe...
(setq redisplay-dont-pause t)
;http://www.masteringemacs.org/articles/2011/10/02/improving-performance-emacs-display-engine/

(setq next-line-add-newlines t)
;http://www.masteringemacs.org/articles/2011/01/14/effective-editing-movement/

;; better re-running & tweaking of commands
(require 'eshell)
(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)
;http://www.masteringemacs.org/articles/2010/12/13/complete-guide-mastering-eshell/

(require 're-builder)
(setq reb-re-syntax 'string)
;http://www.masteringemacs.org/articles/2011/04/12/re-builder-interactive-regexp-builder/

(require 'find-dired)
(setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))
;http://www.masteringemacs.org/articles/2011/03/25/working-multiple-files-dired/

;; Check for shebang magic in file after save, make executable if found.
(setq my-shebang-patterns 
      (list "^#!/usr/.*/perl\\(\\( \\)\\|\\( .+ \\)\\)-w *.*" 
	    "^#!/usr/.*/sh"
	    "^#!/usr/.*/bash"
	    "^#!/bin/sh"
	    "^#!/bin/bash"
	    "^#!/usr/bin/env python"
	    "^#!/bin/sed -f"
	    "^#!/bin/awk -f"
	    "^#!/usr/bin/awk -f"))
(add-hook 
 'after-save-hook 
 (lambda ()
   (if (not (= (shell-command (concat "test -x " (buffer-file-name))) 0))
       (progn 
	 ;; This puts message in *Message* twice, but minibuffer
	 ;; output looks better.
	 (message (concat "Wrote " (buffer-file-name)))
	 (save-excursion
	   (goto-char (point-min))
	   ;; Always checks every pattern even after
	   ;; match.  Inefficient but easy.
	   (dolist (my-shebang-pat my-shebang-patterns)
	     (if (looking-at my-shebang-pat)
		 (if (= (shell-command  
			 (concat "chmod u+x " (buffer-file-name)))
			0)
		     (message (concat 
			       "Wrote and made executable " 
			       (buffer-file-name))))))))
     ;; This puts message in *Message* twice, but minibuffer output
     ;; looks better.
     (message (concat "Wrote " (buffer-file-name))))))

;; enable visual feedback on selections
(setq transient-mark-mode t)

;; default to unified diffs
(setq diff-switches "-u")

(setq make-backup-files 'non-nil)
(setq
   backup-by-copying t       ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 3
   version-control t)

;; git interfaces
;(add-to-list 'load-path "~/.emacs.d/elpa/git-commit-mode-20140125.1553/")
;(require 'git-commit-mode)
;(add-to-list 'load-path "~/.emacs.d/elpa/git-rebase-mode-20140125.1553/")
;(require 'git-rebase-mode)
;(add-to-list 'load-path "~/.emacs.d/elpa/magit-20140214.1108/")
;(require 'magit)
;(add-to-list 'load-path "~/.emacs.d/elpa/magithub-20121130.1740/")
;(require 'magithub)

(require 'gist)

;; (require 'f90-interface-browser)

(require 'graphviz-dot-mode)

;; Smart TAB behaviour.
(require 'smart-tab)
(global-smart-tab-mode 1)
(setq smart-tab-using-hippie-expand t)

;; Hippie expand customizations
(setq hippie-expand-try-functions-list 
      '(try-expand-dabbrev-visible try-expand-dabbrev try-expand-all-abbrevs try-expand-dabbrev-from-kill try-expand-dabbrev-all-buffers try-complete-file-name-partially try-complete-file-name try-expand-list try-expand-line))

;; ido interface for hippie expand
(require 'cl-lib)
(require 'cl)
(defun my-hippie-expand-completions (&optional hippie-expand-function)
      "Return the full list of possible completions generated by `hippie-expand'.
    The optional argument can be generated with `make-hippie-expand-function'."
      (let ((this-command 'my-hippie-expand-completions)
            (last-command last-command)
            (buffer-modified (buffer-modified-p))
            (hippie-expand-function (or hippie-expand-function 'hippie-expand)))
        (cl-letf (((symbol-function 'ding) 'ignore)) ; avoid the (ding) when hippie-expand exhausts its options.
          (while (progn
                   (funcall hippie-expand-function nil)
                   (setq last-command 'my-hippie-expand-completions)
                   (not (equal he-num -1)))))
        ;; Evaluating the completions modifies the buffer, however we will finish
        ;; up in the same state that we began, and (save-current-buffer) seems a
        ;; bit heavyweight in the circumstances.
        (set-buffer-modified-p buffer-modified)
        ;; Provide the options in the order in which they are normally generated.
        (delete he-search-string (reverse he-tried-table))))
     
    (defmacro my-ido-hippie-expand-with (hippie-expand-function)
      "Generate an interactively-callable function that offers ido-based completion
    using the specified hippie-expand function."
      `(call-interactively
        (lambda (&optional selection)
          (interactive
           (let ((options (my-hippie-expand-completions ,hippie-expand-function)))
             (if options
                 (list (ido-completing-read "Completions: " options)))))
          (if selection
              (he-substitute-string selection t)
            (message "No expansion found")))))
     
    (defun my-ido-hippie-expand ()
      "Offer ido-based completion for the word at point."
      (interactive)
      (my-ido-hippie-expand-with 'hippie-expand))
     
    (global-set-key (kbd "C-c /") 'my-ido-hippie-expand)

;;;;;;;;;;;;;;;;;;;;
;; f90-mode stuff ;;
;;;;;;;;;;;;;;;;;;;;
(add-hook 'f90-mode-hook
          '(lambda ()
	     (setq f90-beginning-ampersand nil)
	     (f90-add-imenu-menu)
	     (abbrev-mode 1)
	     (column-number-mode t)
	     (which-func-mode 1)
	     (flyspell-prog-mode)
	     (hide-ifdef-mode)
	     (highlight-parentheses-mode 1)))

(autoload 'hideshowvis-enable "hideshowvis" "Highlight foldable regions")

(autoload 'hideshowvis-minor-mode
  "hideshowvis"
  "Will indicate regions foldable with hideshow in the fringe."
  'interactive)

(dolist (hook (list 'emacs-lisp-mode-hook
                    'c++-mode-hook
		    'c-mode-hook))
  (add-hook hook 'hideshowvis-enable))


;;Cmake stuff
(require 'cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
		("\\.cmake\\'" . cmake-mode))
	      auto-mode-alist))

(autoload 'cmake-project-mode "cmake-project")
(defun maybe-cmake-project-hook ()
  (if (file-exists-p "CMakeLists.txt") (cmake-project-mode)))
(add-hook 'c-mode-hook 'maybe-cmake-project-hook)
(add-hook 'c++-mode-hook 'maybe-cmake-project-hook)
;(add-hook 'f90-mode-hook 'maybe-cmake-project-hook)

;; smart parens
;(add-to-list 'load-path "~/.emacs.d/elpa/dash-20140214.321/")
(require 'smartparens-config)
(smartparens-global-mode t)

(setq gnus-nntp-server "news.eternal-september.org")
(setq nntp-authinfo-function 'nntp-send-authinfo)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-term-color-vector [unspecified "#14191f" "#d15120" "#81af34" "#deae3e" "#7e9fc9" "#a878b5" "#7e9fc9" "#dcdddd"])
 '(canlock-password "6cd5945ba236f5ec3bf7672640584ee46cacf652")
 '(column-number-mode t)
 '(compilation-always-kill t)
 '(compilation-auto-jump-to-first-error nil)
 '(compilation-scroll-output (quote first-error))
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes (quote ("a3d519ee30c0aa4b45a277ae41c4fa1ae80e52f04098a2654979b1ab859ab0bf" "d9639ebed5f3709f47b53e4bb8eea98a11455ab9336039cf06e9695a0233d5fb" "446c73cdfb49f1dab4c322e51ac00a536fb0e3cb7e6809b9f4616e0858012e92" "1278386c1d30fc24b4248ba69bc5b49d92981c3476de700a074697d777cb0752" "9ea054db5cdbd5baa4cda9d373a547435ba88d4e345f4b06f732edbc4f017dc3" "1f3304214265481c56341bcee387ef1abb684e4efbccebca0e120be7b1a13589" "b6f7795c2fbf75baf3419c60ef7625154c046fc2b10e3fdd188e5757e08ac0ec" "4dacec7215677e4a258e4529fac06e0231f7cdd54e981d013d0d0ae0af63b0c8" default)))
 '(fci-rule-character-color "#192028")
 '(flymake-cursor-number-of-errors-to-display nil)
 '(flymake-info-line-regexp "[rR]emark\\\\|[iI]nfo")
 '(flymake-log-level 3)
 '(flymake-max-parallel-syntax-checks 1)
 '(flymake-no-changes-timeout 4.0)
 '(flymake-number-of-errors-to-display 5)
 '(flymake-start-syntax-check-on-find-file t)
 '(flymake-warn-line-regexp "[wW]arn")
 '(gnus-treat-display-smileys nil)
 '(gnus-treat-x-pgp-sig (quote head))
 '(inhibit-startup-screen t)
 '(org-agenda-files (quote ("~/GTD/Errands.org" "~/GTD/projects/ProjectIndex.org" "~/GTD/Someday.org" "~/GTD/WaitingOn.org" "~/GTD/NextActions.org" "~/GTD/InBox.org")))
 '(save-place t nil (saveplace))
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(speedbar-supported-extension-expressions (quote (".[ch]\\(\\+\\+\\|pp\\|c\\|h\\|xx\\)?" ".tex\\(i\\(nfo\\)?\\)?" ".el" ".emacs" ".l" ".lsp" ".p" ".java" ".js" ".\\(f\\|F\\)\\(90\\|77\\|or\\)?" ".ad[abs]" ".p[lm]" ".tcl" ".m" ".scm" ".pm" ".py" ".g" ".s?html" ".ma?k" "[Mm]akefile\\(\\.in\\)?" "CMakeLists.txt")))
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
 '(tool-bar-mode nil)
 '(vc-svn-header (quote ("$HeadURL$" "$Id$"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

