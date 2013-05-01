;;add more package archives
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;; Save the emacs buffer state to local directory
(desktop-save-mode 1)

;; ditch the iconified tool bar for more coding screen realestate.
(tool-bar-mode -1)
 
;; highlight parentheses
(setq load-path (cons (expand-file-name "~/.emacs.d/elpa/highlight-parentheses-20130323.4/") load-path))
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
 
; 25 files ought to be enough.
(setq recentf-max-saved-items 25)
 
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

;;;;;;;;;;;;;;;;;;;;
;; f90-mode stuff ;;
;;;;;;;;;;;;;;;;;;;;
(add-hook 'f90-mode-hook
          '(lambda ()
	     (setq f90-beginning-ampersand t)
	     (f90-add-imenu-menu)
	     (abbrev-mode 1)
	     (which-func-mode 1)
	     (highlight-parentheses-mode 1)))

(autoload 'hideshowvis-enable "hideshowvis" "Highlight foldable regions")

(autoload 'hideshowvis-minor-mode
  "hideshowvis"
  "Will indicate regions foldable with hideshow in the fringe."
  'interactive)

(dolist (hook (list 'emacs-lisp-mode-hook
                    'c++-mode-hook
		    'c-mode-hook
		    'f90-mode-hook))
  (add-hook hook 'hideshowvis-enable))


;;Cmake stuff
(setq load-path (cons (expand-file-name "~/.emacs.d/elpa/cmake-mode-20110824/") load-path))
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
(add-hook 'f90-mode-hook 'maybe-cmake-project-hook)

;; change to zenburn theme
;; (load-theme 'zenburn)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("9ea054db5cdbd5baa4cda9d373a547435ba88d4e345f4b06f732edbc4f017dc3" "1f3304214265481c56341bcee387ef1abb684e4efbccebca0e120be7b1a13589" "b6f7795c2fbf75baf3419c60ef7625154c046fc2b10e3fdd188e5757e08ac0ec" "4dacec7215677e4a258e4529fac06e0231f7cdd54e981d013d0d0ae0af63b0c8" default)))
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
