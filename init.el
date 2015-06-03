;;add more package archives
(require 'package)
(setq package-archives '(("org" . "http://orgmode.org/elpa/")
			 ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
;;(setq debug-on-error t) ; set this to get stack traces on errors
(setq package-enable-at-startup nil)
(package-initialize)

;; Starts the Emacs server
;;(server-start)

(setq calendar-latitude 38.9047)
(setq calendar-longitude -77.0164)
(setq calendar-location-name "Washington, DC")
(require 'osx-location)

;; mac laptop stuff
(if (eq system-type 'darwin)
    (progn (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
	   (setenv "PATH" (concat "/usr/local/ossh/bin:/usr/local/krb5/bin:" (getenv "PATH"))) ;DoD
	   (osx-clipboard-mode +1)
	   (osx-trash-setup)
	   (setq delete-by-moving-to-trash t)
	   (osx-location-watch)
	   (add-hook 'osx-location-changed-hook
		     (lambda()
		       (setq calendar-latitude osx-location-latitude)
		       (setq calendar-longitude osx-location-longitude)
		       (setq calendar-location-name (concat (number-to-string
	 osx-location-latitude) ", " (number-to-string osx-location-longitude)))))
	   (add-hook 'org-clock-in-hook (lambda () (call-process "/usr/bin/osascript" nil 0 nil "-e" (concat "tell application \"org-clock-statusbar\" to clock in \"" org-clock-current-task "\""))))
	   (add-hook 'org-clock-out-hook (lambda () (call-process "/usr/bin/osascript" nil 0 nil "-e" "tell application \"org-clock-statusbar\" to clock out")))
	   (setq exec-path (append exec-path '("/usr/local/bin")))
	   ;; use Skim as default pdf viewer Skim's displayline is used for
	   ;; forward search (from .tex to .pdf) option -b highlights the current
	   ;; line; option -g opens Skim in the background
	   (setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
	   (setq TeX-view-program-list
		 '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))
	   ;; (if (file-executable-p "/usr/local/bin/aspell")
	   ;;     (progn
	   ;; 	 (setq ispell-program-name "/usr/local/bin/aspell")
	   ;; 	 (setq ispell-extra-args '("-d" "/Library/Application Support/cocoAspell/aspell6-en-6.0-0/en.multi"))
	   ;; 	 ))
	   ))

;; Copy environment variables over if on Mac window system
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

; Make the Control-n and Control-p keys (and the down arrow and up
; arrow keys) scroll the current window one line at a time instead
; of one-half screen at a time.
(setq scroll-step 1)

; Set text files to be utf-8 encoded
(add-to-list 'file-coding-system-alist '("\\.txt" . utf-8-unix) )
(add-to-list 'file-coding-system-alist '("\\.json" . utf-8-unix) )
;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

; Display (or don't display) trailing whitespace characters using an
; unusual background color so they are visible.
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)

; add YAML support
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; AucTex/RefTex stuff
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(setq TeX-PDF-mode t)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-auctex t)


(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

;; Flymake general
(require 'flymake)
(add-to-list 'load-path "~/.emacs.d/elisp/emacs-flymake-cursor/")
(eval-after-load 'flymake '(require 'flymake-cursor))
;(eval-after-load 'flymake '(require 'rfringe))
(add-hook 'find-file-hook 'flymake-find-file-hook)

; Add some convenient keybindings
(eval-after-load 'flymake-mode
    (global-set-key (kbd "C-c n") 'flymake-goto-next-error))
(eval-after-load 'flymake-mode
    (global-set-key (kbd "C-c p") 'flymake-goto-prev-error))

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

;; Flymake for LaTeX (maybe?) from http://www.emacswiki.org/emacs/FlymakeTex
(defun flymake-get-tex-args (file-name)
    (list "chktex" (list "-g" "-l" (expand-file-name "~/.chktexrc") "-v0" "-I" "-x" file-name)))
(push
 '("^\\(\.+\.tex\\):\\([0-9]+\\):\\([0-9]+\\):\\(.+\\)"
   1 2 3 4) flymake-err-line-patterns)
(add-hook 'LaTeX-mode-hook 'flymake-mode)
(add-hook 'TeX-mode-hook 'flymake-mode)

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


(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Org mode stuff
; Enable habit tracking (and a bunch of other modules)
(setq org-modules (quote (org-id
                          org-info
                          org-habit
                          org-inlinetask
			  org-mac-iCal
                          )))

(require 'org)
(setq org-directory "~/org")
(setq org-default-notes-file (concat org-directory "/InBox.org"))
(add-to-list 'auto-mode-alist '("\\(README\\|\\.\\(org\\|org_archive\\)\\)$" . org-mode))

;; for homebrew install of magit... waaaaay faster than building with melpa etc.
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")
(require 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")
(add-hook 'magit-mode-hook 'magit-load-config-extensions)
(global-set-key (kbd "C-c m") 'magit-status)
; ido users
(setq magit-completing-read-function
      'magit-ido-completing-read)

(require 'magit-find-file)
(global-set-key (kbd "C-c f") 'magit-find-file-completing-read)

(require 'git-messenger) ;; You need not to load if you install with package.el
(global-set-key (kbd "C-x v p") 'git-messenger:popup-message)

;;(define-key git-messenger-map (kbd "m") 'git-messenger:copy-message)

;; Enable magit-commit-mode after typing 's', 'S', 'd'
(add-hook 'git-messenger:popup-buffer-hook 'magit-commit-mode)


(require 'yagist)
(setq yagist-view-gist t)
(when (file-readable-p "yagist-github-token") (load-file "yagist-github-token"))

(require 'org-mac-iCal)
(add-hook 'org-agenda-cleanup-fancy-diary-hook
	  (lambda ()
	    (goto-char (point-min))
	    (save-excursion
	      (while (re-search-forward "^[a-z]" nil t)
		(goto-char (match-beginning 0))
		(insert "0:00-24:00 ")))
	    (while (re-search-forward "^ [a-z]" nil t)
	      (goto-char (match-beginning 0))
	      (save-excursion
		(re-search-backward "^[0-9]+:[0-9]+-[0-9]+:[0-9]+ " nil t))
	      (insert (match-string 0)))))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/org-plus-contrib-20150518/"))
(require 'org-checklist)

;; installed via MELPA, see also https://github.com/myuhe/org-gcal.el
(require 'org-gcal)
(setq org-gcal-client-id "776472933466-td33ek751b3tiapa509bb8s5gg8br4f9.apps.googleusercontent.com"
      org-gcal-client-secret "1nl10i6KPD_WIm8jGlK2UpxN"
      org-gcal-file-alist '(("zbeekman@gmail.com" .  "~/org/GoogleCalendar.org")
			    ("janine.beekman@gmail.com" . "~/org/JaninesGoogleCalendar.org")
			    ))
;;4/kDx3FzQ5cHpzryzY07xCALnDTxRMzvvYjCfu8EgjnQA.crgq9Kdn-i8fJvIeHux6iLYvArf4mgI
(add-hook 'org-agenda-mode-hook 'org-gcal-fetch)
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
(setq org-mobile-inbox-for-pull (concat org-directory "/MobileCapture.org"))
(add-hook 'after-init-hook 'org-mobile-pull)
(add-hook 'kill-emacs-hook 'org-mobile-push)
(add-hook 'kill-emacs-hook 'org-gcal-sync)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-todo-keywords
       '((sequence "TODO(t)" "NEXT(n)" "TOVERIFY(v)" "WAITING(w@)" "|" "DONE(d!/@)" "DELEGATED(g@)" "CANCELED(c@)")
	 (sequence "TOPURCHASE(p)" "|" "BOUGHT(b!)")
	 (sequence "EMAILED(e!)" "PHONED(o!)" "MAILED(l!)" "SUBMITTED(s!)" "|"
		   "APPROVED(v!/@)" "DENIED(n!/@)" "RESOLVED(r!/@)")
	 (sequence "|" "MEETING(m)" "CALL(a)")
         (sequence "DONOTDO(D)" "|" "DIDIT(i@)")))

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED"  ("CANCELLED"  . t))
              ("WAITING"    ("WAITING"    . t))
              ("DELEGATED"  ("DELEGATED"  . t))
              ("TOPURCHASE" ("TOPURCHASE" . t))
	      ("NEXT"       ("NEXT"       . t))
	      ("EMAILED"    ("WAITING"    . t) ("NEXT" . nil))
	      ("MAILED"     ("WAITING"    . t) ("NEXT" . nil))
	      ("SUBMITTED"  ("WAITING"    . t) ("NEXT" . nil))
	      (done ("WAITING") ("DELEGATED") ("TOPURCHASE"))
              ("TODO" ("WAITING") ("CANCELLED") ("DELEGATED") ("NEXT"))
              ("NEXT" ("WAITING") ("CANCELLED") ("DELEGATED"))
              ("DONE" ("WAITING") ("CANCELLED") ("DELEGATED"))
	      ("BOUGHT" ("TOPURCHASE")))))
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

;; (setq org-tag-persistent-alist '((:startgroup . nil)
;; 				 ("@work" . ?w) ("@home" . ?h) ("@lunch" . ?u)
;; 				 ("errands" . ?e)
;; 				 (:endgroup . nil)
;; 				 ("laptop" . ?l) ("calls" . ?c) ("workstation" . ?k)))
;; (setq org-tag-alist '((:startgroup . nil)
;; 		      ("@work" . ?w) ("@home" . ?h)
;; 		      ("errands" . ?e)
;; 		      (:endgroup . nil)
;; 		      ("laptop" . ?l) ("calls" . ?c) ("workstation" . ?k)))
; Tags with fast selection keys
(setq org-tag-alist (quote ((:startgroup)
                            ("@office" . ?o)
                            ("@home" . ?h)
                            ("@lunch" . ?u)
                            (:endgroup)
			    ("ERRAND" . ?e)
			    ("LAPTOP" . ?l)
                            ("WORKSTATION" . ?s)
			    ("NEXT" . ?n)
			    ("DELEGATED" . ?d)
			    ("TOPURCHASE" . ?p)
                            ("WAITING" . ?w)
                            ("PERSONAL" . ?P)
                            ("WORK" . ?W)
                            ("NOTE" . ?n)
                            ("CANCELLED" . ?c)
                            ("FLAGGED" . ??)
			    ("THEO" . ?t)
			    ("CAR" . ?C))))

; Allow setting single tags without the menu
(setq org-fast-tag-selection-single-key (quote expert))

; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)

(setq org-capture-templates
      (quote (("t" "todo" entry (file org-default-notes-file)
               "* TODO %? %^G\n%U\n" :clock-in t :clock-resume t)
              ;("r" "respond" entry (file org-default-notes-file)
              ; "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
              ("n" "Note" entry (file org-default-notes-file)
               "* %? :NOTE:\n%U\n%^L\n" :clock-in t :clock-resume t)
	      ("j" "Journal" entry (file+datetree (concat org-directory "/diary.org"))
               "* %?\n%U\n" :clock-in t :clock-resume t)
              ;("w" "org-protocol" entry (file org-default-notes-file)
              ; "* TODO Review %c\n%U\n" :immediate-finish t)
              ("m" "Meeting" entry (file org-default-notes-file)
               "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
              ("p" "Phone call" entry (file org-default-notes-file)
               "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
              ("h" "Habit" entry (file+headline (concat org-directory "/NextActions.org") "Good habits")
               "* NEXT %?\n%U\nSCHEDULED: <%<%Y-%m-%d %a .+1d/3d>>\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")
	      ("u" "Item to Buy" entry (file+headline (concat org-directory "/Errands.org") "Groceries")
	       "* TOPURCHASE %? :ERRAND:TOPURCHASE:\n%U\n")
	      ("b" "Bad Habit" entry (file+headline (concat org-directory "/NextActions.org") "Bad Habits")
               "* DONOTDO %?\n%U\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: DONOTDO\n:END:\n"))))

;; Work machine used to use btsync to sync this folder with my laptop,
;; so as long as the laptop is online this should get coordinated with
;; mobile org once I fix it

;; (require 'org-mobile-sync)
;; (org-mobile-sync-mode 1)

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Some stuff from http://doc.norang.ca/org-mode.html#OrgFiles
;; Remove empty LOGBOOK drawers on clock out
(defun zb/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "LOGBOOK" (point))))

(add-hook 'org-clock-out-hook 'zb/remove-empty-drawer-on-clock-out 'append)

; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)

; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)
(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))
; Use the current window when visiting files and buffers with ido
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)
; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)

;;;; Refile settings
; Exclude DONE state tasks from refile targets
(defun zb/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'zb/verify-refile-target)

;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)

;; Compact the block agenda view
(setq org-agenda-compact-blocks t)

;; helper functions for projects and agenda views
(defun zb/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun zb/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (zb/find-project-task)
      (if (equal (point) task)
          nil
        t))))

(defun zb/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))


(defun zb/is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun zb/is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun zb/list-sublevels-for-projects-indented ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels 'indented)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun zb/list-sublevels-for-projects ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels t)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defvar zb/hide-scheduled-and-waiting-next-tasks t)

(defun zb/toggle-next-task-display ()
  (interactive)
  (setq zb/hide-scheduled-and-waiting-next-tasks (not zb/hide-scheduled-and-waiting-next-tasks))
  (when  (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "%s WAITING and SCHEDULED NEXT Tasks" (if zb/hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

(defun zb/skip-stuck-projects ()
  "Skip trees that are not stuck projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (zb/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                nil
              next-headline)) ; a stuck project, has subtasks but no next task
        nil))))

(defun zb/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  ;; (zb/list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (zb/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun zb/skip-non-projects ()
  "Skip trees that are not projects"
  ;; (zb/list-sublevels-for-projects-indented)
  (if (save-excursion (zb/skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((zb/is-project-p)
            nil)
           ((and (zb/is-project-subtree-p) (not (zb/is-task-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun zb/skip-project-trees-and-habits ()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((zb/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun zb/skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((org-is-habit-p)
        next-headline)
       ((and zb/hide-scheduled-and-waiting-next-tasks
             (member "WAITING" (org-get-tags-at)))
        next-headline)
       ((zb/is-project-p)
        next-headline)
       ((and (zb/is-task-p) (not (zb/is-project-subtree-p)))
        next-headline)
       (t
        nil)))))

(defun zb/skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
When not restricted, skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max))))
           (limit-to-project (marker-buffer org-agenda-restrict-begin)))
      (cond
       ((zb/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (not limit-to-project)
             (zb/is-project-subtree-p))
        subtree-end)
       ((and limit-to-project
             (zb/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       (t
        nil)))))

(defun zb/skip-project-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((zb/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       ((zb/is-project-subtree-p)
        subtree-end)
       (t
        nil)))))

(defun zb/skip-non-project-tasks ()
  "Show project tasks.
Skip project and sub-project tasks, habits, and loose non-project tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((zb/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (zb/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       ((not (zb/is-project-subtree-p))
        subtree-end)
       (t
        nil)))))

(defun zb/skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((zb/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun zb/skip-non-subprojects ()
  "Skip trees that are not projects"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (zb/is-subproject-p)
        nil
      next-headline)))

;; Custom agenda command definitions
(setq org-agenda-custom-commands
      (quote (("N" "Notes" tags "NOTE"
               ((org-agenda-overriding-header "Notes")
                (org-tags-match-list-sublevels t)))
              ("h" "Habits" tags-todo "STYLE=\"habit\""
               ((org-agenda-overriding-header "Habits")
                (org-agenda-sorting-strategy
                 '(todo-state-down effort-up category-keep))))
              (" " "Agenda"
               ((agenda "" nil)
                (tags "REFILE"
                      ((org-agenda-overriding-header "Tasks to Refile")
                       (org-tags-match-list-sublevels nil)))
                (tags-todo "-CANCELLED/!"
                           ((org-agenda-overriding-header "Stuck Projects")
                            (org-agenda-skip-function 'zb/skip-non-stuck-projects)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-HOLD-CANCELLED/!"
                           ((org-agenda-overriding-header "Projects")
                            (org-agenda-skip-function 'zb/skip-non-projects)
                            (org-tags-match-list-sublevels 'indented)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-CANCELLED/!NEXT"
                           ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                                  (if zb/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'zb/skip-projects-and-habits-and-single-tasks)
                            (org-tags-match-list-sublevels t)
                            (org-agenda-todo-ignore-scheduled zb/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines zb/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date zb/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(todo-state-down effort-up category-keep))))
                (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                           ((org-agenda-overriding-header (concat "Project Subtasks"
                                                                  (if zb/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'zb/skip-non-project-tasks)
                            (org-agenda-todo-ignore-scheduled zb/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines zb/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date zb/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                           ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                                  (if zb/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'zb/skip-project-tasks)
                            (org-agenda-todo-ignore-scheduled zb/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines zb/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date zb/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-CANCELLED+WAITING|HOLD/!"
                           ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                                  (if zb/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-non-tasks);this function has no definition
                            (org-tags-match-list-sublevels nil)
                            (org-agenda-todo-ignore-scheduled zb/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines zb/hide-scheduled-and-waiting-next-tasks)))
                (tags "-REFILE/"
                      ((org-agenda-overriding-header "Tasks to Archive")
                       (org-agenda-skip-function 'zb/skip-non-archivable-tasks)
                       (org-tags-match-list-sublevels nil))))
               nil))))

(require 'org-id)
;; time tracking stuff
;;
;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)
;;
;; Show lot of clocking history so it's easy to pick items off the C-F11 list
(setq org-clock-history-length 23)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; Change tasks to NEXT when clocking in
(setq org-clock-in-switch-to-state 'zb/clock-in-to-next)
;; Separate drawers for clocking and logs
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)
;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

(setq zb/keep-clock-running nil)

(defun zb/clock-in-to-next (kw)
  "Switch a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO"))
           (zb/is-task-p))
      "NEXT")
     ((and (member (org-get-todo-state) (list "NEXT"))
           (zb/is-project-p))
      "TODO"))))

;(defun bh/find-project-task () ); moved to zb/find-project-task above

(defun zb/punch-in (arg)
  "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
  (interactive "p")
  (setq zb/keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (zb/clock-in-organization-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
      ; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
          (org-clock-in '(16))
        (zb/clock-in-organization-task-as-default)))))

(defun zb/punch-out ()
  (interactive)
  (setq zb/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun zb/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun zb/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in"
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
              (org-clock-in))
          (when zb/keep-clock-running
            (zb/clock-in-default-task)))))))

(defvar zb/organization-task-id "eb155a82-92b2-4f25-a3c6-0304591af2f9")

(defun zb/clock-in-organization-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find zb/organization-task-id 'marker)
    (org-clock-in '(16))))

(defun zb/clock-out-maybe ()
  (when (and zb/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (zb/clock-in-parent-task)))

(add-hook 'org-clock-out-hook 'zb/clock-out-maybe 'append)

(defun zb/clock-in-task-by-id (id)
  "Clock in a task by id"
  (org-with-point-at (org-id-find id 'marker)
    (org-clock-in nil)))

(defun zb/clock-in-last-task (arg)
  "Clock in the interrupted task if there is one
Skip the default task and get the next one.
A prefix arg forces clock in of the default task."
  (interactive "p")
  (let ((clock-in-to-task
         (cond
          ((eq arg 4) org-clock-default-task)
          ((and (org-clock-is-active)
                (equal org-clock-default-task (cadr org-clock-history)))
           (caddr org-clock-history))
          ((org-clock-is-active) (cadr org-clock-history))
          ((equal org-clock-default-task (car org-clock-history)) (cadr org-clock-history))
          (t (car org-clock-history)))))
    (widen)
    (org-with-point-at clock-in-to-task
      (org-clock-in nil))))

(setq org-time-stamp-rounding-minutes (quote (1 1)))

(setq org-agenda-clock-consistency-checks
      (quote (:max-duration "4:00"
              :min-duration 0
              :max-gap 0
              :gap-ok-around ("4:00"))))

(setq org-clock-out-remove-zero-time-clocks t)

;; Agenda clock report parameters
(setq org-agenda-clockreport-parameter-plist
      (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))

;; Set default column view headings: Task Effort Clock_Summary
(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")

; global Effort estimate values
; global STYLE property values for completion
(setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                    ("STYLE_ALL" . "habit"))))

;; Agenda log mode items to display (closed and state changes by default)
(setq org-agenda-log-mode-items (quote (closed state)))

(setq org-agenda-span 'day)

(setq org-stuck-projects (quote ("" nil nil "")))

(defun zb/skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving"
  (save-restriction
    (widen)
    ;; Consider only tasks with done todo headings as archivable candidates
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
          (subtree-end (save-excursion (org-end-of-subtree t))))
      (if (member (org-get-todo-state) org-todo-keywords-1)
          (if (member (org-get-todo-state) org-done-keywords)
              (let* ((daynr (string-to-int (format-time-string "%d" (current-time))))
                     (a-month-ago (* 60 60 24 (+ daynr 1)))
                     (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
                     (this-month (format-time-string "%Y-%m-" (current-time)))
                     (subtree-is-current (save-excursion
                                           (forward-line 1)
                                           (and (< (point) subtree-end)
                                                (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
                (if subtree-is-current
                    subtree-end ; Has a date in this month or last month, skip it
                  nil))  ; available to archive
            (or subtree-end (point-max)))
        next-headline))))

(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archived Tasks")

; Erase all reminders and rebuilt reminders for today from the agenda
(defun zb/org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

; Rebuild the reminders everytime the agenda is displayed
(add-hook 'org-finalize-agenda-hook 'zb/org-agenda-to-appt 'append)

; This is at the end of my .emacs - so appointments are set up when Emacs starts
(zb/org-agenda-to-appt)

; Activate appointments so we get notifications
(appt-activate t)

; If we leave Emacs running overnight - reset the appointments one minute after midnight
(run-at-time "24:01" nil 'zb/org-agenda-to-appt)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-mode-line-clock ((t (:foreground "red" :box (:line-width -1 :style released-button)))) t))

(setq org-return-follows-link t)

(setq require-final-newline t)

(setq org-src-fontify-natively t)

(defun zb/mark-next-parent-tasks-todo ()
  "Visit each parent task and change NEXT states to TODO"
  (let ((mystate (or (and (fboundp 'org-state)
                          state)
                     (nth 2 (org-heading-components)))))
    (when mystate
      (save-excursion
        (while (org-up-heading-safe)
          (when (member (nth 2 (org-heading-components)) (list "NEXT"))
            (org-todo "TODO")))))))

(add-hook 'org-after-todo-state-change-hook 'zb/mark-next-parent-tasks-todo 'append)
(add-hook 'org-clock-in-hook 'zb/mark-next-parent-tasks-todo 'append)

(defun zb/org-todo (arg)
  (interactive "p")
  (if (equal arg 4)
      (save-restriction
        (zb/narrow-to-org-subtree)
        (org-show-todo-tree nil))
    (zb/narrow-to-org-subtree)
    (org-show-todo-tree nil)))

(global-set-key (kbd "<S-f5>") 'zb/widen)

(defun zb/widen ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-agenda-remove-restriction-lock)
        (when org-agenda-sticky
          (org-agenda-redo)))
    (widen)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "W" (lambda () (interactive) (setq zb/hide-scheduled-and-waiting-next-tasks t) (zb/widen))))
          'append)

(defun zb/restrict-to-file-or-follow (arg)
  "Set agenda restriction to 'file or with argument invoke follow mode.
I don't use follow mode very often but I restrict to file all the time
so change the default 'F' binding in the agenda to allow both"
  (interactive "p")
  (if (equal arg 4)
      (org-agenda-follow-mode)
    (widen)
    (zb/set-agenda-restriction-lock 4)
    (org-agenda-redo)
    (beginning-of-buffer)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "F" 'zb/restrict-to-file-or-follow))
          'append)

(defun zb/narrow-to-org-subtree ()
  (widen)
  (org-narrow-to-subtree)
  (save-restriction
    (org-agenda-set-restriction-lock)))

(defun zb/narrow-to-subtree ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (org-get-at-bol 'org-hd-marker)
          (zb/narrow-to-org-subtree))
        (when org-agenda-sticky
          (org-agenda-redo)))
    (zb/narrow-to-org-subtree)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "N" 'zb/narrow-to-subtree))
          'append)

(defun zb/narrow-up-one-org-level ()
  (widen)
  (save-excursion
    (outline-up-heading 1 'invisible-ok)
    (zb/narrow-to-org-subtree)))

(defun zb/get-pom-from-agenda-restriction-or-point ()
  (or (and (marker-position org-agenda-restrict-begin) org-agenda-restrict-begin)
      (org-get-at-bol 'org-hd-marker)
      (and (equal major-mode 'org-mode) (point))
      org-clock-marker))

(defun zb/narrow-up-one-level ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (zb/get-pom-from-agenda-restriction-or-point)
          (zb/narrow-up-one-org-level))
        (org-agenda-redo))
    (zb/narrow-up-one-org-level)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "U" 'zb/narrow-up-one-level))
          'append)

(defun zb/narrow-to-org-project ()
  (widen)
  (save-excursion
    (zb/find-project-task)
    (zb/narrow-to-org-subtree)))

(defun zb/narrow-to-project ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (zb/get-pom-from-agenda-restriction-or-point)
          (zb/narrow-to-org-project)
          (save-excursion
            (zb/find-project-task)
            (org-agenda-set-restriction-lock)))
        (org-agenda-redo)
        (beginning-of-buffer))
    (zb/narrow-to-org-project)
    (save-restriction
      (org-agenda-set-restriction-lock))))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "P" 'zb/narrow-to-project))
          'append)

(defvar zb/project-list nil)

(defun zb/view-next-project ()
  (interactive)
  (let (num-project-left current-project)
    (unless (marker-position org-agenda-restrict-begin)
      (goto-char (point-min))
      ; Clear all of the existing markers on the list
      (while zb/project-list
        (set-marker (pop zb/project-list) nil))
      (re-search-forward "Tasks to Refile")
      (forward-visible-line 1))

    ; Build a new project marker list
    (unless zb/project-list
      (while (< (point) (point-max))
        (while (and (< (point) (point-max))
                    (or (not (org-get-at-bol 'org-hd-marker))
                        (org-with-point-at (org-get-at-bol 'org-hd-marker)
                          (or (not (zb/is-project-p))
                              (zb/is-project-subtree-p)))))
          (forward-visible-line 1))
        (when (< (point) (point-max))
          (add-to-list 'zb/project-list (copy-marker (org-get-at-bol 'org-hd-marker)) 'append))
        (forward-visible-line 1)))

    ; Pop off the first marker on the list and display
    (setq current-project (pop zb/project-list))
    (when current-project
      (org-with-point-at current-project
        (setq zb/hide-scheduled-and-waiting-next-tasks nil)
        (zb/narrow-to-project))
      ; Remove the marker
      (setq current-project nil)
      (org-agenda-redo)
      (beginning-of-buffer)
      (setq num-projects-left (length zb/project-list))
      (if (> num-projects-left 0)
          (message "%s projects left to view" num-projects-left)
        (beginning-of-buffer)
        (setq zb/hide-scheduled-and-waiting-next-tasks t)
        (error "All projects viewed.")))))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "V" 'zb/view-next-project))
          'append)

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "\C-c\C-x<" 'zb/set-agenda-restriction-lock))
          'append)

(setq org-show-entry-below (quote ((default))))

(defun zb/set-agenda-restriction-lock (arg)
  "Set restriction lock to current task subtree or file if prefix is specified"
  (interactive "p")
  (let* ((pom (zb/get-pom-from-agenda-restriction-or-point))
         (tags (org-with-point-at pom (org-get-tags-at))))
    (let ((restriction-type (if (equal arg 4) 'file 'subtree)))
      (save-restriction
        (cond
         ((and (equal major-mode 'org-agenda-mode) pom)
          (org-with-point-at pom
            (org-agenda-set-restriction-lock restriction-type))
          (org-agenda-redo))
         ((and (equal major-mode 'org-mode) (org-before-first-heading-p))
          (org-agenda-set-restriction-lock 'file))
         (pom
          (org-with-point-at pom
            (org-agenda-set-restriction-lock restriction-type))))))))

;; Limit restriction lock highlighting to the headline only
(setq org-agenda-restriction-lock-highlight-subtree nil)

;; Always hilight the current agenda line
(add-hook 'org-agenda-mode-hook
          '(lambda () (hl-line-mode 1))
          'append)

;; The following custom-set-faces create the highlights


;; Keep tasks with dates on the global todo lists
(setq org-agenda-todo-ignore-with-date nil)

;; Keep tasks with deadlines on the global todo lists
(setq org-agenda-todo-ignore-deadlines nil)

;; Keep tasks with scheduled dates on the global todo lists
(setq org-agenda-todo-ignore-scheduled nil)

;; Keep tasks with timestamps on the global todo lists
(setq org-agenda-todo-ignore-timestamp nil)

;; Remove completed deadline tasks from the agenda view
(setq org-agenda-skip-deadline-if-done t)

;; Remove completed scheduled tasks from the agenda view
(setq org-agenda-skip-scheduled-if-done t)

;; Remove completed items from search results
(setq org-agenda-skip-timestamp-if-done t)

(setq org-agenda-insert-diary-extract-time t)

(setq org-agenda-include-diary t)
(setq org-agenda-diary-file "~/org/diary.org")

;; Include agenda archive files when searching for things
(setq org-agenda-text-search-extra-files (quote (agenda-archives)))

;; Show all future entries for repeating tasks
(setq org-agenda-repeating-timestamp-show-all t)

;; Show all agenda dates - even if they are empty
(setq org-agenda-show-all-dates t)

;; Sorting order for tasks on the agenda
(setq org-agenda-sorting-strategy
      (quote ((agenda habit-down time-up user-defined-up effort-up category-keep)
              (todo category-up effort-up)
              (tags category-up effort-up)
              (search category-up))))

;; Start the weekly agenda on Monday
(setq org-agenda-start-on-weekday 1)

;; Enable display of the time grid so we can see the marker for the current time
(setq org-agenda-time-grid (quote ((daily today remove-match)
                                   #("----------------" 0 16 (org-heading t))
                                   (0900 1100 1300 1500 1700))))

;; Display tags farther right
(setq org-agenda-tags-column -102)

;;
;; Agenda sorting functions
;;
(setq org-agenda-cmp-user-defined 'zb/agenda-sort)

(defun zb/agenda-sort (a b)
  "Sorting strategy for agenda items.
Late deadlines first, then scheduled, then non-late deadlines"
  (let (result num-a num-b)
    (cond
     ; time specific items are already sorted first by org-agenda-sorting-strategy

     ; non-deadline and non-scheduled items next
     ((zb/agenda-sort-test 'zb/is-not-scheduled-or-deadline a b))

     ; deadlines for today next
     ((zb/agenda-sort-test 'zb/is-due-deadline a b))

     ; late deadlines next
     ((zb/agenda-sort-test-num 'zb/is-late-deadline '> a b))

     ; scheduled items for today next
     ((zb/agenda-sort-test 'zb/is-scheduled-today a b))

     ; late scheduled items next
     ((zb/agenda-sort-test-num 'zb/is-scheduled-late '> a b))

     ; pending deadlines last
     ((zb/agenda-sort-test-num 'zb/is-pending-deadline '< a b))

     ; finally default to unsorted
     (t (setq result nil)))
    result))

(defmacro zb/agenda-sort-test (fn a b)
  "Test for agenda sort"
  `(cond
    ; if both match leave them unsorted
    ((and (apply ,fn (list ,a))
          (apply ,fn (list ,b)))
     (setq result nil))
    ; if a matches put a first
    ((apply ,fn (list ,a))
     (setq result -1))
    ; otherwise if b matches put b first
    ((apply ,fn (list ,b))
     (setq result 1))
    ; if none match leave them unsorted
    (t nil)))

(defmacro zb/agenda-sort-test-num (fn compfn a b)
  `(cond
    ((apply ,fn (list ,a))
     (setq num-a (string-to-number (match-string 1 ,a)))
     (if (apply ,fn (list ,b))
         (progn
           (setq num-b (string-to-number (match-string 1 ,b)))
           (setq result (if (apply ,compfn (list num-a num-b))
                            -1
                          1)))
       (setq result -1)))
    ((apply ,fn (list ,b))
     (setq result 1))
    (t nil)))

(defun zb/is-not-scheduled-or-deadline (date-str)
  (and (not (zb/is-deadline date-str))
       (not (zb/is-scheduled date-str))))

(defun zb/is-due-deadline (date-str)
  (string-match "Deadline:" date-str))

(defun zb/is-late-deadline (date-str)
  (string-match "\\([0-9]*\\) d\. ago:" date-str))

(defun zb/is-pending-deadline (date-str)
  (string-match "In \\([^-]*\\)d\.:" date-str))

(defun zb/is-deadline (date-str)
  (or (zb/is-due-deadline date-str)
      (zb/is-late-deadline date-str)
      (zb/is-pending-deadline date-str)))

(defun zb/is-scheduled (date-str)
  (or (zb/is-scheduled-today date-str)
      (zb/is-scheduled-late date-str)))

(defun zb/is-scheduled-today (date-str)
  (string-match "Scheduled:" date-str))

(defun zb/is-scheduled-late (date-str)
  (string-match "Sched\.\\(.*\\)x:" date-str))

(setq org-startup-indented t)

(setq org-cycle-separator-lines 0)

(setq org-show-following-heading t)
(setq org-show-hierarchy-above t)
(setq org-show-siblings (quote ((default))))

(setq org-show-following-heading t)
(setq org-show-hierarchy-above t)
(setq org-show-siblings (quote ((default))))

(global-auto-revert-mode t)

; position the habit graph on the agenda to the right of the default
(setq org-habit-graph-column 50)

(run-at-time "06:00" 86400 '(lambda () (setq org-habit-show-habits t)))

(setq org-use-speed-commands t)
(setq org-speed-commands-user (quote (("0" . ignore)
                                      ("1" . ignore)
                                      ("2" . ignore)
                                      ("3" . ignore)
                                      ("4" . ignore)
                                      ("5" . ignore)
                                      ("6" . ignore)
                                      ("7" . ignore)
                                      ("8" . ignore)
                                      ("9" . ignore)

                                      ("a" . ignore)
                                      ("d" . ignore)
                                      ("h" . zb/hide-other)
                                      ("i" progn
                                       (forward-char 1)
                                       (call-interactively 'org-insert-heading-respect-content))
                                      ("k" . org-kill-note-or-show-branches)
                                      ("l" . ignore)
                                      ("m" . ignore)
                                      ("q" . zb/show-org-agenda)
                                      ("r" . ignore)
                                      ("s" . org-save-all-org-buffers)
                                      ("w" . org-refile)
                                      ("x" . ignore)
                                      ("y" . ignore)
                                      ("z" . org-add-note)

                                      ("A" . ignore)
                                      ("B" . ignore)
                                      ("E" . ignore)
                                      ("F" . zb/restrict-to-file-or-follow)
                                      ("G" . ignore)
                                      ("H" . ignore)
                                      ("J" . org-clock-goto)
                                      ("K" . ignore)
                                      ("L" . ignore)
                                      ("M" . ignore)
                                      ("N" . zb/narrow-to-org-subtree)
                                      ("P" . zb/narrow-to-org-project)
                                      ("Q" . ignore)
                                      ("R" . ignore)
                                      ("S" . ignore)
                                      ("T" . zb/org-todo)
                                      ("U" . zb/narrow-up-one-org-level)
                                      ("V" . ignore)
                                      ("W" . zb/widen)
                                      ("X" . ignore)
                                      ("Y" . ignore)
                                      ("Z" . ignore))))

(defun zb/show-org-agenda ()
  (interactive)
  (if org-agenda-sticky
      (switch-to-buffer "*Org Agenda( )*")
    (switch-to-buffer "*Org Agenda*"))
  (delete-other-windows))

(defun zb/hide-other ()
  (interactive)
  (save-excursion
    (org-back-to-heading 'invisible-ok)
    (hide-other)
    (org-cycle)
    (org-cycle)
    (org-cycle)))

(defvar zb/insert-inactive-timestamp t)

(defun zb/toggle-insert-inactive-timestamp ()
  (interactive)
  (setq zb/insert-inactive-timestamp (not zb/insert-inactive-timestamp))
  (message "Heading timestamps are %s" (if zb/insert-inactive-timestamp "ON" "OFF")))

(defun zb/insert-inactive-timestamp ()
  (interactive)
  (org-insert-time-stamp nil t t nil nil nil))

(defun zb/insert-heading-inactive-timestamp ()
  (save-excursion
    (when zb/insert-inactive-timestamp
      (org-return)
      (org-cycle)
      (zb/insert-inactive-timestamp))))

(add-hook 'org-insert-heading-hook 'zb/insert-heading-inactive-timestamp 'append)

(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
(setq org-clone-delete-id t)


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

;; https://github.com/fasheng/dired-toggle
(global-set-key (kbd "<f5>") 'dired-toggle)


(defun try-to-add-imenu ()
  (condition-case nil (imenu-add-to-menubar "I-Menu") (error nil)))
(add-hook 'font-lock-mode-hook 'try-to-add-imenu)

(require 'imenu-anywhere)
(global-set-key (kbd "C-c .") 'imenu-anywhere)

(autoload 'imenus "imenus" nil t)
(autoload 'imenus-mode-buffers "imenus" nil t)

(require 'dired-imenu)

(autoload 'dired-single-buffer "dired-single" "" t)
(autoload 'dired-single-buffer-mouse "dired-single" "" t)
(autoload 'dired-single-magic-buffer "dired-single" "" t)
(autoload 'dired-single-toggle-buffer-name "dired-single" "" t)

(require 'graphviz-dot-mode)

;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)
;; Remove Yasnippet's default tab key binding
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
;; Set Yasnippet's key binding to shift+tab
(define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
(global-auto-complete-mode t)
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")

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

;; enable ansi colors in compile-mode
(defun colorize-compilation-buffer ()
  (when (eq major-mode 'compilation-mode)
    (ansi-color-apply-on-region compilation-filter-start (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;;;;;;;;;;;;;;;;;;;;
;; f90-mode stuff ;;
;;;;;;;;;;;;;;;;;;;;
(add-hook 'f90-mode-hook
          '(lambda ()
	     (setq f90-beginning-ampersand nil)
;;	     (f90-add-imenu-menu)
	     (abbrev-mode 1)
	     (column-number-mode t)
	     (which-func-mode 1)
	     (hide-ifdef-mode)
	     (flyspell-prog-mode)
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
 '(LaTeX-amsmath-label nil)
 '(LaTeX-default-author "Izaak B. Beekman")
 '(LaTeX-eqnarray-label "eq:")
 '(LaTeX-equation-label "eq:")
 '(LaTeX-math-menu-unicode t)
 '(LaTeX-section-label
   (quote
    (("part" . "part:")
     ("chapter" . "chap:")
     ("section" . "sec:")
     ("subsection" . "sec:")
     ("subsubsection" . "sec:"))))
 '(TeX-source-correlate-method (quote ((dvi . synctex) (pdf . synctex))))
 '(TeX-source-correlate-mode t)
 '(align-dq-string-modes
   (quote
    (vhdl-mode emacs-lisp-mode lisp-interaction-mode lisp-mode scheme-mode c++-mode c-mode java-mode perl-mode cperl-mode python-mode f90-mode)))
 '(align-open-comment-modes
   (quote
    (vhdl-mode emacs-lisp-mode lisp-interaction-mode lisp-mode scheme-mode c++-mode c-mode java-mode perl-mode cperl-mode python-mode makefile-mode f90-mode)))
 '(align-sq-string-modes (quote (perl-mode cperl-mode python-mode f90-mode)))
 '(ansi-term-color-vector
   [unspecified "#14191f" "#d15120" "#81af34" "#deae3e" "#7e9fc9" "#a878b5" "#7e9fc9" "#dcdddd"])
 '(canlock-password "6cd5945ba236f5ec3bf7672640584ee46cacf652")
 '(column-number-mode t)
 '(compilation-always-kill t)
 '(compilation-auto-jump-to-first-error nil)
 '(compilation-scroll-output (quote first-error))
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("6a9606327ecca6e772fba6ef46137d129e6d1888dcfc65d0b9b27a7a00a4af20" "a3d519ee30c0aa4b45a277ae41c4fa1ae80e52f04098a2654979b1ab859ab0bf" "d9639ebed5f3709f47b53e4bb8eea98a11455ab9336039cf06e9695a0233d5fb" "446c73cdfb49f1dab4c322e51ac00a536fb0e3cb7e6809b9f4616e0858012e92" "1278386c1d30fc24b4248ba69bc5b49d92981c3476de700a074697d777cb0752" "9ea054db5cdbd5baa4cda9d373a547435ba88d4e345f4b06f732edbc4f017dc3" "1f3304214265481c56341bcee387ef1abb684e4efbccebca0e120be7b1a13589" "b6f7795c2fbf75baf3419c60ef7625154c046fc2b10e3fdd188e5757e08ac0ec" "4dacec7215677e4a258e4529fac06e0231f7cdd54e981d013d0d0ae0af63b0c8" default)))
 '(desktop-save-mode t)
 '(f90-smart-end-names nil)
 '(fci-rule-character-color "#192028")
 '(flymake-cursor-number-of-errors-to-display nil)
 '(flymake-info-line-regexp "[rR]emark\\\\|[iI]nfo")
 '(flymake-log-level 3)
 '(flymake-max-parallel-syntax-checks 3)
 '(flymake-no-changes-timeout 10.0)
 '(flymake-number-of-errors-to-display 5)
 '(flymake-start-syntax-check-on-find-file t)
 '(flymake-warn-line-regexp "[wW]arn")
 '(gh-md-use-gfm t)
 '(gist-ask-for-description t)
 '(gist-supported-modes-alist
   (quote
    ((f90-mode . "f90")
     (action-script-mode . "as")
     (c-mode . "c")
     (c++-mode . "cpp")
     (clojure-mode . "clj")
     (common-lisp-mode . "lisp")
     (css-mode . "css")
     (diff-mode . "diff")
     (emacs-lisp-mode . "el")
     (lisp-interaction-mode . "el")
     (erlang-mode . "erl")
     (haskell-mode . "hs")
     (html-mode . "html")
     (io-mode . "io")
     (java-mode . "java")
     (javascript-mode . "js")
     (jde-mode . "java")
     (js2-mode . "js")
     (lua-mode . "lua")
     (ocaml-mode . "ml")
     (objective-c-mode . "m")
     (perl-mode . "pl")
     (php-mode . "php")
     (python-mode . "py")
     (ruby-mode . "rb")
     (text-mode . "txt")
     (scala-mode . "scala")
     (sql-mode . "sql")
     (scheme-mode . "scm")
     (smalltalk-mode . "st")
     (sh-mode . "sh")
     (tcl-mode . "tcl")
     (tex-mode . "tex")
     (xml-mode . "xml"))))
 '(gist-view-gist t)
 '(git-commit-confirm-commit t)
 '(git-commit-mode-hook
   (quote
    (turn-on-auto-fill flyspell-mode git-commit-save-message)))
 '(git-messenger:show-detail t)
 '(git-rebase-auto-advance t)
 '(gnus-treat-display-smileys nil)
 '(gnus-treat-x-pgp-sig (quote head))
 '(inhibit-startup-screen t)
 '(magit-diff-options (quote ("--function-context")))
 '(magit-diff-refine-hunk (quote all))
 '(magit-highlight-indentation nil)
 '(magit-log-auto-more t)
 '(magit-log-format-graph-function (quote magit-log-format-unicode-graph))
 '(magit-log-show-gpg-status t)
 '(magit-process-popup-time 60)
 '(magit-repo-dirs (quote ("~/Sandbox")))
 '(magit-revert-backup t)
 '(magit-rewrite-inclusive nil)
 '(org-agenda-files
   (quote
    ("~/org/Errands.org" "~/org/projects/ProjectIndex.org" "~/org/Someday.org" "~/org/WaitingOn.org" "~/org/NextActions.org" "~/org/InBox.org" "~/org/diary.org" "~/org/GoogleCalendar.org" "~/org/MobileCapture.org")))
 '(org-clock-idle-time 10)
 '(osx-clipboard-mode t)
 '(preview-auto-cache-preamble t)
 '(preview-image-type (quote dvipng))
 '(preview-preserve-counters t)
 '(reftex-plug-into-AUCTeX (quote (t t t t t)))
 '(reftex-ref-style-default-list (quote ("Varioref")))
 '(reftex-vref-is-default t)
 '(safe-local-variable-values (quote ((TeX-master . t) (TeX-master . "../thesis.tex"))))
 '(save-place t nil (saveplace))
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(sp-autoescape-string-quote nil)
 '(speedbar-supported-extension-expressions
   (quote
    (".[ch]\\(\\+\\+\\|pp\\|c\\|h\\|xx\\)?" ".tex\\(i\\(nfo\\)?\\)?" ".el" ".emacs" ".l" ".lsp" ".p" ".java" ".js" ".\\(f\\|F\\)\\(90\\|77\\|or\\)?" ".ad[abs]" ".p[lm]" ".tcl" ".m" ".scm" ".pm" ".py" ".g" ".s?html" ".ma?k" "[Mm]akefile\\(\\.in\\)?" "CMakeLists.txt")))
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
 '(tool-bar-mode nil)
 '(vc-make-backup-files t)
 '(vc-svn-header (quote ("$HeadURL$" "$Id$")))
 '(yagist-working-directory "~/.yagist"))

