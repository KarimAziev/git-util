;;; git-util.el --- Collection of git utils -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/git-util
;; Version: 0.1.0
;; Keywords: vc
;; Package-Requires: ((emacs "27.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Collection of git utils.

;;; Code:


(require 'vc-git)
(require 'url-parse)
(require 'shell)
(require 'comint)

(defvar git-util-host-regexp
  (concat "\\("
          "\\(\\(github\\|gitlab\\|gitlab\\.[a-z]+\\)\\.com\\)"
          "\\|"
          "\\(\\(bitbucket\\|salsa[\\.]debian\\|framagit\\|codeberg\\|git[\\.]savannah[\\.]gnu\\|git[\\.]kernel\\|git[\\.]suckless\\|code[\\.]orgmode\\|gitlab[\\.]gnome\\)[\\.]org\\)"
          "\\|"
          "\\(\\(repo[\\.]or\\)[\\.]cz\\)"
          "\\|"
          "\\(git\\.sr\\.ht\\)"
          "\\)")
  "Regexp matching common git hosts.")

(defcustom git-util-autoinstall-chrome-session-dump nil
  "Whether to install https://github.com/lemnos/chrome-session-dump.
It is used as source for git url completions."
  :type 'boolean
  :link "https://github.com/lemnos/chrome-session-dump"
  :group 'git-util)

(defmacro git-util--pipe (&rest functions)
  "Return left-to-right composition from FUNCTIONS."
  (declare (debug t) (pure t) (side-effect-free t))
  `(lambda (&rest args)
     ,@(let ((init-fn (pop functions)))
         (list
          (seq-reduce
           (lambda (acc fn)
             (if (symbolp fn)
                 `(funcall #',fn ,acc)
               `(funcall ,fn ,acc)))
           functions
           (if (symbolp init-fn)
               `(apply #',init-fn args)
             `(apply ,init-fn args)))))))

(defmacro git-util--compose (&rest functions)
  "Return right-to-left composition from FUNCTIONS."
  (declare (debug t) (pure t) (side-effect-free t))
  `(git-util--pipe ,@(reverse functions)))

(defmacro git-util--or (&rest functions)
  "Return an unary function which invoke FUNCTIONS until first non-nil result."
  (declare (debug t) (pure t) (side-effect-free t))
  `(lambda (it) (or
                 ,@(mapcar (lambda (v) (if (symbolp v)
                                           `(,v it)
                                         `(funcall ,v it)))
                           functions))))

(defmacro git-util--and (&rest functions)
  "Return an unary function which invoke FUNCTIONS until first nil result."
  (declare (debug t) (pure t) (side-effect-free t))
  `(lambda (it) (and
                 ,@(mapcar (lambda (v) (if (symbolp v)
                                           `(,v it)
                                         `(funcall ,v it)))
                           functions))))

(defmacro git-util--partial (fn &rest args)
  "Return a partial application of FN to left-hand ARGS.

ARGS is a list of the last N arguments to pass to FN. The result is a new
function which does the same as FN, except that the last N arguments are fixed
at the values with which this function was called."
  (declare (side-effect-free t))
  `(lambda (&rest pre-args)
     ,(car (list (if (symbolp fn)
                     `(apply #',fn (append (list ,@args) pre-args))
                   `(apply ,fn (append (list ,@args) pre-args)))))))

(defmacro git-util--rpartial (fn &rest args)
  "Return a partial application of FN to right-hand ARGS.

ARGS is a list of the last N arguments to pass to FN. The result is a new
function which does the same as FN, except that the last N arguments are fixed
at the values with which this function was called."
  (declare (side-effect-free t))
  `(lambda (&rest pre-args)
     ,(car (list (if (symbolp fn)
                     `(apply #',fn (append pre-args (list ,@args)))
                   `(apply ,fn (append pre-args (list ,@args))))))))

(defvar git-util-chrome-url-chrome-history-file nil
  "Chrome history SQLite database file.")

(defun git-util-chrome-url-chrome-guess-bookmarks-file ()
  "Return the most newer chrome bookmarks file."
  (car
   (seq-sort
    #'file-newer-than-file-p
    (seq-filter
     #'file-exists-p
     `("~/Library/Application Support/Google/Chrome/Profile 1/Bookmarks"
       "~/Library/Application Support/Google/Chrome/Default/Bookmarks"
       "~/AppData/Local/Google/Chrome/User Data/Default/Bookmarks"
       "~/snap/chromium/common/chromium/Default/Bookmarks"
       "~/.config/google-chrome/Default/Bookmarks"
       "~/.config/chromium/Default/Bookmarks"
       ,(substitute-in-file-name
         "$LOCALAPPDATA/Google/Chrome/User Data/Default/Bookmarks")
       ,(substitute-in-file-name
         "$USERPROFILE/Local Settings/Application Data/Google/Chrome/User Data/Default/Bookmarks"))))))

(defun git-util-chrome-url-chrome-guess-history-file ()
  "Return the most newer chrome history file."
  (car
   (seq-sort
    #'file-newer-than-file-p
    (seq-filter
     #'file-exists-p
     `("~/Library/Application Support/Google/Chrome/Profile 1/History"
       "~/Library/Application Support/Google/Chrome/Default/History"
       "~/AppData/Local/Google/Chrome/User Data/Default/History"
       "~/snap/chromium/common/chromium/Default/History"
       "~/.config/google-chrome/Default/History"
       "~/.config/chromium/Default/History"
       ,(substitute-in-file-name
         "$LOCALAPPDATA/Google/Chrome/User Data/Default/History")
       ,(substitute-in-file-name
         "$USERPROFILE/Local Settings/Application Data/Google/Chrome/User Data/Default/History"))))))

(defun git-util-chrome-bookmarks-read-json-file ()
	"Chrome bookmarks read json file."
  (when-let ((file (git-util-chrome-url-chrome-guess-bookmarks-file))
             (json-object-type 'plist)
             (json-array-type 'list))
    (when (file-exists-p file)
      (json-read-file
       file))))

(defun git-util-chrome-list-urls-from-json-plist (plist)
  "Return list of git urls from chrome bookmarks PLIST."
  (when-let ((roots (cdr (plist-get plist :roots))))
    (let ((children)
          (current))
      (while (setq current (pop roots))
        (if (and current (listp current)
                 (plist-get current :children))
            (setq roots (append roots (plist-get current :children)))
          (when (listp current)
            (when-let ((url (plist-get current :url)))
              (when (string-match-p url git-util-host-regexp)
                (setq children (push (plist-get current :url) children)))))))
      children)))

(defun git-util-read-git-urls-from-chrome-history ()
  "Read `git-util-chrome-url-chrome-history-file' and return list of git urls."
  (unless git-util-chrome-url-chrome-history-file
    (setq git-util-chrome-url-chrome-history-file
          (git-util-chrome-url-chrome-guess-history-file)))
  (when (or (null git-util-chrome-url-chrome-history-file)
            (not (file-exists-p git-util-chrome-url-chrome-history-file)))
    (user-error "'%s' doesn't exist, reset `git-util-chrome-url-chrome-history-file'"
                git-util-chrome-url-chrome-history-file))
  (with-temp-buffer
    (erase-buffer)
    (let ((tmp (make-temp-name
                (expand-file-name (temporary-file-directory)
                                  "git-util-chrome-history"))))
      (copy-file
       git-util-chrome-url-chrome-history-file tmp)
      (if (zerop
           (call-process
            "sqlite3" nil t nil
            "-ascii"
            tmp
            "SELECT url, title, last_visit_time FROM urls ORDER BY url DESC"))
          (let (result)
            (goto-char (point-min))
            ;; -ascii delimited by 0x1F and 0x1E
            (while (re-search-forward (rx (group (+? anything)) "\x1e") nil t)
              (let* ((parts (split-string (match-string 1) "\x1f"))
                     (url (car parts)))
                (when (string-match-p git-util-host-regexp url)
                  (push url result))))
            (delete-file tmp)
            (nreverse result))
        (error "Command sqlite3 failed: %s" (buffer-string))))))

(defvar git-util-chrome-sesssion-dump-buffer "*chrome-session-dump*")
(defun git-util-chrome-install-session-dump ()
  "Install chrome-sesion-dump to /usr/bin/chrome-session-dump."
  (let ((default-directory "/sudo::")
        (inhibit-read-only t))
    (async-shell-command
     "sudo curl -o /usr/bin/chrome-session-dump -L 'https://github.com/lemnos/chrome-session-dump/releases/download/v0.0.2/chrome-session-dump-linux' && sudo chmod 755 /usr/bin/chrome-session-dump"
     git-util-chrome-sesssion-dump-buffer)))

(defun git-util-f-dirs-recoursively (directory &optional match depth filter-fn)
  "Return list of directories in DIRECTORY that matches MATCH.
With optional argument DEPTH limit max depth.
If FILTER-FN passed call it with directories."
  (when (or (not (numberp depth))
            (> depth 0))
    (let ((dirs (directory-files directory nil match))
          (it)
          (acc))
      (while (setq it (pop dirs))
        (setq it (expand-file-name it directory))
        (when (and
               (file-readable-p it)
               (file-directory-p it)
               (if filter-fn
                   (funcall filter-fn it)
                 t))
          (push it acc)
          (setq acc (if (or (not (numberp depth))
                            (> depth 0))
                        (append acc (git-util-f-dirs-recoursively it
                                                                  match
                                                                  (when depth
                                                                    (1- depth))
                                                                  filter-fn))
                      acc))))
      acc)))

(defun git-util-chrome-session-dump-get-active-tabs ()
  "Return list of active tabs in google-chrome."
  (when-let ((file (when (file-exists-p "~/.config/google-chrome/")
                     "~/.config/google-chrome/")))
    (if (and git-util-autoinstall-chrome-session-dump
             (not (get-buffer git-util-chrome-sesssion-dump-buffer))
             (not (executable-find "chrome-session-dump")))
        (git-util-chrome-install-session-dump)
      (when (executable-find "chrome-session-dump")
        (split-string
         (shell-command-to-string
          (concat "chrome-session-dump\s" file))
         "\n" t)))))

(defun git-util-call-process (command &rest args)
  "Execute COMMAND with ARGS synchronously.

Returns (STATUS . OUTPUT) when it is done, where STATUS is the returned error
code of the process and OUTPUT is its stdout output."
  (let ((buff (generate-new-buffer command)))
    (with-current-buffer buff
      (let ((status))
        (setq status (apply #'call-process command nil t nil
                            (flatten-list args)))
        (let ((result (string-trim (buffer-string))))
          (if (= 0 status)
              (prog1 result (kill-current-buffer))
            (minibuffer-message result) nil))))))

(defun git-util-directory-files (directory &optional nosort)
  "Return a list of names of files in DIRECTORY excluding \".\" and \"..\".

Names are that are relative to the specified directory.

If NOSORT is non-nil, the list is not sorted--its order is unpredictable.
 Otherwise, the list returned is sorted with string-lessp."
  (directory-files directory nil
                   directory-files-no-dot-files-regexp nosort))

(defun git-util-dir-empty-p (directory)
  "Return t if DIRECTORY is empty."
  (null (git-util-directory-files directory)))

(defun git-util-non-git-dirs-recoursively (directory &optional match depth)
  "Return list of non git directories in DIRECTORY that matches MATCH.
With optional argument DEPTH limit max depth."
  (git-util-f-dirs-recoursively
   directory match depth
   (lambda (it)
     (let ((result (not
                    (or
                     (member (file-name-base it)
                             '("snap" "node_modules" "share"))
                     ;; (string-match-p "[0-9]" it)
                     (file-exists-p (expand-file-name ".git" it))
                     (file-exists-p (expand-file-name "node_modules" it))))))
       result))))

(defun git-util-list-git-urls-from-kill-ring ()
  "Return list of git urls from `kill-ring'."
  (seq-filter (apply-partially #'string-match-p git-util-host-regexp)
              (seq-copy kill-ring)))

(defun git-util-list-git-urls-from-minibuffer-history ()
  "Return list of git urls from `minibuffer-history'."
  (seq-filter (apply-partially #'string-match-p git-util-host-regexp)
              minibuffer-history))

(defun git-util-list-git-urls-from-chrome-bookmarks ()
  "Return list of git urls from chrome bookmarks."
  (git-util-chrome-list-urls-from-json-plist
   (git-util-chrome-bookmarks-read-json-file)))

(defun git-util-git-url-at-point ()
  "Return git urls at point or nil, if none."
  (when-let ((url (thing-at-point 'url t)))
    (when (string-match-p git-util-host-regexp url)
      url)))

(defun git-util-url-get-candidates ()
	"Return list of urls from `kill-ring', buffer, chrome history, bookmarks etc."
  (if-let ((git-url-at-point (git-util-git-url-at-point)))
      (nconc (list git-url-at-point)
             (git-util-chrome-session-dump-get-active-tabs)
             (git-util-list-git-urls-from-kill-ring)
             (git-util-list-git-urls-from-minibuffer-history)
             (git-util-list-git-urls-from-chrome-bookmarks)
             (git-util-read-git-urls-from-chrome-history))))

(defun git-util-exec-in-dir (command project-dir &optional callback)
  "Execute COMMAND in PROJECT-DIR.
If PROJECT-DIR doesn't exists, create new.
Invoke CALLBACK without args."
  (let ((proc)
        (buffer (generate-new-buffer (format "*%s*" command))))
    (progn (switch-to-buffer buffer)
           (with-current-buffer buffer
             (if (file-exists-p project-dir)
                 (setq default-directory project-dir)
               (mkdir project-dir)
               (setq default-directory project-dir))
             (setq proc (start-process-shell-command
                         (nth 0
                              (split-string command))
                         buffer command))
             (shell-command-save-pos-or-erase)
             (shell-mode)
             (view-mode +1))
           (set-process-sentinel
            proc
            (lambda (process _state)
              (let ((output (with-current-buffer
                                (process-buffer process)
                              (buffer-string))))
                (kill-buffer (process-buffer process))
                (if (= (process-exit-status process) 0)
                    (progn
                      (message "finished")
                      (if callback
                          (funcall callback)
                        (dired project-dir)))
                  (user-error (format "%s\n%s" command output))))))
           (set-process-filter proc #'comint-output-filter))))

(defun git-util-f-parent (path)
  "Return the parent directory to PATH without slash."
  (let ((parent (file-name-directory
                 (directory-file-name
                  (expand-file-name path default-directory)))))
    (when (and (file-exists-p path)
               (file-exists-p parent)
               (not (equal
                     (file-truename (directory-file-name
                                     (expand-file-name path)))
                     (file-truename (directory-file-name
                                     (expand-file-name parent))))))
      (if (file-name-absolute-p path)
          (directory-file-name parent)
        (file-relative-name parent)))))

(defun git-util-https-url-p (url)
  "Return t if URL string is githost with https protocol."
  (not (null
        (string-match-p
         (concat "https://" git-util-host-regexp)
         url))))

(defun git-util-ssh-url-p (url)
  "Return t if URL string is githost with git protocol."
  (string-match-p
   (concat "git@" git-util-host-regexp)
   url))

(defun git-util-author ()
  "Return current user name from git config."
  (git-util-call-process "git" "config" "user.name"))

(defun git-util-config (&rest args)
  "Exec git config with ARGS."
  (apply #'git-util-call-process "git" (flatten-list (list "config" args))))

(defun git-util-read-dir (prompt basename)
  "Read a new directory with PROMPT and BASENAME."
  (let* ((dir-files (git-util-directory-files default-directory))
         (default-variants
          (mapcar
           (lambda (dir)
             (if (file-exists-p
                  (expand-file-name basename dir))
                 (let ((count 0)
                       (name basename))
                   (while (and (file-exists-p
                                (expand-file-name name dir))
                               (not (git-util-dir-empty-p
                                     (expand-file-name name dir))))
                     (setq count (1+ count))
                     (setq name (format "%s-%s"
                                        (replace-regexp-in-string
                                         "-[0-9]+$" "" name)
                                        count)))
                   (expand-file-name name dir))
               (expand-file-name basename dir)))
           (git-util-guess-repos-dirs)))
         (variants (if (null dir-files)
                       (append `(,default-directory) default-variants)
                     default-variants)))
    (file-name-as-directory
     (completing-read (or prompt "Directory:\s") variants nil nil
                      (git-util-f-parent
                       (car variants))))))

(defun git-util-clone-confirm (url)
  "Convert URL to ssh format and read it from minibuffer."
  (let ((variants (git-util-get-ssh-variants
                   (if (git-util-ssh-url-p url) url
                     (git-util-url-https-to-ssh url)))))
    (if (> (length variants) 1)
        (completing-read "git clone\s" variants)
      (car variants))))

(defun git-util-guess-repos-dirs-find ()
	"Execute `find' and return list parent directories of git repos."
  (completing-read "Direcotory:\s"
                   (funcall
                    (git-util--compose
                     delete-dups
                     (git-util--partial mapcar #'git-util-f-parent)
                     (git-util--rpartial split-string "\n" t)
                     shell-command-to-string)
                    "find ~/ -name .git -maxdepth 4 -exec dirname {} \\; -prune 2>&1 | grep -v \"Permission denied\"")))

(defun git-util-alist-ssh-hosts ()
  "Return hosts found in .ssh/config."
  (when (file-exists-p "~/.ssh/config")
    (with-temp-buffer
      (insert-file-contents
       "~/.ssh/config")
      (let ((alist))
        (while (re-search-forward
                "\\(HOST[\s\t]\\([^\n]+\\)[\n\s\t]+HOSTNAME[\s\t\n]\\([^\s\t\n]+\\)\\)"
                nil t 1)
          (let ((host (match-string-no-properties 2))
                (hostname (match-string-no-properties 3)))
            (push (cons host hostname) alist)))
        alist))))

(defun git-util-get-ssh-variants (ssh-url)
  "Return variants of git ssh for SSH-URL."
  (let* ((local-alist (git-util-alist-ssh-hosts))
         (cell (with-temp-buffer
                 (save-excursion (insert (replace-regexp-in-string "^git@" ""
                                                                   ssh-url)))
                 (let ((beg (point))
                       (end))
                   (setq end (re-search-forward git-util-host-regexp nil t 1))
                   (cons (buffer-substring-no-properties beg end)
                         (string-trim (buffer-substring-no-properties
                                       end
                                       (point-max))))))))
    (setq local-alist (seq-filter (lambda (it) (equal
                                           (car cell)
                                           (cdr it)))
                                  local-alist))
    (seq-uniq
     (append
      (list ssh-url)
      (mapcar (lambda (it) (concat "git@" (car it) (cdr cell))) local-alist)))))

(defun git-util-get-authors-emails (directory)
  "Return list of all authors in repository DIRECTORY."
  (when-let* ((default-directory (vc-git-root directory))
              (items (git-util-call-process
                      "git" "log" "--all" "--format=%cE")))
    (seq-uniq (split-string items
                            "\n"))))

;;;###autoload
(defun git-util-visit-remote ()
  "Return plist of current git repo as straight recipe :repo, :type and :host."
  (interactive)
  (when-let ((url (cdar (git-util-remotes-alist))))
    (setq url (replace-regexp-in-string
               "\\.git$"
               ""
               (or (git-util-ssh-to-https url)
                   url)))
    (browse-url url)))

(defun git-util-melpa-current-recipe ()
  "Return plist of current git repo as straight recipe :repo, :type and :host."

  (when-let* ((url (cdar (git-util-remotes-alist)))
              (urlobj (url-generic-parse-url (or (git-util-ssh-to-https url)
                                                 url)))
              (host (url-host urlobj))
              (filename (url-filename urlobj)))
    (list :repo (replace-regexp-in-string
                 "^/\\|[\\.]git$" "" filename)
          :type "git"
          :host (file-name-base host))))

(defun git-util-get-straight-recipe-in-dir (directory)
  "Return plist of git repo in DIRECTORY as straight recipe."
  (let ((default-directory directory))
    (git-util-melpa-current-recipe)))

(defun git-util-get-dirs-with-author ()
  "Read git repository of current author or all."
  (let ((author (git-util-config "user.email")))
    (completing-read "Repository: "
                     (git-util-get-git-repos)
                     (when author
                       (git-util--compose
                        (apply-partially #'member author)
                        git-util-get-authors-emails)))))

(defun git-util-repo-status (directory)
  "Return git status for DIRECTORY."
  (when-let ((default-directory directory))
    (git-util-call-process "git" "status")))

(defun git-util-repo-modified-p (directory)
  "Return non nil if DIRECTORY git status is not up to date."
  (when-let* ((default-directory directory)
              (status (git-util-call-process "git" "status"))
              (status-lines (mapcar #'string-trim
                                    (split-string status "\n" t))))
    (or
     (and (nth 1 status-lines)
          (not (string-match-p "Your branch is up to date"
                               (nth 1 status-lines))))
     (seq-intersection status-lines
                       '("Changes not staged for commit:"
                         "Changes to be committed:")))))

(defun git-util-straight-uncommitted-repos ()
  "Return list of modified repo in straight repos directory."
  (when (fboundp 'straight--repos-dir)
    (seq-filter #'git-util-repo-modified-p (git-util-get-git-repos
                                          (straight--repos-dir)))))

(defun git-util-get-git-repos (&optional dir)
  "Return list of git repositories in DIR or home directory."
  (or
   (let ((command (seq-find #'executable-find
                            '("fdfind" "fd" "find"))))
     (pcase command
       ((or "fd" "fdfind")
        (funcall
         (git-util--compose
          (git-util--partial 'mapcar 'km-f-parent-dir)
          (git-util--rpartial split-string "\n" t)
          shell-command-to-string
          (git-util--rpartial string-join "\s"))
         (list command
               (concat "--color=never -HI --max-depth 5 -t d -g '.git' -E Dropbox -E node_modules -E .cache -E .local -E .nvm . "
                       (or dir "~/")))))
       ("find" (funcall
                (git-util--compose
                 (git-util--rpartial split-string "\n" t)
                 shell-command-to-string)
                "find " (or dir "~/") " -name .git -maxdepth 5 -type d -exec dirname {} \\; -prune 2>&1 | grep -v \"Permission denied\""))))
   (nconc
    (list (expand-file-name (or dir "~/")))
    (git-util-non-git-dirs-recoursively
     (or dir "~/") "^[^\\.]"))))

(defun git-util-guess-repos-dirs ()
  "Execute `fdfind' and return list parent directories of git repos."
  (or
   (let ((command (seq-find #'executable-find
                            '("fdfind" "fd" "find"))))
     (pcase command
       ((or "fd" "fdfind")
        (funcall
         (git-util--compose
          delete-dups
          (git-util--partial mapcar (git-util--compose
                                     git-util-f-parent git-util-f-parent))
          (git-util--rpartial split-string "\n" t)
          shell-command-to-string
          (git-util--rpartial string-join "\s"))
         (list command
               "--max-depth 4 --color=never -H -t d -g '.git' -E node_modules -E .cache -E .local -E .nvm . ~/")))
       ("find" (funcall
                (git-util--compose
                 delete-dups
                 (git-util--partial mapcar #'git-util-f-parent)
                 (git-util--rpartial split-string "\n" t)
                 shell-command-to-string)
                "find ~/ -name .git -maxdepth 4 -exec dirname {} \\; -prune 2>&1 | grep -v \"Permission denied\""))))
   (nconc
    (list (expand-file-name "~/"))
    (git-util-non-git-dirs-recoursively
     "~/" "^[^\\.]"))))

(defun git-util-npm-seach-package-info (name)
  "Search NAME with npm and return alist with package info."
  (require 'json)
  (when-let* ((json-object-type 'alist)
              (json-array-type 'list)
              (alist (json-read-from-string
                      (shell-command-to-string
                       (concat "npm search --json " name)))))
    (seq-find (lambda (cell) (equal name (alist-get 'name cell)))
              alist)))

;;;###autoload
(defun git-util-clone-npm-repo ()
  "Clone repository of npm package."
  (interactive)
  (require 'ivy-yarn nil t)
  (when-let* ((result (when (fboundp 'ivy-yarn-read-new-dependency)
                        (ivy-yarn-read-new-dependency)))
              (found (git-util-npm-seach-package-info result)))
    (let ((repo (alist-get 'repository (alist-get 'links found))))
      (git-util-clone-repo repo))))

(defun git-util-clone-read-url ()
	"Read url for git cloning."
  (let ((candidates
         (delete-dups (mapcar #'git-util-normalize-https-url
                              (git-util-url-get-candidates)))))
    (completing-read
     "Clone\s" candidates (lambda (it) (and it
                                       (null (string-match-p "\\?" it))))
     nil)))

;;;###autoload
(defun git-util-clone-repo (&optional url)
  "Clone repository at URL into TARGET-DIR or `km-download-default-repo-dir'."
  (interactive)
  (if-let* ((repo-url (git-util-clone-confirm
                       (or url
                           (git-util-clone-read-url))))
            (basename (file-name-base repo-url))
            (project-dir (git-util-read-dir
                          (format "Clone %s to " basename) basename)))
      (let ((command (read-string "" (string-join
                                      (list "git" "clone" repo-url
                                            project-dir)
                                      "\s"))))
        (setq project-dir (expand-file-name
                           (car (reverse (split-string command)))))
        (git-util-exec-in-dir command project-dir))
    (message "Cannot clone")))

;;;###autoload
(defun git-util-compose-while-not-nil (&rest functions)
  "Return right-to-left composition from FUNCTIONS."
  (let ((fn))
    (setq functions (reverse functions))
    (setq fn (pop functions))
    (lambda (&rest args)
      (let ((arg (unless (null (flatten-list args))
                   (apply fn args))))
        (while (setq fn (unless (null arg)
                          (pop functions)))
          (let ((res (apply fn (list arg))))
            (setq arg res)))
        arg))))

(defun git-util-normalize-https-url (url)
  "Normalize URL to https protocol to ssh."
  (when-let ((urlobj (when (and url
                                (git-util-https-url-p url))
                       (url-generic-parse-url url))))
    (when-let ((host (url-host urlobj))
               (reponame (funcall
                          (git-util-compose-while-not-nil
                           (git-util--rpartial 'km-f-change-ext "git")
                           (git-util--rpartial 'string-join "/")
                           (git-util--partial
                            'km-apply-when
                            (git-util--compose
                             (apply-partially #'<= 2) 'length)
                            (git-util--rpartial 'seq-take 2))
                           (git-util--rpartial 'split-string "/")
                           (apply-partially
                            #'replace-regexp-in-string
                            "^/\\|/$" "")
                           'url-filename)
                          urlobj)))
      (string-trim (concat "https://" host "/" reponame)))))

(defun git-util-url-https-to-ssh (url &optional ssh-host)
  "Transform URL with https protocol to ssh.
With optional argument SSH-HOST also replace host."
  (when-let ((urlobj (when (and url
                                (git-util-https-url-p url))
                       (url-generic-parse-url url))))
    (when-let ((host (url-host urlobj))
               (reponame (funcall
                          (git-util-compose-while-not-nil
                           (git-util--rpartial 'km-f-change-ext "git")
                           (git-util--rpartial 'string-join "/")
                           (git-util--partial
                            'km-apply-when
                            (git-util--compose
                             (apply-partially #'<= 2) 'length)
                            (git-util--rpartial 'seq-take 2))
                           (git-util--rpartial 'split-string "/")
                           (apply-partially
                            #'replace-regexp-in-string
                            "^/\\|/$" "")
                           'url-filename)
                          urlobj)))
      (string-trim (concat "git@" (or ssh-host host) ":" reponame)))))

(defun git-util-ssh-to-https (ssh-remote)
	"Convert SSH-REMOTE to https url."
  (with-temp-buffer
    (save-excursion (insert ssh-remote))
    (when (re-search-forward "@" nil t 1)
      (when-let* ((beg (point))
                  (end (re-search-forward ":" nil t 1)))
        (string-trim
         (concat "https://"
                 (buffer-substring-no-properties
                  beg (1- end))
                 "/"
                 (buffer-substring-no-properties
                  end (point-max))))))))

(defun git-util-remotes-alist ()
  "Return alist of remotes and associated urls (REMOTE-NAME . REMOTE-URL)."
  (when-let ((remotes
              (with-temp-buffer
                (when (= 0 (apply #'call-process "git" nil t nil
                                  '("remote" "-v")))
                  (string-trim (buffer-string))))))
    (seq-uniq
     (mapcar (lambda (l) (let ((parts (split-string l)))
                      (cons (car parts)
                            (cadr parts))))
             (split-string remotes "\n" t)))))

(defun git-util-current-branch (&optional directory)
  "Return current git branch in DIRECTORY.
Default value for DIRECTORY is `default-directory'."
  (if directory
      (let ((default-directory directory))
        (git-util-call-process "git" "rev-parse" "--abbrev-ref" "HEAD"))
    (git-util-call-process "git" "rev-parse" "--abbrev-ref" "HEAD")))

;;;###autoload
(defun git-util-change-remote-to-ssh ()
  "Switch remote urLs from HTTPS to SSH."
  (interactive)
  (if-let ((remotes (git-util-remotes-alist)))
      (let* ((cell (if (> (length remotes) 1)
                       (rassoc
                        (completing-read "Remote" (mapcar #'cdr remotes) nil t)
                        remotes)
                     (car remotes)))
             (new-url (string-trim
                       (read-string (format "Change %s to\s" (cdr cell))
                                    (git-util-url-https-to-ssh (cdr cell))))))
        (message
         (shell-command-to-string
          (string-join
           (list "git remote set-url" (car cell)
                 new-url)
           "\s"))))
    (message "No remotes found")))

(defun git-util-retrieve-issue-key-from-branch (str)
	"Retrieve jira issue from STR."
  (when-let ((re "[[:upper:]]+-[[:digit:]]+"))
    (when (string-match-p re str)
      (replace-regexp-in-string
       (concat ".*?\\(" re "\\).*")
       "\\1"
       str))))

(defvar jiralib-url)
(defun git-util-commit-jira-message-setup ()
	"Try to insert template with jira template."
  (require 'jiralib nil t)
  (when (and (bound-and-true-p jiralib-url)
             (string-prefix-p "https://" jiralib-url))
    (when-let* ((branch (git-util-current-branch default-directory))
                (issue-key (git-util-retrieve-issue-key-from-branch
                            branch))
                (url (concat
                      "[" issue-key "]" "("
                      (replace-regexp-in-string "/$" ""
                                                jiralib-url)
                      "/browse/"
                      issue-key
                      ")"))
                (title (format "%s:" issue-key)))
      (unless (save-excursion
                (re-search-forward
                 (regexp-quote title) nil t 1))
        (insert title)
        (save-excursion
          (end-of-line)
          (newline-and-indent 2)
          (insert (format "%s:" url))
          (indent-for-tab-command))))))

(provide 'git-util)
;;; git-util.el ends here