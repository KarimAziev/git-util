;;; git-util.el --- Collection of git utils -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/git-util
;; Version: 0.1.0
;; Keywords: vc
;; Package-Requires: ((emacs "29.1") (transient "0.4.1") (magit "3.3.0"))
;; SPDX-License-Identifier: GPL-3.0-or-later

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

(declare-function vc-git-root "vc-git")
(declare-function shell-mode "shell-mode")
(declare-function comint-output-filter "comint")
(declare-function json-read-from-string "json")
(declare-function json-read-file "json")
(declare-function url-host "url-parse")
(declare-function url-filename "url-parse")

(require 'transient)
(require 'magit)

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

(defcustom git-util-fd-args '("-E" "node_modules" "-E" "Dropbox" "-E" "Trash")
  "Extra arguments for fdfind."
  :type '(repeat (string))
  :group 'git-util)

(defcustom git-util-magit-open-windows-in-direction 'right
  "What direction to open new windows from the status buffer.
For example, diffs and log buffers. Accepts `left', `right', `up', and `down'."
  :group 'git-util
  :type '(choice
          (const right)
          (const left)
          (const up)
          (const down)))

(defcustom git-util-after-create-repo-hook nil
  "Hook run after creating a repository.

A hook that runs after a repository is successfully created.

Functions added to this hook are executed with the newly created
repository's directory as the current working directory.

This can be used to perform additional setup or configuration
tasks, such as initializing submodules, setting up virtual
environments, or installing dependencies."
  :group 'gh-repo
  :type 'hook)

(defmacro git-util--pipe (&rest functions)
  "Return left-to-right composition from FUNCTIONS."
  (declare (debug t)
           (pure t)
           (side-effect-free t))
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
  (declare (debug t)
           (pure t)
           (side-effect-free t))
  `(git-util--pipe ,@(reverse functions)))

(defun git-util-compose-while-not-nil (&rest functions)
  "Compose FUNCTIONS from functions until a nil result.

Remaining arguments FUNCTIONS are functions that will be composed together."
  (let ((fn))
    (setq functions (reverse functions))
    (setq fn (pop functions))
    (lambda (&rest args)
      (let ((arg
             (unless (null (flatten-list args))
               (apply fn args))))
        (while (setq fn
                     (unless (null arg)
                       (pop functions)))
          (let ((res (apply fn (list arg))))
            (setq arg res)))
        arg))))

(defmacro git-util--or (&rest functions)
  "Return an unary function which invoke FUNCTIONS until first non-nil result."
  (declare (debug t)
           (pure t)
           (side-effect-free t))
  `(lambda (it)
     (or
      ,@(mapcar (lambda (v)
                  (if (symbolp v)
                      `(,v it)
                    `(funcall ,v it)))
                functions))))

(defmacro git-util--and (&rest functions)
  "Return an unary function which invoke FUNCTIONS until first nil result."
  (declare (debug t)
           (pure t)
           (side-effect-free t))
  `(lambda (it)
     (and
      ,@(mapcar (lambda (v)
                  (if (symbolp v)
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

(defmacro git-util-when (pred fn)
  "Return a function that call FN if the result of calling PRED is non-nil.
Both PRED and FN are called with one argument.
If the result of PRED is nil, return the argument as is."
  (declare
   (indent defun))
  `(lambda (arg)
     (if ,(if (symbolp pred)
              `(,pred arg)
            `(funcall ,pred arg))
         ,(if (symbolp fn)
              `(,fn arg)
            `(funcall ,fn arg))
       arg)))

(defun git-util-call-process (command &rest args)
  "Execute COMMAND with ARGS synchronously.

Returns (STATUS . OUTPUT) when it is done, where STATUS is the returned error
code of the process and OUTPUT is its stdout output."
  (let ((buff (generate-new-buffer command)))
    (with-current-buffer buff
      (erase-buffer)
      (let ((status))
        (setq status (apply #'call-process command nil t nil
                            (delq nil (flatten-list args))))
        (let ((result (string-trim (buffer-string))))
          (if (= 0 status)
              (prog1 result (kill-current-buffer))
            (let ((command-with-args (concat command "\s" (string-join
                                                           (delq nil
                                                                 (flatten-list
                                                                  args))
                                                           "\s"))))
              (message "Error %s in %s: %s" command-with-args
                       (when default-directory
                         (abbreviate-file-name
                          default-directory))
                       result))
            nil))))))

(defun git-util-exec-in-dir (command project-dir &optional callback)
  "Execute COMMAND in PROJECT-DIR.
If PROJECT-DIR doesn't exists, create new.
Invoke CALLBACK without args."
  (require 'shell)
  (require 'comint)
  (let ((proc)
        (buffer (generate-new-buffer (format "*%s*" command))))
    (progn (switch-to-buffer buffer)
           (with-current-buffer buffer
             (if (file-exists-p project-dir)
                 (setq default-directory project-dir)
               (mkdir project-dir t)
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
                (if (= (process-exit-status process)
                       0)
                    (progn
                      (message "finished")
                      (if callback
                          (funcall callback)
                        (dired project-dir)))
                  (user-error (format "%s\n%s" command output))))))
           (set-process-filter proc #'comint-output-filter))))

;; file utils

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

(defun git-util-f-non-hidden-dirs (directory &optional full)
  "Return absolute (with FULL) or relative non-hidden directories in DIRECTORY.
The only one exception is made for `user-emacs-directory'."
  (let ((dirs (seq-filter #'file-directory-p
                          (directory-files directory t "\\`[^\\.]"))))
    (if full
        dirs
      (let ((len (length (file-name-as-directory
                          (expand-file-name directory)))))
        (mapcar
         (git-util--rpartial substring len)
         dirs)))))

(defun git-util-shell-command-to-list (command &rest args)
  "Apply shell COMMAND with ARGS and return list of lines from output."
  (when-let* ((result (apply #'git-util-call-process command args)))
    (split-string result "\n")))

(defun git-util-map-search-paths (dirs)
  "Add fd search-path option to every directory in DIRS."
  (mapcan (lambda (it)
            (list "--search-path" it))
          dirs))

(defun git-util-fdfind-get-repo-search-paths (&optional directory)
  "Return flags with non-hidden directories in DIRECTORY to search with `fd'."
  (git-util-map-search-paths
   (git-util-f-non-hidden-dirs (or directory "~/") t)))

(defun git-util-fdfind-get-all-git-repos (&optional directory)
  "Return flags with directories in DIRECTORY to search with `fd'."
  (apply #'git-util-shell-command-to-list
         "fdfind"
         (delq nil
               (nconc (list "--color=never"
                            "--hidden"
                            "--glob"
                            ".git"
                            "-t"
                            "d")
                      (git-util-fdfind-get-repo-search-paths directory)
                      '("-x"
                        "dirname")))))

(defun git-util-fdfind-get-all-repos-parents-dir (&optional directory)
  "List unique parent directories of Git repositories.

Optional argument DIRECTORY is the directory to search for Git repositories. If
not provided, the default is the user's home directory."
  (with-temp-buffer
    (erase-buffer)
    (when-let* ((buff (current-buffer))
               (result (apply #'git-util-call-process
                              "fdfind"
                              (delq nil
                                    (append
                                     git-util-fd-args
                                     (list "--color=never"
                                           "-I"
                                           "--hidden"
                                           "--glob"
                                           ".git"
                                           "-t"
                                           "d"
                                           "--max-depth" "5"
                                           (expand-file-name
                                            (or directory "~/")))
                                     (list "-x" "dirname" "{//}"))))))
      (insert result)
      (shell-command-on-region (point-min)
                               (point-max)
                               "sort" buff)
      (shell-command-on-region (point-min)
                               (point-max)
                               "uniq" buff)
      (split-string (buffer-substring-no-properties (point-min)
                                                    (point-max))
                    "\n" t))))

(defun git-util-f-get-git-repos (&optional directory)
  "Return list of git repositories in DIRECTORY or home directory."
  (unless directory (setq directory (expand-file-name "~/")))
  (let ((dirs
         (or
          (let ((command (seq-find #'executable-find
                                   '("fdfind" "fd" "find"))))
            (pcase command
              ((or "fd" "fdfind")
               (apply #'git-util-shell-command-to-list
                      "fdfind"
                      '("--color=never"
                        "--hidden"
                        "--glob"
                        ".git"
                        "-t"
                        "d")
                      (git-util-fdfind-get-repo-search-paths directory)
                      '("-x"
                        "dirname")))
              ("find"
               (funcall
                (git-util--compose
                 (git-util--rpartial split-string "\n" t)
                 shell-command-to-string)
                "find " directory
                " -name .git -maxdepth 5 -type d -exec dirname {} \\; -prune 2>&1 | grep -v \"Permission denied\""))))
          (nconc
           (list (expand-file-name directory))
           (git-util-f-non-git-dirs-recoursively
            directory "^[^\\.]")))))
    (delete-dups (delq nil dirs))))

(defun git-util-f-get-git-repos-in-dirs (dirs &rest flags)
  "Return list of git repositories in DIRS with FLAGS."
  (or
   (let ((command (seq-find #'executable-find
                            '("fdfind" "fd" "find"))))
     (pcase command
       ((or "fd" "fdfind")
        (apply #'git-util-shell-command-to-list
               "fdfind"
               '("--color=never"
                 "--hidden"
                 "--glob"
                 ".git"
                 "-t"
                 "d")
               flags
               (git-util-map-search-paths (mapcar #'expand-file-name
                                                  dirs))
               '("-x"
                 "dirname")))))))

(defun git-util-f-guess-repos-dirs ()
  "Execute `fdfind' and return list parent directories of git repos."
  (let ((command (seq-find #'executable-find
                           '("fdfind" "fd" "find"))))
    (pcase command
      ((or "fd" "fdfind")
       (git-util-fdfind-get-all-repos-parents-dir))
      ("find"
       (funcall
        (git-util--compose
         delete-dups
         (git-util--partial mapcar #'git-util-f-parent)
         (git-util--rpartial split-string "\n" t)
         shell-command-to-string)
        "find ~/ -name .git -maxdepth 4 -exec dirname {} \\; -prune 2>&1 | grep -v \"Permission denied\""))
      (_ (or
          (nconc
           (list (expand-file-name "~/"))
           (git-util-f-non-git-dirs-recoursively
            "~/" "^[^\\.]")))))))

(defun git-util-filter-repos-by-email (email repos-dirs)
  "Return REPOS-DIRS in which EMAIL is member of authors."
  (seq-filter (git-util--compose
               (apply-partially #'member email)
               git-util-get-authors-emails)
              repos-dirs))

(defun git-util-f-get-dirs-with-author ()
  "Read git repository of current author or all."
  (let ((author (git-util-config "user.email")))
    (completing-read "Repository: "
                     (git-util-f-get-git-repos (read-directory-name
                                                "Search repos in"))
                     (when author
                       (git-util--compose
                        (apply-partially #'member author)
                        git-util-get-authors-emails)))))

(defun git-util-with-every-author-straight-dir (fn)
  "Call FN without args in every straight directory."
  (require 'straight)
  (let ((author
         (let ((default-directory user-emacs-directory))
           (git-util-config "user.email"))))
    (dolist (dir (git-util-f-get-git-repos
                  (when (fboundp 'straight--repos-dir)
                    (straight--repos-dir))))
      (when (member author (git-util-get-authors-emails dir))
        (let ((default-directory dir))
          (funcall fn))))))


(defun git-util-f-parent (path)
  "Return the parent directory to PATH."
  (let ((parent (file-name-directory
                 (directory-file-name
                  (expand-file-name path default-directory)))))
    (when-let* ((dir (and (file-exists-p path)
                         (file-exists-p parent)
                         (not (equal
                               (file-truename (directory-file-name
                                               (expand-file-name path)))
                               (file-truename (directory-file-name
                                               (expand-file-name parent)))))
                         (if (file-name-absolute-p path)
                             (directory-file-name parent)
                           (file-relative-name parent)))))
      (file-name-as-directory dir))))

(defun git-util-f-change-ext (file new-ext)
  "Replace extension of FILE with NEW-EXT."
  (concat (file-name-sans-extension file)
          "." new-ext))

(defun git-util-f-guess-repos-dirs-find ()
  "Execute `find' and return list parent directories of git repos."
  (completing-read "Direcotory:\s"
                   (funcall
                    (git-util--compose
                     delete-dups
                     (git-util--partial mapcar #'git-util-f-parent)
                     (git-util--rpartial split-string "\n" t)
                     shell-command-to-string)
                    "find ~/ -name .git -maxdepth 4 -exec dirname {} \\; -prune 2>&1 | grep -v \"Permission denied\"")))

(defun git-util-f-directory-files (directory &optional nosort)
  "Return a list of names of files in DIRECTORY excluding \".\" and \"..\".

Names are that are relative to the specified directory.

If NOSORT is non-nil, the list is not sorted--its order is unpredictable.
 Otherwise, the list returned is sorted with string-lessp."
  (directory-files directory nil
                   directory-files-no-dot-files-regexp nosort))

(defun git-util-f-dir-empty-p (directory)
  "Return t if DIRECTORY is empty."
  (null (git-util-f-directory-files directory)))

(defun git-util-f-non-git-dirs-recoursively (directory &optional match depth)
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

(defun git-util-read-dir (prompt basename)
  "Read a new directory with PROMPT and BASENAME."
  (let* ((dir-files (git-util-f-directory-files default-directory))
         (default-variants
          (mapcar
           (lambda (dir)
             (if (file-exists-p
                  (expand-file-name basename dir))
                 (let ((count 0)
                       (name basename))
                   (while (and (file-exists-p
                                (expand-file-name name dir))
                               (not (git-util-f-dir-empty-p
                                     (expand-file-name name dir))))
                     (setq count (1+ count))
                     (setq name (format "%s-%s"
                                        (replace-regexp-in-string
                                         "-[0-9]+$" "" name)
                                        count)))
                   (expand-file-name name dir))
               (expand-file-name basename dir)))
           (or
            (git-util-get-projects-parents-dir)
            (git-util-f-guess-repos-dirs))))
         (variants (if (null dir-files)
                       (append `(,default-directory)
                               default-variants)
                     default-variants)))
    (file-name-as-directory
     (completing-read (or prompt "Directory:\s")
                      variants nil nil
                      (git-util-f-parent
                       (car variants))))))

;; chrome

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
  (require 'json)
  (when-let* ((file (git-util-chrome-url-chrome-guess-bookmarks-file))
             (json-object-type 'plist)
             (json-array-type 'list))
    (when (file-exists-p file)
      (json-read-file
       file))))

(defun git-util-chrome-list-urls-from-json-plist (plist)
  "Return list of git urls from chrome bookmarks PLIST."
  (when-let* ((roots (cdr (plist-get plist :roots))))
    (let ((children)
          (current))
      (while (setq current (pop roots))
        (if (and current (listp current)
                 (plist-get current :children))
            (setq roots (append roots (plist-get current :children)))
          (when (listp current)
            (when-let* ((url (plist-get current :url)))
              (when (string-match-p url git-util-host-regexp)
                (setq children (push (plist-get current :url)
                                     children)))))))
      children)))

(defun git-util-chrome-git-urls-from-chrome-history ()
  "Read `git-util-chrome-url-chrome-history-file' and return list of git urls."
  (unless git-util-chrome-url-chrome-history-file
    (setq git-util-chrome-url-chrome-history-file
          (git-util-chrome-url-chrome-guess-history-file)))
  (when (or (null git-util-chrome-url-chrome-history-file)
            (not (file-exists-p git-util-chrome-url-chrome-history-file)))
    (user-error
     "'%s' doesn't exist, reset `git-util-chrome-url-chrome-history-file'"
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
            (while (re-search-forward (rx (group (+? anything)) "\x1e")
                                      nil t)
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

(defun git-util-chrome-session-dump-get-active-tabs ()
  "Return list of active tabs in google-chrome."
  (when-let* ((file
              (when (file-exists-p "~/.config/google-chrome/")
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

;; url utils

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
  (when-let* ((url (thing-at-point 'url t)))
    (when (string-match-p git-util-host-regexp url)
      url)))

(defun git-util-strip-text-props (item)
  "If ITEM is string, return it without text properties.

 If ITEM is symbol, return it is `symbol-name.'
 Otherwise return nil."
  (cond ((stringp item)
         (let ((str (seq-copy item)))
           (set-text-properties 0 (length str)
                                nil str)
           str))
        ((and item (symbolp item))
         (symbol-name item))
        (nil item)))

(defun git-util-url-get-candidates ()
  "Return list of urls from `kill-ring', buffer, chrome history, bookmarks etc."
  (let ((urls (delete nil (nconc
                           (git-util-list-git-urls-from-kill-ring)
                           (git-util-chrome-session-dump-get-active-tabs)
                           (git-util-list-git-urls-from-minibuffer-history)
                           (git-util-list-git-urls-from-chrome-bookmarks)
                           (git-util-chrome-git-urls-from-chrome-history))))
        (git-url-at-point (git-util-git-url-at-point))
        (gui-urls (delq nil (mapcar (git-util--compose
                                     (git-util--and stringp
                                                    git-util-strip-text-props
                                                    (apply-partially
                                                     #'string-match-p
                                                     git-util-host-regexp)
                                                    identity)
                                     (git-util--partial gui-get-selection))
                                    '(CLIPBOARD PRIMARY SECONDARY)))))
    (when git-url-at-point
      (push git-url-at-point urls))
    (when gui-urls
      (setq urls (nconc gui-urls urls)))
    urls))


(defun git-util-https-url-p (url)
  "Return t if URL string is githost with https protocol."
  (and (string-prefix-p "https://" url)
       (string-match-p git-util-host-regexp url)))

(defun git-util-ssh-url-p (url)
  "Return t if URL string is githost with git protocol."
  (string-match-p
   (concat "git@" git-util-host-regexp)
   url))

(defun git-util-config (&rest args)
  "Exec git config with ARGS."
  (apply #'git-util-call-process "git"
         (delq nil (flatten-list (list "config" args)))))

(defun git-util-config-user-name ()
  "Return current user name from git config."
  (git-util-config "user.name"))

(defun git-util-clone-confirm (url)
  "Convert URL to ssh format and read it from minibuffer."
  (let ((variants (git-util-get-ssh-variants
                   (if (git-util-ssh-url-p url)
                       url
                     (git-util-url-https-to-ssh url)))))
    (if (> (length variants)
           1)
        (completing-read "git clone\s" variants)
      (car variants))))



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
            (push (cons host hostname)
                  alist)))
        alist))))

(defun git-util-get-ssh-variants (ssh-url)
  "Return variants of git ssh for SSH-URL."
  (let* ((local-alist (git-util-alist-ssh-hosts))
         (cell (with-temp-buffer
                 (save-excursion
                   (insert (replace-regexp-in-string "^git@" ""
                                                     ssh-url)))
                 (let ((beg (point))
                       (end))
                   (setq end (re-search-forward git-util-host-regexp nil t 1))
                   (cons (buffer-substring-no-properties beg end)
                         (string-trim (buffer-substring-no-properties
                                       end
                                       (point-max))))))))
    (setq local-alist (seq-filter (lambda (it)
                                    (equal
                                     (car cell)
                                     (cdr it)))
                                  local-alist))
    (seq-uniq
     (append
      (list ssh-url)
      (mapcar (lambda (it)
                (concat "git@" (car it)
                        (cdr cell)))
              local-alist)))))

(defun git-util-get-authors-emails (directory)
  "Return list of all contributed emails in repository DIRECTORY."
  (require 'vc-git)
  (when-let* ((git-root (vc-git-root directory))
              (items
               (let ((default-directory (expand-file-name git-root)))
                 (git-util-call-process
                  "git" "log" "--all" "--format=%cE"))))
    (seq-uniq (split-string items "\n"))))

(defun git-util-get-authors-names (directory)
  "Return list of all contributed user names in repository DIRECTORY."
  (when-let*
      ((default-directory directory)
       (output (git-util-call-process "git" "log" "--all" "--format=%cN")))
    (split-string
     output
     "\n" t)))

(defun git-util-get-authors (directory)
  "Return alist of names and emails contributed to repository DIRECTORY."
  (when-let* ((default-directory directory)
              (output (git-util-call-process "git" "shortlog" "-n" "-s" "-e"
                                             "HEAD")))
    (mapcar (lambda (line)
              (let* ((parts (reverse (split-string line nil t)))
                     (email (pop parts))
                     (name (string-join (reverse parts) "\s")))
                (cons (replace-regexp-in-string "^[0-9]+[\s\t]*" "" name)
                      (replace-regexp-in-string "^<\\|>$" "" email))))
            (split-string
             output
             "\n" t))))

;;;###autoload
(defun git-util-visit-remote ()
  "Open the browser to visit a Git repository's remote URL."
  (interactive)
  (require 'url-parse)
  (when-let* ((url (cdar (git-util-remotes-alist))))
    (setq url (replace-regexp-in-string
               "\\.git$"
               ""
               (or (git-util-ssh-to-https url)
                   url)))
    (browse-url url)))

(defun git-util-url-to-recipe (url)
  "Return plist of current git URL as straight recipe :repo, :type and :host."
  (require 'url-parse)
  (when-let* ((urlobj (url-generic-parse-url (or (git-util-ssh-to-https url)
                                                 url)))
              (host (url-host urlobj))
              (filename (url-filename urlobj))
              (base-name (file-name-base host)))
    `(:repo
      ,(replace-regexp-in-string
        "^/\\|[\\.]git$" "" filename)
      :type
      git
      :host
      ,(intern base-name))))

(defun git-util-melpa-current-recipe ()
  "Return plist of current git repo as straight recipe :repo, :type and :host."
  (require 'url-parse)
  (when-let* ((url (cdar (git-util-remotes-alist)))
              (urlobj (url-generic-parse-url (or (git-util-ssh-to-https url)
                                                 url)))
              (host (url-host urlobj))
              (filename (url-filename urlobj)))
    (list
     :repo
     (replace-regexp-in-string
      "^/\\|[\\.]git$" "" filename)
     :type "git"
     :host
     (file-name-base host))))

(defun git-util-melpa-recipe-in-dir (dir &optional package-name)
  "Return recipe for PACKAGE-NAME in DIR.
Recipe is a list, e.g. (PACKAGE-NAME :repo \"owner/repo\" :fetcher github)."
  (let* ((recipe (git-util-straight-recipe-in-dir dir))
         (name (or package-name
                   (car (last (split-string
                               (plist-get recipe :repo) "/" t))))))
    (list (if (stringp name)
              (intern name)
            name)
          :repo (plist-get recipe :repo)
          :fetcher (intern (plist-get recipe :host)))))

(defun git-util-straight-recipe-in-dir (directory)
  "Return plist of git repo in DIRECTORY as straight recipe."
  (let ((default-directory directory))
    (git-util-melpa-current-recipe)))

(defun git-util-repo-status (directory)
  "Return git status for DIRECTORY."
  (when-let* ((default-directory directory))
    (git-util-call-process "git" "status")))

(defun git-util-repo-modified-p (directory)
  "Return non nil if DIRECTORY git status is not up to date."
  (when-let* ((default-directory directory)
              (status (git-util-call-process "git" "status" "--short")))
    (not (string-empty-p status))))

(defun git-util-modified-repos-in-dir (directory)
  "Return list of modified repos in DIRECTORY."
  (seq-filter #'git-util-repo-modified-p (git-util-f-get-git-repos
                                          directory)))

(defun git-util-npm-seach-package-info (name)
  "Search NAME with npm and return alist with package info."
  (require 'json)
  (when-let* ((json-object-type 'alist)
              (json-array-type 'list)
              (alist (json-read-from-string
                      (shell-command-to-string
                       (concat "npm search --json " name)))))
    (seq-find (lambda (cell)
                (equal name (alist-get 'name cell)))
              alist)))

(defun git-util-normalize-url-filename (filename)
  "Transform FILENAME to git filename."
  (funcall (git-util-compose-while-not-nil
            (git-util-when (git-util--compose
                            not
                            (apply-partially #'string-suffix-p
                                             ".git"))
              (git-util--rpartial concat ".git"))
            (git-util--rpartial string-join "/")
            (git-util-when
              (git-util--compose
               (apply-partially #'<= 2)
               length)
              (git-util--rpartial seq-take 2))
            (git-util--rpartial split-string "/")
            (apply-partially
             #'replace-regexp-in-string
             "^/\\|/$" ""))
           filename))

(defun git-util-normalize-https-url (url)
  "Normalize URL to https protocol to ssh."
  (require 'url-parse)
  (when-let* ((urlobj
              (when (and url
                         (git-util-https-url-p url))
                (url-generic-parse-url url))))
    (when-let* ((host (url-host urlobj))
               (reponame (git-util-normalize-url-filename
                          (url-filename urlobj))))
      (string-trim (concat "https://" host "/" reponame)))))

(defun git-util-check-ssh-host (url)
  "Check SSH host from Git URL.

Argument URL is a string representing the url to check for SSH host keys."
  (require 'url-parse)
  (unless (git-util-https-url-p url)
    (setq url (git-util-ssh-to-https url)))
  (when-let* ((urlobj
              (when (and url
                         (git-util-https-url-p url))
                (url-generic-parse-url url))))
    (when-let* ((host (url-host urlobj))
               (reponame (git-util-normalize-url-filename
                          (url-filename urlobj))))
      (git-util-call-process "ssh-keygen" "-F" host))))

(defun git-util-url-https-to-ssh (url &optional ssh-host)
  "Transform URL with https protocol to ssh.
With optional argument SSH-HOST also replace host."
  (require 'url-parse)
  (when-let* ((urlobj
              (when (and url
                         (git-util-https-url-p url))
                (url-generic-parse-url url))))
    (when-let* ((host (url-host urlobj))
               (reponame (git-util-normalize-url-filename
                          (url-filename urlobj))))
      (string-trim (concat "git@" (or ssh-host host)
                           ":" reponame)))))

(defun git-util-ssh-to-https (ssh-remote)
  "Convert SSH-REMOTE to https url."
  (with-temp-buffer
    (save-excursion
      (insert ssh-remote))
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
  (when-let* ((remotes
               (with-temp-buffer
                 (when (= 0 (apply #'call-process "git" nil t nil
                                   '("remote" "-v")))
                   (string-trim (buffer-string))))))
    (seq-uniq
     (mapcar (lambda (l)
               (let ((parts (split-string l)))
                 (cons (car parts)
                       (cadr parts))))
             (split-string remotes "\n" t)))))

(defun git-util-read-remote-alist ()
  "Return cons of (REMOTE-NAME . REMOTE-URL).
If there is more than one remote, read it in minibuffer with completions."
  (when-let* ((remotes (git-util-remotes-alist))
              (cell (if (> (length remotes) 1)
                        (assoc-string
                         (let* ((annotf (lambda (str)
                                          (concat " " (or
                                                       (cdr (assoc str remotes))
                                                       "")))))
                           (completing-read "Remote url: "
                                            (lambda (str pred action)
                                              (if (eq action 'metadata)
                                                  `(metadata
                                                    (annotation-function .
                                                                         ,annotf))
                                                (complete-with-action action
                                                                      remotes
                                                                      str pred)))))
                         remotes)
                      (car remotes))))
    cell))

(defun git-util-current-branch (&optional directory)
  "Return current git branch in DIRECTORY.
Default value for DIRECTORY is `default-directory'."
  (if directory
      (let ((default-directory directory))
        (git-util-call-process "git" "rev-parse" "--abbrev-ref" "HEAD"))
    (git-util-call-process "git" "rev-parse" "--abbrev-ref" "HEAD")))

(defun git-util-jira-retrieve-issue-key-from-branch (str)
  "Retrieve jira issue from STR."
  (let ((re "[[:upper:]]+[-_][[:digit:]]+"))
    (when (string-match-p re str)
      (replace-regexp-in-string
       (concat ".*?\\(" re "\\).*")
       "\\1"
       str))))

(defvar jiralib-url)

(defun git-util-jira-commit-message-setup ()
  "Try to insert template with jira template."
  (require 'jiralib nil t)
  (when (and (bound-and-true-p jiralib-url)
             (string-prefix-p "https://" jiralib-url))
    (when-let* ((branch (git-util-current-branch default-directory))
                (issue-key (git-util-jira-retrieve-issue-key-from-branch
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
                 (regexp-quote title)
                 nil t 1))
        (insert title)
        (save-excursion
          (end-of-line)
          (newline-and-indent 2)
          (insert (format "%s:" url))
          (indent-for-tab-command))))))

;;;###autoload
(defun git-util-clone-npm-repo ()
  "Clone repository of npm package."
  (interactive)
  (require 'ivy-yarn nil t)
  (when-let* ((result
               (when (fboundp 'ivy-yarn-read-new-dependency)
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
     "Clone\s" candidates (lambda (it)
                            (and it
                                 (null (string-match-p "\\?" it))))
     nil)))

(defun git-util-git-top-level-non-git-dir ()
  "Traverse up to non-git directory."
  (let ((dir)
        (curr default-directory))
    (while (setq dir
                 (when curr
                   (locate-dominating-file curr ".git")))
      (setq curr (file-name-parent-directory dir)))
    curr))

(defun git-util-get-projects-parents-dir ()
  "Retrieve the parent directories of all known git projects."
  (require 'project nil t)
  (let ((parents))
    (dolist (dir (project-known-project-roots))
      (let ((parent-dir (file-name-parent-directory dir)))
        (unless (member parent-dir parents)
          (push parent-dir parents))))
    parents))

;;;###autoload
(defun git-util-clone-repo (&optional url)
  "Clone repository at URL."
  (interactive)
  (if-let* ((repo-url (git-util-clone-confirm
                       (or url
                           (git-util-clone-read-url))))
            (basename (file-name-base repo-url))
            (project-dir (git-util-read-dir
                          (format "Clone %s to " basename)
                          basename)))
      (let ((command (read-string "" (string-join
                                      (list "git" "clone" repo-url
                                            project-dir)
                                      "\s"))))
        (setq project-dir (expand-file-name
                           (car (reverse (split-string command)))))
        (git-util-exec-in-dir command project-dir
                              (lambda ()
                                (when (file-exists-p project-dir)
                                  (let ((default-directory project-dir))
                                    (run-hooks
                                     'git-util-after-create-repo-hook))))))
    (message "Cannot clone")))

;;;###autoload
(defun git-util-cycle-remote-url (&optional no-confirm)
  "Switch remote url in current repository from https to ssh, or ssh to https.
With optional argumnt NO-CONFIRM don't prompt in minibuffer."
  (interactive "P")
  (when-let* ((cell (git-util-read-remote-alist))
              (current-url (cdr cell))
              (new-url
               (cond ((not (git-util-check-ssh-host current-url))
                      (when (git-util-ssh-url-p current-url)
                        (git-util-ssh-to-https current-url)))
                     (t (or
                         (git-util-ssh-to-https current-url)
                         (git-util-url-https-to-ssh current-url))
                        (git-util-url-https-to-ssh current-url)))))
    (if no-confirm
        (git-util-call-process
         "git" "remote" "set-url" (car cell)
         new-url)
      (and (yes-or-no-p (format "Change remote %s url from %s to %s?"
                                (car cell)
                                current-url
                                new-url))
           (git-util-call-process
            "git" "remote" "set-url" (car cell)
            new-url)))))

;;;###autoload
(defun git-util-change-remote-to-ssh (&optional no-confirm)
  "Switch remote url in current repository from https to ssh.
With optional argumnt NO-CONFIRM don't prompt in minibuffer."
  (interactive "P")
  (when-let* ((cell (git-util-read-remote-alist))
              (current-url (cdr cell))
              (new-url
               (when (git-util-check-ssh-host current-url)
                 (git-util-url-https-to-ssh current-url))))
    (if no-confirm
        (git-util-call-process
         "git" "remote" "set-url" (car cell)
         new-url)
      (and (yes-or-no-p (format "Change remote %s url from %s to %s?"
                                (car cell)
                                current-url
                                new-url))
           (git-util-call-process
            "git" "remote" "set-url" (car cell)
            new-url)))))


(defun git-util-magit-change-protocol (&optional prompt _initial-input history)
  "Read remote url using converted value of current remote url as initial input.
If the current url is https, use ssh protocol, otherwise - https.
PROMPT and HISTORY are arguments for `read-string'."
  (let* ((remote (substring-no-properties (magit-get-current-remote)))
         (remote-cell (assoc remote
                             (git-util-remotes-alist)))
         (url (cdr remote-cell))
         (new-url (read-string (or prompt
                                   (format "Change %s to\s" url))
                               (or (git-util-url-https-to-ssh url)
                                   (git-util-ssh-to-https url))
                               history)))
    (list new-url)))

(transient-define-infix git-util-magit-remote.<remote>.url ()
  "Configure remote url using converted value as initial input.
If the current url is https, use ssh protocol, otherwise - https.
PROMPT and HISTORY are arguments for `read-string'."
  :class 'magit--git-variable:urls
  :scope #'magit--read-remote-scope
  :reader 'git-util-magit-change-protocol
  :variable "remote.%s.url"
  :multi-value t
  :history-key 'magit-remote.<remote>.*url)

(defun git-util-replace-magit-remote-suffix ()
  "Replace remote url infix in `magit-remote' prefix."
  (transient-replace-suffix 'magit-remote 'magit-remote.<remote>.url
    '("u" git-util-magit-remote.<remote>.url)))


(defun git-util-with-with-every-dir (fn dirs)
  "Call FN in every directory from DIRS."
  (let ((max (length dirs))
        (result))
    (dotimes (k max)
      (let ((file (nth k dirs)))
        (when (and (file-exists-p file)
                   (file-directory-p file))
          (delay-mode-hooks
            (let ((default-directory (expand-file-name file)))
              (when-let* ((res (funcall fn)))
                (push res result)))))))
    (nreverse result)))

;;;###autoload
(defun git-util-straight-dirs-to-ssh ()
  "Fix git remote urls in straight-repos."
  (interactive)
  (when (fboundp 'straight--repos-dir)
    (git-util-with-with-every-dir
     (lambda ()
       (when (= (length (git-util-remotes-alist)) 1)
         (git-util-change-remote-to-ssh)))
     (directory-files (straight--repos-dir) t
                      directory-files-no-dot-files-regexp))))




(defun git-util-retrieve-host (url)
  "Retrieve host alias from the given git URL.
This function takes the URL string of a git repository, and returns the
alias of the host as specified in your SSH config.
Returns nil if '@' symbol is not found in URL."
  (let* ((splitted-url (car (split-string url ":" t)))
         (start (string-match-p "@" splitted-url)))
    (when start
      (substring-no-properties splitted-url (1+ start)))))

(defvar git-util-files-cache (make-hash-table :test 'equal))

(defun git-util-get-file-cache (cache-key)
  "Retrieve cached data if file hasn't changed.

Argument CACHE-KEY is a key used to retrieve the cache entry from
`git-util-files-cache'."
  (let* ((cache
          (gethash cache-key git-util-files-cache))
         (cache-tick (and cache (plist-get cache :tick)))
         (tick (and cache
                    (file-attribute-modification-time (file-attributes
                                                       cache-key
                                                       'string)))))
    (if (and cache (equal cache-tick tick))
        (plist-get cache :cache)
      (remhash cache-key git-util-files-cache))))

(defun git-util-set-file-cache (path content)
  "Cache JavaScript file CONTENT with modification time.

Argument PATH is the file path for which to set the cache.

Argument CONTENT is the content to be cached for the specified file."
  (let* ((cache (gethash path git-util-files-cache))
         (tick (file-attribute-modification-time (file-attributes
                                                  path
                                                  'string))))
    (setq cache (list :tick tick
                      :cache content))
    (puthash path cache
             git-util-files-cache)
    (plist-get cache :cache)))

(defun git-util-parse-ssh-config (file)
  "Parse and return SSH config FILE as an alist of host entries."
  (let ((result)
        (curr))
    (with-temp-buffer
      (insert-file-contents file)
      (while
          (pcase-let
              ((`(,prop ,value)
                (split-string (buffer-substring-no-properties
                               (point)
                               (line-end-position))
                              nil
                              t)))
            (pcase prop
              ("Host"
               (when curr
                 (push curr result))
               (setq curr (cons value (list (cons prop value)))))
              ("#")
              ((pred (stringp))
               (when curr
                 (let ((cell (cdr curr)))
                   (setcdr curr (append cell
                                        (list (cons prop value))))))))
            (zerop (forward-line 1))))
      (when curr
        (push curr result)))
    result))

(defun git-util-get-ssh-config (file)
  "Parse SSH config from FILE and cache it.

Argument FILE is the path to the SSH configuration file to be parsed."
  (when (file-exists-p file)
    (or (git-util-get-file-cache file)
        (git-util-set-file-cache file
                                 (git-util-parse-ssh-config
                                  file)))))

(defun git-util-ssh-host-alist ()
  "Return alist of SSH host and hostnames in `~/.ssh/config'."
  (mapcar (pcase-lambda (`(,k . ,v))
            (cons k (cdr (assoc-string "HostName" v))))
          (git-util-get-ssh-config "~/.ssh/config")))

(defvar forge-alist)
(defun git-util-configure-forge-alist ()
  "Add SSH hosts to `forge-alist' from `~/.ssh/config'."
  (pcase-dolist (`(,host . ,props)
                 (git-util-get-ssh-config "~/.ssh/config"))
    (when-let* ((hostname (cdr (assoc-string "HostName" props))))
      (unless (assoc-string host forge-alist)
        (when-let* ((forge-entry (assoc-string hostname forge-alist))
                    (pos (seq-position forge-alist
                                       forge-entry))
                    (new-entry (cons host (cdr forge-entry))))
          (let ((left (seq-take forge-alist pos))
                (right (seq-drop forge-alist (1+ pos))))
            (setq forge-alist (append left
                                      (list forge-entry)
                                      (list new-entry)
                                      right))))))
    forge-alist))


(defun git-util-find-real-hostname (url)
  "Find and return the real hostname for a git URL in SSH config.

Given a git URL, this function finds the alias of the host as specified
in SSH config.

If the alias cannot be found in the SSH config, return the alias as itself."
  (when-let* ((host (and url
                        (git-util-retrieve-host url))))
    (or (cdr (assoc-string host (git-util-ssh-host-alist)))
        host)))


;;;###autoload
(defun git-util-print-real-hostname ()
  "Find and print the real hostname of the current git repository.
This function uses the \"git remote get-url origin\" command to get the git URL
for the origin remote of the current git repository.

It then parses that URL to get the
host alias, then uses that alias to look up the real hostname in the user's
SSH config file.

If successful, it prints and returns the real hostname. If the lookup
fails, the function prints and returns the alias as the assumed real hostname."
  (interactive)
  (let ((url (car (ignore-errors (process-lines
                                  "git" "remote"
                                  "get-url"
                                  "origin")))))
    (message (git-util-find-real-hostname url))))


(defun git-util-advice-forge-split-url (orig-fn url)
  "Call ORIG-FN (`forge-split-url') with URL and fix the host in the result.

If the real hostname for git URL found in ssh config and
result of calling ORIGN-FN is a cons cell, set the car to the real hostname,
otherwise return result as it.

Supposed to use as advice function for `forge-split-url':

\=(advice-add \\='forge--split-url
              :around \\='git-util-advice-forge-split-url)"
  (require 'forge)
  (let* ((cell (funcall orig-fn url))
         (host
          (when cell
            (git-util-find-real-hostname url))))
    (when host
      (setcar cell host))
    cell))


(defvar git-util-magit--stale-p nil)

(defun git-util-magit-display-buffer-fn (buffer)
  "Display BUFFER based on its major mode and specific conditions.

Argument BUFFER is the buffer to be displayed.

This function is the same as `magit-display-buffer-traditional', except:

- If opened from a commit window, it will open below it.
- Magit process windows are always opened in small windows below the current.
- Everything else will reuse the same window."
  (let ((buffer-mode (buffer-local-value 'major-mode buffer)))
    (display-buffer
     buffer
     (cond ((and (eq buffer-mode 'magit-status-mode)
                 (get-buffer-window buffer))
            '(display-buffer-reuse-window))
           ;; Any magit buffers opened from a commit window should open below
           ;; it. Also open magit process windows below.
           ((or (bound-and-true-p git-commit-mode)
                (eq buffer-mode 'magit-process-mode))
            (let ((size (if (eq buffer-mode 'magit-process-mode)
                            0.35
                          0.7)))
              `(display-buffer-below-selected
                . ((window-height .
                    ,(truncate (* (window-height) size)))))))
           ;; Everything else should reuse the current window.
           ((or (not (derived-mode-p 'magit-mode))
                (not (memq (with-current-buffer buffer major-mode)
                           '(magit-process-mode
                             magit-revision-mode
                             magit-diff-mode
                             magit-stash-mode
                             magit-status-mode))))
            '(display-buffer-same-window))
           ('(git-util-magit-display-buffer-in-direction))))))

(defun git-util-magit-display-buffer-in-direction (buffer alist)
  "`display-buffer-alist' handler that opens BUFFER in a direction.

Argument BUFFER is the buffer to be displayed.

Argument ALIST is an association list containing display options.

This differs from `display-buffer-in-direction' in one way: it will try to use a
window that already exists in that direction. It will split otherwise."
  (let ((direction (or (alist-get 'direction alist)
                       git-util-magit-open-windows-in-direction))
        (origin-window (selected-window)))
    (if-let* (window (window-in-direction direction))
        (unless magit-display-buffer-noselect
          (select-window window))
      (if-let* (window (and (not (one-window-p))
                           (window-in-direction
                            (pcase direction
                              (`right 'left)
                              (`left 'right)
                              ((or `up `above) 'down)
                              ((or `down `below) 'up)))))
          (unless magit-display-buffer-noselect
            (select-window window))
        (let ((window (split-window nil nil direction)))
          (when (and (not magit-display-buffer-noselect)
                     (memq direction '(right down below)))
            (select-window window))
          (display-buffer-record-window 'reuse window buffer)
          (set-window-buffer window buffer)
          (set-window-parameter window 'quit-restore
                                (list 'window 'window origin-window buffer))
          (set-window-prev-buffers window nil))))
    (unless magit-display-buffer-noselect
      (switch-to-buffer buffer t t)
      (selected-window))))

(defvar git-util-magit--pos nil)

(defun git-util-magit-set-window-state-h ()
  "Set the local variable `git-util-magit--pos' with current buffer state."
  (setq-local git-util-magit--pos (list (current-buffer)
                                        (point)
                                        (window-start))))

(defun git-util-magit-restore-window-state-h ()
  "Restore window state using stored buffer position and window start."
  (when (and git-util-magit--pos (eq (current-buffer)
                                     (car git-util-magit--pos)))
    (goto-char (cadr git-util-magit--pos))
    (set-window-start nil (caddr git-util-magit--pos) t)
    (kill-local-variable 'git-util-magit--pos)))

(defun git-util-magit-revert-buffer (buffer)
  "Revert the given BUFFER and refresh its state if it's modified.

Argument BUFFER is the buffer that the function will operate on, specifically by
reverting any changes made to it."
  (with-current-buffer buffer
    (kill-local-variable 'git-util-magit--stale-p)
    (when buffer-file-name
      (if (buffer-modified-p (current-buffer))
          (when (bound-and-true-p vc-mode)
            (vc-refresh-state)
            (force-mode-line-update))
        (revert-buffer t t t)))))

;;;###autoload
(defun git-util-magit-revert-buffer-maybe-h ()
  "Update `vc' and `git-gutter' if out of date."
  (when git-util-magit--stale-p
    (git-util-magit-revert-buffer (current-buffer))))

(defun git-util-sync-fill-column-for-git-commit-mode (&rest _)
  "Set the local `fill-column' to `git-commit-summary-max-length' if it is bound."
  (setq-local fill-column (or (when (boundp 'git-commit-summary-max-length)
                                (symbol-value 'git-commit-summary-max-length))
                              fill-column)))


(provide 'git-util)
;;; git-util.el ends here