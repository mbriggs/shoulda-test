;; Shoulda-Mode
;;
;; modified version, based on the one written by Peter Williams
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; See <http://www.gnu.org/licenses/> for a copy of the GNU General
;; Public License.

(require 'ruby-mode)

(defcustom shoulda-command "ruby \"%f\" %o"
  "The command to run when verifying should specs. \"%f\" will be replaced by the filename being tested. \"%o\" will be replaced by the options to test unit'"
  :type 'string
  :group 'shoulda-mode)

(defun shoulda-beginning-of-example ()
  "Moves point to the beginning of the example in which the point current is."
  (interactive)
  (let ((start (point)))
    (goto-char
     (save-excursion
       (end-of-line)
       (unless (and (search-backward-regexp "^[[:space:]]*should[[:space:]]*(?[\"']" nil t)
                    (save-excursion (ruby-end-of-block) (< start (point))))
         (error "Unable to find an example"))
       (point)))))


(defun shoulda-verify ()
  "Runs the specified spec, or the spec file for the current buffer."
  (interactive)
  (shoulda-run-single-file (shoulda-spec-file-for (shoulda-test-file))))

(defun shoulda-verify-single ()
  "Runs the specified example at the point of the current buffer."
  (interactive)
  (shoulda-run-single-file (shoulda-test-file) "-n" (shoulda-regexp-for-example (shoulda-example-name-at-point))))

(defun shoulda-test-file ()
  (replace-regexp-in-string (concat "^" (eproject-root))
                            "" (buffer-file-name)))

(defun shoulda-regexp-for-example (example-name)
  "Converts example name into a regexp that matched the example name, escaping all regexp special characters"
  (concat "\"/" (replace-regexp-in-string "[]\\[/\\(\\)+?.]" (lambda (m) (concat "\\\\\\\\\\\\\\\\" m)) example-name) "/\""))

(defun shoulda-toggle-spec-and-target ()
  "Switches to the spec for the current buffer if it is a
   non-spec file, or switch to the target of the current buffer
   if the current is a spec"
  (interactive)
  (find-file
   (if (shoulda-buffer-is-spec-p)
       (shoulda-target-file-for (buffer-file-name))
     (shoulda-spec-file-for (buffer-file-name)))))

(defun shoulda-spec-file-for (a-file-name)
  "Find spec for the specified file"
  (if (shoulda-spec-file-p a-file-name)
      a-file-name
    (shoulda-specize-file-name (expand-file-name (replace-regexp-in-string "^\\.\\./[^/]+/" "" (file-relative-name a-file-name (shoulda-spec-directory a-file-name)))
                                               (shoulda-spec-directory a-file-name)))))

(defun shoulda-target-file-for (a-spec-file-name)
  "Find the target for a-spec-file-name"
  (first
   (file-expand-wildcards
    (replace-regexp-in-string "/test/" "/*/" (shoulda-targetize-file-name a-spec-file-name)))))

(defun shoulda-specize-file-name (a-file-name)
  "Returns a-file-name but converted in to a spec file name"
  (concat
   (file-name-directory a-file-name)
   (replace-regexp-in-string "\\(\\.rb\\)?$" "_test.rb" (file-name-nondirectory a-file-name))))

(defun shoulda-targetize-file-name (a-file-name)
  "Returns a-file-name but converted into a non-spec file name"
     (concat (file-name-directory a-file-name)
             (shoulda-file-name-with-default-extension
              (replace-regexp-in-string "_test\\.rb" "" (file-name-nondirectory a-file-name)))))

(defun shoulda-file-name-with-default-extension (a-file-name)
  "Adds .rb file extension to a-file-name if it does not already have an extension"
  (if (file-name-extension a-file-name)
      a-file-name ;; file has a extension already so do nothing
    (concat a-file-name ".rb")))

(defun shoulda-directory-subdirectories (directory)
  "Returns list of subdirectories"
  (remove-if
   (lambda (dir) (or (string-match "^\\.\\.?$" (file-name-nondirectory dir))
                     (not (file-directory-p dir))))
   (directory-files directory t)))

(defun shoulda-parent-directory (a-directory)
  "Returns the directory of which a-directory is a child"
  (file-name-directory (directory-file-name a-directory)))

(defun shoulda-root-directory-p (a-directory)
  "Returns t if a-directory is the root"
  (equal a-directory (shoulda-parent-directory a-directory)))

(defun shoulda-spec-directory (a-file)
  "Returns the nearest spec directory that could contain specs for a-file"
  (if (file-directory-p a-file)
      (or
       (first (directory-files a-file t "^spec$"))
       (if (shoulda-root-directory-p a-file)
           nil
         (shoulda-spec-directory (shoulda-parent-directory a-file))))
    (shoulda-spec-directory (shoulda-parent-directory a-file))))

(defun shoulda-spec-file-p (a-file-name)
  "Returns true if the specified file is a spec"
  (string-match "\\(_\\|-\\)test\\.rb$" a-file-name))

(defun shoulda-buffer-is-spec-p ()
  "Returns true if the current buffer is a spec"
  (and (buffer-file-name)
       (shoulda-spec-file-p (buffer-file-name))))

(defun shoulda-example-name-at-point ()
  "Returns the name of the example in which the point is currently positioned; or nil if it is outside of and example"
  (save-excursion
    (end-of-line)
    (re-search-backward "\\(\\(should\\|context\\)[[:space:](]+['\"]\\|def[[:space:]]+test_\\|should_[^[:space:](]+[[:space:](]['\"]\\)\\(.*\\)$")
    (replace-regexp-in-string "\\(\\(['\"][)[:space:]]*\\(do\\|DO\\|Do\\|{\\)\\)\\|()\\)[[:space:]]*$" "" (match-string 3))))

(defun shoulda-register-verify-redo (redoer)
  "Register a bit of code that will repeat a verification process"
  (let ((redoer-cmd (eval (append '(lambda () (interactive)) (list redoer)))))
    (define-key evil-normal-state-map (kbd ",tr") redoer-cmd)))

(defun shoulda-run-single-file (test-file &rest opts)
  "Runs test with the specified options"
  (rvm-activate-corresponding-ruby)
  (shoulda-register-verify-redo (cons 'shoulda-run-single-file (cons test-file opts)))
  (compile (shoulda-inject-test-file-name (shoulda-inject-options shoulda-command opts) test-file))
  (end-of-buffer-other-window 0))

(defun shoulda-inject-options (cmd-pattern opts-list)
  "Replaces '%o' with options string"
  (replace-regexp-in-string "%o" (mapconcat (lambda (x) x) opts " ") cmd-pattern))

(defun shoulda-inject-test-file-name (cmd-pattern test-file)
  "Replaces '%f' with file name"
  (replace-regexp-in-string "%f" test-file cmd-pattern))

(defadvice compile (around set-rail-test-dir (command &optional comint))
  "set the correct directory and don't pay attention to the gnu pattern"
  (if (string-match "ruby" command)
      (let ((default-directory (eproject-root))
            (compilation-error-regexp-alist (remq 'gnu compilation-error-regexp-alist)))
        ad-do-it)
    ad-do-it))

(ad-activate 'compile)


(add-to-list 'compilation-error-regexp-alist-alist
             '(shoulda "\\[?\\(test/\\(?:unit\\|functional\\)[0-9A-Za-z_./\:-]+\\.rb\\):\\([0-9]+\\)" 1 2))

(add-to-list 'compilation-error-regexp-alist 'shoulda)


(provide 'shoulda-mode)
