;; shoulda-test
;;
;; by Matt Briggs
;; based on shoulda-mode by Peter Williams
;;
;;
;; Greatly reducing the scope of the plugin to do two things
;;
;; should-verify: run the test file you are currently on
;; should-verify-single: run the test that contains the point
;;
;; currently supports def test_ and should "" do syntaxs, but doesn't
;; support test "" do yet
;;
;; eventually I want to integrate this with github.com/mbriggs/rails-test-toggler
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

(defun shoulda-verify ()
  "Runs the specified spec, or the spec file for the current buffer."
  (interactive)
  (shoulda-run-single-file (shoulda-test-file)))

(defun shoulda-verify-single ()
  "Runs the specified example at the point of the current buffer."
  (interactive)
  (shoulda-run-single-file (shoulda-test-file) "-n" (shoulda-regexp-for-example (shoulda-test-name-at-point))))

(defun shoulda-test-file ()
  (replace-regexp-in-string (concat "^" (eproject-root))
                            "" (buffer-file-name)))

(defun shoulda-regexp-for-example (example-name)
  "Converts example name into a regexp that matched the example name, escaping all regexp special characters"
  (concat "\"/" (replace-regexp-in-string "[]\\[/\\(\\)+?.]" (lambda (m) (concat "\\\\\\\\\\\\\\\\" m)) example-name) "/\""))

(defun shoulda-test-name-at-point ()
  "Returns the name of the example in which the point is currently positioned; or nil if it is outside of and example"
  (save-excursion
    (end-of-line)
    (re-search-backward "\\(\\(should\\|context\\)[[:space:](]+['\"]\\|def[[:space:]]+test_\\|should_[^[:space:](]+[[:space:](]['\"]\\)\\(.*\\)$")
    (replace-regexp-in-string "\\(\\(['\"][)[:space:]]*\\(do\\|DO\\|Do\\|{\\)\\)\\|()\\)[[:space:]]*$" "" (match-string 3))))

(defun shoulda-register-verify-redo (redoer)
  "Register a bit of code that will repeat a verification process"
  (let ((redoer-cmd (eval (append '(lambda () (interactive)) (list redoer)))))
    (define-key evil-normal-state-map (kbd ",tl") redoer-cmd)))

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
             '(shoulda "^[^DEPRECATION].*\\[?\\(test/\\(?:unit\\|functional\\)[0-9A-Za-z_./\:-]+\\.rb\\):\\([0-9]+\\)" 1 2))

(add-to-list 'compilation-error-regexp-alist 'shoulda)


(provide 'shoulda-test)
