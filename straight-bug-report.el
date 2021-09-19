;;; straight-bug-report.el --- Bug Reporter -*- lexical-binding: t; -*-
;;; Commentary:
;; straight.el bug reporter
;;; Code:
(require 'straight)
(require 'yodel)

(defvar straight-bug-report--process-buffer "*straight-bug-report*"
  "Name of the bug report buffer.")

(defun straight-bug-report--package-table-row (package)
  "Return formatted table row for PACKAGE."
  (cl-destructuring-bind
      ( &key version url name source &allow-other-keys
        &aux
        ;;@TODO: link directly to branch
        (name (if (and version url) (format "[%s](%s)" name url) name))
        (vc-info
         (apply #'format
                `("%-10s|%-10s|%s"
                  ,@(if version
                        (cl-destructuring-bind
                            ( &key commit ((:commit-url url)) date branch
                              &allow-other-keys
                              &aux
                              (abbrev (substring commit 0 10))
                              (commit
                               (if url
                                   (if (string-match-p "github.com" url)
                                       url
                                     (format "[%s](%s)" abbrev url))
                                 abbrev)))
                            version
                          (list branch commit date))
                      '("nil" "nil" "nil"))))))
      package
    (format "|%s|%s|%s|\n" name vc-info source)))

;;@TODO: rewrite using `yodel-formatter'
(defun straight-bug-report-formatter (report)
  "Format REPORT."
  (yodel-format-as-github-markdown report)
  (let* ((report (plist-get report :report))
         (straight (plist-get report :straight)))
    (goto-char (point-min))
    (delete-region (line-beginning-position) (line-end-position))
    (insert
     (format-time-string "## Straight Bug Report (%Y-%m-%d %H:%M:%S)"
                         (plist-get report :yodel-time)))
    (re-search-forward "\\(?:\\(```emacs-lisp\n\\)[^z-a]*?\\(^```$\\)\\)")
    (replace-match "\\1FORM\\2")
    (re-search-backward "FORM")
    (replace-match "")
    (insert (yodel--pretty-print (read (plist-get straight :form))))
    (goto-char (point-max))
    (forward-line -1)
    (insert "<details open>\n<summary>Packages</summary>\n\n"
            "| Name    | Branch  | Commit  | Date    | Source |\n"
            "|---------|---------|---------|---------|--------|\n")
    (mapc (lambda (p)
            (insert (straight-bug-report--package-table-row p)))
          (sort (plist-get straight :packages)
                (lambda (a b) (string< (plist-get a :name)
                                       (plist-get b :name)))))
    (insert "</details>\n")
    (re-search-backward "^|")
    (when (fboundp 'markdown-cycle) (markdown-cycle))
    (goto-char (point-min))))

;;;###autoload
(defmacro straight-bug-report (&rest args)
  "Test straight.el in a clean environment.
ARGS may be any of the following keywords and their respective values:
  - :pre-bootstrap* (Form)...
      Forms evaluated before bootstrapping straight.el
      e.g. (setq straight-repository-branch \"develop\")
      Note this example is already in the default bootstrapping code.

  - :post-bootstrap* (Form)...
      Forms evaluated in the testing environment after boostrapping.
      e.g. (straight-use-package '(example :type git :host github))

  - :interactive Boolean
      If nil, the subprocess will immediately exit after the test.
      Output will be printed to `straight-bug-report--process-buffer'
      Otherwise, the subprocess will be interactive.

  - :save Boolean
      If non-nil, the test directory is left in the directory stored in the
      variable `temporary-file-directory'. Otherwise, it is
      immediately removed after the test is run.

  - :executable String
      Indicate the Emacs executable to launch.
      Defaults to the path of the current Emacs executable.

  - :raw Boolean
      If non-nil, the raw process output is sent to
      `straight-bug-report--process-buffer'. Otherwise, it is
      formatted as markdown for submitting as an issue.

  - :user-dir String
      If non-nil, the test is run with `user-emacs-directory' set to STRING.
      Otherwise, a temporary directory is created and used.

ARGS are accessible within the :pre/:post-bootsrap phases via the
locally bound plist, straight-bug-report-args."
  (declare (indent 0))
  (let ((form args))
    (setq args (yodel-plist*-to-plist args))
    `(let ((yodel-process-buffer straight-bug-report--process-buffer))
       (yodel
         :formatter straight-bug-report-formatter
         :user-dir ,(let ((user-dir (plist-get args :user-dir)))
                      (expand-file-name (or user-dir (make-temp-name "straight-bug-report."))
                                        ;;@MAYBE: Always expand against temporary-file-directory
                                        ;; We only need to use `default-directory' during `yodel-file'.
                                        (if user-dir default-directory temporary-file-directory)))
         :pre*
         (yodel-file "./bugstrap.el"
           :save t
           :overwrite t
           :with*
           ";; -*- lexical-binding: t; -*-"
           (defvar bootstrap-version)
           (let ((bootstrap-file
                  (expand-file-name "straight/repos/straight.el/bootstrap.el"
                                    user-emacs-directory))
                 (bootstrap-version 5))
             (unless (file-exists-p bootstrap-file)
               (with-current-buffer
                   (url-retrieve-synchronously
                    (concat "https://raw.githubusercontent.com/"
                            "raxod502/straight.el/develop/install.el")
                    'silent 'inhibit-cookies)
                 (goto-char (point-max))
                 (eval-print-last-sexp)))
             (load bootstrap-file nil 'nomessage)))
         (setq yodel-args (plist-put
                           yodel-args
                           :straight
                           (list :parent-version (straight-version)
                                 :form ,(prin1-to-string
                                         (append '(straight-bug-report) form)))))
         :post*
         (progn ,@(if-let ((pre (plist-get args :pre-bootstrap*)))
                      pre
                    '((setq straight-repository-branch "develop"))))
         (load-file "./bugstrap.el")
         (unwind-protect
             (progn ,@(plist-get args :post-bootstrap*))
           (setf (plist-get yodel-args :straight)
                 (plist-put (plist-get yodel-args :straight)
                            :child-version (straight-version))
                 (plist-get yodel-args :straight)
                 (plist-put
                  (plist-get yodel-args :straight)
                  :packages
                  ;;@TODO: decompose this into a function
                  (let ((packages '()))
                    (maphash
                     (lambda (key val)
                       (cl-destructuring-bind
                           ( &key repo local-repo host &allow-other-keys
                             &aux
                             (source (straight-recipe-source key))
                             (url (when (and repo host)
                                    (format "https://%s.com/%s"
                                            (alist-get host '((github . "github")
                                                              (gitlab . "gitlab")))
                                            repo)))
                             (version
                              (when local-repo
                                (let  ((default-directory (straight--repos-dir local-repo)))
                                  (when (file-exists-p default-directory)
                                    (let* ((info (split-string
                                                  (straight--process-output
                                                   "git" "show" "-s" "--format=%H %cs")
                                                  " "))
                                           (commit (car info)))
                                      (list :branch (straight-vc-git--local-branch "HEAD")
                                            :commit commit
                                            :commit-url
                                            (when url
                                              (pcase host
                                                ('github (concat url "/commit/" commit))
                                                ('gitlab (concat url "/-/commit/" commit))
                                                (_ commit)))
                                            :date   (cadr info))))))))
                           (nth 2 val)
                         (push (list :name key :source source :repo repo
                                     :local-repo local-repo :host host
                                     :url url :version version)
                               packages)))
                     straight--build-cache)
                    (nreverse packages)))))
         ,@(when-let ((interactive (plist-get args :interactive)))
             `(:interactive ,interactive))
         ,@(when-let ((save (plist-get args :save)))
             `(:save ,save))
         ,@(when-let ((raw (plist-get args :raw)))
             `(:raw ,raw))
         ,@(when-let ((user-dir (plist-get args :user-dir)))
             `(:user-dir ,user-dir))))))

(provide 'straight-bug-report)

;;; straight-bug-report.el ends here
