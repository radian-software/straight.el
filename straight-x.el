;;; straight-x.el --- Experimental extensions. -*- lexical-binding: t -*-

;; Copyright (C) 2017-2018 Radon Rosborough and contributors

;; Author: Radon Rosborough <radon.neon@gmail.com>
;; Created: 1 Jan 2017
;; Homepage: https://github.com/raxod502/straight.el
;; Keywords: extensions
;; Package-Requires: ((emacs "24.4"))
;; Version: prerelease

;;; Commentary:

;; This file contains experimental extensions to straight.el which are
;; not yet ready for direct inclusion. No guarantees are made about
;; the behavior of code in this file.

;; See straight.el for more information.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'straight)

(defun straight-x-existing-repos ()
  (let (recipes)
    (straight--map-repos
     (lambda (recipe)
       (when (and (plist-get recipe :local-repo)
                  (straight--repository-is-available-p recipe))
         (push recipe recipes))))
    (nreverse recipes)))

(defvar straight-x-all nil)
(defvar straight-x-waiting nil)
(defvar straight-x-running nil)
(defvar straight-x-finished nil)

(defvar straight-x-buffer "*straight*")

(defun straight-x-buffer-header-line ()
  (with-current-buffer straight-x-buffer
    (let ((finished (length straight-x-finished))
          (all (length straight-x-all)))
      (setq header-line-format
            (format "Fetching %d/%d...%s"
                    finished all (if (= finished all) "Done" ""))))))

(defun straight-x-buffer-line (linum new-string)
  (with-current-buffer straight-x-buffer
    (goto-char (point-min))
    (forward-line (1- linum))
    (delete-region (line-beginning-position) (line-end-position))
    (insert new-string)))

(defun straight-x-when-done (process _change)
  ;; Assuming success
  (let* ((recipe (process-get process :recipe))
         (package (plist-get recipe :package))
         (linum (1+ (cl-position recipe straight-x-all :test #'equal))))
    (setq straight-x-running (delete recipe straight-x-running)
          straight-x-finished (cons recipe straight-x-finished))
    (straight-x-buffer-line
     linum
     (format "+ %s: %s"
             (propertize package 'face 'font-lock-keyword-face)
             (if (process-get process :up-to-date)
                 "Already up to date"
               "Updated")))
    (straight-x-buffer-header-line))
  (kill-buffer (process-buffer process))
  (straight-x-start-process))

(defun straight-x-strip (string)
  ;; git clone/fetch --progress
  (car (last (split-string string (rx (or ?\r ?\n)) t " +"))))

(defun straight-x-filter (process output)
  (let* ((recipe (process-get process :recipe))
         (linum (1+ (cl-position recipe straight-x-all :test #'equal))))
    (process-put process :up-to-date nil)
    (straight-x-buffer-line
     linum
     (format "- %s: %s"
             (propertize
              (plist-get recipe :package)
              'face 'font-lock-variable-name-face)
             (straight-x-strip output)))))

(defun straight-x-start-process ()
  (when-let ((recipe (pop straight-x-waiting)))
    (push recipe straight-x-running)
    (straight--with-plist recipe
        (local-repo package)
      (let ((proc (let* ((default-directory (straight--repos-dir local-repo))
                         (process-connection-type nil)
                         (name (format " *straight %s*" package))
                         (buf (generate-new-buffer name)))
                    (start-process name buf "git" "fetch" "--all"))))
        (process-put proc :recipe recipe)
        (process-put proc :up-to-date t)
        (set-process-filter proc #'straight-x-filter)
        (set-process-sentinel proc #'straight-x-when-done)))))

(defvar straight-x-process-limit 10)

(defun straight-x-fetch-all ()
  (interactive)
  (setq straight-x-all (straight-x-existing-repos)
        straight-x-waiting straight-x-all
        straight-x-running nil
        straight-x-finished nil)
  (with-current-buffer (get-buffer-create straight-x-buffer)
    (display-buffer (current-buffer))
    (erase-buffer)
    (straight-x-buffer-header-line)
    (insert (make-string (length straight-x-all) ?\n))
    (cl-loop for recipe in straight-x-all
             for linum from 1
             do (straight-x-buffer-line
                 linum
                 (format "- %s: Waiting"
                         (propertize
                          (plist-get recipe :package)
                          'face 'font-lock-variable-name-face)))))
  (dotimes (_ straight-x-process-limit)
    (straight-x-start-process)))

(defun straight-x-clean-unused-repos ()
  (interactive)
  (dolist (repo (straight--directory-files (straight--repos-dir)))
    (unless (or (straight--checkhash repo straight--repo-cache)
                (not (y-or-n-p (format "Delete repository %S?" repo))))
      (delete-directory (straight--repos-dir repo) 'recursive 'trash))))

(provide 'straight-x)

;;; straight-x.el ends here
