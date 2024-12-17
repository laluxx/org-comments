;;; org-comments.el --- org-mode subset for comments in prog-mode -*- lexical-binding: t -*-

;; Copyright (C) 2024 Laluxx

;; Author: Laluxx
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: convenience org-mode
;; URL: https://github.com/laluxx/org-comments

;;; Commentary:

;; [ ] TODO Color DONE in Gray if part of a DONE header
;; we should require `hl-todo' and modify it to check
;; if "[x] " is on the left color DONE in Gray instead of success

;; [ ] TODO Implement `org-comments-insert-todo' insert it correctly based on contex
;; [ ] TODO Option to go to the next line when we call dwim on this and not that
;; [ ] TODO Syntax highlighting for checkboxes require `org-faces'
;; [x] DONE Hierarchical TODO's

;; TODO [0/4] Keybinds 
;; [ ] TODO `org-comments-meta-up'    M-<up>
;; [ ] TODO `org-comments-meta-down'  M-<down>
;; [ ] TODO `org-comments-meta-left'  M-<left> 
;; [ ] TODO `org-comments-meta-right' M-<right> 

;; This package brings `org-mode' to `prog-mode' trought comments 
;; It currently supports only C (maybe) and elisp

;;; Code:
(defgroup org-comments nil
  "Functionality for handling TODO items in comments."
  :group 'convenience
  :prefix "org-comments-")

(defun org-comments--is-header ()
  "Check if the current line is a header."
  (save-excursion
    (beginning-of-line)
    (and (looking-at "^\\s-*;;\\s-*\\(TODO\\|DONE\\)\\s-*\\[.*?\\]")
         (save-excursion
           (forward-line)
           (looking-at "^\\s-*;;\\s-*\\[")))))

(defun org-comments--find-todo-header ()
  "Find the TODO header for the current section."
  (save-excursion
    (beginning-of-line)
    (while (and (not (bobp))
                (not (org-comments--is-header)))
      (forward-line -1))
    (when (org-comments--is-header)
      (point))))

(defun org-comments--update-todo-header ()
  "Update the TODO header with the correct count of completed items and state."
  (let ((header-point (org-comments--find-todo-header)))
    (when header-point
      (save-excursion
        (goto-char header-point)
        (let ((header-start (point))
              (total 0)
              (done 0))
          (forward-line)
          (while (and (not (eobp))
                      (looking-at "^\\s-*;;\\s-*\\[\\(.\\)\\]")
                      (not (org-comments--is-header)))
            (setq total (1+ total))
            (when (string= (match-string 1) "x")
              (setq done (1+ done)))
            (forward-line))
          (goto-char header-start)
          (when (re-search-forward "\\(TODO\\|DONE\\)\\s-*\\[.*?\\]" (line-end-position) t)
            (replace-match (format "%s [%d/%d]"
                                   (if (= done total) "DONE" "TODO")
                                   done total))))))))

(defun org-comments--toggle-checkbox ()
  "Toggle the checkbox state of the current line."
  (when (re-search-forward "\\[\\(.\\)\\]\\s-*\\(TODO\\|DONE\\)?" (line-end-position) t)
    (let ((checkbox-state (match-string 1))
          (todo-state (match-string 2)))
      (replace-match (if (string= checkbox-state " ") "x" " ") t t nil 1)
      (when todo-state
        (replace-match (if (string= checkbox-state " ") "DONE" "TODO") t t nil 2)))))

;;;###autoload
(defun org-comments-dwim ()
  "Smart function to toggle checkboxes and update TODO states in comments."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (cond
     ((org-comments--is-header)
      (org-comments--update-todo-header))
     ((looking-at "^\\s-*;;\\s-*\\[")
      (org-comments--toggle-checkbox)
      (org-comments--update-todo-header)))))

(provide 'org-comments)
;;; org-comments.el ends here
