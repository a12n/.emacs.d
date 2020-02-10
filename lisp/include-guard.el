;;
;; Include guard generation for C and C++ header files
;;

(defun include-guard-string (n)
  "Generates include guard string from current buffer's file name,
leaving only last N path elements."
  (let* ((path-elements (split-string (buffer-file-name) "/"))
         (path (mapconcat 'identity (last path-elements n) "_"))
         (upcased-path (upcase path))
         (replaced-path (replace-regexp-in-string "[^A-Z0-9_]" "_" upcased-path)))
    (format "%s" replaced-path)))

(defun include-guard-header (guard)
  "Generates include guard file header for a given GUARD."
  (concat "#ifndef " guard "\n"
          "#define " guard "\n"
          "\n"))

(defun include-guard-footer (guard)
  "Generates include guard file footer for a given GUARD."
  (if (eq major-mode 'c++-mode)
      (concat "\n"
              "#endif  // " guard)
    (concat "\n"
            "#endif  /* " guard " */")))

(defun include-guard-insert (n)
  "Inserts include guard header and footer into the current
file. Prefix argument N is about how many path elements to leave in
guard, it's 1 if omitted."
  (interactive "P")
  (when (or (eq major-mode 'c-mode)
            (eq major-mode 'c++-mode))
    (let ((guard (include-guard-string (if n n 1))))
      (save-excursion
        (goto-char (point-min))
        (insert (include-guard-header guard))
        (goto-char (point-max))
        (insert (include-guard-footer guard))))))

(provide 'include-guard)
