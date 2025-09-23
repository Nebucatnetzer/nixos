;;; pylint-flymake.el --- A pylint Flymake backend  -*- lexical-binding: t; -*-
;; https://github.com/10000yendama/emacs.d/blob/9aff55912df122ddc80ba4f69988e149d4ece7c3/elisp/pylint-flymake.el
(defvar-local pylint--flymake-proc nil)

(defcustom flymake-pylint-executable nil
  "The path to the pylint executable, used in flymake."
  :group 'flymake
  :type 'string)

(defun pylint-flymake (report-fn &rest _args)

  (unless (projectile-current-project-buffer-p)
    (error "Only buffers under Projectile-project is supported for pylint"))

  (unless flymake-pylint-executable
    (error "Path to the executable is not set; \
use M-x projectile-edit-dir-locals and define `flymake-pylint-executable`."))

  ;; Not having pylint is a serious problem which should cause
  ;; the backend to disable itself, so an error is signaled.
  ;;
  (unless (executable-find flymake-pylint-executable)
    (error "Cannot find a suitable pylint"))

  ;; If a live process launched in an earlier check was found, that
  ;; process is killed.  When that process's sentinel eventually runs,
  ;; it will notice its obsoletion, since it have since reset
  ;; `pylint-flymake-proc' to a different value
  ;;
  (when (process-live-p pylint--flymake-proc)
    (kill-process pylint--flymake-proc))

  ;; Save the current buffer, the narrowing restriction, remove any
  ;; narrowing restriction.
  ;;
  (let ((source (current-buffer)))
    (save-restriction
      (widen)
      ;; Reset the `pylint--flymake-proc' process to a new process
      ;; calling the pylint tool.
      ;;
      (setq
       pylint--flymake-proc
       (let ((default-directory (projectile-project-root)))
         (make-process
          :name "pylint-flymake" :noquery t :connection-type 'pipe
          ;; Make output go to a temporary buffer.
          ;;
          :buffer (generate-new-buffer " *pylint-flymake*")
          :command `(,flymake-pylint-executable
                     "--msg-template"
                     "\"{path}:{line}:{C}: {msg} ({msg_id}, {symbol})\""
                     "--reports"
                     "n"
                     ,(buffer-file-name source))
          :sentinel
          (lambda (proc _event)
            ;; Check that the process has indeed exited, as it might
            ;; be simply suspended.
            ;;
            (when (memq (process-status proc) '(exit signal))
              (unwind-protect
                  ;; Only proceed if `proc' is the same as
                  ;; `pylint--flymake-proc', which indicates that
                  ;; `proc' is not an obsolete process.
                  ;;
                  (if (with-current-buffer source (eq proc pylint--flymake-proc))
                      (with-current-buffer (process-buffer proc)
                        (goto-char (point-min))
                        ;; Parse the output buffer for diagnostic's
                        ;; messages and locations, collect them in a list
                        ;; of objects, and call `report-fn'.
                        ;;
                        (cl-loop
                         while (search-forward-regexp
                                ;;             :1           :2  : 3
                                ;; {filename  }:{line      }:{C}: ...
                                "^\\(?:.*.py\\):\\([0-9]+\\):\\([IRCWEF]\\): \\(.*\\)$"
                                nil t)
                         for msg = (match-string 3)
                         for (beg . end) = (flymake-diag-region
                                            source
                                            (string-to-number (match-string 1)))
                         for type = (pcase (match-string 2)
                                      ("I" :note)    ; Information
                                      ("R" :warning)    ; Recommendation
                                      ("C" :warning) ; Convention
                                      ("W" :warning) ; Warning
                                      ("E" :error)   ; Error
                                      ("F" :error)   ; Fatal
                                      (_ :error))
                         collect (flymake-make-diagnostic source
                                                          beg
                                                          end
                                                          type
                                                          msg)
                         into diags
                         finally (funcall report-fn diags)))
                    (flymake-log :warning "Canceling obsolete check %s"
                                 proc))
                ;; Cleanup the temporary buffer used to hold the
                ;; check's output.
                ;;
                (kill-buffer (process-buffer proc))))))))
      ;; Send the buffer contents to the process's stdin, followed by
      ;; an EOF.
      ;;
      (process-send-region pylint--flymake-proc (point-min) (point-max))
      (process-send-eof pylint--flymake-proc))))

(defun pylint-setup-flymake-backend ()
  (add-hook 'flymake-diagnostic-functions 'pylint-flymake nil t))

(provide 'pylint-flymake)
