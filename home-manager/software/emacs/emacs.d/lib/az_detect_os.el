(defun is-mac-p ()
  (eq system-type 'darwin))

(defun is-linux-p ()
  (eq system-type 'gnu/linux))

(defun is-windows-p ()
  (or (eq system-type 'ms-dos)
      (eq system-type 'windows-nt)
      (eq system-type 'cygwin)))

(defun is-bsd-p ()
  (eq system-type 'gnu/kfreebsd))
