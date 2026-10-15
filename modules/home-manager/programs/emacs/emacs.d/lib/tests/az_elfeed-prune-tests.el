;; -*- lexical-binding: t; -*-
;; Run with:
;;   ELFEED=$(ls -d /nix/store/*-emacs-elfeed-2026*/share/emacs/site-lisp/elpa/elfeed-* | head -1)
;;   emacs --quick --batch --directory "$ELFEED" \
;;     --load tests/az_elfeed-prune-tests.el --funcall ert-run-tests-batch-and-exit
;; These tests need elfeed, so they are separate from az_elfeed-tests.el.
(require 'ert)
(require 'cl-lib)
(require 'elfeed)

(load (expand-file-name "../az_elfeed.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defmacro az-elfeed-prune-test-with-db (&rest body)
  "Run BODY with an empty elfeed database and an empty image cache."
  (declare (indent 0))
  `(let* ((test-directory (make-temp-file "az-elfeed-prune-" t))
          (elfeed-db-directory (expand-file-name "db" test-directory))
          (az-elfeed-image-cache-directory (expand-file-name "images/" test-directory))
          (elfeed-db nil)
          (elfeed-db-feeds nil)
          (elfeed-db-entries nil)
          (elfeed-db-index nil))
     (unwind-protect
         (progn ,@body)
       (delete-directory test-directory t))))

(defun az-elfeed-prune-test-add-entry (name tags image-urls)
  "Add an entry NAME with TAGS whose content shows IMAGE-URLS."
  (let ((html nil))
    (dolist (url image-urls)
      (push (format "<img src=\"%s\">" url) html))
    (elfeed-db-add
     (list (elfeed-entry--create
            :id (cons "example.org" name)
            :title name
            :feed-id "https://example.org/feed"
            :link (concat "https://example.org/" name)
            :tags tags
            :date 1700000000
            :content (apply #'concat html)
            :content-type 'html)))))

(defun az-elfeed-prune-test-cache-image (url)
  "Write a fake cached response for URL into the image cache."
  (let* ((url-cache-directory az-elfeed-image-cache-directory)
         (file (url-cache-create-filename url)))
    (url-cache-prepare file)
    (write-region "HTTP/1.1 200 OK\n\nimage" nil file nil 'silent)))

(defun az-elfeed-prune-test-entry-exists-p (name)
  "Return non-nil if the entry NAME is in the database."
  (elfeed-db-get-entry (cons "example.org" name)))

(ert-deftest az-elfeed-prune-removes-read-entries-and-their-images ()
  (az-elfeed-prune-test-with-db
    (az-elfeed-prune-test-add-entry "unread" '(unread) '("https://example.org/a.png"
                                                        "https://example.org/shared.png"))
    (az-elfeed-prune-test-add-entry "read" '() '("https://example.org/b.png"
                                                "https://example.org/shared.png"))
    (az-elfeed-prune-test-add-entry "starred" '(star) '("https://example.org/c.png"))
    (az-elfeed-prune-test-add-entry "read-without-images" '() '())
    (dolist (url '("https://example.org/a.png"
                   "https://example.org/b.png"
                   "https://example.org/c.png"
                   "https://example.org/shared.png"))
      (az-elfeed-prune-test-cache-image url))

    (az-elfeed--prune-read-entries)

    (should (az-elfeed-prune-test-entry-exists-p "unread"))
    (should (az-elfeed-prune-test-entry-exists-p "starred"))
    (should-not (az-elfeed-prune-test-entry-exists-p "read"))
    (should-not (az-elfeed-prune-test-entry-exists-p "read-without-images"))
    (should (az-elfeed--image-cached-p "https://example.org/a.png"))
    (should (az-elfeed--image-cached-p "https://example.org/c.png"))
    (should (az-elfeed--image-cached-p "https://example.org/shared.png"))
    (should-not (az-elfeed--image-cached-p "https://example.org/b.png"))))

(ert-deftest az-elfeed-prune-second-run-changes-nothing ()
  (az-elfeed-prune-test-with-db
    (az-elfeed-prune-test-add-entry "unread" '(unread) '("https://example.org/a.png"))
    (az-elfeed-prune-test-add-entry "read" '() '("https://example.org/b.png"))
    (az-elfeed-prune-test-cache-image "https://example.org/a.png")
    (az-elfeed--prune-read-entries)
    (az-elfeed--prune-read-entries)
    (should (= (hash-table-count elfeed-db-entries) 1))
    (should (az-elfeed--image-cached-p "https://example.org/a.png"))))

(ert-deftest az-elfeed-prefetch-skips-entries-that-get-pruned ()
  (az-elfeed-prune-test-with-db
    (az-elfeed-prune-test-add-entry "read" '() '("https://example.org/b.png"))
    (az-elfeed-prune-test-add-entry "unread" '(unread) '("https://example.org/a.png"))
    (let ((queued-urls nil))
      (cl-letf (((symbol-function 'url-queue-retrieve)
                 (lambda (url &rest _arguments) (push url queued-urls))))
        (az-elfeed--prefetch-images (az-elfeed-prune-test-entry-exists-p "read"))
        (az-elfeed--prefetch-images (az-elfeed-prune-test-entry-exists-p "unread")))
      (should (equal queued-urls '("https://example.org/a.png"))))))
