;; -*- lexical-binding: t; -*-
;; Run with:
;;   emacs --quick --batch --load tests/az_elfeed-tests.el --funcall ert-run-tests-batch-and-exit
;; enable-feeds is unset, so loading the module does not need elfeed.
(require 'ert)

(load (expand-file-name "../az_elfeed.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(ert-deftest az-elfeed-image-urls-absolute ()
  (should (equal (az-elfeed--image-urls
                  "<p><img src=\"https://example.org/comic.png\"></p>"
                  "https://example.org/")
                 '("https://example.org/comic.png"))))

(ert-deftest az-elfeed-image-urls-relative ()
  (should (equal (az-elfeed--image-urls
                  "<img src=\"/strips/1.png\"><img src=\"2.png\">"
                  "https://example.org/blog/")
                 '("https://example.org/strips/1.png"
                   "https://example.org/blog/2.png"))))

(ert-deftest az-elfeed-image-urls-skips-tracking-pixels ()
  (should (equal (az-elfeed--image-urls
                  "<img src=\"https://t.example/p.gif\" width=\"1\" height=\"1\"><img src=\"https://example.org/a.png\" height=\"0\">"
                  "https://example.org/")
                 nil)))

(ert-deftest az-elfeed-image-urls-skips-data-uris ()
  (should (equal (az-elfeed--image-urls
                  "<img src=\"data:image/png;base64,iVBORw0KGgo=\">"
                  "https://example.org/")
                 nil)))

(ert-deftest az-elfeed-image-urls-removes-duplicates ()
  (should (equal (az-elfeed--image-urls
                  "<img src=\"https://example.org/a.png\"><img src=\"https://example.org/a.png\">"
                  "https://example.org/")
                 '("https://example.org/a.png"))))

(ert-deftest az-elfeed-image-urls-no-images ()
  (should (equal (az-elfeed--image-urls "<p>Only text.</p>" "https://example.org/")
                 nil)))

(ert-deftest az-elfeed-image-urls-srcset-without-src ()
  (should (equal (az-elfeed--image-urls
                  "<img srcset=\"https://example.org/big.png 2000w\">"
                  "https://example.org/")
                 '("https://example.org/big.png"))))
