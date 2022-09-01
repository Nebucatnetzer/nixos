;; web-mode for general web development
(use-package web-mode
  :ensure t
  :mode
  (("\\.phtml\\'" . web-mode)
   ("\\.tpl\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)
   ("\\.html?\\'" . web-mode))
  :config
  (add-to-list 'auto-mode-alist '("\\.php$" . my/php-setup))
  (add-to-list 'auto-mode-alist '("\\.phpi$" . my/php-setup)))

(use-package company-web
  :ensure t
  :after web-mode
  :commands company-web-html
  :config
  (require 'company-web-html)

  ;; Tide completion support in web-mode with company-mode
  (defun az-web-mode-hook ()
    "Hook for `web-mode'."
    (set (make-local-variable 'company-backends)
         '(company-tide company-web-html company-yasnippet company-files)))

  (add-hook 'web-mode-hook 'az-web-mode-hook)

  ;; Enable JavaScript completion between <script>...</script> etc.
  (defadvice company-tide (before web-mode-set-up-ac-sources activate)
    "Set `tide-mode' based on current language before running company-tide."
    (if (equal major-mode 'web-mode)
        (let ((web-mode-cur-language
               (web-mode-language-at-pos)))
          (if (or (string= web-mode-cur-language "javascript")
                  (string= web-mode-cur-language "jsx"))
              (unless tide-mode (tide-mode))
            (if tide-mode (tide-mode -1)))))))

(use-package company-restclient
  :ensure t
  :after (restclient company)
  :config (add-to-list 'company-backends 'company-restclient))
