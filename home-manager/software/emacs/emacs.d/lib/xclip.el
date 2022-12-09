(use-package xclip
  :init
  (when (equal system-name "co-ws-con4")
    (setq xclip-program "powershell.exe"))
  :config
  (xclip-mode 1))
