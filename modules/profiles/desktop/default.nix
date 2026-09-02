{
  config,
  inputs,
  pkgs,
  unstable-pkgs,
  ...
}:
let
  azPkgs = import "${inputs.self}/pkgs" { inherit pkgs unstable-pkgs; };
in
{
  imports = [
    "${inputs.self}/modules/profiles/management"
    "${inputs.self}/modules/programs/calibre"
    "${inputs.self}/modules/programs/libimobiledevice"
    "${inputs.self}/modules/programs/rapid-photo-downloader"
    "${inputs.self}/modules/programs/signal"
    "${inputs.self}/modules/services/nginx-acme-base"
    "${inputs.self}/modules/services/pipewire"
  ];
  networking.networkmanager = {
    enable = true;
    plugins = [
      pkgs.networkmanager-fortisslvpn
      pkgs.networkmanager-iodine
      pkgs.networkmanager-l2tp
      pkgs.networkmanager-openconnect
      pkgs.networkmanager-openvpn
      pkgs.networkmanager-sstp
      pkgs.networkmanager-strongswan
      pkgs.networkmanager-vpnc
    ];
  };

  users.users."${config.az-username}".extraGroups = [
    config.services.samba.usershares.group
  ];
  services = {
    flatpak.enable = true;
    fwupd.enable = true;
    smartd.notifications.systembus-notify.enable = true;
    udisks2.enable = true;
    # Grant Vial hidraw access to the Cornix keyboard (matches any Vial device)
    udev.extraRules = ''
      KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{serial}=="*vial:f64c2b3c*", MODE="0660", GROUP="users", TAG+="uaccess"
    '';
    # Enable the X11 windowing system.
    libinput = {
      enable = true;
      touchpad = {
        accelSpeed = "0.3";
        disableWhileTyping = true;
        scrollMethod = "twofinger";
      };
    };
    samba = {
      enable = true;
      openFirewall = true;
      usershares.enable = true;
    };
    xserver = {
      enable = true;
      serverFlagsSection = ''
        Option "BlankTime" "0"
        Option "StandbyTime" "0"
        Option "SuspendTime" "0"
        Option "OffTime" "0"
      '';
      xkb = {
        layout = "us";
        options = "compose:caps";
      };
    };
  };

  fonts = {
    packages = [
      pkgs._0xpropo
      pkgs._0xproto
      pkgs.dejavu_fonts
      pkgs.gyre-fonts
      pkgs.fira
      pkgs.source-code-pro
      pkgs.source-sans-pro
    ];
  };

  programs = {
    firefox = {
      enable = true;
      languagePacks = [
        "en-GB"
        "de"
      ];
      nativeMessagingHosts.packages = [
        pkgs.kdePackages.plasma-browser-integration
        pkgs.ff2mpv
      ];
      preferences = {
        "browser.aboutConfig.showWarning" = false; # Warning when opening about:config
        "browser.contentblocking.category" = "strict"; # Strong content blocking
        "browser.disableResetPrompt" = true; # "Looks like you haven't started Firefox in a while."
        "browser.ml.chat.enabled" = false; # Disable ChatGPT
        "browser.newtabpage.activity-stream.feeds.topsites" = true; # I like to see my top sites
        "browser.newtabpage.activity-stream.section.highlights.includeVisited" = false;
        "browser.newtabpage.activity-stream.showSponsoredTopSites" = false; # Hide advertising
        "browser.newtabpage.activity-stream.topSitesRows" = 4;
        "browser.onboarding.enabled" = false; # "New to Firefox? Let's get started!" tour
        "browser.shell.checkDefaultBrowser" = false; # Don't check if Firefox is the default, we know it is.
        "browser.startup.couldRestoreSession.count" = 2; # Restore tabs on start
        "browser.tabs.warnOnClose" = true;
        "browser.urlbar.showSearchSuggestionsFirst" = false; # Show history results before internet search
        "extensions.pocket.enabled" = false;
        "media.webrtc.camera.allow-pipewire" = true;
        "services.sync.engine.passwords" = false; # Don't ask for passwords
        "widget.use-xdg-desktop-portal.file-picker" = 1;
      };
    };
  };
  environment = {
    systemPackages = [
      # what I consider to be system packages
      pkgs.adwaita-icon-theme
      pkgs.appimage-run
      pkgs.brightnessctl
      pkgs.git-annex
      pkgs.lollypop
      pkgs.networkmanager-openvpn
      pkgs.pavucontrol
      pkgs.pdfgrep
      pkgs.steam-run # FHS runtime for running dynamically-linked binaries
      pkgs.v4l-utils # required for video capture, e.g. Raspberry Pi
      pkgs.vial # configure the Cornix keyboard layout
      pkgs.wally-cli # tool to flash a ZSA keyboard
      unstable-pkgs.zapp # tool to flash Oryx links onto ZSA keyboards

      # moved from the desktop home-manager profile
      azPkgs.az-media
      azPkgs.dap-sync
      azPkgs.download-articles
      azPkgs.download-playlist
      azPkgs.download-video
      azPkgs.jdownloader
      azPkgs.raiffeisen-csv-cleanup
      azPkgs.sidecar-cleanup
      azPkgs.toggle-keyboard
      azPkgs.tube-podder
      azPkgs.video-to-mpv
      azPkgs.watch-playlist
      azPkgs.watch-random-video
      azPkgs.watch-video

      # photography packages
      pkgs.art
      unstable-pkgs.darktable
      pkgs.digikam
      pkgs.hugin
      pkgs.tesseract # OCR in Digikam

      pkgs.czkawka
      pkgs.dbeaver-bin
      pkgs.keepassxc
      pkgs.libreoffice-qt-fresh
      pkgs.meld
      pkgs.plex-desktop
      pkgs.remmina
      pkgs.syncthingtray
      pkgs.tagger
      unstable-pkgs.telegram-desktop
      unstable-pkgs.zotero
    ];
    sessionVariables = {
      DEFAULT_BROWSER = "${pkgs.firefox}/bin/firefox";
    };
  };

  xdg = {
    portal = {
      enable = true;
      xdgOpenUsePortal = true;
      extraPortals = [
        pkgs.xdg-desktop-portal-gtk # required for plex
        pkgs.kdePackages.xdg-desktop-portal-kde
      ];
    };
  };
}
