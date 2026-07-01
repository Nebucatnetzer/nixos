{
  lib,
  pkgs,
  config,
  ...
}:
{
  nixpkgs.overlays = lib.singleton (
    _: prev: {
      kdePackages = prev.kdePackages // {
        plasma-workspace =
          let

            # the package we want to override
            basePkg = prev.kdePackages.plasma-workspace;

            # a helper package that merges all the XDG_DATA_DIRS into a single directory
            xdgdataPkg = pkgs.stdenv.mkDerivation {
              name = "${basePkg.name}-xdgdata";
              buildInputs = [ basePkg ];
              dontUnpack = true;
              dontFixup = true;
              dontWrapQtApps = true;
              installPhase = ''
                mkdir -p $out/share
                ( IFS=:
                  for DIR in $XDG_DATA_DIRS; do
                    if [[ -d "$DIR" ]]; then
                      cp -r $DIR/. $out/share/
                      chmod -R u+w $out/share
                    fi
                  done
                )
              '';
            };

            # undo the XDG_DATA_DIRS injection that is usually done in the qt wrapper
            # script and instead inject the path of the above helper package
            derivedPkg = basePkg.overrideAttrs {
              preFixup = ''
                for index in "''${!qtWrapperArgs[@]}"; do
                  if [[ ''${qtWrapperArgs[$((index+0))]} == "--prefix" ]] && [[ ''${qtWrapperArgs[$((index+1))]} == "XDG_DATA_DIRS" ]]; then
                    unset -v "qtWrapperArgs[$((index+0))]"
                    unset -v "qtWrapperArgs[$((index+1))]"
                    unset -v "qtWrapperArgs[$((index+2))]"
                    unset -v "qtWrapperArgs[$((index+3))]"
                  fi
                done
                qtWrapperArgs=("''${qtWrapperArgs[@]}")
                qtWrapperArgs+=(--prefix XDG_DATA_DIRS : "${xdgdataPkg}/share")
                qtWrapperArgs+=(--prefix XDG_DATA_DIRS : "$out/share")
              '';
            };

          in
          derivedPkg;
      };
    }
  );
  environment = {
    plasma6.excludePackages = with pkgs.kdePackages; [
      elisa
      kate
    ];
    systemPackages = [
      pkgs.kdePackages.audiocd-kio

      # caldav/cardav
      pkgs.kdePackages.akonadi # backend for PIM
      pkgs.kdePackages.akonadi-calendar
      pkgs.kdePackages.akonadi-calendar-tools
      pkgs.kdePackages.kaddressbook
      pkgs.kdePackages.kdepim-addons # display calendar events in the taskbar calendar
      pkgs.kdePackages.kdepim-runtime # backend for PIM
      pkgs.kdePackages.akonadiconsole # required for mail
      pkgs.kdePackages.akonadi-search # required for mail
      pkgs.kdePackages.kmail # required for mail
      pkgs.kdePackages.kmail-account-wizard # required for mail
      pkgs.kdePackages.korganizer # required to connect to caldav

      pkgs.kdePackages.kauth
      pkgs.kdePackages.kwallet-pam # for kwallet automatic login
      pkgs.kdePackages.kde-gtk-config
      (pkgs.kdePackages.spectacle.override {
        tesseractLanguages = [
          "deu"
          "eng"
        ];
      })
      pkgs.krename
      pkgs.exiftool
    ];
  };
  services = {
    desktopManager.plasma6.enable = true;
    displayManager.sddm.wayland.enable = true;
    displayManager.sddm.enable = true;
  };
  programs.xwayland.enable = true;
  programs.partition-manager.enable = true;
  home-manager.users.${config.az-username} = {
    home.file.".local/share/kio/servicemenus/removeMetadata.desktop".text = ''
      # Based on https://github.com/Merrit/kde-dolphin-remove-metadata
      [Desktop Entry]
      Type=Service
      MimeType=image/*
      Actions=removeMetadata

      [Desktop Action removeMetadata]
      Name=Remove Metadata
      Name[de]=Metadaten löschen
      Name[fr]=Retirer les métadonnées
      Name[nl]=Metagegevens wissen
      Name[pl]=Usunąć Metadane
      Name[zh_TW]=移除中繼資料
      Icon=document-cleanup
      Exec=${pkgs.bash}/bin/bash -c '${pkgs.kdePackages.kdialog}/bin/kdialog --yesno "Exif metadata is going to be removed. Are you sure?" --title "Confirmation" && ${pkgs.exiftool}/bin/exiftool -all= "$@" -tagsFromFile @ -Orientation -overwrite_original' -- %U
    '';
  };
}
