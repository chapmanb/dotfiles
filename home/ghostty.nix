{ config, lib, pkgs, ... }:

let
  ghostty = "${pkgs.ghostty}/bin/ghostty";
  brave = "${pkgs.brave}/bin/brave";
  redshiftGtk = "${pkgs.redshift}/bin/redshift-gtk";
  browserExtensions = builtins.fromJSON (builtins.readFile ../config/chromium/extensions.json);
in {
  home.packages = [ pkgs.xdg-terminal-exec ];
  home.sessionVariables.TERMINAL = ghostty;

  programs.brave = {
    enable = true;
    extensions = builtins.attrNames browserExtensions.extensions;
  };

  dconf.settings = {
    "org/gnome/desktop/default-applications/terminal" = {
      exec = ghostty;
      exec-arg = "-e";
    };
    "org/gnome/settings-daemon/plugins/media-keys".custom-keybindings = lib.mkAfter [
      "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/ghostty/"
    ];
    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/ghostty" = {
      name = "Ghostty";
      command = ghostty;
      binding = "<Primary><Alt>t";
    };
    "org/gnome/shell" = {
      disable-user-extensions = false;
      enabled-extensions = [
        "auto-move-windows@gnome-shell-extensions.gcampax.github.com"
        "desktop-icons@csoriano"
        "ubuntu-appindicators@ubuntu.com"
        "ubuntu-dock@ubuntu.com"
      ];
    };
    "org/gnome/shell/extensions/auto-move-windows" = {
      application-list = [
        "google-chrome.desktop:2"
        "firefox.desktop:2"
        "brave-browser.desktop:2"
        "com.brave.Browser.desktop:2"
        "org.gnome.Terminal.desktop:3"
        "org.wezfurlong.wezterm.desktop:3"
        "com.mitchellh.ghostty.desktop:3"
        "code.desktop:4"
        "Zoom.desktop:1"
        "antigravity.desktop:4"
        "cursor.desktop:4"
      ];
    };
  };

  # Ubuntu's systemd cannot activate Ghostty's packaged user service.
  xdg.desktopEntries."com.mitchellh.ghostty" = lib.mkIf config.targets.genericLinux.enable {
    name = "Ghostty";
    comment = "A terminal emulator";
    exec = "${ghostty} --gtk-single-instance=true";
    icon = "com.mitchellh.ghostty";
    categories = [ "System" "TerminalEmulator" ];
    terminal = false;
    startupNotify = true;
    settings = {
      Version = "1.0";
      DBusActivatable = "false";
      TryExec = ghostty;
      StartupWMClass = "com.mitchellh.ghostty";
      Keywords = "terminal;tty;pty;";
      X-GNOME-UsesNotifications = "true";
      X-KDE-Shortcuts = "Ctrl+Alt+T";
      X-TerminalArgExec = "-e";
      X-TerminalArgTitle = "--title=";
      X-TerminalArgAppId = "--class=";
      X-TerminalArgDir = "--working-directory=";
      X-TerminalArgHold = "--wait-after-command";
    };
    actions.new-window = {
      name = "New Window";
      exec = "${ghostty} --gtk-single-instance=true";
    };
  };

  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "x-scheme-handler/http" = [ "brave-browser.desktop" ];
      "x-scheme-handler/https" = [ "brave-browser.desktop" ];
      "text/html" = [ "brave-browser.desktop" ];
      "application/xhtml+xml" = [ "brave-browser.desktop" ];
    };
  };

  xdg.configFile = {
    "ghostty/config.ghostty".text = ''
      font-family = JetBrains Mono
      font-size = 11
    '';

    "xdg-terminals.list".text = "com.mitchellh.ghostty.desktop\n";

    "redshift/redshift.conf".text = ''
      [redshift]
      location-provider=manual

      [manual]
      lat=42.36
      lon=-71.06
    '';

    # Replace the unmanaged legacy entry so both terminals cannot autostart.
    "autostart/gnome-terminal.desktop" = {
      force = true;
      text = ''
        [Desktop Entry]
        Type=Application
        Name=GNOME Terminal
        Hidden=true
      '';
    };
    "autostart/com.mitchellh.ghostty.desktop".text = ''
      [Desktop Entry]
      Type=Application
      Name=Ghostty
      Exec=${ghostty}
      TryExec=${ghostty}
      Terminal=false
      OnlyShowIn=GNOME;
      X-GNOME-Autostart-enabled=true
    '';
    "autostart/brave-browser.desktop" = {
      force = true;
      text = ''
        [Desktop Entry]
        Type=Application
        Name=Brave Web Browser
        Exec=${brave}
        TryExec=${brave}
        Terminal=false
        OnlyShowIn=GNOME;
        X-GNOME-Autostart-enabled=true
      '';
    };
    "autostart/redshift-gtk.desktop" = {
      force = true;
      text = ''
        [Desktop Entry]
        Type=Application
        Name=Redshift
        Exec=${redshiftGtk}
        TryExec=${redshiftGtk}
        Icon=redshift
        Terminal=false
        OnlyShowIn=GNOME;
        Hidden=false
        X-GNOME-Autostart-enabled=true
      '';
    };
  };
}
