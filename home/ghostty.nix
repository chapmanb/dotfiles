{ config, lib, pkgs, ... }:

let
  ghostty = "${pkgs.ghostty}/bin/ghostty";
in {
  home.packages = [ pkgs.xdg-terminal-exec ];
  home.sessionVariables.TERMINAL = ghostty;

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
  };
}
