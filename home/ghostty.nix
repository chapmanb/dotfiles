{ lib, pkgs, ... }:

let
  ghostty = "${pkgs.unstable.ghostty}/bin/ghostty";
in {
  home.packages = [ pkgs.gnome-terminal pkgs.xdg-terminal-exec ];
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

  xdg.configFile = {
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
