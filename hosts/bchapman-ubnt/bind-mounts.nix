{ lib, ... }:

let
  directories = [
    "ginkgo" "personal" "Documents" ".dotfiles" ".ssh" ".gnupg"
    ".password-store" ".aws" ".kube" ".vpn" ".snowflake" ".dbt"
    ".pi" ".claude" ".codex" ".agents" ".gemini"
  ];
in {
  # Require the backing mount: never silently write into an empty substitute.
  # .config and .local/share deliberately remain local to each OS.
  fileSystems = lib.genAttrs (map (dir: "/home/bchapman/${dir}") directories)
    (target: {
      device = "/mnt/ubuntu/home/bchapman/${baseNameOf target}";
      fsType = "none";
      options = [ "bind" "x-systemd.requires-mounts-for=/mnt/ubuntu" ];
      depends = [ "/mnt/ubuntu" ];
    });
}
