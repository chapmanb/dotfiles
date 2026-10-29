{ config, lib, pkgs, ... }:

let
  dotfiles = "${config.home.homeDirectory}/.dotfiles";
  # The upstream slow-log script has an unusable Perl shebang and needs my_print_defaults.
  mysql84 = pkgs.symlinkJoin {
    name = "mysql-${pkgs.mysql84.version}-slowlog-runtime";
    paths = [ pkgs.mysql84 ];
    nativeBuildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      rm "$out/bin/mysqldumpslow"
      makeWrapper ${pkgs.perl}/bin/perl "$out/bin/mysqldumpslow" \
        --add-flags "${pkgs.mysql84}/bin/mysqldumpslow" \
        --prefix PATH : "${pkgs.mysql84}/bin"
    '';
  };
in {
  home.username = "bchapman";
  home.homeDirectory = "/home/bchapman";
  home.stateVersion = "24.11";
  programs.home-manager.enable = true;

  # Nix owns tools and base runtimes; project environments and agents stay mutable.
  home.packages = with pkgs; [
    ripgrep fd bat fzf jq rclone restic pkgs.unstable.zstd git glab neovim ghostty tmux
    awscli2 snowflake-cli babashka clojure leiningen ruff pyright tree-sitter
    xclip wl-clipboard pass gnupg
    pkgs.unstable.nodejs_24 python315 uv pixi bun deno pnpm
    brave libreoffice inkscape redshift
    gh go-task glow lazygit gitu mdbook pandoc clj-kondo clojure-lsp
    kubectl kubernetes-helm k9s argo-workflows terraform terraform-docs google-cloud-sdk pre-commit
    ffmpeg graphviz imagemagick qpdf sqlite ncdu parallel datamash tree fx jetbrains-mono
    bubblewrap gnupatch curl xdg-utils socat yt-dlp
    mysql84
  ];

  fonts.fontconfig.enable = true;

  home.file = {
    ".bashrc".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/bashrc";
    ".gitconfig".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/gitconfig";
    ".inputrc".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/inputrc";
    ".tmux.conf".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/tmux.conf";
    ".vimrc".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/vimrc";
    ".config/pip/pip.conf".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/pip.conf";
    ".config/msmtp/config".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/msmtprc";
    ".config/nvim/init.lua".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/config/nvim/init.lua";
    ".config/nvim/nvim-pack-lock.json".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/config/nvim/nvim-pack-lock.json";
    # Ubuntu's /run/opengl-driver bridge follows this ABI-matching Mesa link.
    ".local/state/nix-opengl-driver".source = pkgs.unstable.mesa;
    ".config/herdr/config.toml".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/config/herdr/config.toml";
    ".config/home-manager".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}";
    ".config/nix/nix.conf".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/config/nix/nix.conf";
    ".datomic/dev-local.edn".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/datomic/dev-local.edn";
    ".lein/profiles.clj".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/lein/profiles.clj";
    ".tmux".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/tmux";
    ".w3m".source = config.lib.file.mkOutOfStoreSymlink "${dotfiles}/w3m";
  };

  home.sessionVariables = {
    SSL_CERT_FILE = "/etc/ssl/certs/ca-certificates.crt";
    REQUESTS_CA_BUNDLE = "/etc/ssl/certs/ca-certificates.crt";
    CURL_CA_BUNDLE = "/etc/ssl/certs/ca-certificates.crt";
    NODE_EXTRA_CA_CERTS = "/etc/ssl/certs/ca-certificates.crt";
    GIT_SSL_CAINFO = "/etc/ssl/certs/ca-certificates.crt";
  };

  systemd.user.services.dotfiles-daily-backup = {
    Unit = {
      Description = "Daily dotfiles snapshot backup and browsable sync";
      Wants = [ "network-online.target" ];
      After = [ "network-online.target" ];
    };
    Service = {
      Type = "oneshot";
      ExecStart = "/home/bchapman/.dotfiles/bin/daily-backup run";
      ExecStartPost = "/home/bchapman/.dotfiles/bin/nightly-maintenance";
      # User services do not read bashrc or Home Manager's shell session script.
      Environment = lib.mapAttrsToList (name: value: "${name}=${value}")
        (config.home.sessionVariables // {
          PATH = "${lib.makeBinPath config.home.packages}:/usr/bin:/bin";
        });
      UMask = "0077";
    };
  };

  systemd.user.timers.dotfiles-daily-backup = {
    Unit.Description = "Run the hybrid backup and nightly maintenance at 10pm Eastern";
    Timer = {
      OnCalendar = "*-*-* 22:00:00 America/New_York";
      Persistent = true;
      Unit = "dotfiles-daily-backup.service";
    };
    Install.WantedBy = [ "timers.target" ];
  };
}
