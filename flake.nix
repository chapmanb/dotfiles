{
  description = "Brad's unified Home Manager and NixOS dotfiles";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/release-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, nixpkgs-unstable, home-manager }:
    let
      system = "x86_64-linux";
      unstableOverlay = final: prev:
        let
          unstable = import nixpkgs-unstable {
            inherit system;
            config.allowUnfree = true;
          };
          snowflakePyPkgs = unstable.python314Packages.overrideScope (pyFinal: pyPrev: {
            click = pyPrev.click.overridePythonAttrs (old: {
              version = "8.1.7";
              src = unstable.fetchFromGitHub {
                owner = "pallets";
                repo = "click";
                tag = "8.1.7";
                hash = "sha256-8YqIKRyw5MegnRwAO7YTCZateEFQFTH2PHpE8gTPTow=";
              };
              build-system = [ pyFinal.setuptools ];
              doCheck = false;
            });
            typer = pyPrev.typer.overridePythonAttrs (old: {
              version = "0.17.3";
              src = unstable.fetchFromGitHub {
                owner = "fastapi";
                repo = "typer";
                tag = "0.17.3";
                hash = "sha256-ir4RL1Cdq0ENr0ojiJvrdFaZHb4bF8q4rLcKeowliR0=";
              };
              dependencies = (old.dependencies or []) ++ [ pyFinal.click pyFinal.typing-extensions ];
              postPatch = "";
              doCheck = false;
            });
            snowflake-connector-python = pyPrev.snowflake-connector-python.overridePythonAttrs (old: {
              doCheck = false;
              dependencies = (old.dependencies or []) ++ [ pyFinal.boto3 pyFinal.botocore ];
            });
            snowflake-core = pyPrev.snowflake-core.overridePythonAttrs { doCheck = false; };
          });
        in {
          inherit unstable;
          inherit (unstable)
            neovim uv pixi bun python315 deno pnpm ruff pyright
            rclone restic glab brave google-chrome;
          ghostty = unstable.ghostty.overrideAttrs (old: {
            patches = (old.patches or []) ++ [ ./patches/ghostty-child-environment.patch ];
          });
          snowflake-cli = (unstable.snowflake-cli.override {
            python3Packages = snowflakePyPkgs;
          }).overridePythonAttrs (old: rec {
            version = "3.31.0";
            src = unstable.fetchFromGitHub {
              owner = "snowflakedb";
              repo = "snowflake-cli";
              tag = "v${version}";
              hash = "sha256-VMoyGwDI/RL7ZkIfOU2ftZoKaUcI/+/54o0xtjhMBLQ=";
            };
            patches = [];
            dependencies = old.dependencies ++ (with snowflakePyPkgs; [
              click configobj jaraco-context keyring protobuf python-dotenv websocket-client
            ]);
            pythonRemoveDeps = [ "snowflake-snowpark-python" ];
            doCheck = false;
          });
        };
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [ unstableOverlay ];
      };
    in {
      overlays.default = unstableOverlay;

      packages.${system}.daemonProfile = pkgs.symlinkJoin {
        name = "nix-daemon-profile-${pkgs.unstable.nixVersions.latest.version}";
        paths = [
          pkgs.unstable.nixVersions.latest
          pkgs.unstable.nixVersions.latest.man
          pkgs.unstable.cacert
        ];
        passthru.nixVersion = pkgs.unstable.nixVersions.latest.version;
      };

      homeConfigurations."bchapman@bchapman-ubnt" = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [
          ./home
          ./home/ghostty.nix
          {
            targets.genericLinux.enable = true;
            # Ubuntu's older GIO recurses indefinitely when native and newer
            # Nix MIME inheritance definitions are combined. Use the host database.
            xdg.mime.enable = false;
          }
        ];
      };

      nixosConfigurations.bchapman-ubnt = nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [
          ./hosts/bchapman-ubnt
          home-manager.nixosModules.home-manager
          {
            nixpkgs.config.allowUnfree = true;
            nixpkgs.overlays = [ unstableOverlay ];
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.bchapman.imports = [ ./home ./home/ghostty.nix ];
          }
        ];
      };
    };
}
