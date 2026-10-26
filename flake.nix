{
  description = "Brad's unified Home Manager and NixOS dotfiles";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/release-24.11";
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
              version = prev.python312Packages.click.version;
              src = prev.python312Packages.click.src;
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
            neovim wezterm uv pixi bun nodejs_24 python315 deno pnpm ruff pyright
            rclone restic glab brave google-chrome;
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

      homeConfigurations."bchapman@bchapman-ubnt" = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [
          ./home
          { targets.genericLinux.enable = true; }
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
            home-manager.users.bchapman = import ./home;
          }
        ];
      };
    };
}
