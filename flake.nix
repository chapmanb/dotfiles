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
        in {
          inherit unstable;
          inherit (unstable)
            neovim wezterm uv pixi bun nodejs_24 deno pnpm ruff pyright
            rclone restic glab brave google-chrome;
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
