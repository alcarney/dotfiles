{
  description = "Dotfiles";

  inputs = {
    nixpkgs-stable.url = "github:NixOS/nixpkgs/nixos-23.11";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    awdur = {
      url = "github:swyddfa/awdur";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { awdur, nixpkgs-stable, nixpkgs, home-manager, ... }:
    let
      system = "x86_64-linux";
      pkgs-stable = nixpkgs-stable.legacyPackages.${system};
      pkgs = import nixpkgs { inherit system; overlays = [awdur.overlays.default];};
    in {
      homeConfigurations."alex" = home-manager.lib.homeManagerConfiguration {
        # Use the latest and greatest for the user env.
        inherit pkgs;

        # Specify your home configuration modules here, for example,
        # the path to your home.nix.
        modules = [ ./home.nix ];

        # Optionally use extraSpecialArgs
        # to pass through arguments to home.nix
      };

      packages."${system}".default = pkgs.callPackage ./docs {};

      nixosConfigurations.alex-desktop = nixpkgs-stable.lib.nixosSystem {
        inherit system;
        # Use stable branch for the base system.
        pkgs = pkgs-stable;
        modules = [ ./systems/alex-desktop/configuration.nix ];
      };
    };
}
