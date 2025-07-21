{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      fenix,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        name = "rune";
        pkgs = import nixpkgs { inherit system; };
        fenixPkgs = fenix.packages.${system};
        rustToolchain = fenixPkgs.combine [
          fenixPkgs.stable.cargo
          fenixPkgs.stable.rustc
          fenixPkgs.stable.rust-src
          fenixPkgs.stable.rust-std
          fenixPkgs.stable.rustfmt
          fenixPkgs.stable.clippy
          fenixPkgs.complete.miri
          fenixPkgs.targets.x86_64-pc-windows-gnu.stable.cargo
          fenixPkgs.targets.x86_64-pc-windows-gnu.stable.rustc
          fenixPkgs.targets.x86_64-pc-windows-gnu.stable.rust-src
          fenixPkgs.targets.x86_64-pc-windows-gnu.stable.rust-std
          fenixPkgs.targets.x86_64-pc-windows-gnu.stable.rustfmt
          fenixPkgs.targets.x86_64-pc-windows-gnu.stable.clippy
        ];
      in
      {
        # nix develop
        devShell = pkgs.mkShell {
          packages = with pkgs; [
            rustToolchain
          ];

          env = {
            RUST_SRC_PATH = "${rustToolchain}/lib/rustlib/src/rust/library";
          };
        };
      }
    );
}
