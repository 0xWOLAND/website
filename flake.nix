{
  description = "Hakyll dev shell";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-darwin" ];
      forAllSystems = f:
        nixpkgs.lib.genAttrs systems (system: f (import nixpkgs { inherit system; }));
    in {
      devShells = forAllSystems (pkgs: {
        default = pkgs.mkShell {
          packages = with pkgs; [ cabal-install ghc pkg-config zlib ];
        };
      });

      packages = forAllSystems (pkgs:
        let
          site = pkgs.haskellPackages.callCabal2nix "site" self { };
        in {
          inherit site;
          default = site;
        });
    };
}
