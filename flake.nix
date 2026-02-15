{
  description = "Minimal Hakyll dev shell";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-darwin" ];
    in {
      devShells = builtins.listToAttrs (map (system: {
        name = system;
        value.default =
          let pkgs = import nixpkgs { inherit system; };
          in pkgs.mkShell {
            packages = with pkgs; [ cabal-install ghc pkg-config zlib ];
          };
      }) systems);
    };
}
