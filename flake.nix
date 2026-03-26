{
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-2511";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      haskellNix,
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (
      system:
      let
        overlays = [
          haskellNix.overlay
          (final: _prev: {
            hixProject = final.haskell-nix.hix.project {
              src = ./.;
              evalSystem = "x86_64-linux";
              name = "text-game";
              compiler-nix-name = "ghc9122";
              shell.tools.cabal = "latest";
              shell.withHoogle = false;
              shell.tools.haskell-language-server = "latest";
            };
          })
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        haskell = pkgs.hixProject.flake { };
      in
      {
        devShells.default = pkgs.mkShell {
          name = "text-game";
          inputsFrom = [
            haskell.devShells.default
          ];
          packages = with pkgs; [
            ghcid
          ];
        };
        packages.default = haskell.packages."text-game:exe:text-game";
      }
    );
}
