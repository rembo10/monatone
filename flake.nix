{
  description = "monatone - audio metadata library";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        
        haskellPackages = pkgs.haskell.packages.ghc910;
        
        monatone = (haskellPackages.callCabal2nix "monatone" ./. {}).overrideAttrs (oldAttrs: {
          # Make ffmpeg available during test phase
          nativeBuildInputs = (oldAttrs.nativeBuildInputs or []) ++ [ pkgs.ffmpeg ];
          checkInputs = (oldAttrs.checkInputs or []) ++ [ pkgs.ffmpeg ];
        });
        
      in
      {
        packages = {
          default = monatone;
          monatone = monatone;
        };

        devShells.default = haskellPackages.shellFor {
          packages = p: [ monatone ];
          
          buildInputs = with haskellPackages; [
            cabal-install
            haskell-language-server
            ghcid
            hlint
            ormolu
            cabal2nix
          ] ++ [
            pkgs.ffmpeg
          ];

          shellHook = ''
            echo "monatone development environment"
            echo "GHC version: $(ghc --version)"
            echo "Cabal version: $(cabal --version | head -n1)"
          '';
        };
      });
}
