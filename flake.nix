{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
  outputs = { self, nixpkgs }:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      minihasklisp = pkgs.haskellPackages.callCabal2nix "minihasklisp" ./. { };
    in
    {
      packages.x86_64-linux.default = minihasklisp;

      devShells.x86_64-linux.default =
        pkgs.haskellPackages.shellFor {
          packages = p: [ minihasklisp ];
          withHoogle = true;
          buildInputs = [
            pkgs.haskellPackages.haskell-language-server
            pkgs.haskellPackages.ghcid
            pkgs.haskellPackages.cabal-install
          ];
        };
    };
}
