{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
  outputs = { self, nixpkgs }:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      minihasklisp = pkgs.haskellPackages.callCabal2nix "minihasklisp" ./. { };
    in
    {
      defaultPackage.x86_64-linux = minihasklisp;

      devShell.x86_64-linux =
        pkgs.haskellPackages.shellFor {
          packages = p: [ minihasklisp ];
          withHoogle = true;
          buildInputs = [
            pkgs.haskellPackages.haskell-language-server
            pkgs.haskellPackages.ghcid
            pkgs.haskellPackages.cabal-install
          ];
          # Change the prompt to show that you are in a devShell
          shellHook = "export PS1='\\[\\e[1;34m\\]dev > \\[\\e[0m\\]'";
        };
    };
}
