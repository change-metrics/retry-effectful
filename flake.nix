{
  inputs = {
    nixpkgs.url =
      "github:NixOS/nixpkgs/abddaec7149b62550305c0d20cf9651f8413da77";
  };
  outputs = { self, nixpkgs }:
    let
      pkgs = import nixpkgs { system = "x86_64-linux"; };
      pkg = pkgs.haskellPackages.callCabal2nix "retry-effectful" self { };
    in {
      devShell."x86_64-linux" = pkgs.haskellPackages.shellFor {
        packages = p: [ pkg ];
        buildInputs = with pkgs; [ cabal-install ];
      };
    };
}
