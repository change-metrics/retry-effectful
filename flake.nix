{
  inputs = {
    hspkgs.url =
      "github:podenv/hspkgs/8596beefeb8471cbafae9bdefb6cb5de8dbc5627";
  };
  outputs = { self, hspkgs }:
    let
      pkgs = hspkgs.pkgs;
      pkg = pkgs.hspkgs.callCabal2nix "retry-effectful" self { };
    in {
      devShell."x86_64-linux" = pkgs.hspkgs.shellFor {
        packages = p: [ pkg ];
        buildInputs = with pkgs; [
          cabal-install
          haskell-language-server
          fourmolu
        ];
      };
    };
}
