{
  inputs = { nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable"; };

  outputs = { self, nixpkgs }:
    let pkgs = import nixpkgs { localSystem = "x86_64-linux"; };
    in {
      devShell.x86_64-linux = pkgs.mkShell { buildInputs = [ pkgs.lean4 ]; };
    };
}
