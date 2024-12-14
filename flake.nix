{
  inputs = {
    opam-nix.url = "github:tweag/opam-nix";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.follows = "opam-nix/nixpkgs";
    cpspg.url = "github:Brychlikov/cpspg";
    cpspg.inputs.opam-nix.follows = "opam-nix";
  };
  outputs = { self, flake-utils, opam-nix, nixpkgs, cpspg }@inputs:
    let package = "dbl";
    in flake-utils.lib.eachSystem ["x86_64-linux"] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        on = opam-nix.lib.${system};
        scope =
          on.buildDuneProject { } package ./. { ocaml-system = "*"; };
        overlay = final: prev:
          {
            ${package} = prev.${package}.overrideAttrs 
              (oa: { nativeBuildInputs = oa.nativeBuildInputs ++ [ cpspg.packages.${system}.default ]; });
          };
      in rec {
        legacyPackages = scope.overrideScope overlay;

        packages.default = self.legacyPackages.${system}.${package};
        devShells.default = with pkgs; pkgs.mkShell {
          buildInputs = [ 
              cpspg.packages.${system}.default
              # Source file formatting
              nixpkgs-fmt
              ocamlformat
              # For `dune build --watch ...`
              fswatch
              # For `dune build @doc`
              ocamlPackages.odoc
              # OCaml editor support
              ocamlPackages.ocaml-lsp
              # Nicely formatted types on hover
              ocamlPackages.ocamlformat-rpc-lib
              # Fancy REPL thing
              ocamlPackages.utop
          ];
        };
      });
}
