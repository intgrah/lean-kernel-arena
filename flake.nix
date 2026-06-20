{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
  inputs.rust-overlay = {
    url = "github:oxalica/rust-overlay";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = { self, nixpkgs, rust-overlay }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ rust-overlay.overlays.default ];
      };
    in
    {
      devShell.${system} = pkgs.stdenv.mkDerivation rec {
        name = "lean-kernel-arena";
        buildInputs = with pkgs; [
          (python3.withPackages (p : with p; [ jinja2 pyyaml jsonschema markdown ]))
          elan
          (rust-bin.stable."1.95.0".default)
          perf
          libffi
          libffi.dev
          pkg-config
          just
          pypy
          monolith
          nodejs
        ];
      };
    };
}
