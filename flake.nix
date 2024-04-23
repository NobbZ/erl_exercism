{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }: let
    pkgs = nixpkgs.legacyPackages.x86_64-linux;
    inherit (pkgs) glibcLocales;
    inherit (pkgs.stdenv) isLinux;
    inherit (pkgs.lib) optionalString;

    beamPkgs = let bn = pkgs.beam_nox; in bn.packagesWith bn.interpreters.erlangR25;
    inherit (beamPkgs) erlang rebar3;
  in {
    devShells.x86_64-linux.default = pkgs.mkShell {
      packages = [erlang rebar3];

      LOCALE_ARCHIVE = optionalString isLinux "${glibcLocales}/lib/locale/locale-archive";
      LANG = "en_US.UTF-8";
      ERL_INCLUDE_PATH = "${erlang}/lib/erlang/use/include";
    };
  };
}