{ nixpkgsSrc ? builtins.fetchTarball {
  url =
    "https://github.com/nixos/nixpkgs/archive/17af7a98d2e7dfdfdae5292b8dbd7bab28eeaf8d.tar.gz"; # refs/heads/master
  sha256 = "11yk5q0jk7l30siv28b8qhb4s55ypmif6mp2nv90mwq1c6n11p1x";
}, pkgs ? import nixpkgsSrc { }, compiler ? null, extraOverrides ? _: _: { }
, modifier ? x: x }:

let
  haskellPackages = if compiler == null then
    pkgs.haskellPackages
  else
    pkgs.haskell.packages.${compiler};

  clashSrc = pkgs.fetchFromGitHub {
    owner = "expipiplus1";
    repo = "clash-compiler";
    rev = "a41d61c9109357e034691896798ebfc1b83b24af"; # joe-yosys-sva
    sha256 = "0c5f201h5cfr8zibrjki9v791fipqd2b9rs6n5ck003wv50nsvsr";
  };

in haskellPackages.developPackage {
  name = "";
  root = pkgs.nix-gitignore.gitignoreSource [ ] ./.;
  overrides = with pkgs.haskell.lib;
    pkgs.lib.composeExtensions (self: _super:
      pkgs.lib.mapAttrs
      (_: p: with pkgs.haskell.lib; dontCheck (disableLibraryProfiling p)) {
        clash-ghc = self.callCabal2nix "" (clashSrc + "/clash-ghc") { };
        clash-lib = self.callCabal2nix "" (clashSrc + "/clash-lib") { };
        clash-prelude = self.callCabal2nix "" (clashSrc + "/clash-prelude") { };
      }) extraOverrides;
  inherit modifier;
}

