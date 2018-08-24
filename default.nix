{ rev ? "beaa0984c63f6e93a9ddfae18c70d39602e90cca",
  outputSha256 ? "04lnkb5zvks7x7vvx8d0k0vnwx4hv51zh520fqxphhnkazvz0bxd"
}:

let
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    sha256 = outputSha256;
  };
  pkgs = import nixpkgs {};
  haskellPackages = pkgs.haskell.packages.ghc843.override (oldAttrs: {
    overrides = self: super: {
      blog = super.callCabal2nix "blog" ./. {};
    };
  });
in
haskellPackages.blog
