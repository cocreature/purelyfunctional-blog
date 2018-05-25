{ rev ? "090b7cc8f1bae880fc5542987ede2bcb444d20bf",
  outputSha256 ? "0n23hlbg5np3882qbr2wbh7nz5wvnljszy0899cv628aqqrmym6i"
}:
let
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    sha256 = outputSha256;
  };
  pkgs = import nixpkgs {};
  haskellPackages = (pkgs.haskell.packages.ghc842.extend (pkgs.haskell.lib.packageSourceOverrides {
    blog = ./.;
  }));
in
haskellPackages.shellFor {
  packages = p: [p.blog];
}
