{
  description = "MemoryMaster";
  nixConfig.bash-prompt = "[nix(MemoryMaster)] ";

  inputs = {
    hspkgs.url =
      "github:podenv/hspkgs/ba5d181089900f376f765e4a6889bd30c4f96993";
    flake-utils.url = "github:numtide/flake-utils";
    butler.url =
      "github:TristanCacqueray/haskell-butler/be3abc12fe83805b1e9e8f7ca8e87137202be197";
  };

  outputs = { self, hspkgs, flake-utils, butler }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = hspkgs.pkgs;

        packageName = "MemoryMaster";

        haskellExtend = hpFinal: hpPrev: {
          appPackage = hpPrev.callCabal2nix packageName self { };
          butler = pkgs.haskell.lib.dontHaddock hpPrev.butler;
        };

        hsPkgs = (pkgs.hspkgs.extend butler.haskellExtend).extend haskellExtend;
        appExe = pkgs.haskell.lib.justStaticExecutables hsPkgs.appPackage;

        containerHome = "var/lib/${packageName}";
        mkContainerHome = "mkdir -p -m 744 ${containerHome}";
        container = pkgs.dockerTools.buildLayeredImage {
          name = "localhost/memory-master";
          contents = [ pkgs.coreutils pkgs.bash pkgs.openssl appExe ];
          extraCommands = "${mkContainerHome} && mkdir -p -m 777 tmp";
          tag = "latest";
          created = "now";
          config = {
            Env = [
              "HOME=/${containerHome}"
              # Use fakeroot to avoid `No user exists for uid` error
              "LD_PRELOAD=${pkgs.fakeroot}/lib/libfakeroot.so"
            ];
          };
        };

      in {
        defaultExe = appExe;
        defaultPackage = hsPkgs.appPackage;
        packages.container = container;
        devShell = hsPkgs.shellFor {
          packages = p: [ p.appPackage ];

          buildInputs = with pkgs; [
            ghcid
            ormolu
            cabal-install
            hlint
            haskell-language-server
          ];
        };
      });
}
