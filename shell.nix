{ compiler ? "default" }: let

  stockholm = import <stockholm>;

  inherit (stockholm.systems.${lib.krops.getHostName}) config pkgs;
  inherit (stockholm) lib;

  haskellPackages =
    if compiler == "default"
      then pkgs.haskellPackages
      else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage (import ./.) {};

  exeName = "Reaktor";
in

  lib.overrideDerivation drv.env (oldAttrs: {
    shellHook = ''
      pname=${lib.shell.escape (lib.baseNameOf (toString ./.))}

      WORKDIR=${toString ./.}
      CACHEDIR=$HOME/tmp/$pname
      HISTFILE=$CACHEDIR/bash_history

      mkdir -p "$CACHEDIR"

      main=$CACHEDIR/main-${lib.currentSystem}

      build() {(
        set -efu
        cd "$WORKDIR"
        options=$(
          ${pkgs.cabal-read}/bin/ghc-options "$WORKDIR/$pname.cabal" ${exeName}
        )
        ghc $options \
            -odir "$CACHEDIR" \
            -hidir "$CACHEDIR" \
            -o "$main" \
            -isrc \
            src/main.hs
      )}

      run() {(
        set -efu
        cd "$WORKDIR"
        exec "$main" "$@"
      )}

      cd "$WORKDIR"
    '';
  })
