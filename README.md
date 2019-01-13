

  first create a configuration file, say config.json, that looks like follows:

    {
      "nick": "somenick",
      "useNickServ": false
    }

  or, if the nick is registered with NickServ, then:

    {
      "nick": "somenick",
      "pass": "somepass"
    }

  then enter the Nix shell for development:

    cabal2nix . > default.nix && nix-shell -I stockholm=~/stockholm

  and run the REPL:

    build
    run config.json

  or, interactively:

    ghci -isrc -Wall src/main.hs
    withArgs ["config.json"] main
    :r

