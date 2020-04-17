

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

# HTTP API

  Reaktor can provide an HTTP API so external applications can control
  its behavior.  At the moment this is restricted to send PRIVMSGs to
  registered channels.

## Enable the HTTP API

  To enable the HTTP API, a listening address has to be configured.
  This address can be a TCP port, specified like follows:

    {
      "API": {
        "listen": "inet://127.0.0.1:7777"
      }
    }

  or it can be an Unix domain socket, specified like follows:

    {
      "API": {
        "listen": "unix:/path/to/reaktor.sock"
      }
    }

## Example usage of the HTTP API

  Let's say your reaktor instance has been configured to listen to
  inet://localhost:7777, and the register plugin has been configured
  to join #somechannel.  Then it is possible to send a PRIVMSG to
  this channel using e.g. following command:

    curl -fsSv http://localhost:7777/ \
        -H content-type:application/json \
        -d "$(jq -n '{command:"PRIVMSG",params:["#somechannel","derp!"]}')"

  And similarly if unix:/path/to/reaktor.sock has been used instead:

    curl -fsSv --unix-socket /path/to/reaktor.sock http://dontcare/ \
        -H content-type:application/json \
        -d "$(jq -n '{command:"PRIVMSG",params:["#somechannel","derp!"]}')"
