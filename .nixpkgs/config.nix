{
  packageOverrides = super: let self = super.pkgs; in
  {

    myPy = self.python.buildEnv.override {
      extraLibs = [ self.pythonPackages.websocket_client  self.pythonPackages.sexpdata ];
      ignoreCollisions = true;
    };

    myCoolVim = self.lib.overrideDerivation (self.vim_configurable.override { python = self.myPy; }) (o: {
      aclSupport              = false;
      cscopeSupport           = true;
      darwinSupport           = false;
      fontsetSupport          = true;
      ftNixSupport            = true;
      gpmSupport              = true;
      gui                     = false;
      hangulinputSupport      = false;
      luaSupport              = true;
      multibyteSupport        = true;
      mzschemeSupport         = true;
      netbeansSupport         = false;
      nlsSupport              = false;
      perlSupport             = false;
      pythonSupport           = true;
      rubySupport             = true;
      sniffSupport            = false;
      tclSupport              = false;
      ximSupport              = false;
      xsmpSupport             = false;
      xsmp_interactSupport    = false;
    });

    # haskell

    profiledHaskell = super.haskell // {
      packages = super.haskell.packages // {
        lts-3_7 = super.haskell.packages.lts-3_7.override {
          overrides = self: super: {
            mkDerivation = args: super.mkDerivation (args // {
              enableLibraryProfiling = true;
            });
          };
        };
      };
    };

    haskell = super.haskell // {
      packages = super.haskell.packages // {
        lts = super.haskell.packages.lts.override {
          overrides = self: super: {
             # mysql        = self.disableTest super.mysql;
             #mysql-simple = lib.dontCheck super.mysql-simple;
             haskell-kubernetes = self.callPackage /home/aaronlevin/dev/soundcloud/haskell-kubernetes/default.nix { };

            #should-not-typecheck = self.callPackage /home/aaronlevin/dev/oss/should-not-typecheck/default.nix { };
            #servant = self.callPackage /home/aaronlevin/dev/oss/servant/servant/default.nix { };
            #servant-client = self.callPackage /home/aaronlevin/dev/oss/servant/servant-client/default.nix { };
            #servant-docs = self.callPackage /home/aaronlevin/dev/oss/servant/servant-docs/default.nix { };
            #servant-mock = self.callPackage /home/aaronlevin/dev/oss/servant/servant-mock/default.nix { };
            #servant-server = self.callPackage /home/aaronlevin/dev/oss/servant/servant-server/default.nix { };
            #Glob = self.callPackage /home/aaronlevin/dev/oss/Glob/default.nix { };
            #idris = self.callPackage /home/aaronlevin/dev/oss/Idris-dev/default.nix { };
          };
        };
      };
    };

    profileEnv = self.profiledHaskell.packages.lts-3_7.ghcWithHoogle (haskellPackages: with haskellPackages; [
      aeson
      async
      base64-bytestring
      bytestring
      ekg
      lens
      mtl
      http-client
      pipes
      pipes-concurrency
      pipes-group
      pipes-postgresql-simple
      postgresql-simple
      text
      time
      wreq

      cabal-install
    ]);

    haskEnv = self.haskell.packages.lts.ghcWithHoogle (haskellPackages: with haskellPackages; [

        # libraries
        aeson
        acid-state
        arrows
        async
        attoparsec
        base-compat
        bifunctors
        bytestring
        bytestring-conversion
        bytes
        cereal
        comonad
        configurator
        containers
        contravariant
        country-codes
        cgi
        criterion
        cron
        cryptonite
        data-default
        deepseq
        Earley
        either
        ekg
        foldl
        Frames
        free
        #hasql
        #hasql-backend
        #hasql-postgres
        #hasql-postgres-options
        hpack
        hspec
        http-client
        http-media
        http-streams
        http-types
        HUnit
        io-streams
        lens
        lens-aeson
        lens-family
        microlens
        mtl
        mysql-simple
        nats
        network
        normaldistribution
        optparse-applicative
        parallel
        path-io
        pipes
        pipes-aeson
        pipes-bytestring
        pipes-concurrency
        pipes-extras
        pipes-group
        pipes-http
        pipes-parse
        pipes-postgresql-simple
        pipes-safe
        pipes-text
        #pool
        postgresql-simple
        profunctors
        QuickCheck
        quickcheck-instances
        scientific
        semigroups
        semigroupoids
        servant
        servant-client
        servant-docs
        servant-mock
        servant-server
        should-not-typecheck
        simple-sql-parser
        singletons
        shelly
        text
        time
        transformers
        transformers-compat
        turtle
        utf8-string
        uuid
        vector
        vault
        vinyl
        void
        wai
        warp
        wreq
        zip-archive

        # local
        haskell-kubernetes

        # tools
        cabal-install
        cabal2nix
        codex
        ghc-mod
        ghcid
        haskintex
        hasktags
        hlint
        idris
        # psc-ide
        purescript
        # stack
        # stackage-upload
        stylish-haskell
        xmonad
        xmonad-contrib
        xmonad-extras
        xmobar
    ]);
  };

  allowBroken = true;
  allowUnfree = true;
}
