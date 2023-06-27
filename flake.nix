{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      with nixpkgs.legacyPackages.${system};
      let
        t = lib.trivial;
        hl = haskell.lib;

        name = "subsite-with-static";

        project = devTools:
          let addBuildTools = (t.flip hl.addBuildTools) devTools;
          in haskellPackages.developPackage {
            root = lib.sourceFilesBySuffices ./. [ ".cabal" ".hs" ];
            name = name;
            returnShellEnv = !(devTools == [ ]);

            modifier = (t.flip t.pipe) [
              addBuildTools
              hl.dontHaddock
              hl.enableStaticLibraries
              hl.justStaticExecutables
              hl.disableLibraryProfiling
              hl.disableExecutableProfiling
            ];

            overrides = self: super: {
              yesod-core =
                self.callCabal2nix "yesod-core" (builtins.fetchGit {
                     url = "https://github.com/AriFordsham/yesod";
                     ref = "ari/subsites";
                     rev = "8be44a8cf4f755a90de64cb61fe22eb3570d8915";
                  } + "/yesod-core") {};

            };
          };

      in {
        packages.pkg = project [ ];

        defaultPackage = self.packages.${system}.pkg;

        devShell = project (with haskellPackages; [
          cabal-fmt
          cabal-install
          haskell-language-server
          hlint
        ]);
      });
}
