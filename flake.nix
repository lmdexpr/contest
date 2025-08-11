{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    opam-repository = { url = "github:ocaml/opam-repository"; flake = false; };

    flake-utils.url = "github:numtide/flake-utils";

    opam-nix = {
      url = "github:tweag/opam-nix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
        opam-repository.follows = "opam-repository";
      };
    };
  };
  outputs = { flake-utils, opam-nix, nixpkgs, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        on = opam-nix.lib.${system};
        
        src = ./.;
        localNames =
          with builtins;
          filter
            (f: !isNull f)
            (map
              (f:
                let f' = match "(.*)\.opam$" f; in
                if isNull f' then null else elemAt f' 0)
              (attrNames (readDir src)));

        localPackagesQuery =
          with builtins; listToAttrs (map
            (p: {
              name = p;
              value = "*";
            })
            localNames);

        devPackagesQuery = {
          ocaml-lsp-server = "*";
          utop = "*";
        };

        query = devPackagesQuery // localPackagesQuery;

        overlay = self: super:
          with builtins;
          let
            super' = mapAttrs
              (p: _:
                if hasAttr "passthru" super.${p} && hasAttr "pkgdef" super.${p}.passthru
                then super.${p}.overrideAttrs (_: { opam__with_test = "false"; opam__with_doc = "false"; })
                else super.${p})
              super;
            local' = mapAttrs
              (p: _:
                super.${p}.overrideAttrs (_: {
                  doNixSupport = false;
                }))
              localPackagesQuery;
          in
          super' // local';

        scope =
          let
            scp = on.buildOpamProject'
              {
                inherit pkgs;
                resolveArgs = { with-test = true; with-doc = true; };
                pinDepends = true;
              }
              src
              query;
          in
          scp.overrideScope overlay;

        devPackages = builtins.attrValues
          (pkgs.lib.getAttrs (builtins.attrNames devPackagesQuery) scope);

        atcoder-cli = pkgs.buildNpmPackage {
          pname = "atcoder-cli";
          version = "2.2.0";
          src = pkgs.fetchFromGitHub {
            owner = "Tatamo";
            repo = "atcoder-cli";
            rev = "f385e71ba270716f5a94e3ed9bd23a24f78799d0";
            sha256 = "sha256-7pbCTgWt+khKVyMV03HanvuOX2uAC0PL9OLmqly7IWE=";
          };
          npmDepsHash = "sha256-ufG7Fq5D2SOzUp8KYRYUB5tYJYoADuhK+2zDfG0a3ks=";
          npmPackFlags = [ "--ignore-scripts" ];
          NODE_OPTIONS = "--openssl-legacy-provider";
        };

      in {
        legacyPackages = pkgs;

        packages = with builtins; listToAttrs (map (p: {
          name = p;
          value = scope.${p};
        }) localNames);

        devShells.default =
          pkgs.mkShell {
            inputsFrom = builtins.map (p: scope.${p}) localNames;
            buildInputs = with pkgs; devPackages ++ [ 
              nil 
              nixpkgs-fmt 
              cargo
              online-judge-tools
              atcoder-cli
              unzip
              xclip
            ];
            shellHook = ''
              acc config oj-path `which oj`
            '';
          };
      });
}
