{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    purescript-overlay = {
      url = "github:thomashoneyman/purescript-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = { self, nixpkgs, flake-utils, purescript-overlay }:
    let supportedSystems = flake-utils.lib.defaultSystems;
    in flake-utils.lib.eachSystem supportedSystems (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ purescript-overlay.overlays.default ];
        };
        pc-config = pkgs.writeText "process-compose.yaml" (builtins.toJSON {
          version = "0.5";
          processes = {
            spago-frontend = {
              command =
                "${pkgs.spago}/bin/spago bundle-app --watch --main Frontend --to frontend.js";
              is_tty = true;
            };
            spago-backend = {
              command =
                "${pkgs.spago}/bin/spago bundle-app --watch --main Backend --to backend.js";
              is_tty = true;
            };
            bun = { command = "${pkgs.bun}/bin/bun run --watch backend.js"; };
          };
        });
      in {
        apps = {
          dev = {
            type = "app";
            program = (pkgs.writeScript "process-compose" ''
              #!/bin/sh
              ${pkgs.process-compose}/bin/process-compose --config ${pc-config} "$@"
            '').outPath;
          };
        };
        devShells = {
          default = pkgs.mkShell {
            buildInputs = with pkgs; [
              purescript
              spago
              nodePackages_latest.purs-tidy
              nodePackages_latest.purescript-language-server
            ];
          };
        };
      });
}
