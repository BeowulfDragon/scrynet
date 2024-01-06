{
  description = "A livestreaming service?";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-23.05";
  };

  outputs = { self, nixpkgs }:
    let forAllSystems = function:
      nixpkgs.lib.genAttrs [
        "x86_64-linux"
        "aarch64-linux"
      ] (system: function nixpkgs.legacyPackages.${system});
    in {
      devShells = forAllSystems (pkgs: {
        default = pkgs.mkShell {
          buildInputs = with pkgs; [
            erlang
            rebar3
            ffmpeg_6 # replace with just ffmpeg when that becomes the default in nixpkgs (should be 23.11)
          ];
        };
      });
    };
}
