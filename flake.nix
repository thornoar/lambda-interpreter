{
    inputs = {
        nixpkgs = {
            url = "github:NixOS/nixpkgs/nixos-24.05";
        };
    };

    outputs = { self, nixpkgs }:
    let
        system = "x86_64-linux";
        pkgs = nixpkgs.legacyPackages.${system};
        # overlay = final: prev: {
        #     lambda-interpreter = final.callCabal2nix "lambda-interpreter" ./. { };
        # };
        # myHaskellPackages = pkgs.haskellPackages.extend overlay;
    in
    {
        # packages.${system}.default = myHaskellPackages.lambda-interpreter;
        packages.${system}.default = pkgs.haskellPackages.callCabal2nix "lambda-interpreter" ./. { };
        apps.${system}.default = {
            type = "app";
            program = "${self.packages.${system}.default}/bin/lambda-interpreter";
        };
    };
}
