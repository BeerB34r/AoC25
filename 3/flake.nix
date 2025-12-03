{
	inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

	outputs = { self, nixpkgs }: {
		devShell."x86_64-linux" =
			with import nixpkgs { system = "x86_64-linux"; };
			pkgs.mkShell {
				buildInputs = [
					haskellPackages.ghc
					haskellPackages.cabal-install
					haskellPackages.cabal2nix
				];
			};

	};
}
