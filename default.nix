pkgs:
let
  findOpamFile = src:
    let
      opamFiles = (builtins.filter (x: !isNull (builtins.match ".*.opam" x))
        (builtins.attrNames (builtins.readDir src)));
    in if builtins.length opamFiles == 1 then
      builtins.head opamFiles
    else
      builtins.throw "Unable to determine a single opam file in ${src} (${
        toString opamFiles
      }), unable to proceed" "";
in rec {
  opam-nix = pkgs.stdenv.mkDerivation {
    name = "opam-nix";
    src = ./opam-nix.hs;
    unpackPhase = "true";
    nativeBuildInputs = [ pkgs.ghc ];
    buildPhase = "ghc -o $out $src";
    phases = [ "buildPhase" ];
  };

  # Generate a nix file from an opam file
  opam2nix =
    { src, opamFile ? findOpamFile src, name ? opamFile + ".nix", ... }:
    pkgs.runCommandNoCC name { }
    "cat ${src}/${opamFile} | ${opam-nix} > $out";

  # Traverse OPAM repository, producing an extension to
  # ocamlPackages than includes all the packages in the repo.
  traverseOPAMRepo = repo: self: super:
    let
      pkgNames = builtins.readDir "${repo}/packages";
      parseVersion = n: builtins.head (builtins.match "[^.]*.(.*)" n);
      versions = pkg:
        map parseVersion
        (builtins.attrNames (builtins.readDir "${repo}/packages/${pkg}"));
      latestVersion = builtins.foldl'
        (x: acc: if builtins.compareVersions x acc == 1 then x else acc) "";
      opamPkgs = builtins.mapAttrs (name: _:
        let
          file = version:
            opam2nix {
              src = "${repo}/packages/${name}/${name}.${version}";
              opamFile = "opam";
              name = "${name}.nix";
            };
          package = version:
            self.callPackage (file version) {
              extraArgs = {
                pname = name;
                inherit version;
              };
            };
          latest = package (latestVersion (versions name));
          others = map package (versions name);
        in latest // {
          versions = builtins.listToAttrs (map (p: {
            name = p.version;
            value = p;
          }) others);
        }) pkgNames;
    in opamPkgs;

  # Same as traverseOPAMRepo, but packages in super take precedence over packages from the new repo.
  traverseOPAMRepo' = repo: self: super:
    let traversed = traverseOPAMRepo repo self super;
    in traversed // builtins.mapAttrs (name: v:
      v // {
        versions = (traversed.${name} or { versions = { }; }).versions;
      }) super;

  # Extension that adds callOPAMPackage to the package set
  callOPAMPackage = self: super: {
    callOPAMPackage = src: extraArgs: overrides:
      (self.callPackage (opam2nix (extraArgs // { inherit src; }))
      (overrides // { inherit extraArgs; })).overrideAttrs ({ buildInputs, ... }@args: {
          inherit src;
          buildInputs = buildInputs ++ extraArgs.extraBuildInputs or [ ];
          propagatedBuildInputs = buildInputs
            ++ extraArgs.extraBuildInputs or [ ];
        });
  };
}
