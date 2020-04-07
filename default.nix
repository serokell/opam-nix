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

  subdirs = where:
    map (x: "${where}/${x.name}") (builtins.filter (x: x.type == "directory")
      (builtins.attrValues
        (builtins.mapAttrs (name: type: { inherit name type; })
          (builtins.readDir where))));

  findOPAMFiles = where:
    let
      contents = builtins.readDir where;
      fName = builtins.match "(.*)\\.opam";
      files = map (name: builtins.head (fName name))
        (builtins.filter (name: !isNull (fName name))
          (builtins.attrNames contents));
      subpkgs = builtins.concatMap findOPAMFiles (subdirs where);
    in if files == [ ] then
      subpkgs
    else
      (map (name: {
        inherit name subpkgs;
        src = where;
      }) files);

  dereferenceAll = src:
    pkgs.runCommandNoCC "remove-symlinks" { }
    "cp -Lr --no-preserve=mode,ownership ${src} $out";
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
  opam2nix = { src, opamFile ? findOpamFile src, name ? opamFile + ".nix" }:
    pkgs.runCommandNoCC name { }
    "( cat ${src}/${opamFile}; echo '' ) | ${opam-nix} > $out";

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
      if (builtins.isAttrs v) && (traversed ? ${name}) then
        v // { versions = traversed.${name}.versions; }
      else
        v) super;

  __callOPAMPackage = xs: self: super:
    builtins.concatMap (p:
      [{
        name = p.name;
        value = (self.callPackage (opam2nix {
          inherit (p) name src;
          opamFile = "${p.name}.opam";
        }) {
          extraArgs = {
            inherit (p) name;
            src = builtins.path {
              name = "${p.name}-source";
              path = builtins.unsafeDiscardStringContext p.src;
            };
            pname = p.name;
          };
        });
      }] ++ __callOPAMPackage p.subpkgs self super) xs;

  # Extension that adds all the packages from src to the package set
  callOPAMPackage = src: self: super:
    builtins.listToAttrs
    (__callOPAMPackage (findOPAMFiles (dereferenceAll src)) self super);

  # Overrides package sources with values from sources.
  # Use to speed up builds and reduce the amount of rebuilds due to tarball-ttl expiry.
  cacheSources = sources: self:
    builtins.mapAttrs (name: pkg:
      if sources ? ${name} then
        pkg.overrideAttrs (_: { src = sources.${name}; })
      else
        pkg);
}
