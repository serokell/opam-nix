# opam-nix

A library for packaging OCaml software.

TL;DR:
```nix
# ocaml-overlay.nix
self: super:
let
  opam-nix = import (builtins.fetchTarball https://github.com/balsoft/opam-nix/archive/master.tar.gz) self.buildPackages;
in
{
  ocamlPackages = super.ocaml-ng.ocamlPackages_4_07.overrideScope'
    (builtins.foldl' self.lib.composeExtensions (_: _: { }) [
      (opam-nix.traverseOPAMRepo' (builtins.fetchTarball https://github.com/ocaml/opam-repository/archive/master.tar.gz))
      (opam-nix.traverseOPAMRepo ../my-cool-repo)
      opam-nix.callOPAMPackage
      (oself: osuper: {
        my-cool-package = oself.callOPAMPackage ./. {
          pname = "my-cool-package";
          version = self.lib.commitIdFromGitRepo ./.git;
        } { };
        
        conf-gmp = self.gmp; # "External" (native) dependencies
        ppx_tools_versioned = osuper.ppx_tools_versioned.versions."5.2.3"; # Force a version
        bigstring = osuper.bigstring.overrideAttrs (_: { doCheck = false; }); # Disable tests
      })
  ]);
}
# 
```

## Contents

You should only ever need to use the nix part of this.

### `opam2nix`

Turns `src/opamFile` into a `pname.nix` file that can later be imported. If opamFile is not provided, finds it in `src`, and if `pname` is not provided, it is `opamFile`.

### `traverseOPAMRepo`

Traverses the repo given to it. Returns an extension to `ocamlPackages` which contains all the packages from the repo with an additional `versions` attribute in every package, that contains an attrset of all the available package versions. Note that this is lazy, meaning it will generate nix expressions only for packages that are evaluated.

### `traverseOPAMRepo'`

The same as `traverseOPAMRepo`, but respects `super` more than `self`, meaning that it will leave all the packages from `super` as-is. Useful for traversing the upstream `opam-repository` in a manner that leaves packages from nixpkgs intact.

### `callOPAMPackage`

Extends `ocamlPackages` with `callOPAMPackage`, which takes three arguments: the first is the source, the second is `extraArgs`, the third is `overrides`. `extraArgs` can be used to override attributes of `mkDerivation` call, as well as override `opamFile` argument of `opam2nix` and inject dependencies with `extraBuildInputs`.

## FIXME

While this does work for my use-cases, it's obviously awful and needs help. If you actually know ocaml, you can help by rewriting `opam-nix.hs` in it using `opam-file-format`!

## TODO

- Proper parser;
- Automatic version resolution;
- Support for cross-compilation and proper "external" dependencies (currently they're hacked together).
