<!--
   - SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io/>
   -
   - SPDX-License-Identifier: MPL-2.0
   -->

# opam-nix

WARNING: THIS REPOSITORY IS ARCHIVED AND NO LONGER MAINTAINED

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
      (opam-nix.callOPAMPackage ./.)
      (oself: osuper: {
        conf-gmp = self.gmp; # "External" (native) dependencies
        ppx_tools_versioned = osuper.ppx_tools_versioned.versions."5.2.3"; # Force a version
        bigstring = osuper.bigstring.overrideAttrs (_: { doCheck = false; }); # Disable tests
      })
  ]);
}
# default.nix
import <nixpkgs> { overlays = [ (import ./ocaml-overlay.nix) ]; }
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

Injects all opam packages that are found in src into the package set.

### `cacheSources`

Caches sources for packages that are in `sources`. Use after `traverseOPAMRepo` to speed up builds.

## TODO

- Proper parser;
- Automatic version resolution;
- Support for cross-compilation and proper "external" dependencies.

## Legal

This repository is licensed under Mozilla Public License, version 2.

## About Serokell

This repository is maintained with ❤️ by [Serokell](https://serokell.io/).
The names and logo for Serokell are trademark of Serokell OÜ.

We love open source software! See [our other projects](https://serokell.io/community?utm_source=github) or [hire us](https://serokell.io/hire-us?utm_source=github) to design, develop and grow your idea!
