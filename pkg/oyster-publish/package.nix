{
  lib,
  buildNpmPackage,
  nodejs_22,
}:

buildNpmPackage {
  pname = "oyster-publish";
  inherit ((lib.importJSON ./package.json)) version;

  src = lib.fileset.toSource {
    root = ./.;
    fileset = lib.fileset.difference ./. (
      lib.fileset.unions (
        map lib.fileset.maybeMissing [
          ./node_modules
          ./dist
          ./.astro
          ./package.nix
        ]
      )
    );
  };

  nodejs = nodejs_22;
  npmDepsHash = "sha256-zjlEke7CdI53pDPkbEtIcZ1A+pE7cMc5Pe73+NVBafA=";

  # The OCaml parser is vendored as src/lib/oystermark/oystermark.cjs, so there
  # is nothing to build
  dontNpmBuild = true;

  meta = {
    description = "Publish an Obsidian vault as a static site";
    homepage = "https://github.com/hon-gyu/oyster/tree/main/pkg/oyster-publish";
    mainProgram = "oyster-publish";
  };
}
