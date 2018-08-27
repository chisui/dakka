{ nixpkgs ? <nixpkgs>
, pkgs ? import nixpkgs {}
, texlive ? pkgs.texlive
, pandoc ? pkgs.pandoc
, mkDerivation ? pkgs.stdenv.mkDerivation
}:
{ name, version, src
, documentFile ? src
# additional build inputs
, buildInputs ? []
# additional texlive packages passed to texlive.combine
, texlivePackages ? {}
# raw pandoc args that aren't covered by arguments to mkPandoc (yet)
, additionalPandocArgs ? []
# a list of pandoc filters. Each of these will result in a `--filter` argument
# to pandoc. The filters themselves have to be deriviations themselves and are
# added to the buildInputs. The name is either the contents of 
# `pandocFilterName` on the filter or the derivation name if no explict name was given.
, filters ? []
# The template has to point to a latex file and has to list all required
# texlive packages in an attribute called `texlivePackages`.
, template ? null
# one to one mapping for pandoc arguments
, from ? null
, to ? null
, listings ? true
, toc ? false
, csl ? null
, bibliography ? null
, number-sections ? false
, top-level-division ? null
, verbose ? false
}@args:

with builtins; 
let
  isDir = src != documentFile;
  customTexlive = if to != "latex" && match "^.*\\.pdf$" name == null
    then [] # don't build it if we don't need it
    else [(
      texlive.combine (
        { inherit (texlive) scheme-basic; }
        // (if template != null
            then template.texlivePackages or []
            # default tempalte packages
            else { inherit (texlive) 
              lm collection-fontsrecommended listings;
            }
        )
        // texlivePackages
      )
    )];

  pandocArgs = 
    let 
      mkArg = name:
        let a = args.${name} or null;
        in if a == true                then ["--${name}"]
        else if !isBool a && a != null then ["--${name} ${a}"]
        else [];
      toFilterName = f: f.pandocFilterName or (parseDrvName f.name).name;
    in concatLists (map mkArg [ 
        "from" "to" "bibliography" "csl" "template" "top-level-division"
        "listings" "toc" "number-sections" "verbose"
      ])
      ++ map (d: "--filter ${toFilterName d}") filters
      ++ additionalPandocArgs;

  mkDrvArgs = {
    inherit name version src;

    buildInputs = [ pandoc ] ++ buildInputs ++ filters ++ customTexlive;

    configurePhase = ":";
    buildPhase = "pandoc ${documentFile} ${concatStringsSep " " pandocArgs} -o $out";
    installPhase = ":";
    fixupPhase = ":";
  } // (
    # can't unpack single file
    if isDir
      then {} 
      else { unpackPhase = ":"; }
  );
in mkDerivation mkDrvArgs

