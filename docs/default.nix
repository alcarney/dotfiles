{
  stdenv,
  python3Packages,
}:

let awdur = python3Packages.awdur; in

stdenv.mkDerivation {
  pname = "dotfiles-site";
  version = "1.0";

  src = ./.;

  buildCommand = ''
    mkdir $out
    ${awdur}/bin/awdur bind "$src/index.rst"
    cp out.html $out/index.html
  '';
}
