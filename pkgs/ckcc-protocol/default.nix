with import <nixpkgs> {};
with python3Packages;
# { stdenv, fetchFromGitHub, buildPythonPackage, pytest
# , ecdsa , mnemonic, protobuf, hidapi }:
{}:
buildPythonPackage rec {
  pname = "ckcc-protocol";
  version = "0.8.0";

  src = fetchFromGitHub {
    owner = "Coldcard";
    repo = "ckcc-protocol";
    rev = "v${version}";
    sha256 = "1nnff7im147md9by3m1lvrfyiwvszy2g4j3r09d69w9x4ycwqb7w";
  };

  propagatedBuildInputs = [ protobuf ];

  buildInputs = [ ecdsa ];

  checkInputs = [ pytest ];

  # tests requires hardware
  doCheck = false;

  # Remove impossible dependency constraint
  # postPatch = "sed -i -e 's|hidapi==|hidapi>=|' setup.py";

  meta = with stdenv.lib; {
    description = "Python library and command line tool for communicating with your Coldcard over USB";
    homepage = https://github.com/coldcard/ckcc-protocol;
    license = licenses.gpl3;
    maintainers = with maintainers; [ dnixty ];
  };
}
