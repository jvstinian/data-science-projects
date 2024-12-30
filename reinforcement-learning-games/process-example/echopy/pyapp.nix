{ lib, python3Packages }:
with python3Packages;
buildPythonApplication {
  pname = "echo";
  version = "1.0";

  propagatedBuildInputs = [ ];

  src = ./.;
}
