{pkgs, ...}: {
  kernel.python.minimal = {
    enable = true;
  };
  kernel.r.minimal = {
    enable = true;
  };
  kernel.haskell.minimal = {
    enable = true;
  };
}
