# Python Bindings For Ada

This is an example of how to call Ada functions from Python.


# Nix

We outline how to build the package in a Nix environment.

Create an environment with the necessary packages as follows:
```
nix-shell -p gnat gprbuild cmake pkg-config python3 "with python3Packages; [ build setuptools cython scikit-build-core wheel]"
```

Build the package using `python3 -m build`.
Unpack the wheel with
```
wheel unpack --dest temp-ada-pkg/ dist/ada_wrapper_pkg-0.0.1-cp312-cp312-linux_x86_64.whl 
```

To test the package, use
```
cd temp-ada-pkg/
PYTHONPATH=$PYTHONPATH:./ada_wrapper_pkg-0.0.1 python -c 'import ada_wrapper'
```


# Maintenance

This build works with nixos-25.05 packages.  Updates might be required to
maintain the approach used here.  We might not maintain this work.


# References

* [ada4cmake](https://github.com/mosteo/ada4cmake/tree/master) This seems like a good reference, though
  I don't think it will work with NixOS currently as the linking step doesn't seem to resolve `ada(init|final)`.
* [How integrate gnatmake/gnatbind/gnatlink in CMake files for C/Ada code?](https://stackoverflow.com/questions/61389071/how-integrate-gnatmake-gnatbind-gnatlink-in-cmake-files-for-c-ada-code)
  This stackoverflow question and the [answer](https://stackoverflow.com/a/61405085) directed me to stand-alone
  encapsulated libraries which seem to be what I am looking for.
* [Building a stand-alone library](https://gcc.gnu.org/onlinedocs//gnat_ugn/Building-a-Stand-alone-Library.html)
* [scikit python extensions](https://scikit-build.readthedocs.io/en/latest/cmake-modules/PythonExtensions.html)
