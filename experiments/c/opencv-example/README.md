# opencv-example

`opencv` is not in a stackage LTS. 
Moreover, the package is marked as broken in the Nix packages repo we are currently using (23.05).  
To build the derivation, we use the environment variable to ignore the broken package: 
```
NIXPKGS_ALLOW_BROKEN=1 nix-build 
```

We had issues building opencv with the current Haskell packages in NixOS (which should be based on ghc-9.2.8), 
so we dropped down to `ghc810`, and were able to build `opencv`.

# opencv documentation 

See commands `dilate` and `morphologyDefaultBorderValue` in 
[Image Filtering](https://docs.opencv.org/3.4.20/d4/d86/group__imgproc__filter.html).

See command `threshold` in 
[Miscellaneous Image Transformations](https://docs.opencv.org/3.4.20/d7/d1b/group__imgproc__misc.html).

# imutils code

The methods referenced in the tutorial can be found in 
[convenience.py](https://github.com/PyImageSearch/imutils/blob/master/imutils/convenience.py).

# References

* [Motion detection with simple script](https://raspberrypi.stackexchange.com/questions/81905/raspberry-pi-camera-motion-detection-via-simple-script)
* [Basic motion detection and tracking with Python and OpenCV](https://pyimagesearch.com/2015/05/25/basic-motion-detection-and-tracking-with-python-and-opencv/)


