# Tracking History
```
export HISTFILE=./bash_history 
history -a
```

# Nix Setup

Note that `cudaPackages.cuda_opencl` is not the necessary package.
Use the following instead:
```
nix-shell -p futhark pkg-config ocl-icd opencl-headers
```

# Build

For C,
``` 
futhark c fact.fut 
echo 32 | ./fact 
``` 

For opencl, 
```
futhark opencl fact.fut 
echo 5 | ./fact 
```
