# RECS Energy Consumption Regressions

This purpose of this notebook provided here is to reproduce the energy consumption regressions described in 
*The carbon footprint of household energy use in the United States* 
by Goldstein, Gounaridis, and Newell 
[(PNAS)](https://www.pnas.org/doi/abs/10.1073/pnas.1922205117) 
and more specifically the 
[Appendix](https://www.pnas.org/doi/suppl/10.1073/pnas.1922205117/suppl_file/pnas.1922205117.sapp.pdf).

# How To Use
All work is contained in the Jupyter notebook.  Start a Jupyter notebook server from this directory or 
copy `RECSRegressions.ipynb` to a running Jupyter instance.  

To run this notebook, you will need a copy of the 
[2015 Residential Energy Consumption Survey (RECS) data](https://www.eia.gov/consumption/residential/data/2015/csv/recs2015_public_v4.csv).

# For Nix Users
A nix-shell environment has been provided. 
Currently it is pinned to a specific version of the channel `nixos-23.11`. 
It will start a Jupyter notebook server, which can be accessed through a browser 
at [localhost:8888](http://localhost:8888/).

