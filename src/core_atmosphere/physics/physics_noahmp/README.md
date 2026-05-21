![noahmp_logo_update](https://github.com/NCAR/noahmp/assets/43385564/1fb47fc2-99bd-4360-9ed0-6d5656c29626)


[![DOI](https://zenodo.org/badge/236657733.svg)](https://zenodo.org/badge/latestdoi/236657733)


# Noah-MP<sup>®</sup> Community Model Repository

Noah-MP<sup>®</sup> is a widely-used state-of-the-art land surface model used in many research and operational weather/climate models (e.g., HRLDAS, WRF, MPAS, WRF-Hydro/NWM, NOAA/UFS, NASA/LIS, etc.).

This is the official Noah-MP land surface model unified repository for code downloading and contribution. Noah-MP is a community open-source model developed with the contributions from the entire scientific community. For development, maintenance, and release of the community Noah-MP GitHub code, please contact: Cenlin He (cenlinhe@ucar.edu) and Fei Chen (feichen@ucar.edu).

Noah-MP model website: https://ral.ucar.edu/solutions/products/noah-multiparameterization-land-surface-model-noah-mp-lsm


## New: Release of Noah-MP version 5.0 (Refactored/Modernized version)

The latest Noah-MP model version (version 5.0) has been released in March 9, 2023, which is a modernized/refactored version by re-writing the entire model with modern Fortran code infrastructure and data structures. All future Noah-MP developments and updates will be made only to this modernized/refactored version. The version 5.0 has the same model physics as the version 4.5, but with a different code infrastructure. More details about the Noah-MP version 5.0 can be found in the model description paper (He et al., 2023b, in review) and the technical documentation (He et al. 2023a). Currently, the Noah-MP version 5.0 coupling with HRLDAS has been completed, but its coupling with other host models (e.g., WRF-Hydro, NASA/LIS, WRF, MPAS, UFS, etc.) is still on-going.


## Noah-MP technical documentation and model description papers

Technical documentation freely available at http://dx.doi.org/10.5065/ew8g-yr95

**To cite the technical documentation**:  He, C., P. Valayamkunnath, M. Barlage, F. Chen, D. Gochis, R. Cabell, T. Schneider, R. Rasmussen, G.-Y. Niu, Z.-L. Yang, D. Niyogi, and M. Ek (2023): The Community Noah-MP Land Surface Modeling System Technical Description Version 5.0, (No. NCAR/TN-575+STR). doi:10.5065/ew8g-yr95

**Original Noah-MP model description paper**:   Niu, G. Y., Yang, Z. L., Mitchell, K. E., Chen, F., Ek, M. B., Barlage, M., ... & Xia, Y. (2011). The community Noah land surface model with multiparameterization options (Noah‐MP): 1. Model description and evaluation with local‐scale measurements. Journal of Geophysical Research: Atmospheres, 116(D12).

**Noah-MP version 5.0 model description paper**:  He, C., Valayamkunnath, P., Barlage, M., Chen, F., Gochis, D., Cabell, R., Schneider, T., Rasmussen, R., Niu, G.-Y., Yang, Z.-L., Niyogi, D., and Ek, M.: Modernizing the open-source community Noah with multi-parameterization options (Noah-MP) land surface model (version 5.0) with enhanced modularity, interoperability, and applicability, Geosci. Model Dev., 16, 5131–5151, https://doi.org/10.5194/gmd-16-5131-2023, 2023.


## Noah-MP GitHub structure

**The folders**:

1. docs/: Noah-MP variable glossary and technical documentation;

2. drivers/: Noah-MP driver and interface code to connect to different host models (each host model will has its own subdirectory under this driver/);

3. parameters/: Noah-MP parameter table (note that the original 3 parameter tables have been merged into one NoahmpTable.TBL starting from version 5.0);

4. src/: Noah-MP source code modules;

5. utility/: Noah-MP utility code.

**The branches**:

1. "master" branch: (currently version 5.0), most stable & latest version, updated whenever there are bug fixes or major model update/release (by merging from the "develop" branch);

2. "develop" branch: (currently version 5.0), used for continuous NoahMP development, keep updated by including bug fixes and code updates (e.g., new physics options, processes, etc.); 

3. other version release branches: store different released code versions.


## Important notes

This GitHub repository only provides the Noah-MP source code and driver/interface code. To run Noah-MP in either offline or online mode, users need to have the host model system/framework coupled with Noah-MP. 

NCAR also maintains and releases the HRLDAS (High Resolution Land Data Assimilation System) coupled with Noah-MP to allow offline Noah-MP simulations. Please see the HRLDAS GitHub repository (https://github.com/NCAR/hrldas) for details. For users who are interested in other host models that couple with Noah-MP, please refer to those host model GitHub repositories. 

For users who are interested in previous Noah-MP code versions (prior to version 5.0), please refer to the different GitHub branches in this repository. Particularly, the "release-v4.5-WRF" branch has the same model physics as the Noah-MP version 5.0, but with an old model code structures, which is consistent with the Noah-MP code released along with WRF version 4.5.


## Code contribution via GitHub

Users are welcome to make code development and contributions through GitHub pull requests. The pull request will be reviewed by the Noah-MP model physics and code release team, and if everything looks good, the pull request of new code development or bug fixes will be merged into the develop branch. During each year's major version release period, the updated develop branch will be further merged into the master branch for official release of a new Noah-MP model version.

Some suggestions for model developers to contribute to Noah-MP code through the GitHub repository (typical procedures):

1. Step (1) Create a fork of this official Noah-MP repository to your own GitHub account; 

2. Step (2) Create a new branch based on the latest "develop" branch and make code updates/changes in the forked repository under your own account; 

3. Step (3) Finalize and test the code updates you make; 

4. Step (4) Submit a pull request for your code updates from your own forked Github repository to the "develop" branch of this official Noah-MP repository;

5. Step (5) The Noah-MP physics and code review committee reviews and tests the model updates in the submitted pull request and discusses with the developer if there is any problem; 

6. Step (6) The Noah-MP physics and code review committee confirms the pull request and merges the updated code to the "develop" branch in this official Noah-MP repository;

7. Step (7) The Noah-MP physics and code review committee merges the updated "develop" branch to the master branch during the annual release of new model versions.


## License

The license and terms of use for this software can be found [here](https://github.com/NCAR/noahmp/blob/develop/LICENSE.txt)

