1) Starting at the project's root, make sure no .mod and .o files exist (that may have been compiled previously by wrong compiler versions):
find . -name \*.o -type f -delete
find . -name \*.mod -type f -delete
2) Install compilers with correct version:
sudo apt-get install g++-8
sudo apt-get install gfortran-8
3) In local_software, execute iolib_installation.sh:
cd local_software
source iolib_installation.sh: 
4) Back to the root, execute setup_env.sh: 
cd ..
source local_software/setup_env.sh
5) Compile MPAS:
make gfortran CORE=init_atmosphere OPENMP=true USE_PIO2=true AUTOCLEAN=true
make gfortran CORE=atmosphere OPENMP=true USE_PIO2=true AUTOCLEAN=true
