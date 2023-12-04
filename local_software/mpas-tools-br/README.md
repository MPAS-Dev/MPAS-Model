# mpas-tools-br

This is a [Singularity](https://docs.sylabs.io/guides/3.7/user-guide/index.html) definition file to build a Singularity Image Format (SIF) for use with the [MPAS-BR](https://github.com/pedrospeixoto/MPAS-BR) distribution. You can use Singularity to download a base Linux image that can be custom-tailored to install the software you need in the container (see the [SingularityRecipe](https://github.com/cfbastarz/SingularityRecipe) with and example). The instructions used in this README file use the provided definition file `mpas-tools-br.def` to build the container image for use with the MPAS-BR repository.

The key idea is to provide a base system as a common ground, where all users will find the tools needed to run the MPAS-BR software. By doing so, users can avoid common mistakes in system configuration.

An introduction to Singularity, its purpose, goals, and characteristics, are provided in [Singularity: Scientific containers for mobility of compute](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5426675/pdf/pone.0177459.pdf).

## Singularity and Apptainer

Recently, Singularity was renamed as Apptainer. Apptainer is the open source version of Singularity and as such, is maintaned as hosted by the [Linux Foundation](https://www.linuxfoundation.org/). Singularity and Apptainer are full compatible and share the same software infrastructure with different roadmaps.

Information on Apptainer, its principles and goals are provided in [https://apptainer.org/news/community-announcement-20211130/](https://apptainer.org/news/community-announcement-20211130/).

## Software available

The `mpas-tools-br` comes with the [MPAS-Tools](https://github.com/MPAS-Dev/MPAS-Tools) pre-installed with software like `jigsaw` (v0.9.14) and other tools needed to construct grids for the MPAS model. Take a look at the contents of the `mpas-tools-br.def` file to get a list of the software shipped within the container.

* Base system: Ubuntu Linux 20.04;
* Compilers: GNU Fortran, C, and C++ v11.4.0;
* Miniconda Python distribution;
* `mpas-tools` Python environment with MPAS-Tools [v0.27.0](https://github.com/MPAS-Dev/MPAS-Tools/releases/tag/0.27.0) installed among several other packages.

Instructions on how to build yourself a SIF image is provided in the section [Building the SIF image](#building-the-sif-image).

## Using the container to run the MPAS-BR

### Pulling the Container

In order to use the `mpas-tool-br` container, you will need to install Singularity. If you have Anaconda/Miniconda Python installed on Linux or Windows WSL, create an environment and install Singularity in it. If you want to run Singularity images within Mac OS X, you will need VirtualBox. We provide instructions at the end of this section.

**Note:** For this tutorial, we will use Singularity v3.7 (or greater) from the [Conda-Forge](https://conda-forge.org/) and, by the time of this writing, [Apptainer v1.2.5](https://github.com/apptainer/apptainer/releases/tag/v1.2.5) was already released. Feel free to choose which software version to use and make the changes accordingly (i.e., replace the `singularity` command by `apptainer` and environment variables with the prefix `SINGULARITY_` to `APPTAINER_`). You may find it easier to install Apptainer through you favourite software packaging system (e.g., `apt`, `rpm`). See the [Apptainer releases page](https://github.com/apptainer/apptainer/releases) for futher details.

First, create a Conda environment to install Singularity:

```bash
conda create -n singularity
conda activate singularity
conda install -c conda-forge singularity
```

Next, pull a copy of the `mpas-tool-br.sif` container (~2.8GB) from the Sylabs Cloud (you don't need to create an account):

```bash
singularity pull library://cfbasz/default/mpas-tools-br
```

**Optional:** The image is signed by the author and you can check the container fingerprint with the command:

```bash
singularity verify mpas-tools-br_latest.sif
```

The output must be:

```bash
Verifying image: mpas-tools-br_latest.sif
[LOCAL]   Signing entity: Carlos Frederico Bastarz (development key) <carlos.bastarz@inpe.br>
[LOCAL]   Fingerprint: 0DF6CEC4DBF78AD54EF541EB9C45F085F2287079
Objects verified:
ID  |GROUP   |LINK    |TYPE
------------------------------------------------
1   |1       |NONE    |Def.FILE
2   |1       |NONE    |JSON.Generic
3   |1       |NONE    |JSON.Generic
4   |1       |NONE    |FS
Container verified: mpas-tools-br_latest.sif
```

**Note:** Sometimes this checking can fail.

#### Installing Singularity in Mac OS X

In a nutshell, you'll need to install the `brew` package manager:

```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

Then, install `virtualbox`, `vagrant`, and `vagrant-manager`:

```bash
brew install --cask virtualbox
brew install --cask vagrant
brew install --cask vagrant-manager
```

At this point, you will have the software needed to run a virtualization of Linux. Bear in mind that the Windows Subsystem for Linux (WSL) is something similar, meaning that running Linux in Windows and Mac OS is just a virtualization. But in these cases, Singularity needs to run inside a virtualized Linux, and if you want to run the `mpas-tools-br` container, you will be adding an extra layer of virtualization. But it works.

Next, bootstrap a Linux base system where Singularity will be installed:

```bash
vagrant init sylabs/singularity-3.7-ubuntu-bionic64
```

The previous command will create a `Vagrantfile` with the definitions of your virtual machine (you can further customize to use more cores and RAM from the host; by default, it will use just 1 core and 1GB of RAM). Take a look at a customized `Vagrantfile` inside this repo to use more cores and RAM.

To initialize the virtual machine, use the command:

```bash
vagrant up
```

And to log in, use:

```bash
vagrant ssh
```

**Note:** If you want to open another terminal session to use Singularity (within the virtual machine), just issue the command `vagrant ssh` at the same place the `Vagrantfile` is. Once all work is done, just shut down the virtual machine with the command `vagrant halt`. If you want to start over, just do `vagrant up` and `vagrant ssh`. This is the way to use VirtualBox without the graphical interface.

## Running the MPAS-BR inside the container

In this section, it shows how to use the `mpas-tools-br_latest.sif` container to run the MPAS-BR software.

First, open a shell environment from the container:

```bash
singularity shell -e --env DISPLAY=$DISPLAY --bind $HOME:$HOME mpas-tools-br_latest.sif
```

**Notes:**

* The option `-e` will keep the previous exported shell variables;
* The option `--env DISPLAY=$DISPLAY` will allow using the display;
* The option `--bind $HOME:$HOME` will bind your home directory to the container home directory. If you have data on other disks in your computer, you can bind it as well, just add another bind like so: `--bind $HOME:$HOME --bind /extra:/data`. In this case, the `/extra` from the host machine will be mounted in the `/data` in the container.

You will notice that the prompt will change to `Singularity>`.

Next, source the Miniconda environment variables to activate the `mpas-tools` environment with `conda`:

```bash
source /opt/miniconda/etc/profile.d/conda.sh

conda activate mpas-tools
```

**Optional:** If you want, you can set some aliases to help you get around the container shell:

```bash
alias ls='ls --color'
alias la='ls -ltr --color'
```

To run the MPAS-BR software, choose a path and clone the repository. Here we'll use the `$HOME` directory:

```bash
cd $HOME

git clone https://github.com/pedrospeixoto/MPAS-BR.git
```

At this point, you already have all the software needed to run the MPAS-BR. By following Pedro Peixoto's classes, proceed as follows to compile, run, and plot the grid and some initial fields. For you convenience, follow the commands:

1. Compile the MPAS-BR code:

    ```bash
    cd $HOME/MPAS-BR

    make gfortran CORE=init_atmosphere PNETCDF=/usr
    make gfortran CORE=atmosphere AUTOCLEAN=true PNETCDF=/usr
    ```

2. Prepare the MPAS grid:

    ```bash
    cd $HOME/MPAS-BR/grids/utilities/jigsaw

    python3 spherical_grid.py -g unif -o unif240km -r 240 -l 240
    ```

3. Plot the grid:

    ```bash
    cd $HOME/MPAS-BR/post_proc/py/grid_maps

    python3 mpas_plot_grid.py -g ../../../grids/utilities/jigsaw/unif240km/unif240km_mpas.nc -o ../../../grids/utilities/jigsaw/unif240km/unif240km_mpas.jpg

    display $HOME/MPAS-BR/grids/utilities/jigsaw/unif240km/unif240km_mpas.jpg
    ```

4. Test the `metis` to create a grid partition

    ```bash
    cd $HOME/MPAS-BR/grids/utilities/jigsaw/unif240km

    gpmetis -minconn -contig -niter=200 unif240km_graph.info 4
    ```

5. Prepare the initial fields:

    ```bash
    cd $HOME/MPAS-BR/benchmarks/monan-class-example/jw_baroclinic_wave

    ln -s $HOME/MPAS-BR/init_atmosphere_model .

    ./init_atmosphere_model
    ```

6. Plot the initial fields and some grid properties:

    **Note:** once the figure opens up and if it has several levels, hit the spacebar to loop through levels.

    ```bash
    cd $HOME/MPAS-BR/benchmarks/monan-class-example/jw_baroclinic_wave

    python3 ../../../post_proc/py/scalar_lat_lon_2d_plot/mpas_plot_scalar.py -v theta x1.40962.init.nc

    display figures/theta/theta_0_*

    python3 ../../../post_proc/py/plot_scalar_on_native_grid/mpas_plot.py -f x1.40962.init.nc -o x1.40962.theta.jpg -v theta -l 0 -t 0

    display x1.40962.theta.jpg

    python3 ../../../post_proc/py/plot_scalar_on_native_grid/mpas_plot.py -f x1.40962.init.nc -v areaCell -o areaCell.jpg

    display areaCell.jpg

    python3 ../../../post_proc/py/plot_scalar_on_native_grid/mpas_plot.py -f x1.40962.init.nc -v areaTriangle -o areaTriangle.jpg

    display areaTriangle.jpg

    cd $HOME/MPAS-BR/grids/utilities/jigsaw/unif240km

    python3 ../../../../post_proc/py/plot_scalar_on_native_grid/mpas_plot.py -f unif240km_mpas.nc -v triangleQuality -o triangleQuality.jpg

    display triangleQuality.jpg
    ```

7. Prepare the initial fields using `mpirun`:

    **Note:** In step 5, we already created the initial fields for MPAS, but this time we will do it again by using the grid partition prepared in step 4 using the `metis` software.

    ```bash
    cd $HOME/MPAS-BR/benchmarks/monan-class-example/jw_baroclinic_wave

    mpirun -n 4 ./init_atmosphere_model
    ```

8. Check the created MPAS input file:

    ```bash
    ncdump -h x1.40962.init.nc
    ```

9. Check the model log for errors:

    You should thoroughly check the file! As a shortcut, check the number of `Error messages` (must be 0) at the end of the file:

    ```bash
    cat log.init_atmosphere.0000.out | grep 'Error messages ='
    ```

    **Note:** Since we already run the `init_atmosphere_model` executable, the file `x1.40962.init.nc` is already on disk and MPAS will throw an error message:

    ```bash
    ERROR: Writing to stream 'output' would clobber file 'x1.40962.init.nc'
    ERROR:     but clobber_mode is set to 'never_modify'.
    ```

    To avoid this, you can rename the file: `mv x1.40962.init.nc x1.40962.init.nc.bak` and run the model again to create the initial conditions using the partitioned grid.

10. JW Baroclinic Instability simulation:

    Prepare the initial conditions. Change the namelists to use the `unif240km_mpas.nc` and `unif240km_jw-bi.init.nc` files as input and output streams, respectivelly:

    * In `namelist.init_atmosphere` file, change the option `config_block_decomp_file_prefix` from `x1.40962.graph.info.part.` to `unif240km_graph.info.part.`;
    * In `streams.init_atmosphere` file:
        * In the `immutable_stream name="input"` section, change the option `filename_template` from `x1.40962.grid.nc` to `unif240km_mpas.nc`;
        * In the `immutable_stream name="output"` section, change the option `filename_template` from `x1.40962.init.nc` to `unif240km_jw-bi.init.nc`. 

    Link the `unif240km_mpas.nc` and `unif240km_graph.info.part.4` files from the `jigsaw/unif240km` directory:

    ```bash
    cd $HOME/MPAS-BR/benchmarks/monan-class-example/jw_baroclinic_wave
    
    ln -s $HOME/MPAS-BR/grids/utilities/jigsaw/unif240km/unif240km_mpas.nc .
    ln -s $HOME/MPAS-BR/grids/utilities/jigsaw/unif240km/unif240km_graph.info.part.4 .
    ```

    Run the `init_atmosphere_model` executable to prepare the initial conditions:
    
    ```bash
    mpirun -n 4 ./init_atmosphere_model
    ```

    Check the results:

    * Check for errors in the `log.init_atmosphere.0000.out` file: `cat log.init_atmosphere.0000.out | grep 'Error messages ='` (it should return `Error messages =                     0`).
    * The file `unif240km_jw-bi.init.nc` must be created upon the `init_atmosphere_model` completion.

    Link the `atmosphere_model` executable:

    ```bash
    cd $HOME/MPAS-BR/benchmarks/monan-class-example/jw_baroclinic_wave

    ln -s ../../../atmosphere_model .
    ```

    Modify the MPAS namelists:

    * Make changes to `namelist.atmosphere`, `streams.atmosphere` and `stream_list.atmosphere.output` namelists (for reference, take a look in the namelists folder inside this repo).

    Run the atmosphere model:

    ```bash 
    mpirun -n 4 ./atmosphere_model
    ```

    Upon `atmosphere_model` completion, there will be created the following files:

    * Restart files:
        * `restart.0000-01-06_00.00.00.nc`;
        * `restart.0000-01-11_00.00.00.nc`;
        * `restart.0000-01-16_00.00.00.nc`;
        * `restart_timestamp`;
    * Output file: `output_unif240km_jw-bi.nc`;
    * Output log file: `log.atmosphere.0000.out` (it must output `Error messages =                     0` at the end).

    Plot some of the output fields (for level 0 and timestep 12):

    ```bash
    cd $HOME/MPAS-BR/benchmarks/monan-class-example/jw_baroclinic_wave

    python3 ../../../post_proc/py/plot_scalar_on_native_grid/mpas_plot.py -v vorticity -f output_unif240km_jw-bi.nc -l 0 -t 12 -o unif240km_jw-bi-vorticity-l0_t12.jpg

    display unif240km_jw-bi-vorticity-l0_t12.jpg

    python3 ../../../post_proc/py/plot_scalar_on_native_grid/mpas_plot.py -v uReconstructZonal -f output_unif240km_jw-bi.nc -l 0 -t 12 -o unif240km_jw-bi-uReconstructZonal-l0_t12.jpg

    display unif240km_jw-bi-uReconstructZonal-l0_t12.jpg

    python3 ../../../post_proc/py/plot_scalar_on_native_grid/mpas_plot.py -v uReconstructMeridional -f output_unif240km_jw-bi.nc -l 0 -t 12 -o unif240km_jw-bi-uReconstructMeridional-l0_t12.jpg

    display unif240km_jw-bi-uReconstructMeridional-l0_t12.jpg

    python3 ../../../post_proc/py/plot_scalar_on_native_grid/mpas_plot.py -v pressure -f output_unif240km_jw-bi.nc -l 0 -t 12 -o unif240km_jw-bi-pressure-l0_t12.jpg

    display unif240km_jw-bi-pressure-l0_t12.jpg
    ```

# Building the SIF image

If you want to customize the `mpas-tools-br.def` bootstrap file to build your own container, make the desired modifications, and follow the provided instructions. You may also want to tweak the Python `environment.yml` file (used during the container build process) to, e.g., change the MPAS-Tools version.

To build the Singularity container using the bootstrap file, first, you need to install Singularity. If you have Anaconda/Miniconda Python installed on a Linux or Windows WSL machine, create an environment and install Singularity in it:

```bash
conda create -n singularity
conda activate singularity
conda install -c conda-forge singularity
```

**Note:** See the section [Installing Singularity in Mac OS X](#installing-singularity-in-mac-os-x) on how to install singularity in Mac OS X.

Next, create `cache` and `tmp` directories and instruct Singularity to use them during the building process. This might avoid potential problems memory usage and with the image size:

```bash
mkdir -p $HOME/cache
mkdir -p $HOME/tmp

export SINGULARITY_DISABLE_CACHE=false
export SINGULARITY_CACHEDIR=$HOME/cache
export SINGULARITY_TMPDIR=$HOME/tmp
```

Next, build the SIF image as follows:

```bash
singularity build --fakeroot mpas-tools-br.sif mpas-tools-br.def
```

**Note:** The build process might take >30min depending on your machine.

carlos.bastarz@inpe.br (04/12/2023)
