# mpas-tools-br

This is a [Singularity](https://docs.sylabs.io/guides/3.7/user-guide/index.html) definition file to build a Singularity Image Format (SIF) for use with the [MPAS-BR](https://github.com/pedrospeixoto/MPAS-BR) distribution. You can use Singularity to download a base Linux image that can be custom-tailored to install the software you need in the container (see the [SingularityRecipe](https://github.com/cfbastarz/SingularityRecipe) with and example). The instructions used in this README file use the provided definition file [`mpas-tools-br.def`](local_software/mpas-tools-br/mpas-tools-br.def) to build the container image for use with the MPAS-BR repository.

The key idea is to provide a base system as a common ground, where all users will find the tools needed to run the MPAS-BR software. By doing so, users can avoid common mistakes in system configuration.

An introduction to Singularity, its purpose, goals, and characteristics, is provided in [Singularity: Scientific containers for mobility of compute](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5426675/pdf/pone.0177459.pdf).

## Software available

The `mpas-tools-br` comes with the [MPAS-Tools](https://github.com/MPAS-Dev/MPAS-Tools) pre-installed with software like `jigsaw` and other tools needed to construct grids for the MPAS model. Take a look at the contents of the `mpas-tools-br.def` file to get a list of the software shipped within the container.

* Base system: Ubuntu Linux 20.04;
* Compilers: GNU Fortran, C, and C++ version 11;
* Miniconda Python distribution;
* `mpas-tools` Python environment.

## Building the SIF image

If you want to customize the `mpas-tools-br.def` bootstrap file to build your container, make the desired modifications, and follow the provided instructions. If you just want to use the container, skip to the next section ([Using the container to run the MPAS-BR](#using-the-container-to-run-the-mpas-br)).

To build the Singularity container using the bootstrap file, first, you need to install Singularity. If you have Anaconda/Miniconda Python installed on a Linux or Windows WSL machine, create an environment and install Singularity in it:

```bash
conda create -n singularity
conda activate singularity
conda install -c conda-forge singularity
```

**Note:** See the end of the next section on how to install singularity in Mac OS X.

Next, create `cache` and `tmp` directories and instruct Singularity to use them during the building process. This might avoid potential problems with the image size:

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

## Using the container to run the MPAS-BR

### Pulling the Container

As shown in the previous section, to create and run the container, you need to install Singularity. Again, if you have Anaconda/Miniconda Python installed on Linux or Windows WSL, create an environment and install Singularity in it. If you want to run Singularity images within Mac OS X, you will need VirtualBox, and we will leave the instructions to do so at the end of this section.

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

The previous command will create a `Vagrantfile` with the definitions of your virtual machine (you can further customize to use more cores and RAM from the host; by default, it will use just 1 core and 1GB of RAM).

To initialize the virtual machine, use the command:

```bash
vagrant up
```

And to log in, use:

```bash
vagrant ssh
```

**Note:** If you want to open another terminal session to use Singularity (within the virtual machine), just issue the command `vagrant ssh` at the same place the `Vagrantfile` is. Once all work is done, just shut down the virtual machine with the command `vagrant halt`. If you want to start over, just do `vagrant up` and `vagrant ssh`. This is the way to use VirtualBox without the graphical interface.

### Running the MPAS-BR inside the container

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

4. Prepare the initial fields:

    ```bash
    cd $HOME/MPAS-BR/benchmarks/monan-class-example/jw_baroclinic_wave

    ln -s $HOME/MPAS-BR/init_atmosphere_model .
    ./init_atmosphere_model
    ```

5. Plot the initial fields and some grid properties:

    ```bash
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

carlos.bastarz@inpe.br (09/11/2023)
