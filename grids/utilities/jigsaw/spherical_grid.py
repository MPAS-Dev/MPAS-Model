#!/usr/bin/env python
#
#  Basic script to creat spherical grids for MPAS-Atmosphere
#  by Pedro S. Peixoto Dec 2021 <ppeixoto@usp.br>
#
#  Based on 
#  http://mpas-dev.github.io/MPAS-Tools/stable/mesh_creation.html#spherical-meshes
#  and Jigsaw scripts: https://github.com/dengwirda/jigsaw-python/tree/master/tests
#  
# Pre-requisites:
# 0) Install the conda enviroment MPAS-Tools
# 1) Get the requirements https://github.com/pedrospeixoto/MPAS-Tools/blob/master/conda_package/dev-spec.txt
# 2) Create enviroment
#     $ conda config --add channels conda-forge
#     $ conda create --name mpas-tools --file dev-spec.txt 
# 3) Install the mpas-tools pack
#     $ conda install mpas_tools
# 4) Use it with $ conda activate mpas-tools
#
# - To install jigsaw use conda https://github.com/dengwirda/jigsaw-geo-python/ )
 
import numpy as np
import argparse
import os


#
import xarray

import jigsawpy as jig
import jigsaw_util as jutil

from mpas_tools.mesh.creation.jigsaw_to_netcdf import jigsaw_to_netcdf
from mpas_tools.mesh.conversion import convert
from mpas_tools.io import write_netcdf

#import mpas_tools
#print(mpas_tools.__file__) 

###TO DO:
#
# - Add a tetris case
# - Variable resolution function - set ID number for each case
# - Read mesh xyz from file and convert to mpas grid (after possible scvt optimization)
#
#


def main(args):

    earth_radius=6371.0e3
    
    out_dir=os.path.abspath(args.output)
    out_base=os.path.basename(args.output)
    out_basepath=out_dir+"/"+out_base
    out_filename=out_dir+"/"+out_base+"_mpas.nc"
    
    if not os.path.exists(out_dir):
        os.makedirs(out_dir)
    else:
        print("Base dir already exists: ", out_dir)

    if(args.opt=="unif" or args.opt=="localref"):

        #Density based grid
        if (args.opt=="unif"):
            cellWidth, lon, lat = jutil.cellWidthVsLatLon(args.r)
        elif (args.opt=="localref"):
            cellWidth, lon, lat = jutil.localrefVsLatLon(args.r)

        mesh_file = jutil.jigsaw_gen_sph_grid(cellWidth, lon, lat, basename=out_basepath) 

    elif(args.opt=="icos"):

        #Icosahedral grid
        mesh_file = jutil.jigsaw_gen_icos_grid(basename=out_basepath, level=4)
        
    else:
        print("Unknown option")
        exit(1)
    
    #Convert jigsaw mesh to netcdf
    jigsaw_to_netcdf(msh_filename=mesh_file,
                         output_name=out_basepath+'_triangles.nc', on_sphere=True,
                         sphere_radius=earth_radius)

    #convert to mpas grid specific format
    write_netcdf(
            convert(xarray.open_dataset(out_basepath+'_triangles.nc'), 
            dir=out_dir,
            graphInfoFileName=out_basepath+"_graph.info"),
            out_filename)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawTextHelpFormatter)

    
    parser.add_argument(
        "-r",
        "--resolution",
        dest="r",
        required=True,
        type=float,
        help="Resolution of grid (depends on the grid choice, see -g)",
        metavar="FLOAT")
    
    parser.add_argument(
        "-o",
        "--output",
        dest="output",
        default="grid",
        help="output basename for directory and files.",
        metavar="STR")

    parser.add_argument(
        "-g",
        "--grid",
        dest="opt",
        default="icos",
        type=str,
        help="""Grid option: \n 
        " unif": Uniform resolution spherical grid (hand tune cellWidthVsLatLon function) \n
                 Provide also a resolution with -r 30 (in km) \n
        " icos": Spherical icosahedral grid \n
                 Provide also a resolution with -r 6 (grid refinement level) \n
        " localref" : Variable resolution spherical grid (hand tune cellWidthVsLatLon function) \n
                 Provide also a resolution with -r 30 (in km) \n
            """,
        metavar="STR")

    
    args = parser.parse_args()
    main(args)