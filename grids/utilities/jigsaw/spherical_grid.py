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
    p = bool(args.plots)
    print(p)
    if not os.path.exists(out_dir):
        os.makedirs(out_dir)
    else:
        print("Base dir already exists: ", out_dir)

    if(args.opt=="unif" or args.opt=="localref"):

        #Density based grid
        if (args.opt=="unif"):
            cellWidth, lon, lat = jutil.cellWidthVsLatLon(args.r)
        elif (args.opt=="localref"):
            cellWidth, lon, lat = jutil.localrefVsLatLon(args.r, l=args.l,
                        radius_high=args.rad, transition_radius=args.tr, 
                        clon = args.clon, clat=args.clat, p=p)

        mesh_file = jutil.jigsaw_gen_sph_grid(cellWidth, lon, lat, basename=out_basepath) 

    elif(args.opt=="icos"):
        
        if int(args.l) > 11:
            print("Please provide a reasonable refinment level - from 1 to 15. Current value too large ", int(args.l))
            print(" Setting level to 4")
            args.l = 4
            
        #Icosahedral grid
        mesh_file = jutil.jigsaw_gen_icos_grid(basename=out_basepath, level=int(args.l))
        
    else:
        print("Unknown option")
        exit(1)
    
    #Convert jigsaw mesh to netcdf
    jigsaw_to_netcdf(msh_filename=mesh_file,
                         output_name=out_basepath+'_triangles.nc', on_sphere=True,
                         sphere_radius=1.0)

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
        "-r",  "--high", dest="r",
        required=False, default=30, type=float,
        help="Grid spacing for high resolution area - depends on the grid \
choice, see -g (default: 30 km)",
        metavar="FLOAT")
    
    parser.add_argument(
        "-l", "--low", dest="l", 
        required=False, default=150, type=float,
        help="Level of refinement on icosahedral grid, or global grid spacing \
            (resolution area) for localref\
            grid option, see -g (default: 150 km)",
        metavar="FLOAT")
    
    parser.add_argument(
        "-rad", "--radius", dest="rad", 
        required=False, default=50, type=float,
        help="radius of influence of high resolution area in km - only valid \
for localref grid option, see -g (default: 50 km)",
        metavar="FLOAT")

    parser.add_argument(
        "-tr",  "--transitionradius", dest="tr",
        required=False, default=600, type=float,
        help="radius of transition zone between high and low resolution in km \
- only valid for local ref grid option, see -g (default: 600 km)",
        metavar="FLOAT")
    
    parser.add_argument(
        "-clat",  "--center_latitude", dest="clat",
        required=False, default=0.0, type=float,
        help="Latitude (in degrees) of centre point of refinement \
- only valid for local ref grid option, see -g (default: 0 deg)",
        metavar="FLOAT")
    
    parser.add_argument(
        "-clon",  "--center_longitude", dest="clon",
        required=False, default=0.0, type=float,
        help="Longitude (in degrees) of centre point of refinement \
- only valid for local ref grid option, see -g (default: 0 deg)",
        metavar="FLOAT")
    
    parser.add_argument(
        "-o", "--output", dest="output",
        required=False, default="grid", type=str,
        help="output basename for directory and files.",
        metavar="STR")

    parser.add_argument(
        "-p", "--plot", dest="plots",
        required=False, default=0, type=int,
        help="do plots of grid resolutions (0 for no plots, any other value \
for creating plots)",
        metavar="INT")
    
    parser.add_argument(
        "-g", "--grid", dest="opt",
        required=True, default="icos", type=str,
        help="""Grid option: \n 
        "unif": Uniform resolution spherical grid (hand tune \
cellWidthVsLatLon function) \n
        "icos": Spherical icosahedral grid \n
        "localref" : Variable resolution spherical grid (hand tune \
cellWidthVsLatLon function) \n
            """,
        metavar="STR")

    
    args = parser.parse_args()
    
    main(args)