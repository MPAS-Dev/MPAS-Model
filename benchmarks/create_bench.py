#-------------------------------------------------
#  Script to generate namelists and stream for MPAS
#  Author: P. Peixoto <ppeixoto@usp.br>
#  Last update: Mar 2022
# --------------------------------------------------
import f90nml
import os
import argparse
import subprocess
import sys

import mpas_benchmarks as bench

# Get args: init or run core
args = bench.call_parser()
work_dir = os.getenv('MPAS_DIR')

#Define grid
grid_name = "x1.10242"
grid_dir = work_dir+"/grids/grids/"+grid_name

#Init_atmosphere setup
if args.init:
    b_init = bench.Bench(args)
    

    nml_options = {"nhyd_model":{}, "dimensions": {}, "decomposition":{} }
    stream_options = {"input":{}, "output":{}, "surface" : {}}

    nml_options["nhyd_model"]["config_init_case"] = 1
    nml_options["nhyd_model"]["config_hcm_staggering"] = False
    nml_options["dimensions"]["config_nvertlevels"] = 20
    nml_options["decomposition"]["config_block_decomp_file_prefix"] = grid_dir+"/"+grid_name+".graph.info.part."

    b_name = grid_name + ".tc_"+str(nml_options["nhyd_model"]["config_init_case"]) \
        + ".hcm_"+str(nml_options["dimensions"]["config_nvertlevels"]) \
        + ".lv_"+str(nml_options["dimensions"]["config_nvertlevels"])

    b_dir = b_init.bench_dir+"/"+b_name
    print("New bench dir:", b_dir)

    stream_options["input"]["filename_template"] = grid_dir+"/"+grid_name+".grid.nc"
    stream_options["output"]["filename_template"] = b_dir+"/"+b_name+".init.nc"
    stream_options["surface"]["filename_template"] = b_dir+"/"+b_name+".sfc_update.nc"
    stream_options["output"]["clobber_mode"] = "overwrite"

    b_init.set_options(nml_options, stream_options, b_dir)

