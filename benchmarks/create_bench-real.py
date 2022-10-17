#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#-------------------------------------------------
#  Script to generate namelists and stream for MPAS
#  Author: Danilo Couto de Souza <danilo.oceano@gmail.com>
#  Last update: Oct 2022
#  Adapted from: P. Peixoto <ppeixoto@usp.br>
# --------------------------------------------------
"""
Created on Tue Oct 11 14:25:30 2022

    This script will run the MPAS-A for testing distinct model configurations.
It is required that the terrestrial static fields, surface fields and the
meteorological variables pre-processing were performed previously. When using
this scrtips, firstly make sure that MPAS_DIR in on path should direct towards 
the path to MPAS-BR directory) and then check if folder ./inputs contain all 
required files for running the cases:
    1. namelists and streams files: contains model configuration options
    2. <grid_name>.grid.nc: computational mesh;
    3. <grid_name>.graph.info.part.<n>: partition file for runing the model in 
    paralel mode using n cores

@author: daniloceano
"""

import f90nml
import os
import argparse
import subprocess
import sys
import shutil
import itertools
import datetime

import mpas_benchmarks_RealCase as bench


# Get args: init or run core
args = bench.call_parser()

#Workspace
work_dir = os.getenv('MPAS_DIR')
b_name = args.name
b_main_dir = work_dir+"/benchmarks/"+b_name

# =============================================================================
# ## DEFINE PARAMETERS FOR NAMELIST.INIT ##
# define grid
grid_name = "x1.10242"
grid_dir = work_dir+"/grids/grids/"
# define dates in format: 'YYYY-MM-DD_hh:mm:ss'
init_date = '1992-06-05_09:00:00'
run_duration = '9_16:00'
n_vert_levels = 70
# path to geographical data
geog_data_path = '/p1-nemo/danilocs/mpas/mpas_tutorial/geog/'
# prefix used to generate intermediatie files with ungrib.exe
met_prefix = 'ERA5'
sfc_prefix = 'SST'
# Interval of the SST files
sfc_interval = 3600
# highest model level
ztop = 30000.0

# =============================================================================
## LOOP OVER DESIRED OPTIONS ##
# difussion length coefs
loop_parameter1 = [0.,120000.0]
loop_parameter1_name = "len_disp"
# smag coefficients
loop_parameter2 = [0.00, 0.05]
loop_parameter2_name = "visc4_2dsmag"

# Set true the option refering to the stage of model run where the parameters
# are set
par_in_static = False
par_in_met = False
par_in_sfc = False
par_in_run = False
# =============================================================================


def static_interp(par1,par2):
    
    nml_init_opts = {"nhyd_model":{}, "dimensions": {}, 
                     "data_sources":{}, "preproc_stages": {}}
    # Real-data initialization case
    nml_init_opts["nhyd_model"]["config_init_case"] = 7
    # Should be set to 1 for this step (see doc)  
    nml_init_opts["dimensions"]["config_nvertlevels"] = 1
    nml_init_opts["dimensions"]["config_nvertlevels"] = 1
    nml_init_opts["dimensions"]["config_nsoillevels"] = 1
    nml_init_opts["dimensions"]["config_nfglevels"] = 1
    nml_init_opts["dimensions"]["config_nfgsoillevels"] = 1
    ## Be careful to path to files for land files!! ##
    nml_init_opts["data_sources"]["config_geog_data_path"] = \
        '/p1-nemo/danilocs/mpas/mpas_tutorial/geog/'
    # Enable and disable steps of pre-processing fields
    nml_init_opts["preproc_stages"]["config_static_interp"] = True
    nml_init_opts["preproc_stages"]["config_native_gwd_static"] = True
    nml_init_opts["preproc_stages"]["config_vertical_grid"] = False
    nml_init_opts["preproc_stages"]["config_met_interp"] = False
    nml_init_opts["preproc_stages"]["config_input_sst"] = False
    nml_init_opts["preproc_stages"]["config_frac_seaice"] = False

    b_name = grid_name+"."+loop_parameter1_name+"."+str(par1)+"."+\
        loop_parameter2_name+"."+str(par2)

    b_dir = b_main_dir+"/"+b_name

    str_init_opt = {"input":{}, "output":{}}

    str_init_opt["input"]["filename_template"] = grid_dir+"/"+grid_name+".grid.nc"
    str_init_opt["output"]["filename_template"] = b_dir+"/init/"+b_name+".static.nc"
    str_init_opt["output"]["clobber_mode"] = "overwrite"

    return nml_init_opts, b_dir, str_init_opt

def init_interp(par1,par2):
    
    nml_init_opts = {"nhyd_model":{}, "dimensions": {}, 
                     "data_sources":{}, "preproc_stages": {},
                     "decomposition":{}}
    # Real-data initialization case
    nml_init_opts["nhyd_model"]["config_init_case"] = 7
    nml_init_opts["nhyd_model"]["config_start_time"] = init_date
    # Now, set the vertical levels correctly
    nml_init_opts["dimensions"]["config_nvertlevels"] = n_vert_levels
    nml_init_opts["dimensions"]["config_nsoillevels"] = 4
    nml_init_opts["dimensions"]["config_nfglevels"] = 38
    nml_init_opts["dimensions"]["config_nfgsoillevels"] = 4
    ## Be careful to path to files for land files!! ##
    nml_init_opts["data_sources"]["config_met_prefix"] = \
        work_dir+"/input_data/"+met_prefix
    # Enable and disable steps of pre-processing fields
    nml_init_opts["preproc_stages"]["config_static_interp"] = False
    nml_init_opts["preproc_stages"]["config_native_gwd_static"] = False
    nml_init_opts["preproc_stages"]["config_vertical_grid"] = True
    nml_init_opts["preproc_stages"]["config_met_interp"] = True
    nml_init_opts["preproc_stages"]["config_input_sst"] = False
    nml_init_opts["preproc_stages"]["config_frac_seaice"] = True
    # Decomposition file for running in parallel
    nml_init_opts["decomposition"]["config_block_decomp_file_prefix"] = \
        grid_dir+grid_name+'.graph.info.part.'

    b_name = grid_name+"."+loop_parameter1_name+"."+str(par1)+"."+\
        loop_parameter2_name+"."+str(par2)

    b_dir = b_main_dir+"/"+b_name
    
    

    str_init_opt = {"input":{}, "output":{}}

    str_init_opt["input"]["filename_template"] = b_dir+"/init/"+b_name+".static.nc"
    str_init_opt["output"]["filename_template"] = b_dir+"/init/"+b_name+".init.nc"
    str_init_opt["output"]["clobber_mode"] = "overwrite"

    return nml_init_opts, b_dir, str_init_opt

def sfc_update(par1,par2):
    
    start = datetime.datetime.strptime(init_date, '%Y-%m-%d_%H:%M:%S')
    dt = datetime.datetime.strptime(run_duration,'%d_%H:%M')
    end = start + datetime.timedelta(days=dt.day,hours=dt.hour)
    finish_date = end.strftime('%Y-%m-%d_%H:%M:%S')
    
    nml_init_opts = {"nhyd_model":{}, "dimensions": {}, 
                     "data_sources":{}, "preproc_stages": {}}
    # Real-data initialization case
    nml_init_opts["nhyd_model"]["config_init_case"] = 8
    nml_init_opts["nhyd_model"]["config_start_time"] = init_date
    nml_init_opts["nhyd_model"]["config_start_time"] = finish_date
    ## Be careful with the interval for updating the sfc conditions
    nml_init_opts["data_sources"]["config_sfc_prefix"] = \
        work_dir+"/input_data/"+sfc_prefix
    nml_init_opts["data_sources"]["config_fg_interval "] = sfc_interval
    # Enable and disable steps of pre-processing fields
    nml_init_opts["preproc_stages"]["config_static_interp"] = False
    nml_init_opts["preproc_stages"]["config_native_gwd_static"] = False
    nml_init_opts["preproc_stages"]["config_vertical_grid"] = False
    nml_init_opts["preproc_stages"]["config_met_interp"] = False
    nml_init_opts["preproc_stages"]["config_input_sst"] = True
    nml_init_opts["preproc_stages"]["config_frac_seaice"] = True
    # Decomposition file for running in parallel
    nml_init_opts["decomposition"]["config_block_decomp_file_prefix"] = \
        grid_dir+grid_name+'.graph.info.part.'

    b_name = grid_name+"."+loop_parameter1_name+"."+str(par1)+"."+\
        loop_parameter2_name+"."+str(par2)

    b_dir = b_main_dir+"/"+b_name

    str_init_opt = {"surface":{}}

    str_init_opt["surface"]["filename_template"] = \
        b_dir+"/init/"+b_name+".sfc_update.nc"
    str_init_opt["output"]["filename_interval"] = sfc_interval
    
    return nml_init_opts, b_dir, str_init_opt

for par1, par2 in itertools.product(loop_parameter1, loop_parameter2):

    # #Runtime options

    # nml_opts = {"nhyd_model":{}, "damping": {}, "decomposition":{}, "physics":{}  }

    # nml_opts["nhyd_model"]["config_time_integration_order"] = 2
    # nml_opts["nhyd_model"]["config_dt"] = 120
    # nml_opts["nhyd_model"]["config_run_duration"] = '1_00:00:00'
    # nml_opts["nhyd_model"]["config_horiz_mixing"] = '2d_smagorinsky'
    # nml_opts["nhyd_model"]["config_len_disp"] = par1 #1200000.
    # nml_opts["nhyd_model"]["config_visc4_2dsmag"] = par2 #0.05
    # nml_opts["nhyd_model"]["config_smdiv"] = 0.1
    # nml_opts["decomposition"]["config_block_decomp_file_prefix"] = grid_dir+"/"+grid_name+".graph.info.part."
    # nml_opts["physics"]["config_physics_suite"] = 'none'

    # b_full_name = b_dir+"/run."+"smag_"+str(nml_opts["nhyd_model"]["config_len_disp"])+ \
    #     ".visc4smag_"+str(nml_opts["nhyd_model"]["config_visc4_2dsmag"])

    # str_opt = {"input":{}, "output":{}, "restart" : {}, "diagnostics" : {}, "surface" : {}}

    # str_opt["input"]["filename_template"] = str_init_opt["output"]["filename_template"]
    # str_opt["output"]["filename_template"] = b_full_name+"/out.nc"
    # str_opt["output"]["output_interval"] = "1:00:00"
    # str_opt["output"]["clobber_mode"] = "overwrite"
    # str_opt["surface"]["filename_template"] = str_init_opt["surface"]["filename_template"]
    # str_opt["diagnostics"]["filename_template"] = b_full_name+"/diag.nc"
    # str_opt["diagnostics"]["output_interval"] = "1:00:00"
    # str_opt["diagnostics"]["clobber_mode"] = "overwrite"

    # Setup for creating static fields
    if args.static:
        opts = static_interp(par1,par2)
        nml_init_opts, b_dir, str_init_opt = opts[0], opts[1], opts[2]
        b_init = bench.Bench(args, dummy_string="Init")
        b_init.set_options(nml_init_opts, str_init_opt, b_dir+"/init")
        print("Benchmark dir:", b_dir)
        if(not par_in_static):
            break
     
    # Setup for vertical grid generation and initial field interpolation
    # Make sure the static file exists!
    if args.init:
        opts = init_interp(par1,par2)
        nml_init_opts, b_dir, str_init_opt = opts[0], opts[1], opts[2]
        b_init = bench.Bench(args, dummy_string="Init")
        b_init.set_options(nml_init_opts, str_init_opt, b_dir+"/init")
        print("Benchmark dir:", b_dir)
        if(not par_in_static):
            break
        
    # Setup for generating periodic SST and sea-ice Updates
    # Make sure the init and static files exist!
    if args.sfc:
        opts = sfc_update(par1,par2)
        nml_init_opts, b_dir, str_init_opt = opts[0], opts[1], opts[2]
        b_init = bench.Bench(args, dummy_string="Init")
        b_init.set_options(nml_init_opts, str_init_opt, b_dir+"/init")
        print("Benchmark dir:", b_dir)
        if(not par_in_static):
            break
            
    # else:
    #     #Make sure the init test exists!
    #     b_init = bench.Bench(args, dummy_string=" Pars:"+str(par1)+" - "+str(par2))
    #     b_init.set_options(nml_opts, str_opt, b_full_name)

    #     shutil.copy(work_dir+"/benchmarks/inputs/stream_list.atmosphere.diagnostics", b_full_name+"/stream_list.atmosphere.diagnostics")
    #     shutil.copy(work_dir+"/benchmarks/inputs/stream_list.atmosphere.output", b_full_name+"/stream_list.atmosphere.output")
    #     shutil.copy(work_dir+"/benchmarks/inputs/stream_list.atmosphere.surface", b_full_name+"/stream_list.atmosphere.surface")

    #     print("Benchmark dir:", b_full_name)

    #     if len(loop_parameter)+len(loop_parameter2)>1:
    #         args.make = False

