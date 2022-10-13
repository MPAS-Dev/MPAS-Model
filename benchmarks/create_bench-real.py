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
    3. <grid_name>.static.nc: static terrestrial files interpolated on
    the computational mesh;
    4. <grid_name>.sfc_update.nc: sea surface temperature, land-mask and 
    sea-ice  fields interpolated into the model mesh;
    5. <grid_name>.init.nc: meteorological variables for the model cold-start
    interpolated into the mesh;
    6. <grid_name>.graph.info.part.<n>: partition file for runing the model in 
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

import mpas_benchmarks_RealCase as bench

# Get args: init or run core
args = bench.call_parser()

#Workspace
work_dir = os.getenv('MPAS_DIR')
b_name = args.name
b_main_dir = work_dir+"/benchmarks/"+b_name

#Define grid
grid_name = "cyclones_test"
# grid_dir = work_dir+"/benchmarks/inputs/"+grid_name
grid_dir = work_dir+"/benchmarks/inputs/"


#loop over options

# smag coefficients
#                  none, default, 
loop_parameter = [0.,120000.0]
loop_parameter2 = [0.00, 0.05]
par_in_init = False

for par1, par2 in itertools.product(loop_parameter, loop_parameter2):
    
    #Init options
    nml_init_opts = {"nhyd_model":{}, "dimensions": {}, "decomposition":{} }

    nml_init_opts["nhyd_model"]["config_init_case"] = 7
    nml_init_opts["nhyd_model"]["config_hcm_staggering"] = False
    nml_init_opts["dimensions"]["config_nvertlevels"] = 20
    nml_init_opts["decomposition"]["config_block_decomp_file_prefix"] = grid_dir+"/"+grid_name+".graph.info.part."

    b_name = grid_name + ".tc_"+str(nml_init_opts["nhyd_model"]["config_init_case"]) \
        + ".hcm_"+str(nml_init_opts["nhyd_model"]["config_hcm_staggering"]) \
        + ".lv_"+str(nml_init_opts["dimensions"]["config_nvertlevels"])

    b_dir = b_main_dir+"/"+b_name

    str_init_opt = {"input":{}, "output":{}, "surface" : {}}

    str_init_opt["input"]["filename_template"] = grid_dir+"/"+grid_name+".grid.nc"
    str_init_opt["output"]["filename_template"] = b_dir+"/init/"+b_name+".init.nc"
    str_init_opt["surface"]["filename_template"] = b_dir+"/init/"+b_name+".sfc_update.nc"
    str_init_opt["output"]["clobber_mode"] = "overwrite"

    #Runtime options

    nml_opts = {"nhyd_model":{}, "damping": {}, "decomposition":{}, "physics":{}  }

    nml_opts["nhyd_model"]["config_time_integration_order"] = 2
    nml_opts["nhyd_model"]["config_dt"] = 120
    nml_opts["nhyd_model"]["config_run_duration"] = '1_00:00:00'
    nml_opts["nhyd_model"]["config_horiz_mixing"] = '2d_smagorinsky'
    nml_opts["nhyd_model"]["config_len_disp"] = par1 #1200000.
    nml_opts["nhyd_model"]["config_visc4_2dsmag"] = par2 #0.05
    nml_opts["nhyd_model"]["config_smdiv"] = 0.1
    nml_opts["decomposition"]["config_block_decomp_file_prefix"] = grid_dir+"/"+grid_name+".graph.info.part."
    nml_opts["physics"]["config_physics_suite"] = 'none'

    b_full_name = b_dir+"/run."+"smag_"+str(nml_opts["nhyd_model"]["config_len_disp"])+ \
        ".visc4smag_"+str(nml_opts["nhyd_model"]["config_visc4_2dsmag"])

    str_opt = {"input":{}, "output":{}, "restart" : {}, "diagnostics" : {}, "surface" : {}}

    str_opt["input"]["filename_template"] = str_init_opt["output"]["filename_template"]
    str_opt["output"]["filename_template"] = b_full_name+"/out.nc"
    str_opt["output"]["output_interval"] = "1:00:00"
    str_opt["output"]["clobber_mode"] = "overwrite"
    str_opt["surface"]["filename_template"] = str_init_opt["surface"]["filename_template"]
    str_opt["diagnostics"]["filename_template"] = b_full_name+"/diag.nc"
    str_opt["diagnostics"]["output_interval"] = "1:00:00"
    str_opt["diagnostics"]["clobber_mode"] = "overwrite"


    #Init_atmosphere setup
    if args.init:
        b_init = bench.Bench(args, dummy_string="Init")
        b_init.set_options(nml_init_opts, str_init_opt, b_dir+"/init")
        print("Benchmark dir:", b_dir)
        if(not par_in_init):
            break
    else:
        #Make sure the init test exists!
        b_init = bench.Bench(args, dummy_string=" Pars:"+str(par1)+" - "+str(par2))
        b_init.set_options(nml_opts, str_opt, b_full_name)

        shutil.copy(work_dir+"./inputs/stream_list.atmosphere.diagnostics", b_full_name+"/stream_list.atmosphere.diagnostics")
        shutil.copy(work_dir+"./inputs/stream_list.atmosphere.output", b_full_name+"/stream_list.atmosphere.output")
        shutil.copy(work_dir+"./inputs/stream_list.atmosphere.surface", b_full_name+"/stream_list.atmosphere.surface")

        print("Benchmark dir:", b_full_name)

        if len(loop_parameter)+len(loop_parameter2)>1:
            args.make = False

