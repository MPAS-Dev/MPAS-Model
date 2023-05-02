#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Mar 18 18:48:09 2023

Created by:
    Danilo Couto de Souza
    Universidade de São Paulo (USP)
    Instituto de Astornomia, Ciências Atmosféricas e Geociências
    São Paulo - Brazil
    
Contact:
    danilo.oceano@gmail.com
    
Adapted from: P. Peixoto <ppeixoto@usp.br>

-------------------------------------------------------------------------------

This script was created with the intent to set an environment for testing
    distinct model configurations for the MPAS-A model. 

For using this script it is highly advisible that you are very familiar with 
    the MPAS-A structure, the steps required for running the model its 
    namelists and streams files.

For running the model, it is necessary to perform all steps for generating 
    static fields file (static), meteorological initial conditions (init) and 
    surface update files (sfc), this last one being optional. Here's some 
    example of how to use this code:

    1) Firsty, make sure that the path to the MPAS-BR directory is set in the
    environment variable MPAS_DIR. 
    
    2) Then, go to the first section of this script and set the desired 
    options, such as grid name and path, initial date, etc.
    
    3) For testing distinct options, it is necessary to change the 
    loop_parameters in this script second section and then change the options 
    in the script's third section.
    
Now, it's time to go to the actual pre-processing stages. Supose that you are 
    going to run some tests called "test" (using the most of your creative
    abillities), and you will test the model parameters len_disp and
    visc4_2dsmag, using the custom MPAS grid x1.10242. Here's how to perform 
    the first step, i.e., the static fields interpolation process. On the 
    terminal, on the bechmark directory:
    
    python create_bench-real.py --static --name test --bdir ./

This will create the directory "test" with a subdirectory called
    x1.10242.len_disp.visc4_2dsmag, with another subdirectory inside it, named
    init, containing the namelist.init_atmosphere, streams.init_atmosphere 
    files and a symbolic link for the model core init_atmosphere_model. Then, 
    let's start the static fields interpolation with the command:
    
    python run_bench-real.py --static --name test \
    --bdir cyclone_bench/x1.10242.len_disp.visc4_2dsmag
    
This process will take up to an hour, depending on the machine. Suppose you did
    not find any erros in this stage, we can move to updating the namelist and
    streams files for creating the the initial contiions for starting the model.
    After doing the WRF ungrib process for generating intermediate files, move
    those files to the $MPAS_DIR/input_files directory and, on the benchmark
    directory, run:
    
    python create_bench-real.py --init --name test --bdir ./
    
And then, for creating the init file:
        
     python run_bench-real.py --init --name test \
     --bdir cyclone_bench/x1.10242.len_disp.visc4_2dsmag
     
If the simulation will be performed using sea surface temperature boundary
    conditions from another dataset, it is required to create the sfc_update 
    file. As for the init files, it is required to perform the ungrib process 
    in the files containing the SST, land-sea mask and sea-ice fraction data, 
    for all the model run lenght. Also, adjust in the second section of this 
    script the the interval for updating the surface conditions (sfc_interval).
    Also, the  sst_update must be set to True. After moving the SST
    intermediate files to the $MPAS_DIR/input_files directory, on the benchmark
    directory, run:
    
    python create_bench-real.py --sfc --name test --bdir ./
    
And then, for creating the sfc_update file:
        
    python run_bench-real.py --sfc --name test \
    --bdir cyclone_bench/x1.10242.len_disp.visc4_2dsmag
    
Now, with all the pre-processing stages ready, one can set the environment for
    actually running the model. First, on the benchmark directory, run:
    
    python create_bench-real.py --run --name cyclone_bench --bdir ./
    
This will create a list of subdirectories (benchmarks), one for each 
    combination of model configuration, from the loop_parameter options. Now,
    let's start running each bench created:
    
    python run_bench-real.py --run --name test \
    --bdir cyclone_bench/x1.10242.len_disp.visc4_2dsmag

-------------------------------------------------------------------------------

TO DO: Read loop_parameters from a text file. Use default_inputs for defining 
    the parameters for creating the benchmarks, instead of specifying them here.

@author: daniloceano
"""

import os
import shutil
import itertools
import datetime
import mpas_benchmarks_RealCase as bench

# Get args: init or run core
args = bench.call_parser()


# Workspace
work_dir = os.getenv('MPAS_DIR')
b_name = args.name
b_main_dir = work_dir+"/benchmarks/"+b_name

# 1 ===========================================================================
# ## DEFINE PARAMETERS FOR NAMELIST.INIT ##
# define grid
grid_name = "Catarina_250-8km"
grid_dir = work_dir+"/grids/grids/Catarina_250-8km/"
# define dates in format: 'YYYY-MM-DD_hh:mm:ss'
init_date = '2004-03-21_00:00:00'
run_duration = '2_00:00:00'
len_disp = 800.0
n_vert_levels = 72
# path to geographical data
geog_data_path = '/p1-nemo/danilocs/mpas/mpas_tutorial/geog/'
# prefix used to generate intermediatie files with ungrib.exe
met_prefix = 'ERA5'
sfc_prefix = 'SST'
# Interval of the SST files
sfc_interval = 21600
# Model timestep
dt = 48
# SST Update on or off
sst_update = True

# 2 ===========================================================================
## LOOP OVER DESIRED OPTIONS ##
# Microphysics parametrization choice
loop_parameter1 = ['mp_thompson','mp_wsm6']
# Convective parametrization choice
loop_parameter2 = ["cu_ntiedtke",'cu_tiedtke', 'cu_grell_freitas']
# Boundary layer choice
loop_parameter3 = ["bl_ysu",'bl_mynn']

# 3 ===========================================================================
def static_interp():
    
    nml_init_opts = {"nhyd_model":{}, "dimensions": {}, 
                     "data_sources":{}, "preproc_stages": {}}
    # Real-data initialization case
    nml_init_opts["nhyd_model"]["config_init_case"] = 7
    # Should be set to 1 for this step (see doc)  
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

    b_name = grid_name+".physics-pbl_sst"

    b_dir = b_main_dir+"/"+b_name

    str_init_opt = {"input":{}, "output":{}}

    str_init_opt["input"]["filename_template"] = grid_dir+"/"+grid_name+".grid.nc"
    str_init_opt["output"]["filename_template"] = b_dir+"/init/"+b_name+".static.nc"
    str_init_opt["output"]["clobber_mode"] = "overwrite"

    return nml_init_opts, b_dir, str_init_opt

def init_interp():
    
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
    nml_init_opts["preproc_stages"]["config_frac_seaice"] = False
    # Decomposition file for running in parallel
    nml_init_opts["decomposition"]["config_block_decomp_file_prefix"] = \
        grid_dir+grid_name+'.graph.info.part.'

    b_name = grid_name+".physics-pbl_sst"

    b_dir = b_main_dir+"/"+b_name
    

    str_init_opt = {"input":{}, "output":{}}

    str_init_opt["input"]["filename_template"] = b_dir+"/init/"+b_name+".static.nc"
    str_init_opt["output"]["filename_template"] = b_dir+"/init/"+b_name+".init.nc"
    str_init_opt["output"]["clobber_mode"] = "overwrite"

    return nml_init_opts, b_dir, str_init_opt

def sfc_update():
    
    start = datetime.datetime.strptime(init_date, '%Y-%m-%d_%H:%M:%S')
    lenght = datetime.datetime.strptime(run_duration,'%d_%H:%M:%S')
    end = start + datetime.timedelta(days=lenght.day,hours=lenght.hour)
    finish_date = end.strftime('%Y-%m-%d_%H:%M:%S')
    
    nml_init_opts = {"nhyd_model":{}, "decomposition": {}, 
                     "data_sources":{}, "preproc_stages": {}}
    # Real-data initialization case
    nml_init_opts["nhyd_model"]["config_init_case"] = 8
    nml_init_opts["nhyd_model"]["config_start_time"] = init_date
    nml_init_opts["nhyd_model"]["config_stop_time"] = finish_date
    ## Be careful with the interval for updating the sfc conditions
    nml_init_opts["data_sources"]["config_sfc_prefix"] = \
        work_dir+"/input_data/"+sfc_prefix
    nml_init_opts["data_sources"]["config_fg_interval"] = sfc_interval
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

    b_name = grid_name+".physics-pbl_sst"

    b_dir = b_main_dir+"/"+b_name

    str_init_opt = {"input":{}, "output":{}, "surface":{}}

    # Setup grid and init file
    str_init_opt["input"]["filename_template"] = b_dir+"/init/"+b_name+".static.nc"
    str_init_opt["output"]["filename_template"] = b_dir+"/init/"+b_name+".init.nc"
    str_init_opt["output"]["clobber_mode"] = "overwrite"    
    # Setup surface update file
    str_init_opt["surface"]["filename_template"] = \
        b_dir+"/init/"+b_name+".sfc_update.nc"
    str_init_opt["surface"]["filename_interval"] = str(sfc_interval)
    str_init_opt["surface"]["output_interval"] = str(sfc_interval)
    str_init_opt["surface"]["clobber_mode"] = "overwrite"
    
    return nml_init_opts, b_dir, str_init_opt

def run(par1,par2, par3):
    #Runtime options

    nml_opts = {"nhyd_model":{}, "damping": {},
                "decomposition":{}, "physics":{}  }

    nml_opts["nhyd_model"]["config_dt"] = dt
    nml_opts["nhyd_model"]["config_start_time"] = init_date
    nml_opts["nhyd_model"]["config_run_duration"] = run_duration
    nml_opts["nhyd_model"]["config_len_disp"] = len_disp
    nml_opts["nhyd_model"]["config_visc4_2dsmag"] = 0.05
    nml_opts["decomposition"]["config_block_decomp_file_prefix"] =\
        grid_dir+"/"+grid_name+".graph.info.part."
    nml_opts["physics"]["config_sst_update"] = sst_update
    nml_opts["physics"]["config_microp_scheme"] = par1
    nml_opts["physics"]["config_convection_scheme"] = par2
    nml_opts["physics"]["config_pbl_scheme"] = par3
        
    b_name = grid_name+".physics-pbl_sst"
    b_dir = b_main_dir+"/"+b_name
    b_full_name = b_dir+"/run."+"best-physics_sst"+"."+str(par1)+\
        "."+str(par2)+"."+str(par3)

    str_opt = {"input":{}, "output":{}, "restart" : {},
               "diagnostics" : {}, "surface" : {}}
    
    str_opt["input"]["filename_template"] = b_dir+"/init/"+b_name+".init.nc"
    str_opt["output"]["filename_template"] = b_full_name+"/history.$Y-$M-$D_$h.$m.$s.nc"
    str_opt["output"]["output_interval"] = "1:00:00"
    str_opt["surface"]["filename_template"] = \
        b_dir+"/init/"+b_name+".sfc_update.nc"
    str_opt["surface"]["filename_interval"] = str(sfc_interval)
    str_opt["surface"]["input_interval"] = str(sfc_interval)
    str_opt["diagnostics"]["filename_template"] = b_full_name+"/diag.nc"
    str_opt["diagnostics"]["output_interval"] = "3:00:00"
    str_opt["diagnostics"]["clobber_mode"] = "overwrite"
    
    return nml_opts, b_full_name, str_opt

# 4 ===========================================================================
# Setup for creating static fields
if args.static:
    opts = static_interp()
    nml_init_opts, b_dir, str_init_opt = opts[0], opts[1], opts[2]
    b_init = bench.Bench(args, dummy_string="Init", b_dir=b_dir)
    b_init.set_options(nml_init_opts, str_init_opt, b_dir+"/init")
    print("Benchmark dir:", b_dir)
 
# Setup for vertical grid generation and initial field interpolation
# Make sure the static file exists!
elif args.init:
    opts = init_interp()
    nml_init_opts, b_dir, str_init_opt = opts[0], opts[1], opts[2]
    b_init = bench.Bench(args, dummy_string="Init", b_dir=b_dir)
    b_init.set_options(nml_init_opts, str_init_opt, b_dir+"/init")
    print("Benchmark dir:", b_dir)
    
# Setup for generating periodic SST and sea-ice Updates
# Make sure the init and static files exist!
elif args.sfc:
    opts = sfc_update()
    nml_init_opts, b_dir, str_init_opt = opts[0], opts[1], opts[2]
    b_init = bench.Bench(args, dummy_string="Init", b_dir=b_dir)
    b_init.set_options(nml_init_opts, str_init_opt, b_dir+"/init")
    print("Benchmark dir:", b_dir)


# 5 ===========================================================================     
#Make sure the init test exists!
elif args.run:
    for par1, par2, par3 in itertools.product(loop_parameter1, loop_parameter2, loop_parameter3):
        opts = run(par1,par2, par3)
        nml_opts, b_full_name, str_opt = opts[0], opts[1], opts[2]
        b_init = bench.Bench(args,
                             dummy_string=" Pars:"+str(par1)+" - "+str(par2)+" - "+str(par3))
        b_init.set_options(nml_opts, str_opt, b_full_name)
    
        shutil.copy(work_dir+"/default_inputs/stream_list.atmosphere.diagnostics", b_full_name+"/stream_list.atmosphere.diagnostics")
        shutil.copy(work_dir+"/default_inputs/stream_list.atmosphere.output", b_full_name+"/stream_list.atmosphere.output")
        shutil.copy(work_dir+"/default_inputs/stream_list.atmosphere.surface", b_full_name+"/stream_list.atmosphere.surface")
    
        print("Benchmark dir:", b_full_name)
    
        if len(loop_parameter1)+len(loop_parameter2)>1:
            args.make = False
                
        else:
            print("Argument not recognized!")

