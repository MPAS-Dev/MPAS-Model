#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#-------------------------------------------------
#  Script to generate namelists and stream for MPAS
#  Author: Danilo Couto de Souza <danilo.oceano@gmail.com>
#  Last update: Oct 2022
#  Adapted from: P. Peixoto <ppeixoto@usp.br>
# --------------------------------------------------
"""
Created on Tue Oct 11 14:31:11 2022

@author: daniloceano
"""



import os
import argparse
import subprocess
import sys
import xml.etree.ElementTree as ET
import f90nml
import fnmatch

def call_parser():
    parser = argparse.ArgumentParser()
    # Arguments for different steps of model pre-processing routines and
    # model intialization
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument('--static', action='store_true',
                       help='Use init_atmosphere core to interpolate static \
fields. See Users Guide Section 7.2.1')
    group.add_argument('--init', action='store_true',
                       help='Use init_atmosphere core for vertical grid\
generation and initial field interpolation. See Users Guide Section 7.2.2')
    group.add_argument('--sfc', action='store_true',
                       help='Use init_atmosphere core for generating\
periodic SST and sea-ice updates. See Users Guide Section 8.1')
    group.add_argument('--run', action='store_true',
                       help='Use run atm core')
    # Optional arguments
    parser.add_argument('--make', action='store_true', help='evoke make',
                        default=False)
    parser.add_argument('--name', type=str, default='basic_test',
                        help='''main name of benchmark''')
    parser.add_argument('--bdir', type=str, default='basic_test/x1.10242', 
                        help='''path to directory of benchmark''')
    parser.add_argument('--threads', type=str, default='2', 
                        help='''number of threads for running the model. \
Only works for --init, --sfc and --run and is only usable for using the \
run_bench.py (default: 2)''')

    args = parser.parse_args()
    return args

def find(pattern, path):
    result = []
    for root, dirs, files in os.walk(path):
        for name in files:
            if fnmatch.fnmatch(name, pattern):
                result.append(os.path.join(root, name))
    return result

class LogFollower:
    def __init__(self, fp):
        self.position = 0
        self.fp = fp

    def seek(self):
        self.fp.seek(self.position)

    def has(self):
        self.seek()
        return '\n' in self.fp.read()

    def __iter__(self):
        while self.has():
            self.seek()
            line = self.fp.read().split('\n')[0]
            yield line

            # advance position - this is the 'state machine' part!
            self.position += len(line) + 1

class Bench:
    def __init__(self, args, dummy_string="", b_dir=""):
        print()
        print("--------------------------------------------------")
        print("Setting up benchmark:", args.name, dummy_string)
        print("--------------------------------------------------")
        self.args = args
        self.name = args.name
        self.work_dir = os.getenv('MPAS_DIR')
        self.bench_dir = self.work_dir+"/benchmarks"
        self.b_dir = b_dir
        print("Working directory:", self.work_dir)

        #Check if dir exists, or create
        #Create dir for output
        self.bench_dir = self.bench_dir+"/"+self.name
        if not os.path.isdir(self.bench_dir):
            os.makedirs(self.bench_dir)

        ## Set mpas to get defauls namelists and streams ##
        # If using --run will use the atmosphere_model, but for any other
        # option, will use the init_atmosphere model
        if args.run:
            self.core="atmosphere_model"
            self.core0="atmosphere"
            self.nml_filename = "namelist.atmosphere"
            self.stream_filename = "streams.atmosphere"
        else:
            self.core="init_atmosphere_model"
            self.core0="init_atmosphere"
            self.nml_filename = "namelist.init_atmosphere"
            self.stream_filename = "streams.init_atmosphere"

        # If interpolating static fields (first step for running the model),
        # or setting the environment for model run (namelist.atmosphere), it 
        # will copy namelist from defaults.
        if args.static or args.run:
            self.default_nml=self.work_dir+"/default_inputs/"+self.nml_filename
            self.default_stream=self.work_dir+"/default_inputs/"+self.stream_filename
        # But if running any other steps, re-use the namelist generated
        # in the previous steps 
        else:
            self.default_nml=self.b_dir+"/init/"+self.nml_filename
            self.default_stream=self.b_dir+"/init/"+self.stream_filename

        if not os.path.isfile(self.default_nml):
            print("Default namelist not found, compiling mpas..")
            args.make = True

        #make mpas
        if args.make:
            make_args = ["make"]
            make_args.append("gfortran")
            make_args.append("CORE="+self.core0)
            make_args.append("OPENMP=true")
            make_args.append("USE_PIO2=true")
            make_args.append("AUTOCLEAN=true")

            with subprocess.Popen(make_args, stdout = subprocess.PIPE,
                                  stderr=subprocess.PIPE, cwd=self.work_dir,
                                  shell=False) as p:
                for line in p.stdout:
                    print(line, end='\n') # process line here
            
            if p.returncode != 0:
                raise Exception( f'Invalid result: { p.returncode }' )

            

    def set_options(self, nml_options = {}, stream_options={}, b_dir="bdir"):
        
        #Make bench directory
        if not os.path.isdir(b_dir):
            os.makedirs(b_dir)

        #Link mpas to folder
        link = ["ln", "-sf", self.work_dir+"/"+self.core, b_dir+"/"+self.core ]
        subprocess.run(link, stdout = subprocess.PIPE, stderr=subprocess.PIPE,
                       cwd=self.work_dir, shell=False)
        
        # If running model, link WRF physics files
        if self.args.run:
            for file in ["OZONE_PLEV.TBL", "OZONE_LAT.TBL", "OZONE_DAT.TBL",
                         "LANDUSE.TBL", "GENPARM.TBL", "COMPATIBILITY",
                         "CAM_AEROPT_DATA.DBL","CAM_ABS_DATA.DBL",
                         "VERSION", "VEGPARM.TBL", "SOILPARM.TBL", 
                         "RRTMG_SW_DATA.DBL", "RRTMG_SW_DATA",
                         "RRTMG_LW_DATA.DBL", "RRTMG_LW_DATA",
                         "MP_THOMPSON_freezeH2O_DATA.DBL",
                         "MP_THOMPSON_QIautQS_DATA.DBL",
                         "MP_THOMPSON_QRacrQG_DATA.DBL",
                         "MP_THOMPSON_QRacrQS_DATA.DBL"]:
                link = ["ln", "-sf", self.work_dir+"/"+file, b_dir+"/"]
                subprocess.run(link, stdout = subprocess.PIPE,
                               stderr=subprocess.PIPE, cwd=self.work_dir, 
                               shell=False)

        #Namelist
        nml = f90nml.read(self.default_nml)
        self.nml_dict_orig = nml
        print("  Setting up namelist:")
        #print(nml)
        for name in nml:
            if name in nml_options.keys():
                for op in nml_options[name]:
                    print("    ", name, op, nml_options[name][op])
                    nml[name][op] = nml_options[name][op]
        #print(nml)
        self.nml_dict = nml
        nml.write(b_dir+"/"+self.nml_filename, force=True)
        print()


        #Streams
        stream_tree = ET.parse(self.default_stream)
        stream_root = stream_tree.getroot()
        self.stream_root_orig = stream_root
        print("  Setting up streams:")
        for s in stream_root:
            #print(s.tag, s.attrib)
            name = s.attrib['name']
            if name in stream_options.keys():
                for op in stream_options[name]: 
                    print("    ", name, op, stream_options[name][op])
                    # strm[name][op] = stream_options[name][op] #
                    s.set(op, stream_options[name][op])
            #print(s.attrib)
        #print(ET.tostringlist(stream_root))
        self.stream_root = stream_root
        stream_tree.write(b_dir+"/"+self.stream_filename)