import os
import sys
import time
import pickle as pkle

import numpy as np
import matplotlib.collections as mplcollections
import matplotlib.patches as patches
import matplotlib.path as path

''' This module creates or retrives a collection of MPL Path Patches for an MPAS unstructured mesh.

Given an MPAS mesh file, `get_mpas_patches` will create a Path Patch for each MPAS grid, by looping
over a Cell's vertices. Because this operation is a nCell * nEdge operation, it will take some
quite some time.

However, once a patch collection is created it is saved (using Python's Pickle module) as a 'patch'
file. This patch file can be loaded for furture plots on that mesh, which will speed up future
plots creation.

This module was created with much help and guidence from the following repository:

* https://github.com/lmadaus/mpas_python

'''
def update_progress(job_title, progress):
    length = 40
    block = int(round(length*progress))
    msg = "\r{0}: [{1}] {2}%".format(job_title, "#"*block + "-"*(length-block),
    round(progress*100, 2))
    if progress >= 1: msg += " DONE\r\n"
    sys.stdout.write(msg)
    sys.stdout.flush()

def get_mpas_patches(mesh, pickle=True, pickleFile=None):
    nCells = len(mesh.dimensions['nCells'])
    nEdgesOnCell = mesh.variables['nEdgesOnCell']
    verticesOnCell = mesh.variables['verticesOnCell']
    latVertex = mesh.variables['latVertex']
    lonVertex = mesh.variables['lonVertex']

    mesh_patches = [None] * nCells

    if pickleFile:
        pickle_fname = pickleFile
    else:
        pickle_fname = mesh.config_block_decomp_file_prefix.split('/')[-1]
        pickle_fname = pickle_fname.split('.')[0]
        pickle_fname = pickle_fname+'.'+str(nCells)+'.'+'patches'

    print(pickle_fname)

    if(os.path.isfile(pickle_fname)):
        pickled_patches = open(pickle_fname,'rb')
        try:
            patch_collection = pkle.load(pickled_patches)
            pickled_patches.close()
            print("Pickle file (", pickle_fname, ") loaded succsfully")
            return patch_collection
        except:
            print("ERROR: Error while trying to read the pickled patches")
            print("ERROR: The pickle file may be corrupted or was not created")
            print("ERROR: succesfully!")
            sys.exit(-1)

    print("\nNo pickle file found, creating patches...")
    print("If this is a large mesh, then this proccess will take a while...")

    for cell in range(len(mesh.dimensions['nCells'])):
        # For each cell, get the latitude and longitude points of its vertices
        # and make a patch of that point vertices
        vertices = verticesOnCell[cell,:nEdgesOnCell[cell]]
        vertices = np.append(vertices, vertices[0:1])

        vertices -= 1

        vert_lats = np.array([])
        vert_lons = np.array([])

        for lat in mesh.variables['latVertex'][vertices]:
            vert_lats = np.append(vert_lats, lat * (180 / np.pi)) 

        for lon in mesh.variables['lonVertex'][vertices]:
            vert_lons = np.append(vert_lons, lon * (180 / np.pi))

        # Normalize latitude and longitude
        diff = np.subtract(vert_lons, vert_lons[0])
        vert_lons[diff > 180.0] = vert_lons[diff > 180.] - 360.
        vert_lons[diff < -180.0] = vert_lons[diff < -180.] + 360.

        coords = np.vstack((vert_lons, vert_lats)) 

        cell_path = np.ones(vertices.shape) * path.Path.LINETO
        cell_path[0] = path.Path.MOVETO
        cell_path[-1] = path.Path.CLOSEPOLY
        cell_patch = path.Path(coords.T, 
                               codes=cell_path, 
                               closed=True,
                               readonly=True)
                               
        mesh_patches[cell] = patches.PathPatch(cell_patch)
                                               
        update_progress("Creating Patch file: "+pickle_fname, cell/nCells)
            
    print("\n")

    # Create patch collection
    patch_collection = mplcollections.PatchCollection(mesh_patches)

    # Pickle the patch collection
    pickle_file = open(pickle_fname, 'wb')
    pkle.dump(patch_collection, pickle_file)
    pickle_file.close()

    print("\nCreated a patch file for mesh: ", pickle_file)
    return patch_collection

