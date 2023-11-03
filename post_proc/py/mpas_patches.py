import os
import sys
import time
import pickle as pkle
import copy
import numpy as np

import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

import matplotlib.collections as mplcollections
import matplotlib.patches as patches
import matplotlib.path as path
import matplotlib.cm as cm
from mpl_toolkits.basemap import Basemap

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

def get_mpas_patches(mesh, type="cell", pickle=True, pickleFile=None):
    if (type=="edge"):
        patch_collection = get_mpas_patches_edge(mesh, pickle, pickleFile)
    else:
        patch_collection = get_mpas_patches_cell(mesh, pickle, pickleFile)
    return patch_collection

def get_mpas_patches_cell(mesh, pickle=True, pickleFile=None):
    nCells = len(mesh.dimensions['nCells'])
    nEdgesOnCell = mesh.variables['nEdgesOnCell']
    verticesOnCell = mesh.variables['verticesOnCell']
    latVertex = mesh.variables['latVertex']
    lonVertex = mesh.variables['lonVertex']

    mesh_patches = [None] * nCells

    if pickleFile:
        pickle_fname = pickleFile
    else:
        try:
            pickle_fname = mesh.config_block_decomp_file_prefix.split('/')[-1]
            pickle_fname = pickle_fname.split('.')[0]
            pickle_fname = pickle_fname+'.'+str(nCells)+'.'+'patches'
        except:
            pickle_fname = 'patches'

    print(pickle_fname)

    if(os.path.isfile(pickle_fname)):
        pickled_patches = open(pickle_fname,'rb')
        try:
            patch_collection = pkle.load(pickled_patches)
            pickled_patches.close()
            print("Pickle file (", pickle_fname, ") loaded succesfully")
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

def get_mpas_patches_edge(mesh, pickle=True, pickleFile=None):
    #nCells = len(mesh.dimensions['nCells'])
    nEdges = len(mesh.dimensions['nEdges'])
    #nEdgesOnCell = mesh.variables['nEdgesOnCell']
    verticesOnEdge = mesh.variables['verticesOnEdge']
    cellsOnEdge = mesh.variables['cellsOnEdge']
    latVertex = mesh.variables['latVertex']
    lonVertex = mesh.variables['lonVertex']
    latCell = mesh.variables['latCell']
    lonCell = mesh.variables['lonCell']
    latEdge = mesh.variables['latEdge']
    lonEdge = mesh.variables['lonEdge']

    mesh_patches = [None] * nEdges

    if pickleFile:
        pickle_fname = pickleFile
    else:
        pickle_fname = mesh.config_block_decomp_file_prefix.split('/')[-1]
        pickle_fname = pickle_fname.split('.')[0]
        pickle_fname = pickle_fname+'.'+str(nEdges)+'.'+'ed.patches'

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

    for edge in range(len(mesh.dimensions['nEdges'])):
        # For each edge, get the latitude and longitude points of its vertices
        # and make a patch of that point vertices
        vertices = verticesOnEdge[edge,:]
        cells = cellsOnEdge[edge, :]
        vertices -= 1
        cells -= 1
        
        lats = np.array([])
        lons = np.array([])

        #print(edge, latEdge[edge]* (180 / np.pi), lonEdge[edge]* (180 / np.pi))
        lats = np.append(lats, latVertex[vertices[0]] * (180 / np.pi)) 
        lons = np.append(lons, lonVertex[vertices[0]] * (180 / np.pi)) 
        
        lats = np.append(lats, latCell[cells[1]] * (180 / np.pi)) 
        lons = np.append(lons, lonCell[cells[1]] * (180 / np.pi)) 
        
        lats = np.append(lats, latVertex[vertices[1]] * (180 / np.pi)) 
        lons = np.append(lons, lonVertex[vertices[1]] * (180 / np.pi)) 
        
        lats = np.append(lats, latCell[cells[0]] * (180 / np.pi)) 
        lons = np.append(lons, lonCell[cells[0]] * (180 / np.pi)) 
        
        lats = np.append(lats, latVertex[vertices[0]] * (180 / np.pi)) 
        lons = np.append(lons, lonVertex[vertices[0]] * (180 / np.pi)) 

        # Normalize latitude and longitude
        diff = np.subtract(lons, lons[0])
        lons[diff > 180.0] = lons[diff > 180.] - 360.
        lons[diff < -180.0] = lons[diff < -180.] + 360.

        coords = np.vstack((lons, lats)) 
        #print(coords)
        
        cell_path = np.ones((5,)) * path.Path.LINETO
        cell_path[0] = path.Path.MOVETO
        cell_path[-1] = path.Path.CLOSEPOLY
        cell_patch = path.Path(coords.T, 
                               codes=cell_path, 
                               closed=True,
                               readonly=True)
                               
        mesh_patches[edge] = patches.PathPatch(cell_patch)
                                               
        update_progress("Creating Patch file: "+pickle_fname, edge/nEdges)
            
    print("\n")

    # Create patch collection
    patch_collection = mplcollections.PatchCollection(mesh_patches)

    # Pickle the patch collection
    pickle_file = open(pickle_fname, 'wb')
    pkle.dump(patch_collection, pickle_file)
    pickle_file.close()

    print("\nCreated a patch file for mesh: ", pickle_file)
    return patch_collection


def plot_var_in_patch(var, patch_collect, label, title, outfile):

    #Make a copy of the collection, to avoid sideeffects
    patch_collection = copy.deepcopy(patch_collect)

    fig = plt.figure()
    ax = plt.gca()

    #bmap = Basemap(projection='cyl', 
    #           llcrnrlat=-90,
    #           urcrnrlat=90,
    #           llcrnrlon=0,
    #           urcrnrlon=360,
    #           resolution='l')

    bmap = Basemap(projection='cyl', 
               llcrnrlat=-90,
               urcrnrlat=90,
               llcrnrlon=-180,
               urcrnrlon=180,
               resolution='l')

    bmap.drawcoastlines()

    bmap.drawparallels(range(-90, 90, 30), linewidth=0.1, labels=[1,0,0,0], color='g')
    bmap.drawmeridians(range(0, 360, 45), linewidth=0.1, labels=[0,0,0,1], color='g', rotation=45)

    color_map = cm.bwr
    
    vmax = np.max(var)
    vmin = np.min(var)
    patch_collection.set_array(var)
    #patch_collection.set_edgecolors("")         # No Edge Colors
    patch_collection.set_antialiaseds(True)    # Blends things a little
    patch_collection.set_cmap(color_map)        # Select our color_map
    #patch_collection.set_norm(None)
    patch_collection.set_clim(vmin, vmax)

    ''' Now apply the patch_collection to our axis '''
    ax.add_collection(patch_collection)

    '''
    Add a colorbar (if desired), and add a label to it. In this example the
    color bar will automatically be generated. See ll-plotting for a more
    advance colorbar example.

    https://matplotlib.org/api/colorbar_api.html
    '''
    plt.rcParams['axes.grid'] = False
    cbar = plt.colorbar(patch_collection, fraction=0.046, pad=0.04)
    cbar.set_label(label)
    #color_map = cm.gist_ncar
    style = 'ggplot'
    
    plt.title(title)
    plt.style.use(style) # Set the style that we choose above

    ''' Save the file, remove the patch_collection, and close the figure.
    You'll need to always remove the patch_collection when generating
    plots on the same collection, else MPL will complain.
    '''
    plt.savefig(outfile, dpi=500)
    patch_collection.remove()
    plt.close(fig)
