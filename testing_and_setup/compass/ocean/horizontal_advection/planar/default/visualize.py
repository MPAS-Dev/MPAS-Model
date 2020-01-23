#!/usr/bin/env python
"""
This is a visualization method ot check the test case
created by: Jamil gafur
created on: 1/21/20

expectation: a circle moving diagonally Northeast and overflowing to the bottom left
"""

# Read in the data
import xarray as xr
# Plot and save the individual images
import matplotlib.pyplot as plt
# Creates a gif
import imageio
# Creates image directory
import os
# Grabs the KPP file
import glob




# Print iterations progress
def printProgressBar (iteration, total, prefix = '', suffix = '', decimals = 1, length = 100, fill = '█', printEnd = "\r"):
    """
    Call in a loop to create terminal progress bar
    @params:
        iteration   - Required  : current iteration (Int)
        total       - Required  : total iterations (Int)
        prefix      - Optional  : prefix string (Str)
        suffix      - Optional  : suffix string (Str)
        decimals    - Optional  : positive number of decimals in percent complete (Int)
        length      - Optional  : character length of bar (Int)
        fill        - Optional  : bar fill character (Str)
        printEnd    - Optional  : end character (e.g. "\r", "\r\n") (Str)
    """
    percent = ("{0:." + str(decimals) + "f}").format(100 * (iteration / float(total)))
    filledLength = int(length * iteration // total)
    bar = fill * filledLength + '-' * (length - filledLength)
    print('\r%s |%s| %s%% %s' % (prefix, bar, percent, suffix), end = printEnd)
    # Print New Line on Complete
    if iteration == total: 
        print()


def main():
  # load in the data
  filename = None
  filename = glob.glob("forward/output/KPP*")[0]
  if filename == None:
    print("KPP File not found in forward/output ending program")
    quit()

  data = xr.open_dataset(filename)
  # set the proper bounds with alittle extra padding
  plt.xlim(-10,data.xCell.shape[0]+10)
  plt.ylim(-10,data.yCell.shape[0]+10)
  gif_frames = data.tracer1.shape[0]

  # make directory to save images in
  dir_name = "images"
  os.makedirs(dir_name, exist_ok=True)

  # cycle through each frame and plot its data and save it as a figure
  # progress bar for making frames
  printProgressBar(0, gif_frames, prefix = "Saving Frames", suffix = "Compleate", length=50)
  for i in range(gif_frames):
    # updates progress bar
    printProgressBar(i+1, gif_frames, prefix = "Saving Frames", suffix = "Compleate", length=50)
 
    plt.scatter(data.xCell, data.yCell, c=data.tracer1[i,:,99])
    plt.savefig(dir_name +"/"+ str(i)+".png")

  # new line for next progress bar
  print("\n")

  # holds the imagenames
  images = []
  # progress bar for mergin frames
  printProgressBar(0, gif_frames+1, prefix = "Making Gif", suffix = "Compleate", length=50)
  # cycles through all the frames and saves the data to the array
  for i in range(gif_frames):
    images.append(imageio.imread(dir_name+"/"+str(i)+".png"))
    printProgressBar(i+1, gif_frames+1, prefix = "Making Gif", suffix = "Compleate", length=50)

  # creates the gif
  imageio.mimsave('KPP_tracer1.gif', images)    
  printProgressBar(gif_frames+1, gif_frames+1, prefix = "Making Gif", suffix = "Compleate", length=50)
main()
