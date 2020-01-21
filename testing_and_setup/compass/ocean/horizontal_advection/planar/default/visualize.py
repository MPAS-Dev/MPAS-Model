"""
This is a visualization method ot check the test case
created by: Jamil gafur
created on: 1/21/20

expectation: a circle moving diagonally Northeast and overflowing to the bottom left
"""

# Read in the data
import xarray as xr
# plot and save the individual images
import matplotlib.pyplot as plt
#creates a gif
import imageio



def main():
  # load in the data
  filename = "forward/output/KPP_test.0001-01-01_00.00.00.nc"
  data = xr.open_dataset(filename)
  # set the proper bounds
  plt.xlim(0,10000)
  plt.ylim(0,8000)
  gif_frames = data.tracer1.shape[0]
  print("Total Number of Frames to plot: {}".format(gif_frames))

  # cycle through each frame and plot its data and save it as a figure
  for i in range(gif_frames):
    plt.scatter(data.xCell, data.yCell, c=data.tracer1[i,:,99])
    plt.savefig(str(i)+".png")

  # holds the imagenames
  images = []
  # cycles through all the frames and saves the data to the array
  for i in range(gif_frames):
      images.append(imageio.imread(str(i)+".png"))
  # creates the gif
  imageio.mimsave('KPP_tracer1.gif', images)    




main()
