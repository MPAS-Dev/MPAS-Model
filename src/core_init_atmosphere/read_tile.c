#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

/* Opens and reads a binary tile file and returns its contents into an array.


   The interface for the following function is described as:

   use iso_c_binding, only : c_int, c_char, c_float, c_ptr, c_loc
 
   interface
      integer (c_int) function c_get_tile(file, nx, ny, x_halo, y_halo, word_size, tile) bind(C)
         use iso_c_binding, only : c_int, c_char, c_float, c_ptr
         character (c_char), intent(in) :: file
         integer (c_int), intent(in), value :: nx
         integer (c_int), intent(in), value :: ny
         integer (c_int), intent(in), value :: x_halo
         integer (c_int), intent(in), value :: y_halo
         integer (c_int), intent(in), value :: word_size
         type (c_ptr), value :: tile
      end function c_get_tile
   end interface


   To pass in the tile argument do the following in fortran:


   ! Define the tile with attributes POINTER, and CONTIGUOUS
   ! and a define a c_ptr.

   real (c_float), dimension(:,:), pointer, contiguous :: tile
   type (c_ptr) :: tile_ptr

   ! Then set the c_ptr point to our contiguous tile. After tile tile_ptr
   ! will be 'pointing' to our tile.

   tile_ptr = c_loc(tile)

   ! Then pass it into C:

   iErr = c_get_tile(file, nx, ny, x_halo, y_halo, word_size, tile_ptr)



   Because we have defined our tile array to be CONTIGUOUS, we can safely pass
   the c_loc (ie the address) to C and have C loop over its contents via
   implicit pointer arithmatic!

*/

#define GEOG_BIG_ENDIAN 0
#define GEOG_LITTLE_ENDIAN 1

/* int c_get_tile(char *file, int nx, int ny, int *tile[nx][ny])
 *
 * char *file        - The path, realtive or absolute to the file
 * int nx            - The size of the x direction of the tile
 * int ny            - The size of the y direction of the tile
 * int *tile[nx][ny] - The array to hold the tile values on return
 *
 * returns - 1 if success and -1 if file does not exist
 */

int c_get_tile(char *file, 
               int nx, 
               int ny, 
               int x_halo, 
               int y_halo, 
               int word_size, 
               float* tile)
{
    int fd;                /* File Descriptor */
    int i, j;
    unsigned char *buf;
	 size_t numBytes = 0;
    int value = 0;

    int narray = (nx + x_halo) * (ny + y_halo); /* Extent of bytes we have to read */
    float tile_1d[narray];             

    /* Allocate enough space for the entire file */
    buf = (unsigned char *) malloc(sizeof(unsigned char)*(word_size) * narray);

    fd = open(file, O_RDONLY);
    if (fd == -1){
		perror("OPEN ERROR: ");
        printf("C filename was: %s\n", file);
        return -1;
    }

    numBytes = read(fd, buf, word_size * narray); // Read in all the values at once
    if (numBytes == -1){
        perror("READ ERROR: ");
        return -1;
    }
    if(close(fd) == -1){
        perror("CLOSE ERROR: ");
        return -1;
    }


    for(i=0; i < narray ; i++){
        switch(word_size) {
            case 2: 
                /* Shift the first byte read by 8 bytes to make room for the 
                 * 2nd byte we have read.
                 */

                value = (int) (buf[ word_size * i ] << 8 | buf[ word_size * i + 1 ]);

                /* Special case for a negative value. Our sign bit is currently
                 * at the most significant bit (MSB) for an 8-bit value, but it 
                 * needs to bet at the MSB for a 16-bit value.
                 */
                if(buf[word_size * i] >> 7 == 1)
                    value -= 1 << ( 8 * word_size);
                tile_1d[i] = (float) value;
                break;
        }
    }

    for(i=0; i < nx + x_halo; i++){
        for(j=0; j < ny + y_halo; j++){
            /* Place the values into the fortran interoperable array and return */
            tile[j * (nx + x_halo) + i] = tile_1d[ j * (nx + x_halo) + i ];
        }
    }

    free(buf);

    return 1;
}
