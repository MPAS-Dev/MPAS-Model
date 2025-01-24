#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <limits.h>
#include "smiol.h"
#include "smiol_utils.h"

#ifdef SMIOL_PNETCDF
#include "pnetcdf.h"
#define PNETCDF_DEFINE_MODE 0
#define PNETCDF_DATA_MODE 1
#define MAX_REQS 256
#endif

#define START_COUNT_READ 0
#define START_COUNT_WRITE 1

/*
 * Local functions
 */
int build_start_count(struct SMIOL_file *file, const char *varname,
                      const struct SMIOL_decomp *decomp,
                      int write_or_read, size_t *element_size,
                      size_t *basic_type_size, int *ndims,
                      int *has_unlimited_dim,
                      size_t **start, size_t **count);

#ifdef SMIOL_PNETCDF
int write_chunk_pnetcdf(struct SMIOL_file *file,
                        int varidp,
                        int ndims,
                        int has_unlimited_dim,
                        size_t basic_type_size,
                        MPI_Comm io_file_comm,
                        const void *buf_p,
                        MPI_Offset *mpi_start,
                        MPI_Offset *mpi_count
                       );

int read_chunk_pnetcdf(struct SMIOL_file *file,
                       int varidp,
                       int ndims,
                       int has_unlimited_dim,
                       size_t basic_type_size,
                       MPI_Comm io_file_comm,
                       void *buf_p,
                       MPI_Offset *mpi_start,
                       MPI_Offset *mpi_count
                      );
#endif


/********************************************************************************
 *
 * SMIOL_fortran_init
 *
 * Initialize a SMIOL context from Fortran.
 *
 * This function is a simply a wrapper for the SMOIL_init routine that is intended
 * to be called from Fortran. Accordingly, the first argument is of type MPI_Fint
 * (a Fortran integer) rather than MPI_Comm.
 *
 ********************************************************************************/
int SMIOL_fortran_init(MPI_Fint comm, int num_io_tasks, int io_stride,
                       struct SMIOL_context **context)
{
	return SMIOL_init(MPI_Comm_f2c(comm), num_io_tasks, io_stride, context);
}


/********************************************************************************
 *
 * SMIOL_init
 *
 * Initialize a SMIOL context.
 *
 * Initializes a SMIOL context, within which decompositions may be defined and
 * files may be read and written. The input argument comm is an MPI communicator,
 * and the input arguments num_io_tasks and io_stride provide the total number
 * of I/O tasks and the stride between those I/O tasks within the communicator.
 *
 * Upon successful return the context argument points to a valid SMIOL context;
 * otherwise, it is NULL and an error code other than MPI_SUCCESS is returned.
 *
 * Note: It is assumed that MPI_Init has been called prior to this routine, so
 *       that any use of the provided MPI communicator will be valid.
 *
 ********************************************************************************/
int SMIOL_init(MPI_Comm comm, int num_io_tasks, int io_stride,
               struct SMIOL_context **context)
{
	MPI_Comm smiol_comm;

	/*
	 * Before dereferencing context below, ensure that the pointer
	 * the context pointer is not NULL
	 */
	if (context == NULL) {
		return SMIOL_INVALID_ARGUMENT;
	}

	/*
	 * We cannot check for every possible invalid argument for comm, but
	 * at least we can verify that the communicator is not MPI_COMM_NULL
	 */
	if (comm == MPI_COMM_NULL) {
		/* Nullifying (*context) here may result in a memory leak, but this
		 * seems better than disobeying the stated behavior of returning
		 * a NULL context upon failure
		 */
		(*context) = NULL;

		return SMIOL_INVALID_ARGUMENT;
	}

	*context = (struct SMIOL_context *)malloc(sizeof(struct SMIOL_context));
	if ((*context) == NULL) {
		return SMIOL_MALLOC_FAILURE;
	}

	/*
	 * Initialize context
	 */
	(*context)->lib_ierr = 0;
	(*context)->lib_type = SMIOL_LIBRARY_UNKNOWN;

	(*context)->num_io_tasks = num_io_tasks;
	(*context)->io_stride = io_stride;


	/*
	 * Make a duplicate of the MPI communicator for use by SMIOL
	 */
	if (MPI_Comm_dup(comm, &smiol_comm) != MPI_SUCCESS) {
		free((*context));
		(*context) = NULL;
		return SMIOL_MPI_ERROR;
	}
	(*context)->fcomm = MPI_Comm_c2f(smiol_comm);

	if (MPI_Comm_size(smiol_comm, &((*context)->comm_size)) != MPI_SUCCESS) {
		free((*context));
		(*context) = NULL;
		return SMIOL_MPI_ERROR;
	}

	if (MPI_Comm_rank(smiol_comm, &((*context)->comm_rank)) != MPI_SUCCESS) {
		free((*context));
		(*context) = NULL;
		return SMIOL_MPI_ERROR;
	}

	return SMIOL_SUCCESS;
}


/********************************************************************************
 *
 * SMIOL_finalize
 *
 * Finalize a SMIOL context.
 *
 * Finalizes a SMIOL context and frees all memory in the SMIOL_context instance.
 * After this routine is called, no other SMIOL routines that make reference to
 * the finalized context should be called.
 *
 ********************************************************************************/
int SMIOL_finalize(struct SMIOL_context **context)
{
	MPI_Comm smiol_comm;

	/*
	 * If the pointer to the context pointer is NULL, assume we have nothing
	 * to do and declare success
	 */
	if (context == NULL) {
		return SMIOL_SUCCESS;
	}

	if ((*context) == NULL) {
		return SMIOL_SUCCESS;
	}

	smiol_comm = MPI_Comm_f2c((*context)->fcomm);
	if (MPI_Comm_free(&smiol_comm) != MPI_SUCCESS) {
		free((*context));
		(*context) = NULL;
		return SMIOL_MPI_ERROR;
	}

	free((*context));
	(*context) = NULL;

	return SMIOL_SUCCESS;
}


/********************************************************************************
 *
 * SMIOL_inquire
 *
 * Inquire about a SMIOL context.
 *
 * Detailed description.
 *
 ********************************************************************************/
int SMIOL_inquire(void)
{
	return SMIOL_SUCCESS;
}


/********************************************************************************
 *
 * SMIOL_open_file
 *
 * Opens a file within a SMIOL context.
 *
 * Depending on the specified file mode, creates or opens the file specified
 * by filename within the provided SMIOL context.
 *
 * The bufsize argument specifies the size in bytes of the buffer to be attached
 * to the file by I/O tasks; at present this buffer is only used by the Parallel-
 * NetCDF library if the file is opened with a mode of SMIOL_FILE_CREATE or
 * SMIOL_FILE_WRITE. A bufsize of 0 will force the use of the Parallel-NetCDF
 * blocking write interface, while a nonzero value enables the use of the
 * non-blocking, buffered interface for writing.
 *
 * When a file is opened with a mode of SMIOL_FILE_CREATE, the fformat argument
 * is used to set the file format. Otherwise fformat is ignored.
 *
 * Upon successful completion, SMIOL_SUCCESS is returned, and the file handle
 * argument will point to a valid file handle and the current frame for the
 * file will be set to zero. Otherwise, the file handle is NULL and an error
 * code other than SMIOL_SUCCESS is returned.
 *
 ********************************************************************************/
int SMIOL_open_file(struct SMIOL_context *context, const char *filename,
                    int mode, struct SMIOL_file **file, size_t bufsize, int fformat)
{
	int io_group;
	MPI_Comm io_file_comm;
	MPI_Comm io_group_comm;
	int ierr;


	/*
	 * Before dereferencing file below, ensure that the pointer
	 * the file pointer is not NULL
	 */
	if (file == NULL) {
		return SMIOL_INVALID_ARGUMENT;
	}

	/*
	 * Check that context is valid
	 */
	if (context == NULL) {
		return SMIOL_INVALID_ARGUMENT;
	}

	*file = (struct SMIOL_file *)malloc(sizeof(struct SMIOL_file));
	if ((*file) == NULL) {
		return SMIOL_MALLOC_FAILURE;
	}

	/*
	 * Save pointer to context for this file
	 */
	(*file)->context = context;
	(*file)->frame = (SMIOL_Offset) 0;


	/*
	 * Determine whether a task is an I/O task or not, and compute
	 * the I/O group to which each task belongs
	 */
	(*file)->io_task = context->comm_rank % context->io_stride == 0 ? 1 : 0;
	io_group = context->comm_rank / context->io_stride;

	/*
	 * If there are fewer than comm_size / io_stride I/O tasks, some
	 * tasks that were set to I/O tasks above will actually not perform
	 * I/O. Also, place all remainder tasks in the last I/O group
	 */
	if (io_group >= context->num_io_tasks) {
		(*file)->io_task = 0;
		io_group = context->num_io_tasks - 1;
	}

	/*
	 * Create a communicator for communicating within a group of tasks
	 * associated with an I/O task
	 */
	ierr = MPI_Comm_split(MPI_Comm_f2c(context->fcomm), io_group,
	                      context->comm_rank, &io_group_comm);
	if (ierr != MPI_SUCCESS) {
		free((*file));
		(*file) = NULL;
		return SMIOL_MPI_ERROR;
	}
	(*file)->io_group_comm = MPI_Comm_c2f(io_group_comm);


	/*
	 * Create a communicator for collective file I/O operations among
	 * I/O tasks (i.e., io_task == 1)
	 */
	ierr = MPI_Comm_split(MPI_Comm_f2c(context->fcomm), (*file)->io_task,
	                      context->comm_rank, &io_file_comm);
	if (ierr != MPI_SUCCESS) {
		free((*file));
		(*file) = NULL;
		return SMIOL_MPI_ERROR;
	}
	(*file)->io_file_comm = MPI_Comm_c2f(io_file_comm);


	if (mode & SMIOL_FILE_CREATE) {
#ifdef SMIOL_PNETCDF
		if ((*file)->io_task) {
			/*
			 * Convert fformat to a PNetCDF file creation mode
			 */
			int filecmode;
			if (fformat == SMIOL_FORMAT_CDF2) {
				filecmode = NC_64BIT_OFFSET;
			} else if (fformat == SMIOL_FORMAT_CDF5) {
				filecmode = NC_64BIT_DATA;
			} else {
				free((*file));
				(*file) = NULL;
				MPI_Comm_free(&io_file_comm);
				MPI_Comm_free(&io_group_comm);
				return SMIOL_INVALID_FORMAT;
			}

			ierr = ncmpi_create(io_file_comm, filename,
			                    (filecmode | NC_CLOBBER),
			                    MPI_INFO_NULL,
			                    &((*file)->ncidp));
		}
		(*file)->state = PNETCDF_DEFINE_MODE;
#endif
	} else if (mode & SMIOL_FILE_WRITE) {
#ifdef SMIOL_PNETCDF
		if ((*file)->io_task) {
			ierr = ncmpi_open(io_file_comm, filename,
			                  NC_WRITE, MPI_INFO_NULL,
			                  &((*file)->ncidp));
		}
		(*file)->state = PNETCDF_DATA_MODE;
#endif
	} else if (mode & SMIOL_FILE_READ) {
#ifdef SMIOL_PNETCDF
		if ((*file)->io_task) {
			ierr = ncmpi_open(io_file_comm, filename,
			                  NC_NOWRITE, MPI_INFO_NULL,
			                  &((*file)->ncidp));
		}
		(*file)->state = PNETCDF_DATA_MODE;
#endif
	} else {
		free((*file));
		(*file) = NULL;
		MPI_Comm_free(&io_file_comm);
		MPI_Comm_free(&io_group_comm);
		return SMIOL_INVALID_ARGUMENT;
	}

#ifdef SMIOL_PNETCDF
	MPI_Bcast(&ierr, 1, MPI_INT, 0, io_group_comm);
	if (ierr != NC_NOERR) {
		free((*file));
		(*file) = NULL;
		MPI_Comm_free(&io_file_comm);
		MPI_Comm_free(&io_group_comm);
		context->lib_type = SMIOL_LIBRARY_PNETCDF;
		context->lib_ierr = ierr;
		return SMIOL_LIBRARY_ERROR;
	}

	(*file)->bufsize = 0;
	(*file)->n_reqs = 0;
	(*file)->reqs = NULL;

	if (mode & SMIOL_FILE_CREATE || mode & SMIOL_FILE_WRITE) {
		if (bufsize > 0 && (*file)->io_task) {
			(*file)->bufsize = bufsize;
			ierr = ncmpi_buffer_attach((*file)->ncidp,
			                           (MPI_Offset)bufsize);
			(*file)->reqs = malloc(sizeof(int) * (size_t)MAX_REQS);
		}

		if (bufsize > 0) {
			MPI_Bcast(&ierr, 1, MPI_INT, 0, io_group_comm);
			if (ierr != NC_NOERR) {
				if ((*file)->reqs != NULL) {
					free((*file)->reqs);
					(*file)->reqs = NULL;
				}
				free((*file));
				(*file) = NULL;
				MPI_Comm_free(&io_file_comm);
				MPI_Comm_free(&io_group_comm);
				context->lib_type = SMIOL_LIBRARY_PNETCDF;
				context->lib_ierr = ierr;
				return SMIOL_LIBRARY_ERROR;
			}
		}
	}
#endif

	return SMIOL_SUCCESS;
}


/********************************************************************************
 *
 * SMIOL_close_file
 *
 * Closes a file within a SMIOL context.
 *
 * Closes the file associated with the provided file handle. Upon successful
 * completion, SMIOL_SUCCESS is returned, the file will be closed, and all memory
 * that is uniquely associated with the file handle will be deallocated.
 * Otherwise, an error code other than SMIOL_SUCCESS will be returned.
 *
 ********************************************************************************/
int SMIOL_close_file(struct SMIOL_file **file)
{
	MPI_Comm io_file_comm;
	MPI_Comm io_group_comm;
#ifdef SMIOL_PNETCDF
	int ierr;
#endif


	/*
	 * If the pointer to the file pointer is NULL, assume we have nothing
	 * to do and declare success
	 */
	if (file == NULL) {
		return SMIOL_SUCCESS;
	}

	io_file_comm = MPI_Comm_f2c((*file)->io_file_comm);
	io_group_comm = MPI_Comm_f2c((*file)->io_group_comm);

#ifdef SMIOL_PNETCDF
	if ((*file)->io_task) {
		ierr = NC_NOERR;
		if ((*file)->n_reqs > 0) {
			int statuses[MAX_REQS];

			ierr = ncmpi_wait_all((*file)->ncidp, (*file)->n_reqs,
			                      (*file)->reqs, statuses);
			(*file)->n_reqs = 0;
		}
		if ((*file)->reqs != NULL) {
			free((*file)->reqs);
		}
	}
	MPI_Bcast(&ierr, 1, MPI_INT, 0, io_group_comm);
	if (ierr != NC_NOERR) {
		((*file)->context)->lib_type = SMIOL_LIBRARY_PNETCDF;
		((*file)->context)->lib_ierr = ierr;
		free((*file));
		(*file) = NULL;
		return SMIOL_LIBRARY_ERROR;
	}

	ierr = NC_NOERR;
	if ((*file)->io_task && (*file)->bufsize > 0) {
		ierr = ncmpi_buffer_detach((*file)->ncidp);
	}
	MPI_Bcast(&ierr, 1, MPI_INT, 0, io_group_comm);
	if (ierr != NC_NOERR) {
		((*file)->context)->lib_type = SMIOL_LIBRARY_PNETCDF;
		((*file)->context)->lib_ierr = ierr;
		free((*file));
		(*file) = NULL;
		return SMIOL_LIBRARY_ERROR;
	}

	if ((*file)->io_task) {
		ierr = ncmpi_close((*file)->ncidp);
	}
	MPI_Bcast(&ierr, 1, MPI_INT, 0, io_group_comm);
	if (ierr != NC_NOERR) {
		((*file)->context)->lib_type = SMIOL_LIBRARY_PNETCDF;
		((*file)->context)->lib_ierr = ierr;
		free((*file));
		(*file) = NULL;
		return SMIOL_LIBRARY_ERROR;
	}
#endif

	if (MPI_Comm_free(&io_file_comm) != MPI_SUCCESS) {
		free((*file));
		(*file) = NULL;
		return SMIOL_MPI_ERROR;
	}

	if (MPI_Comm_free(&io_group_comm) != MPI_SUCCESS) {
		free((*file));
		(*file) = NULL;
		return SMIOL_MPI_ERROR;
	}

	free((*file));
	(*file) = NULL;

	return SMIOL_SUCCESS;
}


/********************************************************************************
 *
 * SMIOL_define_dim
 *
 * Defines a new dimension in a file.
 *
 * Defines a dimension with the specified name and size in the file associated
 * with the file handle. If a negative value is provided for the size argument,
 * the dimension will be defined as an unlimited or record dimension.
 *
 * Upon successful completion, SMIOL_SUCCESS is returned; otherwise, an error
 * code is returned.
 *
 ********************************************************************************/
int SMIOL_define_dim(struct SMIOL_file *file, const char *dimname, SMIOL_Offset dimsize)
{
#ifdef SMIOL_PNETCDF
	MPI_Comm io_group_comm;
	int dimidp;
	int ierr;
	MPI_Offset len;
#endif

	/*
	 * Check that file handle is valid
	 */
	if (file == NULL) {
		return SMIOL_INVALID_ARGUMENT;
	}

	/*
	 * Check that dimension name is valid
	 */
	if (dimname == NULL) {
		return SMIOL_INVALID_ARGUMENT;
	}

#ifdef SMIOL_PNETCDF
	io_group_comm = MPI_Comm_f2c(file->io_group_comm);

	/*
	 * The parallel-netCDF library does not permit zero-length dimensions
	 */
	if (dimsize == (SMIOL_Offset)0) {
		return SMIOL_INVALID_ARGUMENT;
	}

	/*
	 * Handle unlimited / record dimension specifications
	 */
	if (dimsize < (SMIOL_Offset)0) {
		len = NC_UNLIMITED;
	}
	else {
		len = (MPI_Offset)dimsize;
	}

	/*
	 * If the file is in data mode, then switch it to define mode
	 */
	if (file->state == PNETCDF_DATA_MODE) {
		if (file->io_task) {
			ierr = ncmpi_redef(file->ncidp);
		}
		MPI_Bcast(&ierr, 1, MPI_INT, 0, io_group_comm);
		if (ierr != NC_NOERR) {
			file->context->lib_type = SMIOL_LIBRARY_PNETCDF;
			file->context->lib_ierr = ierr;
			return SMIOL_LIBRARY_ERROR;
		}
		file->state = PNETCDF_DEFINE_MODE;
	}

	if (file->io_task) {
		ierr = ncmpi_def_dim(file->ncidp, dimname, len, &dimidp);
	}
	MPI_Bcast(&ierr, 1, MPI_INT, 0, io_group_comm);
	if (ierr != NC_NOERR) {
		file->context->lib_type = SMIOL_LIBRARY_PNETCDF;
		file->context->lib_ierr = ierr;
		return SMIOL_LIBRARY_ERROR;
	}
#endif

	return SMIOL_SUCCESS;
}


/********************************************************************************
 *
 * SMIOL_inquire_dim
 *
 * Inquires about an existing dimension in a file.
 *
 * Inquire about the size of an existing dimension and optionally inquire if the
 * given dimension is the unlimited dimension or not. If dimsize is a non-NULL
 * pointer then the dimension size will be returned in dimsize. For unlimited
 * dimensions, the current size of the dimension is returned; future writes of
 * additional records to a file can lead to different return sizes for
 * unlimited dimensions.
 *
 * If is_unlimited is a non-NULL pointer and if the inquired dimension is the
 * unlimited dimension, is_unlimited will be set to 1; if the inquired
 * dimension is not the unlimited dimension then is_unlimited will be set to 0.
 *
 * Upon successful completion, SMIOL_SUCCESS is returned; otherwise, an error
 * code is returned.
 *
 ********************************************************************************/
int SMIOL_inquire_dim(struct SMIOL_file *file, const char *dimname,
                      SMIOL_Offset *dimsize, int *is_unlimited)
{
#ifdef SMIOL_PNETCDF
	MPI_Comm io_group_comm;
	int dimidp = 0;
	int ierr;
	MPI_Offset len = 0;
#endif
	/*
	 * Check that file handle is valid
	 */
	if (file == NULL) {
		return SMIOL_INVALID_ARGUMENT;
	}

	/*
	 * Check that dimension name is valid
	 */
	if (dimname == NULL) {
		return SMIOL_INVALID_ARGUMENT;
	}

	/*
	 * Check that dimension size is not NULL
	 */
	if (dimsize == NULL && is_unlimited == NULL) {
		return SMIOL_INVALID_ARGUMENT;
	}

	if (dimsize != NULL) {
		(*dimsize) = (SMIOL_Offset)0;   /* Default dimension size if no library provides a value */
	}

	if (is_unlimited != NULL) {
		(*is_unlimited) = 0; /* Return 0 if no library provides a value */
	}

#ifdef SMIOL_PNETCDF
	io_group_comm = MPI_Comm_f2c(file->io_group_comm);

	if (file->io_task) {
		ierr = ncmpi_inq_dimid(file->ncidp, dimname, &dimidp);
	}
	MPI_Bcast(&ierr, 1, MPI_INT, 0, io_group_comm);
	if (ierr != NC_NOERR) {
		(*dimsize) = (SMIOL_Offset)(-1);  /* TODO: should there be a well-defined invalid size? */
		file->context->lib_type = SMIOL_LIBRARY_PNETCDF;
		file->context->lib_ierr = ierr;
		return SMIOL_LIBRARY_ERROR;
	}

	/*
	 * Inquire about dimsize
	 */
	if (dimsize != NULL) {
		if (file->io_task) {
			ierr = ncmpi_inq_dimlen(file->ncidp, dimidp, &len);
		}
		MPI_Bcast(&ierr, 1, MPI_INT, 0, io_group_comm);
		if (ierr != NC_NOERR) {
			(*dimsize) = (SMIOL_Offset)(-1);  /* TODO: should there be a well-defined invalid size? */
			file->context->lib_type = SMIOL_LIBRARY_PNETCDF;
			file->context->lib_ierr = ierr;
			return SMIOL_LIBRARY_ERROR;
		}

		(*dimsize) = (SMIOL_Offset)len;
/* TO DO: what if SMIOL_Offset is different in size from MPI_LONG */
		MPI_Bcast(dimsize, 1, MPI_LONG, 0, io_group_comm);
	}


	/*
	 * Inquire if this dimension is the unlimited dimension
	 */
	if (is_unlimited != NULL) {
		int unlimdimidp = 0;
		if (file->io_task) {
			ierr = ncmpi_inq_unlimdim(file->ncidp, &unlimdimidp);
		}
		MPI_Bcast(&ierr, 1, MPI_INT, 0, io_group_comm);
		if (ierr != NC_NOERR) {
			file->context->lib_type = SMIOL_LIBRARY_PNETCDF;
			file->context->lib_ierr = ierr;
			return SMIOL_LIBRARY_ERROR;
		}

		if (file->io_task) {
			if (unlimdimidp == dimidp) {
				(*is_unlimited) = 1;
			} else {
				(*is_unlimited) = 0; /* Not the unlimited dim */
			}
		}
		MPI_Bcast(is_unlimited, 1, MPI_INT, 0, io_group_comm);
	}
#endif

	return SMIOL_SUCCESS;
}


/********************************************************************************
 *
 * SMIOL_define_var
 *
 * Defines a new variable in a file.
 *
 * Defines a variable with the specified name, type, and dimensions in an open
 * file pointed to by the file argument. The varname and dimnames arguments
 * are expected to be null-terminated strings, except if the variable has zero
 * dimensions, in which case the dimnames argument may be a NULL pointer.
 *
 * Upon successful completion, SMIOL_SUCCESS is returned; otherwise, an error
 * code is returned.
 *
 ********************************************************************************/
int SMIOL_define_var(struct SMIOL_file *file, const char *varname, int vartype, int ndims, const char **dimnames)
{
#ifdef SMIOL_PNETCDF
	MPI_Comm io_group_comm;
	int *dimids;
	int ierr;
	int i;
	nc_type xtype;
	int varidp;
#endif

	/*
	 * Check that file handle is valid
	 */
	if (file == NULL) {
		return SMIOL_INVALID_ARGUMENT;
	}

	/*
	 * Check that variable name is valid
	 */
	if (varname == NULL) {
		return SMIOL_INVALID_ARGUMENT;
	}

	/*
	 * Check that the variable type is valid - handled below in a library-specific way...
	 */

	/*
	 * Check that variable dimension names are valid
	 */
	if (dimnames == NULL && ndims > 0) {
		return SMIOL_INVALID_ARGUMENT;
	}

#ifdef SMIOL_PNETCDF
	io_group_comm = MPI_Comm_f2c(file->io_group_comm);

	dimids = (int *)malloc(sizeof(int) * (size_t)ndims);
	if (dimids == NULL) {
		return SMIOL_MALLOC_FAILURE;
	}

	/*
	 * Build a list of dimension IDs
	 */
	for (i=0; i<ndims; i++) {
		if (file->io_task) {
			ierr = ncmpi_inq_dimid(file->ncidp,
			                       dimnames[i], &dimids[i]);
		}
		MPI_Bcast(&ierr, 1, MPI_INT, 0, io_group_comm);
		if (ierr != NC_NOERR) {
			free(dimids);
			file->context->lib_type = SMIOL_LIBRARY_PNETCDF;
			file->context->lib_ierr = ierr;
			return SMIOL_LIBRARY_ERROR;
		}
	}

	/*
	 * Translate SMIOL variable type to parallel-netcdf type
	 */
	switch (vartype) {
		case SMIOL_REAL32:
			xtype = NC_FLOAT;
			break;
		case SMIOL_REAL64:
			xtype = NC_DOUBLE;
			break;
		case SMIOL_INT32:
			xtype = NC_INT;
			break;
		case SMIOL_CHAR:
			xtype = NC_CHAR;
			break;
		default:
			free(dimids);
			return SMIOL_INVALID_ARGUMENT;
	}

	/*
	 * If the file is in data mode, then switch it to define mode
	 */
	if (file->state == PNETCDF_DATA_MODE) {
		if (file->io_task) {
			ierr = ncmpi_redef(file->ncidp);
		}
		MPI_Bcast(&ierr, 1, MPI_INT, 0, io_group_comm);
		if (ierr != NC_NOERR) {
			file->context->lib_type = SMIOL_LIBRARY_PNETCDF;
			file->context->lib_ierr = ierr;
			return SMIOL_LIBRARY_ERROR;
		}
		file->state = PNETCDF_DEFINE_MODE;
	}

	/*
	 * Define the variable
	 */
	if (file->io_task) {
		ierr = ncmpi_def_var(file->ncidp, varname, xtype, ndims, dimids,
		                     &varidp);
	}
	MPI_Bcast(&ierr, 1, MPI_INT, 0, io_group_comm);
	if (ierr != NC_NOERR) {
		free(dimids);
		file->context->lib_type = SMIOL_LIBRARY_PNETCDF;
		file->context->lib_ierr = ierr;
		return SMIOL_LIBRARY_ERROR;
	}

	free(dimids);
#endif

	return SMIOL_SUCCESS;
}


/********************************************************************************
 *
 * SMIOL_inquire_var
 *
 * Inquires about an existing variable in a file.
 *
 * Inquires about a variable in a file, and optionally returns the type
 * of the variable, the dimensionality of the variable, and the names of
 * the dimensions of the variable. Which properties of the variable to return
 * (type, dimensionality, or dimension names) is indicated by the status of
 * the pointers for the corresponding properties: if the pointer is a non-NULL
 * pointer, the property will be set upon successful completion of this routine.
 *
 * If the names of a variable's dimensions are requested (by providing a non-NULL
 * actual argument for dimnames), the size of the dimnames array must be at least
 * the number of dimensions in the variable, and each character string pointed
 * to by an element of dimnames must be large enough to accommodate the corresponding
 * dimension name.
 *
 ********************************************************************************/
int SMIOL_inquire_var(struct SMIOL_file *file, const char *varname, int *vartype, int *ndims, char **dimnames)
{
#ifdef SMIOL_PNETCDF
	MPI_Comm io_group_comm;
	int *dimids;
	int varidp = 0;
	int ierr;
	int i;
	int xtypep;
	int ndimsp = 0;
#endif

	/*
	 * Check that file handle is valid
	 */
	if (file == NULL) {
		return SMIOL_INVALID_ARGUMENT;
	}

	/*
	 * Check that variable name is valid
	 */
	if (varname == NULL) {
		return SMIOL_INVALID_ARGUMENT;
	}

	/*
	 * If all output arguments are NULL, we can return early
	 */
	if (vartype == NULL && ndims == NULL && dimnames == NULL) {
		return SMIOL_SUCCESS;
	}

	/*
	 * Provide default values for output arguments in case
	 * no library-specific below is active
	 */
	if (vartype != NULL) {
		*vartype = SMIOL_UNKNOWN_VAR_TYPE;
	}
	if (ndims != NULL) {
		*ndims = 0;
	}

#ifdef SMIOL_PNETCDF
	io_group_comm = MPI_Comm_f2c(file->io_group_comm);

	/*
	 * Get variable ID
	 */
	if (file->io_task) {
		ierr = ncmpi_inq_varid(file->ncidp, varname, &varidp);
	}
	MPI_Bcast(&ierr, 1, MPI_INT, 0, io_group_comm);
	if (ierr != NC_NOERR) {
		file->context->lib_type = SMIOL_LIBRARY_PNETCDF;
		file->context->lib_ierr = ierr;
		return SMIOL_LIBRARY_ERROR;
	}

	/*
	 * If requested, inquire about variable type
	 */
	if (vartype != NULL) {
		if (file->io_task) {
			ierr = ncmpi_inq_vartype(file->ncidp, varidp, &xtypep);
		}
		MPI_Bcast(&ierr, 1, MPI_INT, 0, io_group_comm);
		if (ierr != NC_NOERR) {
			file->context->lib_type = SMIOL_LIBRARY_PNETCDF;
			file->context->lib_ierr = ierr;
			return SMIOL_LIBRARY_ERROR;
		}
		MPI_Bcast(&xtypep, 1, MPI_INT, 0, io_group_comm);

		/* Convert parallel-netCDF variable type to SMIOL variable type */
		switch (xtypep) {
			case NC_FLOAT:
				*vartype = SMIOL_REAL32;
				break;
			case NC_DOUBLE:
				*vartype = SMIOL_REAL64;
				break;
			case NC_INT:
				*vartype = SMIOL_INT32;
				break;
			case NC_CHAR:
				*vartype = SMIOL_CHAR;
				break;
			default:
				*vartype = SMIOL_UNKNOWN_VAR_TYPE;
		}
	}

	/*
	 * All remaining properties will require the number of dimensions
	 */
	if (ndims != NULL || dimnames != NULL) {
		if (file->io_task) {
			ierr = ncmpi_inq_varndims(file->ncidp, varidp, &ndimsp);
		}
		MPI_Bcast(&ierr, 1, MPI_INT, 0, io_group_comm);
		if (ierr != NC_NOERR) {
			file->context->lib_type = SMIOL_LIBRARY_PNETCDF;
			file->context->lib_ierr = ierr;
			return SMIOL_LIBRARY_ERROR;
		}
		MPI_Bcast(&ndimsp, 1, MPI_INT, 0, io_group_comm);
	}

	/*
	 * If requested, inquire about dimensionality
	 */
	if (ndims != NULL) {
		*ndims = ndimsp;
	}

	/*
	 * If requested, inquire about dimension names
	 */
	if (dimnames != NULL) {
		dimids = (int *)malloc(sizeof(int) * (size_t)ndimsp);
		if (dimids == NULL) {
			return SMIOL_MALLOC_FAILURE;
		}

		if (file->io_task) {
			ierr = ncmpi_inq_vardimid(file->ncidp, varidp, dimids);
		}
		MPI_Bcast(&ierr, 1, MPI_INT, 0, io_group_comm);
		if (ierr != NC_NOERR) {
			file->context->lib_type = SMIOL_LIBRARY_PNETCDF;
			file->context->lib_ierr = ierr;
			free(dimids);
			return SMIOL_LIBRARY_ERROR;
		}

		for (i = 0; i < ndimsp; i++) {
			int len;

			if (dimnames[i] == NULL) {
				return SMIOL_INVALID_ARGUMENT;
			}
			if (file->io_task) {
				ierr = ncmpi_inq_dimname(file->ncidp, dimids[i],
				                         dimnames[i]);
			}
			MPI_Bcast(&ierr, 1, MPI_INT, 0, io_group_comm);
			if (ierr != NC_NOERR) {
				file->context->lib_type = SMIOL_LIBRARY_PNETCDF;
				file->context->lib_ierr = ierr;
				free(dimids);
				return SMIOL_LIBRARY_ERROR;
			}

			if (file->io_task) {
				len = (int)strnlen(dimnames[i],
				                   (size_t)NC_MAX_NAME);
				len++; /* Include the terminating '\0' character */
			}
			MPI_Bcast(&len, 1, MPI_INT, 0, io_group_comm);
			MPI_Bcast(dimnames[i], len, MPI_CHAR, 0, io_group_comm);
		}

		free(dimids);
	}
#endif

	return SMIOL_SUCCESS;
}


/********************************************************************************
 *
 * SMIOL_put_var
 *
 * Writes a variable to a file.
 *
 * Given a pointer to a SMIOL file that was previously opened with write access
 * and the name of a variable previously defined in the file with a call to
 * SMIOL_define_var, this routine will write the contents of buf to the variable
 * according to the decomposition described by decomp.
 *
 * If decomp is not NULL, the variable is assumed to be decomposed across MPI
 * ranks, and all ranks with non-zero-sized partitions of the variable must
 * provide a valid buffer. For decomposed variables, all MPI ranks must provide
 * a non-NULL decomp, regardless of whether a rank has a non-zero-sized
 * partition of the variable.
 *
 * If the variable is not decomposed -- that is, all ranks store identical
 * values for the entire variable -- all MPI ranks must provide a NULL pointer
 * for the decomp argument. As currently implemented, this routine will write
 * the buffer for MPI rank 0 to the variable; however, this behavior should not
 * be relied on.
 *
 * If the variable has been successfully written to the file, SMIOL_SUCCESS will
 * be returned. Otherwise, an error code indicating the nature of the failure
 * will be returned.
 *
 ********************************************************************************/
int SMIOL_put_var(struct SMIOL_file *file, const char *varname,
                  const struct SMIOL_decomp *decomp, const void *buf)
{
	int ierr;
	int ndims;
	size_t element_size;
	size_t basic_size;
	int has_unlimited_dim;
	void *out_buf = NULL;
	size_t *start;
	size_t *count;

	void *agg_buf = NULL;
	const void *agg_buf_cnst = NULL;


	/*
	 * Basic checks on arguments
	 */
	if (file == NULL || varname == NULL) {
		return SMIOL_INVALID_ARGUMENT;
	}

	/*
	 * Work out the start[] and count[] arrays for writing this variable
	 * in parallel
	 */
	ierr = build_start_count(file, varname, decomp,
	                         START_COUNT_WRITE, &element_size, &basic_size, &ndims,
	                         &has_unlimited_dim,
	                         &start, &count);
	if (ierr != SMIOL_SUCCESS) {
		return ierr;
	}

	/*
	 * Communicate elements of this field from MPI ranks that compute those
	 * elements to MPI ranks that write those elements. This only needs to
	 * be done for decomposed variables.
	 */
	if (decomp) {
		out_buf = malloc(element_size * decomp->io_count);
		if (out_buf == NULL) {
			free(start);
			free(count);

			return SMIOL_MALLOC_FAILURE;
		}

		if (decomp->agg_factor != 1) {
			MPI_Datatype dtype;
			MPI_Comm agg_comm;

			ierr = MPI_Type_contiguous((int)element_size,
			                           MPI_UINT8_T, &dtype);
			if (ierr != MPI_SUCCESS) {
				fprintf(stderr, "MPI_Type_contiguous failed with code %i\n", ierr);
				return SMIOL_MPI_ERROR;
			}

			ierr = MPI_Type_commit(&dtype);
			if (ierr != MPI_SUCCESS) {
				fprintf(stderr, "MPI_Type_commit failed with code %i\n", ierr);
				return SMIOL_MPI_ERROR;
			}

			agg_buf = malloc(element_size * decomp->n_compute_agg);
			if (agg_buf == NULL && decomp->n_compute_agg > 0) {
				return SMIOL_MALLOC_FAILURE;
			}

			agg_comm = MPI_Comm_f2c(decomp->agg_comm);

			ierr = MPI_Gatherv((const void *)buf,
			                   (int)decomp->n_compute, dtype,
			                   (void *)agg_buf,
			                   (const int *)decomp->counts,
			                   (const int *)decomp->displs,
			                   dtype, 0, agg_comm);
			if (ierr != MPI_SUCCESS) {
				fprintf(stderr, "MPI_Gatherv failed with code %i\n", ierr);
				return SMIOL_MPI_ERROR;
			}

			ierr = MPI_Type_free(&dtype);
			if (ierr != MPI_SUCCESS) {
				fprintf(stderr, "MPI_Type_free failed with code %i\n", ierr);
				return SMIOL_MPI_ERROR;
			}

			agg_buf_cnst = agg_buf;
		} else {
			agg_buf_cnst = buf;
		}

		ierr = transfer_field(decomp, SMIOL_COMP_TO_IO,
		                      element_size, agg_buf_cnst, out_buf);
		if (ierr != SMIOL_SUCCESS) {
			free(start);
			free(count);
			free(out_buf);
			return ierr;
		}

		if (decomp->agg_factor != 1) {
			free(agg_buf);
		}
	}

/* TO DO: could check that out_buf has size zero if not file->io_task */

	/*
	 * Write out_buf
	 */
#ifdef SMIOL_PNETCDF
	{
		int j;
		int varidp = 0;
		const void *buf_p;
		MPI_Offset *mpi_start;
		MPI_Offset *mpi_count;
		MPI_Comm io_group_comm;
		MPI_Comm io_file_comm;

		io_group_comm = MPI_Comm_f2c(file->io_group_comm);
		io_file_comm = MPI_Comm_f2c(file->io_file_comm);

		if (file->state == PNETCDF_DEFINE_MODE) {
			if (file->io_task) {
				ierr = ncmpi_enddef(file->ncidp);
			}
			MPI_Bcast(&ierr, 1, MPI_INT, 0, io_group_comm);
			if (ierr != NC_NOERR) {
				file->context->lib_type = SMIOL_LIBRARY_PNETCDF;
				file->context->lib_ierr = ierr;

				if (decomp) {
					free(out_buf);
				}
				free(start);
				free(count);

				return SMIOL_LIBRARY_ERROR;
			}
			file->state = PNETCDF_DATA_MODE;
		}

		if (file->io_task) {
			ierr = ncmpi_inq_varid(file->ncidp, varname, &varidp);
		}
		MPI_Bcast(&ierr, 1, MPI_INT, 0, io_group_comm);
		if (ierr != NC_NOERR) {
			file->context->lib_type = SMIOL_LIBRARY_PNETCDF;
			file->context->lib_ierr = ierr;

			if (decomp) {
				free(out_buf);
			}
			free(start);
			free(count);

			return SMIOL_LIBRARY_ERROR;
		}

		if (decomp) {
			buf_p = out_buf;
		} else {
			buf_p = buf;
		}

		mpi_start = malloc(sizeof(MPI_Offset) * (size_t)ndims);
		if (mpi_start == NULL) {
			free(start);
			free(count);

			return SMIOL_MALLOC_FAILURE;
		}

		mpi_count = malloc(sizeof(MPI_Offset) * (size_t)ndims);
		if (mpi_count == NULL) {
			free(start);
			free(count);
			free(mpi_start);

			return SMIOL_MALLOC_FAILURE;
		}

		for (j = 0; j < ndims; j++) {
			mpi_start[j] = (MPI_Offset)start[j];
			mpi_count[j] = (MPI_Offset)count[j];
		}

		if (file->io_task) {
			ierr = write_chunk_pnetcdf(file,
			                           varidp,
			                           ndims,
			                           has_unlimited_dim,
			                           basic_size,
			                           io_file_comm,
			                           buf_p,
			                           mpi_start,
			                           mpi_count
			                          );
		}

		MPI_Bcast(&ierr, 1, MPI_INT, 0, io_group_comm);

		free(mpi_start);
		free(mpi_count);

		if (ierr != NC_NOERR) {
			file->context->lib_type = SMIOL_LIBRARY_PNETCDF;
			file->context->lib_ierr = ierr;

			if (decomp) {
				free(out_buf);
			}
			free(start);
			free(count);

			return SMIOL_LIBRARY_ERROR;
		}
	}
#endif

	/*
	 * Free up memory before returning
	 */
	if (decomp) {
		free(out_buf);
	}

	free(start);
	free(count);

	return SMIOL_SUCCESS;
}


/********************************************************************************
 *
 * SMIOL_get_var
 *
 * Reads a variable from a file.
 *
 * Given a pointer to a SMIOL file and the name of a variable previously defined
 * in the file, this routine will read the contents of the variable into buf
 * according to the decomposition described by decomp.
 *
 * If decomp is not NULL, the variable is assumed to be decomposed across MPI
 * ranks, and all ranks with non-zero-sized partitions of the variable must
 * provide a valid buffer. For decomposed variables, all MPI ranks must provide
 * a non-NULL decomp, regardless of whether a rank has a non-zero-sized
 * partition of the variable.
 *
 * If the variable is not decomposed -- that is, all ranks load identical
 * values for the entire variable -- all MPI ranks must provide a NULL pointer
 * for the decomp argument.
 *
 * If the variable has been successfully read from the file, SMIOL_SUCCESS will
 * be returned. Otherwise, an error code indicating the nature of the failure
 * will be returned.
 *
 ********************************************************************************/
int SMIOL_get_var(struct SMIOL_file *file, const char *varname,
                  const struct SMIOL_decomp *decomp, void *buf)
{
	int ierr;
	int ndims;
	size_t element_size;
	size_t basic_size;
	int has_unlimited_dim;
	void *in_buf = NULL;
	size_t *start;
	size_t *count;

	void *agg_buf = NULL;

	MPI_Comm io_group_comm;
	MPI_Comm io_file_comm;


	/*
	 * Basic checks on arguments
	 */
	if (file == NULL || varname == NULL) {
		return SMIOL_INVALID_ARGUMENT;
	}

	io_group_comm = MPI_Comm_f2c(file->io_group_comm);
	io_file_comm = MPI_Comm_f2c(file->io_file_comm);

	/*
	 * Work out the start[] and count[] arrays for reading this variable
	 * in parallel
	 */
	ierr = build_start_count(file, varname, decomp,
	                         START_COUNT_READ, &element_size, &basic_size, &ndims,
	                         &has_unlimited_dim,
	                         &start, &count);
	if (ierr != SMIOL_SUCCESS) {
		return ierr;
	}

	/*
	 * If this variable is decomposed, allocate a buffer into which
	 * the variable will be read using the I/O decomposition; later,
	 * elements this buffer will be transferred to MPI ranks that compute
	 * on those elements
	 */
	if (decomp) {
		in_buf = malloc(element_size * decomp->io_count);
		if (in_buf == NULL) {
			free(start);
			free(count);

			return SMIOL_MALLOC_FAILURE;
		}

#ifndef SMIOL_PNETCDF
		/*
		 * If no file library provides values for the memory pointed to
		 * by in_buf, the transfer_field call later will transfer
		 * garbage to the output buffer; to avoid returning
		 * non-deterministic values to the caller in this case,
		 * initialize in_buf.
		 */
		memset(in_buf, 0, element_size * decomp->io_count);
		
#endif
	}

/* MGD TO DO: could verify that if not file->io_task, then size of in_buf is zero */

	/*
	 * Read in_buf
	 */
#ifdef SMIOL_PNETCDF
	{
		int j;
		int varidp = 0;
		void *buf_p;
		MPI_Offset *mpi_start;
		MPI_Offset *mpi_count;

		if (file->state == PNETCDF_DEFINE_MODE) {
			if (file->io_task) {
				ierr = ncmpi_enddef(file->ncidp);
			}
			MPI_Bcast(&ierr, 1, MPI_INT, 0, io_group_comm);
			if (ierr != NC_NOERR) {
				file->context->lib_type = SMIOL_LIBRARY_PNETCDF;
				file->context->lib_ierr = ierr;

				if (decomp) {
					free(in_buf);
				}
				free(start);
				free(count);

				return SMIOL_LIBRARY_ERROR;
			}
			file->state = PNETCDF_DATA_MODE;
		}

		if (file->io_task) {
			ierr = ncmpi_inq_varid(file->ncidp, varname, &varidp);
		}
		MPI_Bcast(&ierr, 1, MPI_INT, 0, io_group_comm);
		if (ierr != NC_NOERR) {
			file->context->lib_type = SMIOL_LIBRARY_PNETCDF;
			file->context->lib_ierr = ierr;

			if (decomp) {
				free(in_buf);
			}
			free(start);
			free(count);

			return SMIOL_LIBRARY_ERROR;
		}

		if (decomp) {
			buf_p = in_buf;
		} else {
			buf_p = buf;
		}

		mpi_start = malloc(sizeof(MPI_Offset) * (size_t)ndims);
		if (mpi_start == NULL) {
			free(start);
			free(count);

			return SMIOL_MALLOC_FAILURE;
		}

		mpi_count = malloc(sizeof(MPI_Offset) * (size_t)ndims);
		if (mpi_count == NULL) {
			free(start);
			free(count);
			free(mpi_start);

			return SMIOL_MALLOC_FAILURE;
		}

		for (j = 0; j < ndims; j++) {
			mpi_start[j] = (MPI_Offset)start[j];
			mpi_count[j] = (MPI_Offset)count[j];
		}

		ierr = NC_NOERR;
		if (file->io_task) {
			/*
			 * Finish and flush any pending writes to this file
			 * before reading back a variable
			 */
			if (file->n_reqs > 0) {
				int statuses[MAX_REQS];

				ierr = ncmpi_wait_all(file->ncidp, file->n_reqs,
				                      file->reqs, statuses);
				file->n_reqs = 0;

				if (ierr == NC_NOERR) {
					ierr = ncmpi_sync(file->ncidp);
				}
			}
			if (ierr == NC_NOERR) {
				ierr = read_chunk_pnetcdf(file,
				                          varidp,
				                          ndims,
				                          has_unlimited_dim,
				                          basic_size,
				                          io_file_comm,
				                          buf_p,
				                          mpi_start,
				                          mpi_count
				                         );
			}
		}
		MPI_Bcast(&ierr, 1, MPI_INT, 0, io_group_comm);

		free(mpi_start);
		free(mpi_count);

		if (ierr != NC_NOERR) {
			file->context->lib_type = SMIOL_LIBRARY_PNETCDF;
			file->context->lib_ierr = ierr;

			if (decomp) {
				free(in_buf);
			}
			free(start);
			free(count);

			return SMIOL_LIBRARY_ERROR;
		}
	}
#endif

	/*
	 * Free start/count arrays
	 */
	free(start);
	free(count);

	/*
	 * Communicate elements of this field from MPI ranks that read those
	 * elements to MPI ranks that compute those elements. This only needs to
	 * be done for decomposed variables.
	 */
	if (decomp) {
		if (decomp->agg_factor != 1) {
			agg_buf = malloc(element_size * decomp->n_compute_agg);
			if (agg_buf == NULL && decomp->n_compute_agg > 0) {
				return SMIOL_MALLOC_FAILURE;
			}
		} else {
			agg_buf = buf;
		}

		ierr = transfer_field(decomp, SMIOL_IO_TO_COMP,
		                      element_size, in_buf, agg_buf);

		if (decomp->agg_factor != 1) {
			MPI_Datatype dtype = MPI_DATATYPE_NULL;
			MPI_Comm agg_comm;

			ierr = MPI_Type_contiguous((int)element_size,
			                           MPI_UINT8_T, &dtype);
			if (ierr != MPI_SUCCESS) {
				fprintf(stderr, "MPI_Type_contiguous failed with code %i\n", ierr);
				return SMIOL_MPI_ERROR;
			}

			ierr = MPI_Type_commit(&dtype);
			if (ierr != MPI_SUCCESS) {
				fprintf(stderr, "MPI_Type_commit failed with code %i\n", ierr);
				return SMIOL_MPI_ERROR;
			}

			agg_comm = MPI_Comm_f2c(decomp->agg_comm);

			ierr = MPI_Scatterv((const void *)agg_buf,
			                    (const int*)decomp->counts,
			                    (const int *)decomp->displs,
			                    dtype, (void *)buf,
			                    (int)decomp->n_compute,
			                    dtype, 0, agg_comm);
			if (ierr != MPI_SUCCESS) {
				fprintf(stderr, "MPI_Scatterv failed with code %i\n", ierr);
				return SMIOL_MPI_ERROR;
			}

			free(agg_buf);

			ierr = MPI_Type_free(&dtype);
			if (ierr != MPI_SUCCESS) {
				fprintf(stderr, "MPI_Type_free failed with code %i\n", ierr);
				return SMIOL_MPI_ERROR;
			}
		}

		free(in_buf);

		if (ierr != SMIOL_SUCCESS) {
			return ierr;
		}
	} else {
		/*
		 * For non-decomposed variables, broadcast from I/O tasks
		 * to other tasks in each I/O group
		 */
		MPI_Bcast(buf, (int)element_size, MPI_CHAR, 0, io_group_comm);
	}

	return SMIOL_SUCCESS;
}


/********************************************************************************
 *
 * SMIOL_define_att
 *
 * Defines a new attribute in a file.
 *
 * Defines a new attribute for a variable if varname is not NULL,
 * or a global attribute otherwise. The type of the attribute must be one
 * of SMIOL_REAL32, SMIOL_REAL64, SMIOL_INT32, or SMIOL_CHAR.
 *
 * If the attribute has been successfully defined for the variable or file,
 * SMIOL_SUCCESS is returned.
 *
 ********************************************************************************/
int SMIOL_define_att(struct SMIOL_file *file, const char *varname,
                     const char *att_name, int att_type, const void *att)
{
#ifdef SMIOL_PNETCDF
	MPI_Comm io_group_comm;
	int ierr;
	int varidp = 0;
	nc_type xtype;
#endif

	/*
	 * Check validity of arguments
	 */
	if (file == NULL || att_name == NULL || att == NULL) {
		return SMIOL_INVALID_ARGUMENT;
	}

	/*
	 * Checks for valid attribute type are handled in library-specific
	 * code, below
	 */

#ifdef SMIOL_PNETCDF
	io_group_comm = MPI_Comm_f2c(file->io_group_comm);

	/*
	 * If varname was provided, get the variable ID; else, the attribute
	 * is a global attribute not associated with a specific variable
	 */
	if (varname != NULL) {
		if (file->io_task) {
			ierr = ncmpi_inq_varid(file->ncidp, varname, &varidp);
		}
		MPI_Bcast(&ierr, 1, MPI_INT, 0, io_group_comm);
		if (ierr != NC_NOERR) {
			file->context->lib_type = SMIOL_LIBRARY_PNETCDF;
			file->context->lib_ierr = ierr;
			return SMIOL_LIBRARY_ERROR;
		}
	} else {
		varidp = NC_GLOBAL;
	}

	/*
	 * Translate SMIOL variable type to parallel-netcdf type
	 */
	switch (att_type) {
		case SMIOL_REAL32:
			xtype = NC_FLOAT;
			break;
		case SMIOL_REAL64:
			xtype = NC_DOUBLE;
			break;
		case SMIOL_INT32:
			xtype = NC_INT;
			break;
		case SMIOL_CHAR:
			xtype = NC_CHAR;
			break;
		default:
			return SMIOL_INVALID_ARGUMENT;
	}

	/*
	 * If the file is in data mode, then switch it to define mode
	 */
	if (file->state == PNETCDF_DATA_MODE) {
		if (file->io_task) {
			ierr = ncmpi_redef(file->ncidp);
		}
		MPI_Bcast(&ierr, 1, MPI_INT, 0, io_group_comm);
		if (ierr != NC_NOERR) {
			file->context->lib_type = SMIOL_LIBRARY_PNETCDF;
			file->context->lib_ierr = ierr;
			return SMIOL_LIBRARY_ERROR;
		}
		file->state = PNETCDF_DEFINE_MODE;
	}

	/*
	 * Add the attribute to the file
	 */
	if (file->io_task) {
		if (att_type == SMIOL_CHAR) {
			ierr = ncmpi_put_att(file->ncidp, varidp, att_name,
			                     xtype, (MPI_Offset)strlen(att),
			                     (const char *)att);
		} else {
			ierr = ncmpi_put_att(file->ncidp, varidp, att_name,
			                     xtype, (MPI_Offset)1,
			                     (const char *)att);
		}
	}
	MPI_Bcast(&ierr, 1, MPI_INT, 0, io_group_comm);
	if (ierr != NC_NOERR) {
		file->context->lib_type = SMIOL_LIBRARY_PNETCDF;
		file->context->lib_ierr = ierr;
		return SMIOL_LIBRARY_ERROR;
	}
#endif

	return SMIOL_SUCCESS;
}


/********************************************************************************
 *
 * SMIOL_inquire_att
 *
 * Inquires about an attribute in a file.
 *
 * Inquires about a variable attribute if varname is not NULL, or a global
 * attribute otherwise.
 *
 * If the requested attribute is found, SMIOL_SUCCESS is returned and the memory
 * pointed to by the att argument will contain the attribute value.
 *
 * For character string attributes, no bytes beyond the length of the attribute
 * in the file will be modified in the att argument, and no '\0' character will
 * be added. Therefore, calling code may benefit from initializing character
 * strings before calling this routine.
 *
 * If SMIOL was not compiled with support for any file library, the att_type
 * output argument will always be set to SMIOL_UNKNOWN_VAR_TYPE, and the att_len
 * output argument will always be set to -1; the value of the att output
 * argument will be unchanged.
 *
 ********************************************************************************/
int SMIOL_inquire_att(struct SMIOL_file *file, const char *varname,                                                                  
                      const char *att_name, int *att_type,
                      SMIOL_Offset *att_len, void *att)
{
#ifdef SMIOL_PNETCDF
	MPI_Comm io_group_comm;
	int ierr;
	int varidp = 0;
	nc_type xtypep = 0;
	MPI_Offset lenp = 0;
#endif

	/*
	 * Check validity of arguments
	 */
	if (file == NULL || att_name == NULL) {
		return SMIOL_INVALID_ARGUMENT;
	}

	/*
	 * Set output arguments in case no library sets them later
	 */
	if (att_len != NULL) {
		*att_len = (SMIOL_Offset)-1;
	}

	if (att_type != NULL) {
		*att_type = SMIOL_UNKNOWN_VAR_TYPE;
	}

#ifdef SMIOL_PNETCDF
	io_group_comm = MPI_Comm_f2c(file->io_group_comm);

	/*
	 * If varname was provided, get the variable ID; else, the inquiry is
	 * is for a global attribute not associated with a specific variable
	 */
	if (varname != NULL) {
		if (file->io_task) {
			ierr = ncmpi_inq_varid(file->ncidp, varname, &varidp);
		}
		MPI_Bcast(&ierr, 1, MPI_INT, 0, io_group_comm);
		if (ierr != NC_NOERR) {
			file->context->lib_type = SMIOL_LIBRARY_PNETCDF;
			file->context->lib_ierr = ierr;
			return SMIOL_LIBRARY_ERROR;
		}
	} else {
		varidp = NC_GLOBAL;
	}

	/*
	 * Inquire about attribute type and length
	 */
	if (att != NULL || att_type != NULL || att_len != NULL) {
		if (file->io_task) {
			ierr = ncmpi_inq_att(file->ncidp, varidp, att_name,
			                     &xtypep, &lenp);
		}
		MPI_Bcast(&ierr, 1, MPI_INT, 0, io_group_comm);
		if (ierr != NC_NOERR) {
			file->context->lib_type = SMIOL_LIBRARY_PNETCDF;
			file->context->lib_ierr = ierr;
			return SMIOL_LIBRARY_ERROR;
		}

		MPI_Bcast(&lenp, sizeof(MPI_Offset), MPI_BYTE, 0,
		          io_group_comm);
		MPI_Bcast(&xtypep, sizeof(nc_type), MPI_BYTE, 0,
		          io_group_comm);

		if (att_type != NULL) {
			/* Convert parallel-netCDF type to SMIOL type */
			switch (xtypep) {
				case NC_FLOAT:
					*att_type = SMIOL_REAL32;
					break;
				case NC_DOUBLE:
					*att_type = SMIOL_REAL64;
					break;
				case NC_INT:
					*att_type = SMIOL_INT32;
					break;
				case NC_CHAR:
					*att_type = SMIOL_CHAR;
					break;
				default:
					*att_type = SMIOL_UNKNOWN_VAR_TYPE;
			}
		}

		if (att_len != NULL) {
			*att_len = lenp;
		}
	}


	/*
	 * Inquire about attribute value if requested
	 */
	if (att != NULL) {
		if (file->io_task) {
			ierr = ncmpi_get_att(file->ncidp, varidp, att_name,
			                     att);
		}
		MPI_Bcast(&ierr, 1, MPI_INT, 0, io_group_comm);
		if (ierr != NC_NOERR) {
			file->context->lib_type = SMIOL_LIBRARY_PNETCDF;
			file->context->lib_ierr = ierr;
			return SMIOL_LIBRARY_ERROR;
		}

		switch (xtypep) {
		case NC_FLOAT:
			ierr = MPI_Bcast(att, 1, MPI_FLOAT, 0, io_group_comm);
			break;
		case NC_DOUBLE:
			ierr = MPI_Bcast(att, 1, MPI_DOUBLE, 0, io_group_comm);
			break;
		case NC_INT:
			ierr = MPI_Bcast(att, 1, MPI_INT, 0, io_group_comm);
			break;
		case NC_CHAR:
			ierr = MPI_Bcast(att, (int)lenp, MPI_CHAR, 0,
			                 io_group_comm);
			break;
		}
	}
#endif

	return SMIOL_SUCCESS;
}


/********************************************************************************
 *
 * SMIOL_sync_file
 *
 * Forces all in-memory data to be flushed to disk.
 *
 * Upon success, all in-memory data for the file associatd with the file
 * handle will be flushed to the file system and SMIOL_SUCCESS will be
 * returned; otherwise, an error code is returned.
 *
 ********************************************************************************/
int SMIOL_sync_file(struct SMIOL_file *file)
{
#ifdef SMIOL_PNETCDF
	MPI_Comm io_group_comm;
	int ierr;
#endif

	/*
	 * Check that file is valid
	 */
	if (file == NULL) {
		return SMIOL_INVALID_ARGUMENT;
	}

#ifdef SMIOL_PNETCDF
	io_group_comm = MPI_Comm_f2c(file->io_group_comm);

	/*
	 * If the file is in define mode then switch it into data mode
	 */
	if (file->state == PNETCDF_DEFINE_MODE) {
		if (file->io_task) {
			ierr = ncmpi_enddef(file->ncidp);
		}
		MPI_Bcast(&ierr, 1, MPI_INT, 0, io_group_comm);
		if (ierr != NC_NOERR) {
			file->context->lib_type = SMIOL_LIBRARY_PNETCDF;
			file->context->lib_ierr = ierr;
			return SMIOL_LIBRARY_ERROR;
		}
		file->state = PNETCDF_DATA_MODE;
	}

	if (file->io_task) {
		ierr = NC_NOERR;

		if (file->n_reqs > 0) {
			int statuses[MAX_REQS];

			ierr = ncmpi_wait_all(file->ncidp, file->n_reqs,
			                      file->reqs, statuses);
			file->n_reqs = 0;
		}

		if (ierr == NC_NOERR) {
			ierr = ncmpi_sync(file->ncidp);
		}
	}
	MPI_Bcast(&ierr, 1, MPI_INT, 0, io_group_comm);
	if (ierr != NC_NOERR) {
		file->context->lib_type = SMIOL_LIBRARY_PNETCDF;
		file->context->lib_ierr = ierr;
		return SMIOL_LIBRARY_ERROR;
	}
#endif

	return SMIOL_SUCCESS;
}


/********************************************************************************
 *
 * SMIOL_error_string
 *
 * Returns an error string for a specified error code.
 *
 * Returns an error string corresponding to a SMIOL error code. If the error code is
 * SMIOL_LIBRARY_ERROR and a valid SMIOL context is available, the SMIOL_lib_error_string
 * function should be called instead. The error string is null-terminated, but it
 * does not contain a newline character.
 *
 ********************************************************************************/
const char *SMIOL_error_string(int errno)
{
	switch (errno) {
	case SMIOL_SUCCESS:
		return "Success!";
	case SMIOL_MALLOC_FAILURE:
		return "malloc returned a null pointer";
	case SMIOL_INVALID_ARGUMENT:
		return "invalid subroutine argument";
	case SMIOL_MPI_ERROR:
		return "internal MPI call failed";
	case SMIOL_FORTRAN_ERROR:
		return "Fortran wrapper detected an inconsistency in C return values";
	case SMIOL_LIBRARY_ERROR:
		return "bad return code from a library call";
	case SMIOL_WRONG_ARG_TYPE:
		return "argument is of the wrong type";
	case SMIOL_INSUFFICIENT_ARG:
		return "argument is of insufficient size";
	case SMIOL_INVALID_FORMAT:
		return "invalid format for file creation";
	default:
		return "Unknown error";
	}
}


/********************************************************************************
 *
 * SMIOL_lib_error_string
 *
 * Returns an error string for a third-party library called by SMIOL.
 *
 * Returns an error string corresponding to an error that was generated by
 * a third-party library that was called by SMIOL. The library that was the source
 * of the error, as well as the library-specific error code, are retrieved from
 * a SMIOL context. If successive library calls resulted in errors, only the error
 * string for the last of these errors will be returned. The error string is
 * null-terminated, but it does not contain a newline character.
 *
 ********************************************************************************/
const char *SMIOL_lib_error_string(struct SMIOL_context *context)
{
	if (context == NULL) {
		return "SMIOL_context argument is a NULL pointer";
	}

	switch (context->lib_type) {
#ifdef SMIOL_PNETCDF
	case SMIOL_LIBRARY_PNETCDF:
		return ncmpi_strerror(context->lib_ierr);
#endif
	default:
		return "Could not find matching library for the source of the error";
	}
}


/********************************************************************************
 *
 * SMIOL_set_option
 *
 * Sets an option for the SMIOL library.
 *
 * Detailed description.
 *
 ********************************************************************************/
int SMIOL_set_option(void)
{
	return SMIOL_SUCCESS;
}

/********************************************************************************
 *
 * SMIOL_set_frame
 *
 * Set the frame for the unlimited dimension for an open file
 *
 * For an open SMIOL file handle, set the frame for the unlimited dimension.
 * After setting the frame for a file, writing to a variable that is
 * dimensioned by the unlimited dimension will write to the last set frame,
 * overwriting any current data that maybe present in that frame.
 *
 * SMIOL_SUCCESS will be returned if the frame is successfully set otherwise an
 * error will return.
 *
 ********************************************************************************/
int SMIOL_set_frame(struct SMIOL_file *file, SMIOL_Offset frame)
{
	if (file == NULL) {
		return SMIOL_INVALID_ARGUMENT;
	}
	file->frame = frame;
	return SMIOL_SUCCESS;
}

/********************************************************************************
 *
 * SMIOL_get_frame
 *
 * Return the current frame of an open file
 *
 * Get the current frame of an open file. Upon success, SMIOL_SUCCESS will be
 * returned, otherwise an error will be returned.
 *
 ********************************************************************************/
int SMIOL_get_frame(struct SMIOL_file *file, SMIOL_Offset *frame)
{
	if (file == NULL || frame == NULL) {
		return SMIOL_INVALID_ARGUMENT;
	}
	*frame = file->frame;
	return SMIOL_SUCCESS;
}


/*******************************************************************************
 *
 * SMIOL_create_decomp
 *
 * Creates a mapping between compute elements and I/O elements.
 *
 * Given arrays of global element IDs that each task computes, this routine
 * works out a mapping of elements between compute and I/O tasks.
 *
 * The aggregation factor is used to indicate the size of subsets of ranks
 * that will gather fields onto a single rank in each subset before transferring
 * that field from compute to output tasks; in a symmetric way, it also
 * indicates the size of subsets over which fields will be scattered after they
 * are transferred from input tasks to a single compute tasks in each subset.
 *
 * An aggregation factor of 0 indicates that the implementation should choose
 * a suitable aggregation factor (usually matching the size of shared-memory
 * domains), while a positive integer specifies a specific size for task groups
 * to be used for aggregation.
 *
 * If all input arguments are determined to be valid and if the routine is
 * successful in working out a mapping, the decomp pointer is allocated and
 * given valid contents, and SMIOL_SUCCESS is returned; otherwise a non-success
 * error code is returned and the decomp pointer is NULL.
 *
 *******************************************************************************/
int SMIOL_create_decomp(struct SMIOL_context *context,
                        size_t n_compute_elements, SMIOL_Offset *compute_elements,
                        int aggregation_factor,
                        struct SMIOL_decomp **decomp)
{
	size_t i;
	size_t n_io_elements, n_io_elements_global;
	size_t io_start, io_count;
	SMIOL_Offset *io_elements;
	MPI_Comm comm;
	MPI_Datatype dtype;
	int ierr;

	size_t n_compute_elements_agg;
	SMIOL_Offset *compute_elements_agg = NULL;
	MPI_Comm agg_comm = MPI_COMM_NULL;
	int *counts = NULL;
	int *displs = NULL;
	int actual_agg_factor;


	/*
	 * Minimal check on the validity of arguments
	 */
	if (context == NULL) {
		return SMIOL_INVALID_ARGUMENT;
	}

	if (compute_elements == NULL && n_compute_elements != 0) {
		return SMIOL_INVALID_ARGUMENT;
	}

	if (aggregation_factor < 0) {
		return SMIOL_INVALID_ARGUMENT;
	}

	comm = MPI_Comm_f2c(context->fcomm);

	/*
	 * Figure out MPI_Datatype for size_t... there must be a better way...
	 */
	switch (sizeof(size_t)) {
		case sizeof(uint64_t):
			dtype = MPI_UINT64_T;
			break;
		case sizeof(uint32_t):
			dtype = MPI_UINT32_T;
			break;
		case sizeof(uint16_t):
			dtype = MPI_UINT16_T;
			break;
		default:
			return SMIOL_MPI_ERROR;
	}

	/*
	 * Based on the number of compute elements for each task, determine
	 * the total number of elements across all tasks for I/O. The assumption
	 * is that the number of elements to read/write is equal to the size of
	 * the set of compute elements.
	 */
	n_io_elements = n_compute_elements;
	if (MPI_SUCCESS != MPI_Allreduce((const void *)&n_io_elements,
	                                 (void *)&n_io_elements_global,
	                                 1, dtype, MPI_SUM, comm)) {
		return SMIOL_MPI_ERROR;
	}

	/*
	 * Determine the contiguous range of elements to be read/written by
	 * this MPI task
	 */
	ierr = get_io_elements(context->comm_rank,
	                       context->num_io_tasks, context->io_stride,
	                       n_io_elements_global, &io_start, &io_count);

	/*
	 * Fill in io_elements from io_start through io_start + io_count - 1
	 */
	io_elements = NULL;
	if (io_count > 0) {
		io_elements = (SMIOL_Offset *)malloc(sizeof(SMIOL_Offset)
		                                     * n_io_elements_global);
		if (io_elements == NULL) {
			return SMIOL_MALLOC_FAILURE;
		}
		for (i = 0; i < io_count; i++) {
			io_elements[i] = (SMIOL_Offset)(io_start + i);
		}
	}

	/*
	 * If aggregation_factor != 1, aggregate the list of compute_elements
	 * before building the mapping
	 */
	if (aggregation_factor != 1) {
		int comm_rank = context->comm_rank;

		/*
		 * Create intracommunicators for aggregation
		 */
		if (aggregation_factor == 0) {
			ierr = MPI_Comm_split_type(comm, MPI_COMM_TYPE_SHARED,
			                           comm_rank, MPI_INFO_NULL,
			                           &agg_comm);
		} else {
			ierr = MPI_Comm_split(comm,
			                      (comm_rank / aggregation_factor),
			                      comm_rank,
			                      &agg_comm);
		}
		if (ierr != MPI_SUCCESS) {
			fprintf(stderr, "Error: MPI_Comm_split failed with code %i\n",
			        ierr);
			return SMIOL_MPI_ERROR;
		}

		ierr = MPI_Comm_size(agg_comm, &actual_agg_factor);
		if (ierr != MPI_SUCCESS) {
			fprintf(stderr, "Error: MPI_Comm_size failed with code %i\n",
			        ierr);
			return SMIOL_MPI_ERROR;
		}

		/*
		 * Create aggregated compute_elements list if the actual
		 * aggregation factor is > 1
		 */
		if (actual_agg_factor > 1) {
			aggregate_list(agg_comm, 0, n_compute_elements,
			               compute_elements,
			               &n_compute_elements_agg,
			               &compute_elements_agg, &counts, &displs);
		} else {
			MPI_Comm_free(&agg_comm);
			n_compute_elements_agg = n_compute_elements;
			compute_elements_agg = compute_elements;
		}
	} else {
		actual_agg_factor = 1;
		n_compute_elements_agg = n_compute_elements;
		compute_elements_agg = compute_elements;
	}

	/*
	 * Build the mapping between compute tasks and I/O tasks
	 */
	ierr = build_exchange(context,
	                      n_compute_elements_agg, compute_elements_agg,
	                      io_count, io_elements,
	                      decomp);

	free(io_elements);

	if (actual_agg_factor > 1) {
		(*decomp)->agg_factor = actual_agg_factor;
		(*decomp)->agg_comm = MPI_Comm_c2f(agg_comm);
		(*decomp)->n_compute = n_compute_elements;
		(*decomp)->n_compute_agg = n_compute_elements_agg;
		(*decomp)->counts = counts;
		(*decomp)->displs = displs;

		free(compute_elements_agg);
	}

	/*
	 * If decomp was successfully created, add io_start and io_count values
	 * to the decomp before returning
	 */
	if (ierr == SMIOL_SUCCESS) {
		(*decomp)->io_start = io_start;
		(*decomp)->io_count = io_count;
	}

	return ierr;
}


/********************************************************************************
 *
 * SMIOL_free_decomp
 *
 * Frees a mapping between compute elements and I/O elements.
 *
 * Free all memory of a SMIOL_decomp and returns SMIOL_SUCCESS. If decomp
 * points to NULL, then do nothing and return SMIOL_SUCCESS. After this routine
 * is called, no other SMIOL routines should use the freed SMIOL_decomp.
 *
 ********************************************************************************/
int SMIOL_free_decomp(struct SMIOL_decomp **decomp)
{
	MPI_Comm comm;

	if ((*decomp) == NULL) {
		return SMIOL_SUCCESS;
	}

	free((*decomp)->comp_list);
	free((*decomp)->io_list);

	comm = MPI_Comm_f2c((*decomp)->agg_comm);
	if (comm != MPI_COMM_NULL) {
		MPI_Comm_free(&comm);
	}
	if ((*decomp)->counts != NULL) {
		free((*decomp)->counts);
	}
	if ((*decomp)->displs != NULL) {
		free((*decomp)->displs);
	}

	free((*decomp));
	*decomp = NULL;

	return SMIOL_SUCCESS;
}


/********************************************************************************
 *
 * build_start_count
 *
 * Constructs start[] and count[] arrays for parallel I/O operations
 *
 * Given a pointer to a SMIOL file that was previously opened, the name of
 * a variable in that file, and a SMIOL decomp, this function returns several
 * items that may be used when reading or writing the variable in parallel:
 *
 * 1) The size of each "element" of the variable, where an element is defined as
 *    a contiguous memory range associated with the slowest-varying, non-record
 *    dimension of the variable; for example, a variable
 *    float foo[nCells][nVertLevels] would have an element size of
 *    sizeof(float) * nVertLevels if nCells were a decomposed dimension.
 *
 *    For non-decomposed variables, the element size is the size of one record
 *    of the entire variable.
 *
 * 2) The size of the fundamental datatype for the variable; for example, a
 *    float variable would yield sizeof(float).
 *
 * 3) The number of dimensions for the variable, including any unlimited/record
 *    dimension.
 *
 * 4) Whether the variable has a record (unlimited) dimension.
 *
 * 5) The start[] and count[] arrays (each with size ndims) to be read or written
 *    by an MPI rank using the I/O decomposition described in decomp.
 *
 * If the decomp argument is NULL, the variable is to be read or written as
 * a non-decomposed variable; typically, only MPI rank 0 will write
 * the non-decomposed variable, and all MPI ranks will read the non-decomposed
 * variable.
 *
 * Depending on the value of the write_or_read argument -- either START_COUNT_READ
 * or START_COUNT_WRITE -- the count[] values will be set so that all ranks will
 * read the variable, or only rank 0 will write the variable if the variable is
 * not decomposed.
 *
 ********************************************************************************/
int build_start_count(struct SMIOL_file *file, const char *varname,
                      const struct SMIOL_decomp *decomp,
                      int write_or_read, size_t *element_size,
                      size_t *basic_type_size, int *ndims,
                      int *has_unlimited_dim,
                      size_t **start, size_t **count)
{
	int i;
	int ierr;
	int vartype;
	char **dimnames;
	SMIOL_Offset *dimsizes;

/* TO DO - define maximum string size, currently assumed to be 64 chars */

	/*
	 * Figure out type of the variable, as well as its dimensions
	 */
	ierr = SMIOL_inquire_var(file, varname, &vartype, ndims, NULL);
	if (ierr != SMIOL_SUCCESS) {
		return ierr;
	}

	dimnames = malloc(sizeof(char *) * (size_t)(*ndims));
        if (dimnames == NULL) {
		ierr = SMIOL_MALLOC_FAILURE;
		return ierr;
	}

	for (i = 0; i < *ndims; i++) {
		dimnames[i] = malloc(sizeof(char) * (size_t)64);
	        if (dimnames[i] == NULL) {
			int j;

			for (j = 0; j < i; j++) {
				free(dimnames[j]);
			}
			free(dimnames);

			ierr = SMIOL_MALLOC_FAILURE;
			return ierr;
		}
	}

	ierr = SMIOL_inquire_var(file, varname, NULL, NULL, dimnames);
	if (ierr != SMIOL_SUCCESS) {
		for (i = 0; i < *ndims; i++) {
			free(dimnames[i]);
		}
		free(dimnames);
		return ierr;
	}
	
	dimsizes = malloc(sizeof(SMIOL_Offset) * (size_t)(*ndims));
        if (dimsizes == NULL) {
		ierr = SMIOL_MALLOC_FAILURE;
		return ierr;
	}

	/*
	 * It is assumed that only the first dimension can be an unlimited
	 * dimension, so by inquiring about dimensions from last to first, we
	 * can be guaranteed that has_unlimited_dim will be set correctly at
	 * the end of the loop over dimensions
	 */
	*has_unlimited_dim = 0;
	for (i = (*ndims-1); i >= 0; i--) {
		ierr = SMIOL_inquire_dim(file, dimnames[i], &dimsizes[i],
		                         has_unlimited_dim);
		if (ierr != SMIOL_SUCCESS) {
			for (i = 0; i < *ndims; i++) {
				free(dimnames[i]);
			}
			free(dimnames);
			free(dimsizes);

			return ierr;
		}
	}

	for (i = 0; i < *ndims; i++) {
		free(dimnames[i]);
	}
	free(dimnames);

	/*
	 * Set basic size of each element in the field
	 */
	*element_size = 1;
	switch (vartype) {
		case SMIOL_REAL32:
			*basic_type_size = sizeof(float);
			break;
		case SMIOL_REAL64:
			*basic_type_size = sizeof(double);
			break;
		case SMIOL_INT32:
			*basic_type_size = sizeof(int);
			break;
		case SMIOL_CHAR:
			*basic_type_size = sizeof(char);
			break;
	}
	*element_size = *basic_type_size;

	*start = malloc(sizeof(size_t) * (size_t)(*ndims));
        if (*start == NULL) {
		free(dimsizes);
		ierr = SMIOL_MALLOC_FAILURE;
		return ierr;
	}

	*count = malloc(sizeof(size_t) * (size_t)(*ndims));
        if (*count == NULL) {
		free(dimsizes);
		free(start);
		ierr = SMIOL_MALLOC_FAILURE;
		return ierr;
	}

	/*
	 * Build start/count description of the part of the variable to be
	 * read or written. Simultaneously, compute the product of all
	 * non-unlimited, non-decomposed dimension sizes, scaled by the basic
	 * element size to get the effective size of each element to be read or
	 * written
	 */
	for (i = 0; i < *ndims; i++) {
		(*start)[i] = (size_t)0;
		(*count)[i] = (size_t)dimsizes[i];

		/*
		 * If variable has an unlimited dimension, set start to current
		 * frame and count to one
		 */
		if (*has_unlimited_dim && i == 0) {
			(*start)[i] = (size_t)file->frame;
			(*count)[i] = (size_t)1;
		}

		/*
		 * If variable is decomposed, set the slowest-varying,
		 * non-record dimension start and count based on values from
		 * the decomp structure
		 */
		if (decomp) {
			if ((!*has_unlimited_dim && i == 0) ||
			    (*has_unlimited_dim && i == 1)) {
				(*start)[i] = decomp->io_start;
				(*count)[i] = decomp->io_count;
			} else {
				*element_size *= (*count)[i];
			}
		} else {
			*element_size *= (*count)[i];
		}

		if (write_or_read == START_COUNT_WRITE) {
			/*
			 * If the variable is not decomposed, only MPI rank 0
			 * will have non-zero count values so that all MPI ranks
			 * do no try to write the same offsets
			 */
			if (!decomp && file->context->comm_rank != 0) {
				(*count)[i] = 0;
			}
		}
	}

	free(dimsizes);

	return SMIOL_SUCCESS;
}


#ifdef SMIOL_PNETCDF
/********************************************************************************
 *
 * write_chunk_pnetcdf
 *
 * Write a chunk of a variable to a file using the Parallel-NetCDF library
 *
 * Given a file and information about a variable in the file, write a chunk of
 * memory to the variable according to start/count arrays. If the size of the
 * chunk to be written will fit within any buffer attached to the file, the
 * chunk is written using the Parallel-NetCDF buffered non-blocking interface;
 * otherwise, the chunk is written using the blocking write interface, ensuring
 * that not more than 2 GiB is written in any single call to ncmpi_put_vara_all.
 *
 * The return value from this function will be NC_NOERR in case no errors
 * occurred in calls to the Parallel-NetCDF library, or a Parallel-NetCDF error
 * code otherwise.
 *
 * Within this function, return error codes from MPI calls are ignored.
 *
 ********************************************************************************/
int write_chunk_pnetcdf(struct SMIOL_file *file,
                        int varidp,
                        int ndims,
                        int has_unlimited_dim,
                        size_t basic_type_size,
                        MPI_Comm io_file_comm,
                        const void *buf_p,
                        MPI_Offset *mpi_start,
                        MPI_Offset *mpi_count
                       )
{
	int ierr = NC_NOERR;
	long lusage;
	long max_usage;
	size_t element_size;
	int iter_idx;
	int i;

	/*
	 * For scalar variables (with or without an unlimited dimension),
	 * just write with a single call to the blocking write interface.
	 */
	if (ndims == 0 || (has_unlimited_dim && ndims == 1)) {
		ierr = ncmpi_bput_vara(file->ncidp,
		                       varidp,
		                       mpi_start, mpi_count,
		                       buf_p,
		                       0, MPI_DATATYPE_NULL,
		                       &(file->reqs[(file->n_reqs++)]));
		return ierr;
	}

	/*
	 * Set iter_idx to be the slowest-varying non-record (non-unlimited)
	 * dimension for the variable
	 */
	iter_idx = 0;
	if (has_unlimited_dim) iter_idx++;

	/*
	 * Let element_size be the product of the fastest-varying dimension
	 * sizes beyond the iter_idx dimension multiplied by the basic type
	 * size for this variable.
	 */
	element_size = basic_type_size;
	for (i = iter_idx + 1; i < ndims; i++) {
		element_size *= mpi_count[i];
	}

	/*
	 * Compute the maximum total number of bytes to be written by any MPI
	 * task for their chunks of the variable
	 */
	lusage = (long)element_size * mpi_count[iter_idx];
	MPI_Allreduce(&lusage, &max_usage, 1, MPI_LONG,
	              MPI_MAX, io_file_comm);

	/*
	 * If the maximum size of a chunk of data to be written is larger than
	 * the buffer size, just write through the non-buffered interface;
	 * otherwise, the ncmpi_bput_vara call will fail.
	 */
	if (max_usage > file->bufsize || max_usage > ((MPI_Offset)INT_MAX)) {
		MPI_Offset remaining_count;
		MPI_Offset max_count;
		long done, global_done;
		size_t buf_offset;

		max_count = ((MPI_Offset)INT_MAX) / element_size;
		remaining_count = mpi_count[iter_idx];

		/*
		 * Bound the number of values to be written along the slowest-
		 * varying non-record dimension to ensure that not more than
		 * 2 GiB are written in the call to ncmpi_put_vara_all
		 */
		mpi_count[iter_idx] = (max_count < remaining_count)
		                    ? max_count : remaining_count;
		remaining_count -= mpi_count[iter_idx];
		done = (mpi_count[iter_idx] == 0) ? 1 : 0;
		global_done = 0;
		buf_offset = 0;

		/*
		 * Keep calling ncmpi_put_vara_all on all I/O tasks as long as
		 * at least one task still has data to be written, writing at
		 * most 2 GiB at a time
		 */
		while (!global_done) {
			ierr = ncmpi_put_vara_all(file->ncidp,
			                          varidp,
			                          mpi_start, mpi_count,
			                          &((uint8_t *)buf_p)[buf_offset],
			                          0, MPI_DATATYPE_NULL);

			/*
			 * Update start/count values for slowest non-record
			 * dimension, and determine whether this task still has
			 * data to be written
			 */
			if (!done) {
				buf_offset += (size_t)mpi_count[iter_idx]
				              * element_size;
				mpi_start[iter_idx] += mpi_count[iter_idx];
				mpi_count[iter_idx] = (max_count < remaining_count)
				                    ? max_count : remaining_count;
				remaining_count -= mpi_count[iter_idx];

				done = (mpi_count[iter_idx] == 0) ? 1 : 0;
			}

			if (ierr != NC_NOERR) {
				done = -1;
			}

			/*
			 * Get done status across all I/O tasks
			 */
			MPI_Allreduce(&done, &global_done, 1, MPI_LONG, MPI_MIN,
			              MPI_Comm_f2c(file->io_file_comm));
		};

	} else {
		/*
		 * If executing this else branch, assume bufsize > 0 and
		 * that a buffer has therefore been attached to file.
		 */

		MPI_Offset usage;

		/*
		 * Check how many bytes have been used in buffer on this task
		 */
		ierr = ncmpi_inq_buffer_usage(file->ncidp,
		                              &usage);
		lusage = usage +
		         (long)element_size * mpi_count[iter_idx];

		MPI_Allreduce(&lusage, &max_usage, 1,
		              MPI_LONG, MPI_MAX,
		              io_file_comm);

		/*
		 * If making a buffered write would cause the remaining buffer
		 * size to be exceeded on any task, wait for non-blocking
		 * writes to complete
		 */
		if ((size_t)max_usage > file->bufsize
		    || file->n_reqs == MAX_REQS) {
			ierr = ncmpi_wait_all(file->ncidp, file->n_reqs,
			                      file->reqs, NULL);  /* statuses */
			file->n_reqs = 0;
		}

		if (ierr == NC_NOERR) {
			ierr = ncmpi_bput_vara(file->ncidp,
			                       varidp,
			                       mpi_start, mpi_count,
			                       buf_p,
			                       0, MPI_DATATYPE_NULL,
			                       &(file->reqs[(file->n_reqs++)]));
		}
	}

	return ierr;
}


/********************************************************************************
 *
 * read_chunk_pnetcdf
 *
 * Read a chunk of a variable from a file using the Parallel-NetCDF library
 *
 * Given a file and information about a variable in the file, read a chunk of
 * memory from the variable according to start/count arrays. The chunk is always
 * read using the blocking read interface while ensuring that not more than 2
 * GiB is read in any single call to ncmpi_get_vara_all.
 *
 * The return value from this function will be NC_NOERR in case no errors
 * occurred in calls to the Parallel-NetCDF library, or a Parallel-NetCDF error
 * code otherwise.
 *
 * Within this function, return error codes from MPI calls are ignored.
 *
 ********************************************************************************/
int read_chunk_pnetcdf(struct SMIOL_file *file,
                       int varidp,
                       int ndims,
                       int has_unlimited_dim,
                       size_t basic_type_size,
                       MPI_Comm io_file_comm,
                       void *buf_p,
                       MPI_Offset *mpi_start,
                       MPI_Offset *mpi_count
                      )
{
	int ierr = NC_NOERR;
	int iter_idx;
	MPI_Offset remaining_count;
	MPI_Offset max_count;
	long done, global_done;
	size_t element_size;
	size_t buf_offset;
	int i;

	/*
	 * For scalar variables (with or without an unlimited dimension),
	 * just read with a single call to the blocking read interface.
	 */
	if (ndims == 0 || (has_unlimited_dim && ndims == 1)) {
		ierr = ncmpi_get_vara_all(file->ncidp,
		                          varidp,
		                          mpi_start, mpi_count,
		                          buf_p,
		                          0, MPI_DATATYPE_NULL);
		return ierr;
	}

	/*
	 * Set iter_idx to be the slowest-varying non-record (non-unlimited)
	 * dimension for the variable
	 */
	iter_idx = 0;
	if (has_unlimited_dim) iter_idx++;

	/*
	 * Let element_size be the product of the fastest-varying dimension
	 * sizes beyond the iter_idx dimension multiplied by the basic type
	 * size for this variable.
	 */
	element_size = basic_type_size;
	for (i = iter_idx + 1; i < ndims; i++) {
		element_size *= mpi_count[i];
	}

	max_count = ((MPI_Offset)INT_MAX) / element_size;
	remaining_count = mpi_count[iter_idx];

	/*
	 * Bound the number of values to be read along the slowest-varying
	 * non-record dimension to ensure that not more than 2 GiB are read
	 * in the call to ncmpi_get_vara_all
	 */
	mpi_count[iter_idx] = (max_count < remaining_count)
	                    ? max_count : remaining_count;

	remaining_count -= mpi_count[iter_idx];
	done = (mpi_count[iter_idx] == 0) ? 1 : 0;
	global_done = 0;
	buf_offset = 0;

	/*
	 * Keep calling ncmpi_get_vara_all on all I/O tasks as long as at least
	 * one task still has data to be read, reading at most 2 GiB at a time
	 */
	while (!global_done) {
		ierr = ncmpi_get_vara_all(file->ncidp,
		                          varidp,
		                          mpi_start, mpi_count,
		                          &((uint8_t *)buf_p)[buf_offset],
		                          0, MPI_DATATYPE_NULL);

		/*
		 * Update start/count values for slowest non-record dimension,
		 * and determine whether this task still has data to be read
		 */
		if (!done) {
			buf_offset += (size_t)mpi_count[iter_idx] * element_size;
			mpi_start[iter_idx] += mpi_count[iter_idx];
			mpi_count[iter_idx] = (max_count < remaining_count)
			                    ? max_count : remaining_count;
			remaining_count -= mpi_count[iter_idx];

			done = (mpi_count[iter_idx] == 0) ? 1 : 0;
		}

		if (ierr != NC_NOERR) {
			done = -1;
		}

		/*
		 * Get done status across all I/O tasks
		 */
		MPI_Allreduce(&done, &global_done, 1, MPI_LONG, MPI_MIN,
		              io_file_comm);
	};

	return ierr;
}
#endif
