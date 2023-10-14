#include <stdlib.h>
#include <stdio.h>
#include "smiol_utils.h"

/*
 * Prototypes for functions used only internally by SMIOL utilities
 */
static int comp_sort_0(const void *a, const void *b);
static int comp_sort_1(const void *a, const void *b);
static int comp_sort_2(const void *a, const void *b);
static int comp_search_0(const void *a, const void *b);
static int comp_search_1(const void *a, const void *b);
static int comp_search_2(const void *a, const void *b);


/*******************************************************************************
 *
 * sort_triplet_array
 *
 * Sorts an array of triplets of SMIOL_Offset values in ascending order
 *
 * Given a pointer to an array of SMIOL_Offset triplets, sorts the array in
 * ascending order on the specified entry: 0 sorts on the first value in
 * the triplets, 1 sorts on the second value, and 2 sorts on the third.
 *
 * If the sort_entry is 1 or 2, the relative position of two triplets whose
 * values in that entry match will be determined by their values in the first
 * entry.
 *
 * The sort is not guaranteed to be stable.
 *
 *******************************************************************************/
void sort_triplet_array(size_t n_arr, SMIOL_Offset *arr, int sort_entry)
{
	size_t width = sizeof(SMIOL_Offset) * TRIPLET_SIZE;

	switch (sort_entry) {
	case 0:
		qsort((void *)arr, n_arr, width, comp_sort_0);
		break;
	case 1:
		qsort((void *)arr, n_arr, width, comp_sort_1);
		break;
	case 2:
		qsort((void *)arr, n_arr, width, comp_sort_2);
		break;
	}
}


/*******************************************************************************
 *
 * search_triplet_array
 *
 * Searches a sorted array of triplets of SMIOL_Offset values
 *
 * Given a pointer to a sorted array of SMIOL_Offset triplets, searches
 * the array on the specified entry for the key value. A search_entry value of
 * 0 searches for the key in the first entry of each triplet, 1 searches in
 * the second entry, and 2 searches in the third.
 *
 * If the key is found, the address of the triplet will be returned; otherwise,
 * a NULL pointer is returned.
 *
 * If the key occurs in more than one triplet at the specified entry, there is
 * no guarantee as to which triplet's address will be returned.
 *
 *******************************************************************************/
SMIOL_Offset *search_triplet_array(SMIOL_Offset key,
                                   size_t n_arr, SMIOL_Offset *arr,
                                   int search_entry)
{
	SMIOL_Offset *res;
	SMIOL_Offset key3[TRIPLET_SIZE];
	size_t width = sizeof(SMIOL_Offset) * TRIPLET_SIZE;

	key3[search_entry] = key;

	switch (search_entry) {
	case 0:
		res = (SMIOL_Offset *)bsearch((const void *)&key3,
		                              (const void *)arr, n_arr,
                                              width, comp_search_0);
		break;
	case 1:
		res = (SMIOL_Offset *)bsearch((const void *)&key3,
		                              (const void *)arr, n_arr,
                                              width, comp_search_1);
		break;
	case 2:
		res = (SMIOL_Offset *)bsearch((const void *)&key3,
		                              (const void *)arr, n_arr,
                                              width, comp_search_2);
		break;
	default:
		res = NULL;
	}

	return res;
}


/*******************************************************************************
 *
 * transfer_field
 *
 * Transfers a field between compute and I/O tasks
 *
 * Given a SMIOL_decomp and a direction, which determines whether the input
 * field is transferred from compute tasks to I/O tasks or from I/O tasks to
 * compute tasks, this function transfers the input field to the output field.
 *
 * The size in bytes of the elements in the field to be transferred is given by
 * element_size; for example, a single-precision field would set element_size
 * to sizeof(float).
 *
 * The caller must have already allocated the out_field argument with sufficient
 * space to contain the field.
 *
 * If no errors are detected in the input arguments or in the transfer of
 * the input field to the output field, SMIOL_SUCCESS is returned.
 *
 *******************************************************************************/
int transfer_field(const struct SMIOL_decomp *decomp, int dir,
                   size_t element_size, const void *in_field, void *out_field)
{
	MPI_Comm comm;
	int comm_rank;

	SMIOL_Offset *sendlist = NULL;
	SMIOL_Offset *recvlist = NULL;

	MPI_Request *send_reqs = NULL;
	MPI_Request *recv_reqs = NULL;

	uint8_t **send_bufs = NULL;
	uint8_t **recv_bufs = NULL;
	uint8_t *in_bytes = NULL;
	uint8_t *out_bytes = NULL;

	size_t ii, kk;
	size_t n_neighbors_send;
	size_t n_neighbors_recv;
	int64_t pos;
	int64_t pos_src = -1;
	int64_t pos_dst = -1;

	/*
	 * The following are ints because they correspond to MPI arguments
	 * that are ints, or they iterate over an int bound
	 */
	int taskid;
	int n_send, n_recv;
	int j;


	if (decomp == NULL) {
		return SMIOL_INVALID_ARGUMENT;
	}

	comm = MPI_Comm_f2c(decomp->context->fcomm);
	comm_rank = decomp->context->comm_rank;

	/*
	 * Throughout this function, operate on the fields as arrays of bytes
	 */
	in_bytes = (uint8_t *)in_field;
	out_bytes = (uint8_t *)out_field;

	/*
	 * Set send and recv lists based on exchange direction
	 */
	if (dir == SMIOL_COMP_TO_IO) {
		sendlist = decomp->comp_list;
		recvlist = decomp->io_list;
	} else if (dir == SMIOL_IO_TO_COMP) {
		sendlist = decomp->io_list;
		recvlist = decomp->comp_list;
	} else {
		return SMIOL_INVALID_ARGUMENT;
	}

	/*
	 * Determine how many other MPI tasks to communicate with, and allocate
	 * request lists and buffer pointers
	 */
	n_neighbors_send = (size_t)(sendlist[0]);
	n_neighbors_recv = (size_t)(recvlist[0]);

	/*
	 * Check that we have non-NULL in_field and out_field arguments
	 * in agreement with the number of neighbors to send/recv to/from
	 */
	if ((in_field == NULL && n_neighbors_send != 0)
	    || (out_field == NULL && n_neighbors_recv != 0)) {
		return SMIOL_INVALID_ARGUMENT;
	}

	send_reqs = (MPI_Request *)malloc(sizeof(MPI_Request)
	                                  * n_neighbors_send);
	recv_reqs = (MPI_Request *)malloc(sizeof(MPI_Request)
	                                  * n_neighbors_recv);

	send_bufs = (uint8_t **)malloc(sizeof(uint8_t *) * n_neighbors_send);
	recv_bufs = (uint8_t **)malloc(sizeof(uint8_t *) * n_neighbors_recv);

	/*
	 * Post receives
	 */
	pos = 1;
	for (ii = 0; ii < n_neighbors_recv; ii++) {
		taskid = (int)recvlist[pos++];
		n_recv = (int)recvlist[pos++];
		if (taskid != comm_rank) {
			recv_bufs[ii] = (uint8_t *)malloc(sizeof(uint8_t)
			                                  * element_size
			                                  * (size_t)n_recv);

			MPI_Irecv((void *)recv_bufs[ii],
			          n_recv * (int)element_size,
			          MPI_BYTE, taskid, comm_rank, comm,
			          &recv_reqs[ii]);
		}
		else {
			/*
			 * This is a receive from ourself - save position in
			 * recvlist for local copy, below
			 */
			pos_dst = pos - 1; /* Offset of n_recv */
			recv_bufs[ii] = NULL;
		}
		pos += n_recv;
	}

	/*
	 * Post sends
	 */
	pos = 1;
	for (ii = 0; ii < n_neighbors_send; ii++) {
		taskid = (int)sendlist[pos++];
		n_send = (int)sendlist[pos++];
		if (taskid != comm_rank) {
			send_bufs[ii] = (uint8_t *)malloc(sizeof(uint8_t)
			                                  * element_size
			                                  * (size_t)n_send);

			/* Pack send buffer */
			for (j = 0; j < n_send; j++) {
				size_t out_idx = (size_t)j
				                 * element_size;
				size_t in_idx = (size_t)sendlist[pos]
				                * element_size;

				for (kk = 0; kk < element_size; kk++) {
					send_bufs[ii][out_idx + kk] = in_bytes[in_idx + kk];
				}
				pos++;
			}

			MPI_Isend((void *)send_bufs[ii],
			          n_send * (int)element_size,
			          MPI_BYTE, taskid, taskid, comm,
			          &send_reqs[ii]);
		}
		else {
			/*
			 * This is a send to ourself - save position in
			 * sendlist for local copy, below
			 */
			pos_src = pos - 1; /* Offset of n_send */
			send_bufs[ii] = NULL;
			pos += n_send;
		}
	}

	/*
	 * Handle local copies
	 */
	if (pos_src >= 0 && pos_dst >= 0) {

		/* n_send and n_recv should actually be identical */
		n_send = (int)sendlist[pos_src++];
		n_recv = (int)recvlist[pos_dst++];

		for (j = 0; j < n_send; j++) {
			size_t out_idx = (size_t)recvlist[pos_dst]
			                 * element_size;
			size_t in_idx = (size_t)sendlist[pos_src]
			                * element_size;

			for (kk = 0; kk < element_size; kk++) {
				out_bytes[out_idx + kk] = in_bytes[in_idx + kk];
			}
			pos_dst++;
			pos_src++;
		}
	}

	/*
	 * Wait on receives
	 */
	pos = 1;
	for (ii = 0; ii < n_neighbors_recv; ii++) {
		taskid = (int)recvlist[pos++];
		n_recv = (int)recvlist[pos++];
		if (taskid != comm_rank) {
			MPI_Wait(&recv_reqs[ii], MPI_STATUS_IGNORE);

			/* Unpack receive buffer */
			for (j = 0; j < n_recv; j++) {
				size_t out_idx = (size_t)recvlist[pos]
				                 * element_size;
				size_t in_idx = (size_t)j
				                * element_size;

				for (kk = 0; kk < element_size; kk++) {
					out_bytes[out_idx + kk] = recv_bufs[ii][in_idx + kk];
				}
				pos++;
			}
		}
		else {
			/*
			 * A receive from ourself - just skip to next neighbor
			 * in the recvlist
			 */
			pos += n_recv;
		}

		/*
		 * The receive buffer for the current neighbor can now be freed
		 */
		if (recv_bufs[ii] != NULL) {
			free(recv_bufs[ii]);
		}
	}

	/*
	 * Wait on sends
	 */
	pos = 1;
	for (ii = 0; ii < n_neighbors_send; ii++) {
		taskid = (int)sendlist[pos++];
		n_send = (int)sendlist[pos++];
		if (taskid != comm_rank) {
			MPI_Wait(&send_reqs[ii], MPI_STATUS_IGNORE);
		}

		/*
		 * The send buffer for the current neighbor can now be freed
		 */
		if (send_bufs[ii] != NULL) {
			free(send_bufs[ii]);
		}

		pos += n_send;
	}

	/*
	 * Free request lists and buffer pointers
	 */
	free(send_reqs);
	free(recv_reqs);
	free(send_bufs);
	free(recv_bufs);

	return SMIOL_SUCCESS;
}


/*******************************************************************************
 *
 * aggregate_list
 *
 * Aggregates lists of elements from across all ranks onto a chosen root rank
 *
 * On entry, each MPI rank supplies a list of SMIOL_Offset values as well as the
 * size of that input list. The input list may be zero size.
 *
 * Upon successful return, for the root rank, the out_list argument will point
 * to an allocated array containing the aggregated elements from all MPI ranks
 * in the communicator, and n_out will specify the number of elements in the
 * output array. On all other ranks, n_out will be zero, and out_list will be a
 * NULL pointer.
 *
 * Also on the root rank, the counts and displs arrays will be allocated with
 * size equal to the size of the communicator, and they will contain the number
 * of elements in the aggregated list from each MPI rank as well as the
 * beginning offset in the aggregated list of elements from each rank. On all
 * non-root ranks, counts and displs will be returned as NULL pointers.
 *
 * Although the number of elements in each input list is given by a size_t,
 * the number of elements must not exceed the maximum representable value of
 * a signed integer due to restrictions imposed by MPI argument types.
 * Similarly, it must be ensured that the number of output elements does not
 * exceed the maximum representable value of a signed integer.
 *
 * If no errors occurred, 0 is returned. Otherwise, a value of 1 is returned.
 *
 *******************************************************************************/
int aggregate_list(MPI_Comm comm, int root, size_t n_in, SMIOL_Offset *in_list,
                   size_t *n_out, SMIOL_Offset **out_list,
                   int **counts, int **displs)
{
	int comm_size;
	int comm_rank;
	int err;
	int i;
	int n_in_i;


	*n_out = 0;
	*out_list = NULL;

	*counts = NULL;
	*displs = NULL;

	n_in_i = (int)n_in;

	if (MPI_Comm_size(comm, &comm_size) != MPI_SUCCESS) {
		fprintf(stderr, "Error: MPI_Comm_size failed in aggregate_list\n");
		return 1;
	}

	if (MPI_Comm_rank(comm, &comm_rank) != MPI_SUCCESS) {
		fprintf(stderr, "Error: MPI_Comm_rank failed in aggregate_list\n");
		return 1;
	}

	if (comm_rank == root) {
		*counts = (int *)malloc(sizeof(int) * (size_t)(comm_size));
		*displs = (int *)malloc(sizeof(int) * (size_t)(comm_size));
	}

	/*
	 * Gather the number of input elements from all tasks onto root rank
	 */
	err = MPI_Gather((const void *)&n_in_i, 1, MPI_INT,
	                 (void *)(*counts), 1, MPI_INT, root, comm);
	if (err != MPI_SUCCESS) {
		fprintf(stderr, "Error: MPI_Gather failed in aggregate_list\n");
		return 1;
	}

	/*
	 * Perform a scan of counts to get displs, and compute the number of
	 * output elements on root rank as the sum of the number of input
	 * elements across all tasks in the communicator
	 */
	if (comm_rank == root) {
		(*displs)[0] = 0;
		*n_out = (size_t)(*counts)[0];
		for (i = 1; i < comm_size; i++) {
			(*displs)[i] = (*displs)[i-1] + (*counts)[i-1];
			*n_out += (size_t)(*counts)[i];
		}

		*out_list = (SMIOL_Offset *)malloc(sizeof(SMIOL_Offset)
		                                   * (*n_out));
	}

	/* TO DO: Find an MPI type that is guaranteed to match SMIOL_Offset */
	/*        For now, just return an error if MPI_LONG isn't appropriate */
	if (sizeof(long) != sizeof(SMIOL_Offset)) {
		fprintf(stderr, "Error: sizeof(long) != sizeof(SMIOL_Offset)\n");
		return 1;
	}

	err = MPI_Gatherv((const void *)in_list, n_in_i, MPI_LONG,
	                  (void *)(*out_list), (*counts), (*displs), MPI_LONG,
	                  root, comm);
	if (err != MPI_SUCCESS) {
		fprintf(stderr, "Error: MPI_Gatherv failed in aggregate_list\n");
		return 1;
	}

	return 0;
}


/*******************************************************************************
 *
 * get_io_elements
 *
 * Returns a contiguous range of I/O elements for an MPI task
 *
 * Given the rank of a task, a description of the I/O task arrangement --
 * the number of I/O tasks and the stride between I/O tasks -- as well as the
 * total number of elements to read or write, compute the offset of the first
 * I/O element as well as the number of elements to read or write for the task.
 *
 * If this routine is successful in producing a valid io_start and io_count,
 * a value of 0 is returned; otherwise, a non-zero value is returned.
 *
 *******************************************************************************/
int get_io_elements(int comm_rank, int num_io_tasks, int io_stride,
                    size_t n_io_elements, size_t *io_start, size_t *io_count)
{
	if (io_start == NULL || io_count == NULL) {
		return 1;
	}

	*io_start = 0;
	*io_count = 0;

	if (comm_rank % io_stride == 0) {
		size_t io_rank = (size_t)(comm_rank / io_stride);
		size_t elems_per_task = (n_io_elements / (size_t)num_io_tasks);

		if (io_rank >= num_io_tasks) {
			return 0;
		}

		*io_start = io_rank * elems_per_task;
		*io_count = elems_per_task;

		if (io_rank + 1 == (size_t)num_io_tasks) {
			size_t remainder = n_io_elements
			                   - (size_t)num_io_tasks * elems_per_task;
			*io_count += remainder;
		}
	}

	return 0;
}


/*******************************************************************************
 *
 * build_exchange
 *
 * Builds a mapping between compute elements and I/O elements.
 *
 * Given arrays of global element IDs that each task computes and global element
 * IDs that each task reads/writes, this routine works out a mapping of elements
 * between compute and I/O tasks.
 *
 * If all input arguments are determined to be valid and if the routine is
 * successful in working out a mapping, the decomp pointer is allocated and
 * given valid contents, and SMIOL_SUCCESS is returned; otherwise a non-success
 * error code is returned and the decomp pointer is NULL.
 *
 *******************************************************************************/
int build_exchange(struct SMIOL_context *context,
                   size_t n_compute_elements, SMIOL_Offset *compute_elements,
                   size_t n_io_elements, SMIOL_Offset *io_elements,
                   struct SMIOL_decomp **decomp)
{
	MPI_Comm comm;
	int comm_size;
	int comm_rank;
	int ierr;
	int i, j;
	int count;
	int nbuf_in, nbuf_out;
	SMIOL_Offset *compute_ids;
	SMIOL_Offset *io_ids;
	SMIOL_Offset *buf_in, *buf_out;
	SMIOL_Offset *io_list, *comp_list;
	SMIOL_Offset neighbor;
	MPI_Request req_in, req_out;
	size_t ii;
	size_t idx;
	size_t n_neighbors;
	size_t n_xfer;
	size_t n_xfer_total;
	size_t n_list;

	const SMIOL_Offset UNKNOWN_TASK = (SMIOL_Offset)(-1);


	if (context == NULL) {
		return SMIOL_INVALID_ARGUMENT;
	}

	if (compute_elements == NULL && n_compute_elements != 0) {
		return SMIOL_INVALID_ARGUMENT;
	}

	if (io_elements == NULL && n_io_elements != 0) {
		return SMIOL_INVALID_ARGUMENT;
	}


	comm = MPI_Comm_f2c(context->fcomm);
	comm_size = context->comm_size;
	comm_rank = context->comm_rank;


	/*
	 * Because the count argument to MPI_Isend and MPI_Irecv is an int, at
	 * most 2^31-1 elements can be transmitted at a time. In this routine,
	 * arrays of pairs of SMIOL_Offset values will be transmitted as arrays
	 * of bytes, so n_compute_elements and n_io_elements can be at most
	 * 2^31-1 / sizeof(SMIOL_Offset) / 2.
	 */
	i = 0;
	if (n_compute_elements > (((size_t)1 << 31) - 1)
	                         / sizeof(SMIOL_Offset)
	                         / (size_t)2) {
		i = 1;
	}
	if (n_io_elements > (((size_t)1 << 31) - 1)
	                    / sizeof(SMIOL_Offset)
	                    / (size_t)2) {
		i = 1;
	}

	ierr = MPI_Allreduce((const void *)&i, (void *)&j, 1, MPI_INT, MPI_MAX,
	                     comm);
	if (j > 0) {
		return SMIOL_INVALID_ARGUMENT;
	} else if (ierr != MPI_SUCCESS) {
		return SMIOL_MPI_ERROR;
	}


	/*
	 * Allocate an array, compute_ids, with three entries for each compute
	 * element
	 *    [0] - element global ID
	 *    [1] - element local ID
	 *    [2] - I/O task that reads/writes this element
	 */
	compute_ids = (SMIOL_Offset *)malloc(sizeof(SMIOL_Offset) * TRIPLET_SIZE
	                                     * n_compute_elements);
	if (compute_ids == NULL) {
		return SMIOL_MALLOC_FAILURE;
	}

	/*
	 * Fill in compute_ids array with global and local IDs; rank of I/O task
	 * is not yet known
	 */
	for (ii = 0; ii < n_compute_elements; ii++) {
		compute_ids[TRIPLET_SIZE*ii] = compute_elements[ii]; /* global ID */
		compute_ids[TRIPLET_SIZE*ii+1] = (SMIOL_Offset)ii;   /* local ID */
		compute_ids[TRIPLET_SIZE*ii+2] = UNKNOWN_TASK;       /* I/O task rank */
	}

	/*
	 * Sort the compute_ids array on global element ID
	 * (first entry for each element)
	 */
	sort_triplet_array(n_compute_elements, compute_ids, 0);

	/*
	 * Allocate buffer with two entries for each I/O element
	 *    [0] - I/O element global ID
	 *    [1] - task that computes this element
	 */
	nbuf_out = (int)n_io_elements;
	buf_out = (SMIOL_Offset *)malloc(sizeof(SMIOL_Offset) * (size_t)2
	                                 * (size_t)nbuf_out);
	if (buf_out == NULL) {
		free(compute_ids);
		return SMIOL_MALLOC_FAILURE;
	}

	/*
	 * Fill buffer with I/O element IDs; compute task is not yet known
	 */
	for (ii = 0; ii < n_io_elements; ii++) {
		buf_out[2*ii] = io_elements[ii];
		buf_out[2*ii+1] = UNKNOWN_TASK;
	}

	/*
	 * Iterate through all ranks in the communicator, receiving from "left"
	 * neighbor and sending to "right" neighbor in each iteration.
	 * The objective is to identify, for each I/O element, which MPI rank
	 * computes that element. At the end of iteration, each rank will have
	 * seen the I/O element list from all other ranks.
	 */
	for (i = 0; i < comm_size; i++) {
		/*
		 * Compute the rank whose buffer will be received this iteration
		 */
		SMIOL_Offset src_rank = (comm_rank - 1 - i + comm_size)
		                        % comm_size;

		/*
		 * Initiate send of outgoing buffer size and receive of incoming
		 * buffer size
		 */
		ierr = MPI_Irecv((void *)&nbuf_in, 1, MPI_INT,
		                 (comm_rank - 1 + comm_size) % comm_size,
		                 (comm_rank + i), comm, &req_in);

		ierr = MPI_Isend((const void *)&nbuf_out, 1, MPI_INT,
		                 (comm_rank + 1) % comm_size,
		                 ((comm_rank + 1) % comm_size + i), comm,
		                 &req_out);

		/*
		 * Wait until the incoming buffer size has been received
		 */
		ierr = MPI_Wait(&req_in, MPI_STATUS_IGNORE);

		/*
		 * Allocate incoming buffer
		 */
		buf_in = (SMIOL_Offset *)malloc(sizeof(SMIOL_Offset) * (size_t)2
		                                * (size_t)nbuf_in);

		/*
		 * Initiate receive of incoming buffer
		 */
		count = 2 * nbuf_in;
		count *= (int)sizeof(SMIOL_Offset);
		ierr = MPI_Irecv((void *)buf_in, count, MPI_BYTE,
		                 (comm_rank - 1 + comm_size) % comm_size,
		                 (comm_rank + i), comm, &req_in);

		/*
		 * Wait until the outgoing buffer size has been sent
		 */
		ierr = MPI_Wait(&req_out, MPI_STATUS_IGNORE);

		/*
		 * Initiate send of outgoing buffer
		 */
		count = 2 * nbuf_out;
		count *= (int)sizeof(SMIOL_Offset);
		ierr = MPI_Isend((const void *)buf_out, count, MPI_BYTE,
		                 (comm_rank + 1) % comm_size,
		                 ((comm_rank + 1) % comm_size + i), comm,
		                 &req_out);

		/*
		 * Wait until the incoming buffer has been received
		 */
		ierr = MPI_Wait(&req_in, MPI_STATUS_IGNORE);

		/*
		 * Loop through the incoming buffer, marking all elements that
		 * are computed on this task
		 */
		for (j = 0; j < nbuf_in; j++) {
			/*
			 * If I/O element does not yet have a computing task...
			 */
			if (buf_in[2*j+1] == UNKNOWN_TASK) {
				SMIOL_Offset *elem;

				/*
				 * and if this element is computed on this task...
				 */
				elem = search_triplet_array(buf_in[2*j],
				                            n_compute_elements,
				                            compute_ids, 0);
				if (elem != NULL) {
					/*
					 * then mark the element as being
					 * computed on this task
					 */
					buf_in[2*j+1] = (SMIOL_Offset)comm_rank;

					/*
					 * and note locally which task will
					 * read/write this element
					 */
					elem[2] = src_rank;
				}
			}
		}

		/*
		 * Wait until we have sent the outgoing buffer
		 */
		ierr = MPI_Wait(&req_out, MPI_STATUS_IGNORE);

		/*
		 * Free outgoing buffer and make the input buffer into
		 * the output buffer for next iteration
		 */
		free(buf_out);
		buf_out = buf_in;
		nbuf_out = nbuf_in;
	}

	/*
	 * The output buffer is now the initial buffer with the compute tasks
	 * for each I/O element identified
	 */

	/*
	 * Allocate an array, io_ids, with three entries for each I/O element
	 *    [0] - element global ID
	 *    [1] - element local ID
	 *    [2] - compute task that operates on this element
	 */
	io_ids = (SMIOL_Offset *)malloc(sizeof(SMIOL_Offset) * TRIPLET_SIZE
	                                * n_io_elements);
	if (io_ids == NULL) {
		free(compute_ids);
		free(buf_out);
		return SMIOL_MALLOC_FAILURE;
	}

	/*
	 * Fill in io_ids array with global and local IDs, plus the rank of
	 * the task that computes each element
	 */
	for (ii = 0; ii < n_io_elements; ii++) {
		io_ids[TRIPLET_SIZE*ii] = buf_out[2*ii+0];    /* global ID */
		io_ids[TRIPLET_SIZE*ii+1] = (SMIOL_Offset)ii; /* local ID */
		io_ids[TRIPLET_SIZE*ii+2] = buf_out[2*ii+1];  /* computing task rank */
	}

	free(buf_out);

	/*
	 * Sort io_ids array on task ID (third entry for each element)
	 */
	sort_triplet_array(n_io_elements, io_ids, 2);

	*decomp = (struct SMIOL_decomp *)malloc(sizeof(struct SMIOL_decomp));
	if ((*decomp) == NULL) {
		free(compute_ids);
		free(io_ids);
		return SMIOL_MALLOC_FAILURE;
	}

	/*
	 * Initialize the SMIOL_decomp struct
	 */
	(*decomp)->context = context;
	(*decomp)->comp_list = NULL;
	(*decomp)->io_list = NULL;
	(*decomp)->io_start = 0;
	(*decomp)->io_count = 0;
	(*decomp)->agg_factor = 1;   /* Group with 1 task -> no aggregation */
	(*decomp)->agg_comm = MPI_Comm_c2f(MPI_COMM_NULL);
	(*decomp)->n_compute = 0;
	(*decomp)->n_compute_agg = 0;
	(*decomp)->counts = NULL;
	(*decomp)->displs = NULL;


	/*
	 * Scan through io_ids to determine number of unique neighbors that
	 * compute elements read/written on this task, and also determine
	 * the total number of elements
	 * computed on other tasks that are read/written on this task
	 */
	ii = 0;
	n_neighbors = 0;
	n_xfer_total = 0;
	while (ii < n_io_elements) {
		/* Task that computes this element */
		neighbor = io_ids[TRIPLET_SIZE*ii + 2];

		/* Number of elements to read/write for neighbor */
		n_xfer = 0;

		/*
		 * Since io_ids is sorted on task, as long as task is unchanged,
		 * increment n_xfer
		 */
		while (ii < n_io_elements
		       && io_ids[TRIPLET_SIZE*ii+2] == neighbor) {
			n_xfer++;
			ii++;
		}
		if (neighbor != UNKNOWN_TASK) {
			n_neighbors++;
			n_xfer_total += n_xfer;
		}
	}

	/*
	 * Based on number of neighbors and total number of elements to transfer
	 * allocate the io_list
	 */
	n_list = sizeof(SMIOL_Offset) * ((size_t)1
	                                 + (size_t)2 * n_neighbors
	                                 + n_xfer_total);
	(*decomp)->io_list = (SMIOL_Offset *)malloc(n_list);
	if ((*decomp)->io_list == NULL) {
		free(compute_ids);
		free(io_ids);
		free(*decomp);
		*decomp = NULL;
		return SMIOL_MALLOC_FAILURE;
	}
	io_list = (*decomp)->io_list;

	/*
	 * Scan through io_ids a second time, filling in the io_list
	 */
	io_list[0] = (SMIOL_Offset)n_neighbors;
	idx = 1; /* Index in io_list where neighbor ID will be written, followed
	            by number of elements and element local IDs */

	ii = 0;
	while (ii < n_io_elements) {
		/* Task that computes this element */
		neighbor = io_ids[TRIPLET_SIZE*ii + 2];

		/* Number of elements to read/write for neighbor */
		n_xfer = 0;

		/*
		 * Since io_ids is sorted on task, as long as task is unchanged,
		 * increment n_xfer
		 */
		while (ii < n_io_elements
		       && io_ids[TRIPLET_SIZE*ii+2] == neighbor) {
			if (neighbor != UNKNOWN_TASK) {
				/* Save local element ID in list */
				io_list[idx+2+n_xfer] = io_ids[TRIPLET_SIZE*ii+1];
				n_xfer++;
			}
			ii++;
		}
		if (neighbor != UNKNOWN_TASK) {
			io_list[idx] = neighbor;
			io_list[idx+1] = (SMIOL_Offset)n_xfer;
			idx += (2 + n_xfer);
		}
	}

	free(io_ids);

	/*
	 * Sort compute_ids array on task ID (third entry for each element)
	 */
	sort_triplet_array(n_compute_elements, compute_ids, 2);

	/*
	 * Scan through compute_ids to determine number of unique neighbors that
	 * read/write elements computed on this task, and also determine
	 * the total number of elements read/written on other tasks that are
	 * computed on this task
	 */
	ii = 0;
	n_neighbors = 0;
	n_xfer_total = 0;
	while (ii < n_compute_elements) {
		/* Task that reads/writes this element */
		neighbor = compute_ids[TRIPLET_SIZE*ii + 2];

		/* Number of elements to compute for neighbor */
		n_xfer = 0;

		/*
		 * Since compute_ids is sorted on task, as long as task is
		 * unchanged, increment n_xfer
		 */
		while (ii < n_compute_elements
		       && compute_ids[TRIPLET_SIZE*ii+2] == neighbor) {
			n_xfer++;
			ii++;
		}
		if (neighbor != UNKNOWN_TASK) {
			n_neighbors++;
			n_xfer_total += n_xfer;
		}
	}

	/*
	 * Based on number of neighbors and total number of elements to transfer
	 * allocate the comp_list
	 */
	n_list = sizeof(SMIOL_Offset) * ((size_t)1
	                                 + (size_t)2 * n_neighbors
	                                 + n_xfer_total);
	(*decomp)->comp_list = (SMIOL_Offset *)malloc(n_list);
	if ((*decomp)->comp_list == NULL) {
		free(compute_ids);
		free((*decomp)->io_list);
		free(*decomp);
		*decomp = NULL;
		return SMIOL_MALLOC_FAILURE;
	}
	comp_list = (*decomp)->comp_list;

	/*
	 * Scan through compute_ids a second time, filling in the comp_list
	 */
	comp_list[0] = (SMIOL_Offset)n_neighbors;
	idx = 1; /* Index in compute_list where neighbor ID will be written,
	            followed by number of elements and element local IDs */

	ii = 0;
	while (ii < n_compute_elements) {
		/* Task that reads/writes this element */
		neighbor = compute_ids[TRIPLET_SIZE*ii + 2];

		/* Number of elements to compute for neighbor */
		n_xfer = 0;

		/*
		 * Since compute_ids is sorted on task, as long as task is
		 * unchanged, increment n_xfer
		 */
		while (ii < n_compute_elements
		       && compute_ids[TRIPLET_SIZE*ii+2] == neighbor) {
			if (neighbor != UNKNOWN_TASK) {
				/* Save local element ID in list */
				comp_list[idx+2+n_xfer] = compute_ids[TRIPLET_SIZE*ii+1];
				n_xfer++;
			}
			ii++;
		}
		if (neighbor != UNKNOWN_TASK) {
			comp_list[idx] = neighbor;
			comp_list[idx+1] = (SMIOL_Offset)n_xfer;
			idx += (2 + n_xfer);
		}
	}

	free(compute_ids);

	return SMIOL_SUCCESS;
}


/*******************************************************************************
 *
 * print_lists
 *
 * Writes the contents of comp_list and io_list arrays to a text file
 *
 * Given pointers to the comp_list and io_list arrays from a SMIOL_decomp
 * structure, writes the contents of these arrays to a text file in a human-
 * readable format.
 *
 * Because the comp_list and io_list arrays are unique to each MPI task, this
 * routine takes as an argument the MPI rank of the calling task. The output
 * text file is named list.NNNN.txt, where NNNN is the rank of the task.
 *
 *******************************************************************************/
void print_lists(int comm_rank, SMIOL_Offset *comp_list, SMIOL_Offset *io_list)
{
	char filename[14];
	FILE *f;
	SMIOL_Offset n_neighbors;
	SMIOL_Offset n_elems, neighbor;
	int i, j, k;

	snprintf(filename, 14, "list.%4.4i.txt", comm_rank);

	f = fopen(filename, "w");

	/*
	 * The lists below are structured as follows:
	 *   list[0] - the number of neighbors for which a task sends/recvs
	 *                                                                             |
	 *   list[n] - neighbor task ID                                                | repeated for
	 *   list[n+1] - number of elements, m, to send/recv to/from the neighbor      | each neighbor
	 *   list[n+2 .. n+2+m] - local element IDs to send/recv to/from the neighbor  |
	 *                                                                             |
	 */

	fprintf(f, "===== comp_list for MPI rank %i =====\n", comm_rank);
	fprintf(f, "Our compute elements are read/written on %i tasks\n",
	        (int)comp_list[0]);
	j = 0;
	n_neighbors = comp_list[j++];
	for (i = 0; i < n_neighbors; i++) {
		neighbor = comp_list[j++];
		n_elems = comp_list[j++];
		if (neighbor == comm_rank) {
			fprintf(f, "----- copy %i elements -----\n",
			        (int)n_elems);
		} else {
			fprintf(f, "----- send %i elements to %i -----\n",
			        (int)n_elems, (int)neighbor);
		}
		for (k = 0; k < n_elems; k++) {
			fprintf(f, "  %i\n", (int)comp_list[j+k]);
		}
		j += n_elems;
	}

	fprintf(f, "\n\n");
	fprintf(f, "===== io_list for MPI rank %i =====\n", comm_rank);
	fprintf(f, "Our I/O elements are computed on %i tasks\n",
	        (int)io_list[0]);
	j = 0;
	n_neighbors = io_list[j++];
	for (i = 0; i < n_neighbors; i++) {
		neighbor = io_list[j++];
		n_elems = io_list[j++];
		if (neighbor == comm_rank) {
			fprintf(f, "----- copy %i elements -----\n",
			        (int)n_elems);
		} else {
			fprintf(f, "----- recv %i elements from %i -----\n",
			        (int)n_elems, (int)neighbor);
		}
		for (k = 0; k < n_elems; k++) {
			fprintf(f, "  %i\n", (int)io_list[j+k]);
		}
		j += n_elems;
	}
	fprintf(f, "\n\n");


	fprintf(f, "SMIOL_Offset comp_list_correct[] = { ");
	j = 0;
	n_neighbors = comp_list[j++];
	fprintf(f, "%i", (int)n_neighbors);

	for (i = 0; i < n_neighbors; i++) {
		neighbor = comp_list[j++];
		fprintf(f, ", %i", (int)neighbor);

		n_elems = comp_list[j++];
		fprintf(f, ", %i", (int)n_elems);

		for (k = 0; k < n_elems; k++) {
			fprintf(f, ", %i", (int)comp_list[j+k]);
		}
		j += n_elems;
	}
	fprintf(f, " };\n");

	fprintf(f, "SMIOL_Offset io_list_correct[] = { ");
	j = 0;
	n_neighbors = io_list[j++];
	fprintf(f, "%i", (int)n_neighbors);

	for (i = 0; i < n_neighbors; i++) {
		neighbor = io_list[j++];
		fprintf(f, ", %i", (int)neighbor);

		n_elems = io_list[j++];
		fprintf(f, ", %i", (int)n_elems);

		for (k = 0; k < n_elems; k++) {
			fprintf(f, ", %i", (int)io_list[j+k]);
		}
		j += n_elems;
	}
	fprintf(f, " };\n");

	fclose(f);
}


/*******************************************************************************
 *
 * comp_sort_0
 *
 * Compares two SMIOL_Offset triplets based on their first entry, returning:
 *  1 if the first is larger than the second,
 *  0 if the two are equal, and
 * -1 if the first is less than the second.
 *
 *******************************************************************************/
static int comp_sort_0(const void *a, const void *b)
{
	return (((const SMIOL_Offset *)a)[0] > ((const SMIOL_Offset *)b)[0])
	     - (((const SMIOL_Offset *)a)[0] < ((const SMIOL_Offset *)b)[0]);
}


/*******************************************************************************
 *
 * comp_sort_1
 *
 * Compares two SMIOL_Offset triplets based on their second entry, returning:
 *  1 if the first is larger than the second,
 *  0 if the two are equal, and
 * -1 if the first is less than the second.
 *
 * If the triplets a and b have equal values in their second entry, the values
 * in their first entry will be used to determine the result of the comparison.
 *
 *******************************************************************************/
static int comp_sort_1(const void *a, const void *b)
{
	int res;

	res = (((const SMIOL_Offset *)a)[1] > ((const SMIOL_Offset *)b)[1])
	    - (((const SMIOL_Offset *)a)[1] < ((const SMIOL_Offset *)b)[1]);
	if (res == 0) {
		res = (((const SMIOL_Offset *)a)[0] > ((const SMIOL_Offset *)b)[0])
		    - (((const SMIOL_Offset *)a)[0] < ((const SMIOL_Offset *)b)[0]);
	}
	return res;
}


/*******************************************************************************
 *
 * comp_sort_2
 *
 * Compares two SMIOL_Offset triplets based on their third entry, returning:
 *  1 if the first is larger than the second,
 *  0 if the two are equal, and
 * -1 if the first is less than the second.
 *
 * If the triplets a and b have equal values in their third entry, the values
 * in their first entry will be used to determine the result of the comparison.
 *
 *******************************************************************************/
static int comp_sort_2(const void *a, const void *b)
{
	int res;

	res = (((const SMIOL_Offset *)a)[2] > ((const SMIOL_Offset *)b)[2])
	    - (((const SMIOL_Offset *)a)[2] < ((const SMIOL_Offset *)b)[2]);
	if (res == 0) {
		res = (((const SMIOL_Offset *)a)[0] > ((const SMIOL_Offset *)b)[0])
		    - (((const SMIOL_Offset *)a)[0] < ((const SMIOL_Offset *)b)[0]);
	}
	return res;
}


/*******************************************************************************
 *
 * comp_search_0
 *
 * Compares two SMIOL_Offset triplets based on their first entry, returning:
 *  1 if the first is larger than the second,
 *  0 if the two are equal, and
 * -1 if the first is less than the second.
 *
 *******************************************************************************/
static int comp_search_0(const void *a, const void *b)
{
	return (((const SMIOL_Offset *)a)[0] > ((const SMIOL_Offset *)b)[0])
	     - (((const SMIOL_Offset *)a)[0] < ((const SMIOL_Offset *)b)[0]);
}


/*******************************************************************************
 *
 * comp_search_1
 *
 * Compares two SMIOL_Offset triplets based on their second entry, returning:
 *  1 if the first is larger than the second,
 *  0 if the two are equal, and
 * -1 if the first is less than the second.
 *
 *******************************************************************************/
static int comp_search_1(const void *a, const void *b)
{
	return (((const SMIOL_Offset *)a)[1] > ((const SMIOL_Offset *)b)[1])
	     - (((const SMIOL_Offset *)a)[1] < ((const SMIOL_Offset *)b)[1]);
}


/*******************************************************************************
 *
 * comp_search_2
 *
 * Compares two SMIOL_Offset triplets based on their third entry, returning:
 *  1 if the first is larger than the second,
 *  0 if the two are equal, and
 * -1 if the first is less than the second.
 *
 *******************************************************************************/
static int comp_search_2(const void *a, const void *b)
{
	return (((const SMIOL_Offset *)a)[2] > ((const SMIOL_Offset *)b)[2])
	     - (((const SMIOL_Offset *)a)[2] < ((const SMIOL_Offset *)b)[2]);
}
