/*
 * Copyright (c) 2023, The University Corporation for Atmospheric Research (UCAR).
 *
 * Unless noted otherwise source code is licensed under the BSD license.
 * Additional copyright and license information can be found in the LICENSE file
 * distributed with this code, or at http://mpas-dev.github.com/license.html
 */

#include <stddef.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>
#include "ezxml.h"

#ifdef _MPI
#include "mpi.h"
#endif

#define MSGSIZE 256


/*
 *  Interface routines for writing log messages; defined in mpas_log.F
 *  messageType_c may be any of "MPAS_LOG_OUT", "MPAS_LOG_WARN", "MPAS_LOG_ERR", or "MPAS_LOG_CRIT"
 */
void mpas_log_write_c(const char *message_c, const char *messageType_c);


/*********************************************************************************
 *
 *  Function: read_and_broadcast
 *
 *  Reads the contents of a file into a buffer in distributed-memory parallel code.
 *
 *  The buffer buf is allocated with size bufsize, which will be exactly the
 *  number of bytes in the file fname. Only the master task will actually read the
 *  file, and the contents are broadcast to all other tasks. The mpi_comm argument
 *  is a Fortran MPI communicator used to determine which task is the master task.
 *
 *  A return code of 0 indicates the file was successfully read and broadcast to
 *  all MPI tasks that belong to the communicator.
 *
 *********************************************************************************/
int read_and_broadcast(const char *fname, int mpi_comm, char **buf, size_t *bufsize)
{
	int iofd;
	int rank;
	struct stat s;
	char msgbuf[MSGSIZE];

#ifdef _MPI
	MPI_Comm comm;

	comm = MPI_Comm_f2c((MPI_Fint)mpi_comm);
	if (MPI_Comm_rank(comm, &rank) != MPI_SUCCESS) {
		snprintf(msgbuf, MSGSIZE, "Error getting MPI rank in read_and_broadcast");
		mpas_log_write_c(msgbuf, "MPAS_LOG_ERR");
		return 1;
	}
#else
	rank = 0;
#endif

	if (rank == 0) {
		iofd = open(fname, O_RDONLY);
		if (iofd <= 0) {
			snprintf(msgbuf, MSGSIZE, "Could not open file %s in read_and_broadcast", fname);
			mpas_log_write_c(msgbuf, "MPAS_LOG_ERR");
			return 1;
		}

		fstat(iofd, &s);
		*bufsize = (size_t)s.st_size;
#ifdef _MPI
		if (MPI_Bcast((void *)bufsize, (int)sizeof(size_t), MPI_BYTE, 0, comm) != MPI_SUCCESS) {
			snprintf(msgbuf, MSGSIZE, "Error from MPI_Bcast in read_and_broadcast");
			mpas_log_write_c(msgbuf, "MPAS_LOG_ERR");
			return 1;
		}
#endif
	
		*buf = (char *)malloc(*bufsize);

		if (read(iofd, (void *)(*buf), *bufsize) < 0) {
			snprintf(msgbuf, MSGSIZE, "Error reading from %s in read_and_broadcast", fname);
			mpas_log_write_c(msgbuf, "MPAS_LOG_ERR");
			free(*buf);
			*buf = NULL;
			return 1;
		}

#ifdef _MPI
		if (MPI_Bcast((void *)(*buf), (int)(*bufsize), MPI_CHAR, 0, comm) != MPI_SUCCESS) {
			snprintf(msgbuf, MSGSIZE, "Error from MPI_Bcast in read_and_broadcast");
			mpas_log_write_c(msgbuf, "MPAS_LOG_ERR");
			free(*buf);
			*buf = NULL;
			return 1;
		}
#endif
	}
	else {
#ifdef _MPI
		if (MPI_Bcast((void *)bufsize, (int)sizeof(size_t), MPI_BYTE, 0, comm) != MPI_SUCCESS) {
			snprintf(msgbuf, MSGSIZE, "Error from MPI_Bcast in read_and_broadcast");
			mpas_log_write_c(msgbuf, "MPAS_LOG_ERR");
			return 1;
		}
#endif
		*buf = (char *)malloc(*bufsize);

#ifdef _MPI
		if (MPI_Bcast((void *)(*buf), (int)(*bufsize), MPI_CHAR, 0, comm) != MPI_SUCCESS) {
			snprintf(msgbuf, MSGSIZE, "Error from MPI_Bcast in read_and_broadcast");
			mpas_log_write_c(msgbuf, "MPAS_LOG_ERR");
			free(*buf);
			*buf = NULL;
			return 1;
		}
#endif
	}

	return 0;
}

/********************************************************************************
 *
 * parse_streams_file
 *
 * Parses an MPAS streams file into an XML tree
 *
 * Given the name of an MPAS streams XML file as well as an MPI communicator,
 * this routine reads and broadcasts the file contents to all MPI tasks in the
 * communicator, then parses the file into an ezxml_t struct.
 *
 * Upon success, a valid pointer to a root ezxml_t struct is returned;
 * otherwise, a NULL ezxml_t is returned.
 *
 ********************************************************************************/
ezxml_t parse_streams_file(int mpi_comm, const char *filename)
{
	char *xml_buf;
	size_t bufsize;

	if (read_and_broadcast(filename, mpi_comm, &xml_buf, &bufsize) != 0) {
		return NULL;
	}

	return ezxml_parse_str(xml_buf, bufsize);
}

/********************************************************************************
 *
 * free_streams_file
 *
 * Frees memory associated with an ezxml_t struct.
 *
 ********************************************************************************/
void free_streams_file(ezxml_t xmltree)
{
	ezxml_free(xmltree);
}


/********************************************************************************
 *
 * query_streams_file
 *
 * Returns information about the contents of a previously read streams XML file
 *
 * Given an ezxml_t holding the contents of a streams XML file -- typically from
 * a previous call to parse_streams_file -- returns a 1 if the specified stream
 * (and, optionally, attribute) exists in the file. If the stream and optionally
 * specified attribute are found, and if the attvalue argument is not a NULL
 * pointer, the value of the attribute is also returned.
 *
 * Both immutable and mutable streams can be queried.
 *
 * If the specified stream does not exist, a value of 0 is returned. If the
 * stream is found, but the specified attribute is not defined for the stream, a
 * value of 0 is returned.
 *
 ********************************************************************************/
int query_streams_file(ezxml_t xmltree, const char *streamname, const char *attname, const char **attvalue)
{
	ezxml_t stream_xml;
	const char *streamID;
	const char *attval_local;

	for (stream_xml = ezxml_child(xmltree, "immutable_stream"); stream_xml; stream_xml = ezxml_next(stream_xml)) {
		streamID = ezxml_attr(stream_xml, "name");

		if (strcmp(streamID, streamname) == 0) {
	                if (attname != NULL) {
				attval_local = ezxml_attr(stream_xml, attname);
				if (attval_local != NULL) {
					*attvalue = attval_local;
				} else {
					return 0;
				}
			}
			return 1;
		}
	}

	for (stream_xml = ezxml_child(xmltree, "stream"); stream_xml; stream_xml = ezxml_next(stream_xml)) {
		streamID = ezxml_attr(stream_xml, "name");

		if (strcmp(streamID, streamname) == 0) {
	                if (attname != NULL) {
				attval_local = ezxml_attr(stream_xml, attname);
				if (attval_local != NULL) {
					*attvalue = attval_local;
				} else {
					return 0;
				}
			}
			return 1;
		}
	}

	return 0;
}
