#ifndef XML_STREAM_PARSER_H
#define XML_STREAM_PARSER_H
#include "ezxml.h"

#define MSGSIZE 256

struct stacknode {
    int line;
    char name[MSGSIZE];
    struct stacknode *next;
};

/**
 * @struct stream_time_bounds
 * @brief  Holds optional start and stop time bounds for a stream.
 *
 * This structure encapsulates the optional `start_time` and `stop_time`
 * attributes that may be defined in a `<stream>` or `<immutable_stream>`
 * XML element. Both fields are dynamically allocated strings (via strdup)
 * when extracted, and must be freed by the caller with
 * free_stream_time_bounds().
 */
typedef struct {
    char *start_time;
    char *stop_time;
} stream_time_bounds;

/**
 * @brief Extract start and stop time bounds from a stream XML element.
 *
 * Given an ezXML element corresponding to a stream, this function
 * retrieves the values of the `start_time` and `stop_time` attributes.
 * If present, the attribute strings are duplicated with strdup and
 * stored in a newly constructed stream_time_bounds struct.
 * If absent, the corresponding fields are left as NULL.
 *
 * @param[in]  stream_xml   ezXML handle for a `<stream>` or
 *                          `<immutable_stream>` element.
 *
 * @return A stream_time_bounds struct with dynamically allocated
 *         `start_time` and/or `stop_time` strings (caller must free).
 */
stream_time_bounds extract_stream_time_bounds(ezxml_t stream_xml);

/**
 * @brief Free the memory associated with a stream_time_bounds struct.
 *
 * This routine frees the dynamically allocated strings in a
 * stream_time_bounds struct, if they are non-NULL, and resets
 * the fields to NULL. The struct pointer itself is not freed.
 *
 * @param[in,out] times Pointer to a stream_time_bounds struct whose
 *                      fields should be deallocated. May be NULL.
 */
void free_stream_time_bounds(stream_time_bounds *times);

int uniqueness_check(ezxml_t stream1, ezxml_t stream2);

void parse_xml_tag_name(char *tag_buf, char *tag_name);

size_t parse_xml_tag(
    char *xml_buf, size_t buf_len, char *tag, size_t *tag_len,
    int *line, int *start_line
);

int attribute_check(ezxml_t stream);

int check_streams(ezxml_t streams);

int xml_syntax_check(char *xml_buf, size_t bufsize);

int extract_stream_interval(
    const char *interval, const char *interval_type,
    const char **interval2, const char *streamID, ezxml_t streams
);

void xml_stream_get_attributes(
    char *fname, char *streamname, int *mpi_comm, char *filename,
    char *ref_time, char *filename_interval, char *io_type, int *status
);


#endif //XML_STREAM_PARSER_H
