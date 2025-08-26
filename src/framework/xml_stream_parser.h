#ifndef XML_STREAM_PARSER_H
#define XML_STREAM_PARSER_H
#include "ezxml.h"

#define MSGSIZE 256

struct stacknode {
    int line;
    char name[MSGSIZE];
    struct stacknode *next;
};

typedef struct {
    char *start_time;
    char *stop_time;
} stream_time_bounds;

stream_time_bounds extract_stream_time_bounds(ezxml_t stream_xml);

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
