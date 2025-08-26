#include <string.h>

#include "unity.h"
#include "ezxml.h"
#include "xml_stream_parser.h"
#include "mpi.h"

static char *xml = NULL;
static ezxml_t streams;

void setUp(void) {
	xml = strdup(
      "<streams>"
      "<start_and_stop name=\"start_and_stop\" type=\"output\" filename_template=\"s1_$Y.nc\" output_interval=\"0_01:00:00\" start_time=\"0_00:00:00\" stop_time=\"1_00:00:00\"/>"
      "<start name=\"start\" type=\"output\" filename_template=\"s1_$Y.nc\" output_interval=\"0_01:00:00\" start_time=\"0_00:00:00\"/>"
      "<stop name=\"stop\" type=\"output\" filename_template=\"s1_$Y.nc\" output_interval=\"0_01:00:00\" stop_time=\"1_00:00:00\"/>"
      "<none name=\"stop\" type=\"output\" filename_template=\"s1_$Y.nc\" output_interval=\"0_01:00:00\"/>"
        "</streams>");
    streams = ezxml_parse_str(xml, strlen(xml));
}

void tearDown(void) {
    if (streams) {
        ezxml_free(streams);
        streams = NULL;
    }
    if (xml) {
        free(xml);
        xml = NULL;
    }
}

/* Test with both attributes present */
void test_extract_stream_times_start_and_stop(void) {
    ezxml_t stream = ezxml_child(streams, "start_and_stop");
    stream_times times = extract_stream_times(stream);
    TEST_ASSERT_NOT_NULL(times.start_time);
    TEST_ASSERT_NOT_NULL(times.stop_time);
    TEST_ASSERT_EQUAL_STRING("0_00:00:00", times.start_time);
    TEST_ASSERT_EQUAL_STRING("1_00:00:00", times.stop_time);
    free_stream_times(&times);
    TEST_ASSERT_NULL(times.start_time);
    TEST_ASSERT_NULL(times.stop_time);
}

void test_extract_stream_times_start(void) {
    ezxml_t stream = ezxml_child(streams, "start");
    stream_times times = extract_stream_times(stream);
    TEST_ASSERT_NOT_NULL(times.start_time);
    TEST_ASSERT_NULL(times.stop_time);
    TEST_ASSERT_EQUAL_STRING("0_00:00:00", times.start_time);
    free_stream_times(&times);
    TEST_ASSERT_NULL(times.start_time);
    TEST_ASSERT_NULL(times.stop_time);
}

void test_extract_stream_times_stop(void) {
    ezxml_t stream = ezxml_child(streams, "stop");
    stream_times times = extract_stream_times(stream);
    TEST_ASSERT_NULL(times.start_time);
    TEST_ASSERT_NOT_NULL(times.stop_time);
    TEST_ASSERT_EQUAL_STRING("1_00:00:00", times.stop_time);
    free_stream_times(&times);
    TEST_ASSERT_NULL(times.start_time);
    TEST_ASSERT_NULL(times.stop_time);
}

void test_extract_stream_times_none(void) {
    ezxml_t stream = ezxml_child(streams, "none");
    stream_times times = extract_stream_times(stream);
    TEST_ASSERT_NULL(times.start_time);
    TEST_ASSERT_NULL(times.stop_time);
    free_stream_times(&times);
    TEST_ASSERT_NULL(times.start_time);
    TEST_ASSERT_NULL(times.stop_time);
}


int main(void) {
    UNITY_BEGIN();
    RUN_TEST(test_extract_stream_times_start_and_stop);
    RUN_TEST(test_extract_stream_times_start);
    RUN_TEST(test_extract_stream_times_stop);
    RUN_TEST(test_extract_stream_times_none);
    return UNITY_END();
}