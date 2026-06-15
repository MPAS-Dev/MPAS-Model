#include <stdio.h>
#include <stdlib.h>
#include <regex.h>
#include <string.h>

#define MAX_LEN 1024

static const char *BRE_SPECIAL_CHARS = ".[\\*^$";

/******************************************************************************
 *
 * possible_bre
 *
 * Determine whether an input string may be a Basic Regular Expression (BRE).
 * This function may return false positives (i.e., a return value indicating
 * that a string is a BRE, when in fact the string is not a BRE), but it will
 * never return a false negative (i.e., a return value indicating that a string
 * is not a BRE, when in fact the string is a BRE).
 *
 * Inputs:
 *   s - a null-terminated string
 *
 * Return value: 1 if the input string 's' is possibly a BRE and 0 otherwise.
 *
 ******************************************************************************/
static int possible_bre(const char *s)
{
	for (; *s != '\0'; s++) {
		if (strchr(BRE_SPECIAL_CHARS, *s) != NULL) {
			return 1;
		}
	}
	return 0;
}


/******************************************************************************
 *
 * check_regex_match
 *
 * Determine whether an input string 'str' matches the Basic Regular Expression
 * (BRE) 'pattern'.
 *
 * Inputs:
 *   pattern - a null-terminated string that may contain any valid BRE,
 *             including a simple string
 *   str     - a null-terminated string to be checked against pattern
 *
 * Return value: 1 if the input string str matches the BRE pattern,
 *               0 if the input string does not match the BRE pattern, and
 *              -1 if an error occurred.
 *
 ******************************************************************************/
void check_regex_match(const char * pattern, const char * str, int *imatch)
{
	regex_t regex;
	char bracketed_pattern[MAX_LEN];
	int ierr, len;

	/*
	 * If pattern is a simple string and not a basic regular expression,
	 * a string comparison will suffice
	 */
	if (!possible_bre(pattern)) {
		*imatch = (strcmp(pattern, str) == 0) ? 1 : 0;
		return;
	}

	*imatch = 0;
	len = snprintf(bracketed_pattern, 1024, "^%s$", pattern);
	if ( len >= MAX_LEN ) {
		*imatch = -1;
		return;
	}

	ierr = regcomp(&regex, bracketed_pattern, 0);
	if ( ierr ) {
		*imatch = -1;
		return;
	}

	ierr = regexec(&regex, str, 0, NULL, 0);

	regfree(&regex);

	if ( !ierr ) {
		*imatch = 1;
	} else if ( ierr == REG_NOMATCH ) {
		*imatch = 0;
	} else {
		*imatch = -1;
	}
}
