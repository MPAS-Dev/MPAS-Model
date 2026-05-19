/*
 * regex_matching.c
 *
 * Provides regex pattern matching for MPAS stream list queries.
 *
 * Optimizations over the original:
 *   1. Compile-once-per-query  : regex compiled once per call, not per node.
 *   2. Cross-call pattern cache: each unique pattern is compiled at most once
 *                                for the entire model run (cache persists across
 *                                timesteps and repeated calls).
 *   3. Plain-string fast path  : patterns with no regex metacharacters are
 *                                matched with strcmp(), bypassing the regex
 *                                engine entirely.
 *
 * Public API (called from Fortran via iso_c_binding):
 *   regex_compile_cached(pattern, handle, ierr_out)
 *   regex_exec_cached   (handle,  string, imatch)
 *   regex_free_cached   (handle)              -- no-op; cache owns lifetime
 *
 * Legacy entry point (kept for any other callers):
 *   check_regex_match(pattern, string, imatch)
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <regex.h>

/* ── Tuneable constants ──────────────────────────────────────────────────── */
#define MAX_LEN    1024   /* max length of a bracketed pattern string         */
#define CACHE_SIZE   64   /* max number of unique patterns held in cache;
                             MPAS typically uses far fewer than this.         */

/* ── Regex metacharacter set ─────────────────────────────────────────────── */
static const char *REGEX_META = ".*+?[]{}()\\|^$";

/* ── Cache entry ─────────────────────────────────────────────────────────── */
typedef struct {
    char    bracketed[MAX_LEN]; /* "^<pattern>$" — used as the cache key      */
    char    plain[MAX_LEN];     /* original pattern, only valid when is_plain  */
    regex_t regex;              /* compiled regex,  only valid when !is_plain  */
    int     is_plain;           /* 1 → use strcmp fast path, 0 → use regex    */
    int     valid;              /* 1 → entry is populated and ready to use     */
} regex_cache_entry_t;

static regex_cache_entry_t cache[CACHE_SIZE];
static int cache_count = 0;

/* ── Internal helpers ────────────────────────────────────────────────────── */

/*
 * is_plain_string
 * Returns 1 if `pattern` contains no POSIX regex metacharacters,
 * meaning a simple strcmp is sufficient to match it.
 */
static int is_plain_string(const char *pattern)
{
    for (; *pattern != '\0'; pattern++) {
        if (strchr(REGEX_META, *pattern) != NULL) {
            return 0;
        }
    }
    return 1;
}

/*
 * cache_get_or_compile
 *
 * Looks up `bracketed_pattern` in the cache.  On a miss, compiles it and
 * stores the result.  Returns the cache index on success, or -1 on error.
 *
 * Eviction policy: when the cache is full the oldest entry (index 0) is
 * evicted and the array is shifted left so the newest entry occupies the
 * highest slot.  In practice MPAS stream patterns are fixed at startup so
 * the cache should never fill.
 */
static int cache_get_or_compile(const char *bracketed_pattern,
                                const char *plain_pattern)
{
    int i, idx;

    /* 1. Search for a cache hit ------------------------------------------ */
    for (i = 0; i < cache_count; i++) {
        if (cache[i].valid &&
            strncmp(cache[i].bracketed, bracketed_pattern, MAX_LEN) == 0) {
            return i;   /* hit — no compilation needed */
        }
    }

    /* 2. Cache miss -------------------------------------------------------- */
    if (cache_count >= CACHE_SIZE) {
        /* Evict the oldest entry to make room */
        if (!cache[0].is_plain) {
            regfree(&cache[0].regex);
        }
        memmove(&cache[0], &cache[1],
                (CACHE_SIZE - 1) * sizeof(regex_cache_entry_t));
        cache_count = CACHE_SIZE - 1;
    }

    idx = cache_count;

    /* Populate key fields */
    strncpy(cache[idx].bracketed, bracketed_pattern, MAX_LEN - 1);
    cache[idx].bracketed[MAX_LEN - 1] = '\0';
    cache[idx].valid    = 0;
    cache[idx].is_plain = is_plain_string(plain_pattern);

    if (cache[idx].is_plain) {
        /* Fast path: store the plain pattern for strcmp */
        strncpy(cache[idx].plain, plain_pattern, MAX_LEN - 1);
        cache[idx].plain[MAX_LEN - 1] = '\0';
    } else {
        /* Full regex compilation */
        if (regcomp(&cache[idx].regex, bracketed_pattern, 0) != 0) {
            return -1;  /* compile error */
        }
    }

    cache[idx].valid = 1;
    cache_count++;
    return idx;
}

/* ── Public API ──────────────────────────────────────────────────────────── */

/*
 * regex_compile_cached
 *
 * Looks up or compiles `pattern` (a null-terminated C string) and returns
 * an opaque integer handle for use with regex_exec_cached / regex_free_cached.
 *
 * Arguments:
 *   pattern    [in]  : plain pattern string (without ^ and $)
 *   handle     [out] : opaque cache index; pass to regex_exec_cached
 *   ierr_out   [out] : 0 on success, -1 on error
 */
void regex_compile_cached(const char *pattern,
                          intptr_t   *handle,
                          int        *ierr_out)
{
    char bracketed[MAX_LEN];
    int  len, idx;

    *handle   = -1;
    *ierr_out =  0;

    len = snprintf(bracketed, MAX_LEN, "^%s$", pattern);
    if (len < 0 || len >= MAX_LEN) {
        *ierr_out = -1;
        return;
    }

    idx = cache_get_or_compile(bracketed, pattern);
    if (idx < 0) {
        *ierr_out = -1;
        return;
    }

    *handle = (intptr_t)idx;
}

/*
 * regex_exec_cached
 *
 * Tests whether `str` matches the pattern identified by `handle`.
 *
 * Arguments:
 *   handle  [in]  : opaque handle from regex_compile_cached
 *   str     [in]  : null-terminated C string to test
 *   imatch  [out] : 1 = match, 0 = no match, -1 = error
 */
void regex_exec_cached(intptr_t   *handle,
                       const char *str,
                       int        *imatch)
{
    int idx, ierr;

    *imatch = 0;
    idx = (int)(*handle);

    if (idx < 0 || idx >= cache_count || !cache[idx].valid) {
        *imatch = -1;
        return;
    }

    if (cache[idx].is_plain) {
        /* Fast path: plain-string comparison, no regex overhead */
        *imatch = (strcmp(cache[idx].plain, str) == 0) ? 1 : 0;
        return;
    }

    /* Full regex execution */
    ierr = regexec(&cache[idx].regex, str, 0, NULL, 0);
    if      (!ierr)                *imatch =  1;
    else if (ierr == REG_NOMATCH)  *imatch =  0;
    else                           *imatch = -1;
}

/*
 * regex_free_cached
 *
 * No-op: the cache owns the lifetime of compiled patterns.
 * Present so Fortran callers need not change their call sites if the
 * underlying strategy ever changes.
 */
void regex_free_cached(intptr_t *handle)
{
    *handle = -1;
}

/* ── Legacy entry point ──────────────────────────────────────────────────── */

/*
 * check_regex_match  (original interface — kept for backward compatibility)
 *
 * Compiles and executes the regex in a single call.  Any code that still
 * calls this function will now benefit from the cache transparently.
 */
void check_regex_match(const char *pattern,
                       const char *str,
                       int        *imatch)
{
    intptr_t handle;
    int      ierr;

    regex_compile_cached(pattern, &handle, &ierr);
    if (ierr) {
        *imatch = -1;
        return;
    }
    regex_exec_cached(&handle, str, imatch);
    regex_free_cached(&handle);
}
