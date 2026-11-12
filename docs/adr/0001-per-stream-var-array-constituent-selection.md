# ADR 0001: Per-stream selection of var_array constituents

- **Status:** Proposed (work in progress on branch `framework/io_specify_var_array_constituents`)
- **Date:** 2026-10-15 (last updated 2026-11-12)
- **Author:** Abishek Gopal
- **Commits on this branch:** `44b522566` (Changes to ensure separate streams
  can write different sets of scalars), `ea0686c57` (Changes to logic),
  `c78101980` (Add tmp log statements for input)
- **Earlier history:** `20b7f10f6` (Attempt 1), `3d1dd5dcf`, `0d2f303b5` (WIP)
  on branch `framework/select_scalars_io`

## Context

A var_array in MPAS, such as `scalars`, packs an arbitrary number of constituents
(`qv`, `qc`, `qr`, ..., `nwfa`, `tke`). Each constituent is written to the file as its own variable.
Until now, a stream could include a var_array only as a whole: listing
`<var_array name="scalars"/>` read or wrote every constituent that is active at
run time.

Users have expressed interest in the ability to read and write different I/O streams with different subsets of the
scalars, without having to change Registry.xml and rebuild the code. For example, a high-frequency stream might carry only `qv` while
the history stream carries all hydrometeors.

Before this work, the only way a constituent was left out of a stream was
through Registry packages. When a constituent's package is inactive or when a constituent was excluded through preprocessor directives, the generated code in `atm_generate_structs` never assigns it an index and leaves it out of `constituentNames`. The constituent then doesn't exist for any stream. That mechanism is global and determined at startup, so it can't
express per-stream choices.

## Decision

1. **Syntax.** A stream field name can take the form
   `varArray:constituent1,constituent2,...`. Examples:
   - `<var_array name="scalars:qv,qc"/>` in `streams.atmosphere`,
   - `<var name="scalars:qv,qc"/>`, which works the same way because both
     elements go through `stream_mgr_add_field_c`,
   - `scalars:qv,qc` on a line of a stream list file.

   The XML parser passes the string through unchanged to
   `MPAS_stream_mgr_add_field`, which splits it at the first `:` into the
   var_array name and the constituent list.

2. **Validation when the stream is defined.** `MPAS_stream_mgr_add_field`
   first checks that the var_array exists, using
   `mpas_pool_get_field_info`. It then calls
   `stream_mgr_check_constituents`, which checks that:
   - the field is a 3-D real var_array,
   - every listed name is in its `constituentNames`.

   Any failure is a stream error. A bad list therefore stops stream setup
   instead of quietly producing an incomplete file.

3. **The selection is stored per stream, not on the field.** A new pool,
   `field_cons_pool`, is added to `MPAS_stream_list_type`. It holds the raw
   list under the key `<varArray>:constituents`. The pool is created and
   destroyed together with the stream's other pools.
   `MPAS_stream_mgr_add_stream_fields` copies the entry from a reference
   stream, and `MPAS_stream_mgr_remove_field` removes it.

4. **The list is parsed into a mask when the stream is built.** When
   `build_stream` assembles the low-level stream, it reads the stored list
   and passes the string as-is to `MPAS_streamAddField_3dReal` through the
   new optional argument `constituents`. Inside that routine,
   `parse_constituent_list` turns the string into a logical mask over
   `constituentNames`. The mask is kept on the stream's field-list node as
   `isRequested(:)` (see `mpas_io_streams_types.inc`) and freed with the
   node.
   - An empty list selects every constituent.
   - Without the argument, `isRequested(:)` is all `.true.`, so behaviour is
     unchanged.
   - `parse_constituent_list` lives in `mpas_io_streams` rather than
     `mpas_stream_manager`, because the stream manager depends on
     `mpas_io_streams` and not the other way round.
     `stream_mgr_check_constituents` reuses it for validation.

5. **How the mask is applied.**
   - *Defining variables:* a constituent that isn't selected is never passed
     to `MPAS_streamAddField_generic`. On output it is not defined in the
     file. On input it is not looked up in the file, so its `isAvailable(i)`
     stays `.false.`.
   - *Writing:* the 3-D real write loop skips constituents where
     `isRequested(j)` is `.false.`.
   - *Reading:* the 3-D real read loop skips constituents unless both
     `isAvailable(j)` and `isRequested(j)` are `.true.`. A constituent that
     isn't selected is never read, so its value in memory is whatever it was
     before the read: the Registry default, model-initialised values, or a
     value from an earlier stream.

## Alternatives considered

### A. Mask on the field object (Attempt 1, `20b7f10f6`), rejected

Attempt 1 added `isConstituentActive(:)` to `field3DReal`. That required
changes to:

- `mpas_field_types.inc`,
- the Registry generator (`gen_inc.c`),
- `atm_core_interface` for separately allocated scalars,
- `duplicate_field_array.inc`,
- the field deallocation code.

`stream_mgr_check_constituents` set the mask directly on the field. The I/O
layer then read it during field definition, attribute writing and the write
loop.

It was rejected because a field is a single object that every stream
shares. Whichever stream was set up last overwrote the mask, so two streams
could not choose different subsets. It also put an I/O concern into the
core field type and into generated code. `3d1dd5dcf` reverted all of these
changes.

### B. Pass a logical mask (`activeConstituents`) into `MPAS_streamAddField_3dReal`, replaced

An intermediate version parsed the list in the stream manager and passed a
logical array into `MPAS_streamAddField_3dReal`. It was replaced by passing
the raw string (`constituents`), so that parsing happens once, next to the
`constituentNames` it is checked against. The resulting mask is also kept on
the field-list node, where the read and write loops need it.

### C. Keep using Registry packages, rejected

The two options are #ifdefs or new packages that stand in for output
groupings. #ifdefs need a rebuild. Packages control allocation, are global
and are tied to namelist logic. Neither is the right tool for a choice about
which constituents a stream carries.

### D. A separate attribute on the `<var_array>` XML element, not pursued

An attribute such as `<var_array name="scalars" constituents="qv,qc"/>`
would be clearer. However, it would need changes to `xml_stream_parser.c`
and to the C-to-Fortran interface (`stream_mgr_add_field_c`), and stream
list files, which hold only names, could not express it. Folding the list
into the name lets it pass through the existing interface without new
arguments. Attempt 1 also tried adding C-callable checks
(`stream_mgr_check_field_exists_c`, `stream_mgr_check_constituent_c`).
`0d2f303b5` removed them because validation now happens entirely inside
`MPAS_stream_mgr_add_field`.

## Consequences

### Positive

- Each stream can read or write its own subset of a var_array's
  constituents.
- No changes to field types, the Registry generator or core code. The
  feature lives entirely in:
  - `mpas_stream_manager.F`,
  - `mpas_io_streams.F`,
  - `mpas_io_streams_types.inc`,
  - `mpas_stream_list_types.inc`.
- Existing stream definitions behave exactly as before. The argument is
  optional, and the pool entry is absent when no list is given.
- A misspelled constituent name is a fatal error when streams are set up.

### Limitations of the current code

**Scope**

- **Only 3-D real var_arrays are supported.** `stream_mgr_check_constituents`
  rejects any other type or rank. The read and write changes exist only in
  the `FIELD_3D_REAL` branches of `mpas_io_streams.F`.
- **One list per var_array per stream.** If a stream lists the same
  var_array twice, for example `scalars:qv` and then `scalars:qc`, the second
  entry triggers the existing "already in stream" warning and is ignored.
  The lists are not merged.

**Interaction with packages**

- **A constituent turned off by a package is a fatal error.** If a listed
  constituent exists in the Registry but its package is inactive, it is not
  in `constituentNames`, and validation fails. An example is
  `scalars:qv,nr` with a non-Thompson microphysics scheme. Whole fields that
  are off because of packages are skipped with a message instead. So a
  streams file with a constituent list can't be shared across physics
  configurations unless the list only names constituents active in all of
  them.

**Syntax and parsing**

- **Spaces in the list are not allowed.** `parse_constituent_list` uses
  `trim`, which removes only trailing blanks. `scalars:qv, qc` produces the
  token `" qc"`, which fails validation. In stream list files, which are
  read with `fscanf("%s")`, a space also splits the entry into two field
  names.
- **The `:` character in field names is overloaded.** Any future field name
  containing `:` would be misread as a constituent list.
- **The list length is bounded.** The whole entry, var_array name plus
  list, must fit in `StrKIND` characters, because `MPAS_stream_mgr_add_field`
  copies it into `StrKIND` strings.
- **`MPAS_stream_mgr_remove_field` needs the bare name.** It does not split
  on `:`. Passing `scalars:qv` fails to find the field, and only `scalars`
  removes the entry and its constituent list.

**Input streams**

- **A selected constituent missing from the input file is skipped without
  an error.** This is the existing `isAvailable` behaviour: when the
  variable isn't in the file, the read silently leaves the field unchanged.
  An explicit list doesn't make the listed constituents required.
- **Constituents that aren't selected keep their current values.** Nothing
  is reset or zeroed. Users who read a subset need to make sure the other
  constituents are initialised elsewhere.

**Restart safety**

- **Nothing prevents subsetting `restart` or `input`.** A list on these
  streams will produce a restart file, or initial state, that is missing
  prognostic constituents. Only the user can catch this.

**Code-level issues to fix before merging**

- **Attributes are still processed for constituents that aren't selected.**
  The `put_get_field_atts` loop in `MPAS_streamAddField_3dReal` runs over
  every constituent. For those not defined in the file, `MPAS_io_put_att`
  and `MPAS_io_get_att` fail quietly because no `ierr` is passed. Output is
  unaffected, but the work is wasted and the behaviour is fragile. The loop
  should skip entries where `isAvailable(i)` is `.false.`.
- **Temporary debug logging is still in place:**
  - one log line per constituent each time a stream is built,
  - one log line per constituent on every read and every write,
  - `STREAM_WARNING_WRITE` for every field passed to
    `MPAS_stream_mgr_add_field`,
  - `'Is field ... active in stream'` promoted from `STREAM_DEBUG_WRITE` to
    `STREAM_WARNING_WRITE` in `build_stream`.

  All of these have to be removed or put back at debug level.
- **`stream_mgr_check_constituents` is exported publicly.** It is used only
  inside the module and doesn't follow the `MPAS_stream_mgr_*` naming used
  for public routines, so it should be private.
- **The read-loop condition is redundant.** It tests
  `isAvailable(j) .and. isRequested(j)`, but `isAvailable(j)` can only be
  `.true.` for a requested constituent, so the test can be just
  `isAvailable(j)`.

### Related pre-existing issues found during testing

These are not caused by this change, but they came up while testing it.

- **A per-constituent `default_value` in Registry.xml is silently ignored.**
  `gen_inc.c` reads `default_value` only from the `<var_array>` element, and
  `field3DReal` holds a single `defaultValue` for the whole array. To give
  constituents different initial values, set them in core code, for example
  in `atm_core_init` before the input and restart reads.
- **A failed time seek gives an unclear error.** If an input stream is read
  with a `whence` other than `MPAS_STREAM_EXACT_TIME`, and its file has
  `xtime` but no matching record, `read_stream` passes record 0 to
  `MPAS_readStream`. The model then stops with a low-level library error
  (`SMIOLf_get_var failed with error -5` / `NetCDF: Index exceeds dimension
  bound`) instead of a clear "no record found" message. An example is the
  timestep-loop reads, which use `MPAS_STREAM_LATEST_BEFORE`.
