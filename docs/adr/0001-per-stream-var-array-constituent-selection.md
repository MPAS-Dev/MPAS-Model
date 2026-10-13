# ADR 0001: Per-stream selection of var_array constituents

- **Status:** Proposed (work in progress on branch `framework/select_scalars_io`)
- **Date:** 2026-10-15
- **Author:** Abishek Gopal
- **Commits:** `20b7f10f6` (Attempt 1), `3d1dd5dcf` (Changes to ensure separate streams can write different sets of scalars), `0d2f303b5` (WIP)

## Context

A var_array in MPAS, such as `scalars`, packs an arbitrary number of constituents
(`qv`, `qc`, `qr`, ..., `nwfa`, `tke`). Each constituent is written to the file as its own variable.
Until now, a stream could include a var_array only as a whole: listing
`<var_array name="scalars"/>` wrote every constituent that is active at
run time.

Users have expressed interest in the ability to read and write different I/O streams with different subsets of the
scalars, without having to change Registry.xml and rebuild the code. For example, a high-frequency stream might carry only `qv` while
the history stream carries all hydrometeors.


Before this work, the only way a constituent was left out of output was
through Registry packages. When a constituent's package is inactive or when a constituent was excluded through preprocessor directives, the
generated code in `atm_generate_structs` never assigns it an index and
leaves it out of `constituentNames`. The constituent then doesn't exist for
any stream. That mechanism is global and determined at startup, so it can't
express per-stream choices.

## Decision

1. **Syntax.** A stream field name can take the form
   `varArray:constituent1,constituent2,...`, for example
   `<var_array name="scalars:qv,qc"/>` in `streams.atmosphere` or
   `scalars:qv,qc` in a stream list file. The XML parser passes the string
   through unchanged to `MPAS_stream_mgr_add_field`, which splits it at the
   first `:`.

2. **Validation when the stream is defined.** `MPAS_stream_mgr_add_field`
   calls `stream_mgr_check_constituents`, which checks that:
   - the field exists,
   - it is a 3-D real var_array,
   - every listed name is in its `constituentNames`.

   Any failure is a stream error, so a bad list stops stream setup instead
   of silently producing an incomplete file.

3. **The selection is stored per stream, not on the field.** A new pool,
   `field_cons_pool`, is added to `MPAS_stream_list_type`. It holds the raw
   list under the key `<varArray>:constituents`. The pool is created and
   destroyed together with the stream's other pools.
   `MPAS_stream_mgr_add_stream_fields` copies the entry from a reference
   stream, and `MPAS_stream_mgr_remove_field` removes it.

4. **The mask is applied when the stream is built.** When the low-level
   stream is assembled, the stream manager reads the stored list.
   `parse_constituent_list` turns it into a logical mask over
   `constituentNames`, which is passed to `MPAS_streamAddField_3dReal` as
   the new optional argument `activeConstituents`. Unselected constituents
   are skipped when variables are defined in the file and when attributes
   are written. An empty list selects every constituent. Without the
   argument, behaviour is unchanged.

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

### B. Keep using Registry packages, rejected

Either using #ifdefs or defining new packages to stand in for output groupings. The first option requires a rebuild, and the packages option control
allocation, are global and are tied to namelist logic. They are the wrong
tool for a choice about output content.

### C. A separate attribute on the `<var_array>` XML element, not pursued

An attribute such as `<var_array name="scalars" constituents="qv,qc"/>`
would be clearer. However, it would need changes to `xml_stream_parser.c`
and to the C to Fortran interface (`stream_mgr_add_field_c`). Folding the
list into the name lets it pass through the existing interface, including
stream list files, without any new arguments. Attempt 1 also tried adding
C-callable checks (`stream_mgr_check_field_exists_c`,
`stream_mgr_check_constituent_c`). `0d2f303b5` removed them because
validation now happens entirely inside `MPAS_stream_mgr_add_field`.

## Consequences

### Positive

- Each stream can write its own subset of a var_array's constituents.
- No changes to field types, the Registry generator or core code. The
  feature lives entirely in `mpas_stream_manager.F`, `mpas_io_streams.F`
  and `mpas_stream_list_types.inc`.
- Existing stream definitions behave exactly as before, because the
  argument is optional and the pool entry is absent when no list is given.
- Mistakes in a constituent list cause a fatal error when streams are set
  up.

### Negative and limitations

- **Only 3-D real var_arrays are supported.** Other var_array shapes are
  rejected by `stream_mgr_check_constituents`.
- **Interaction with packages.** If a listed constituent exists in the
  Registry but its package is inactive (for example `scalars:qv,tke` with
  LES off), it is missing from `constituentNames` and the stream fails
  validation. Users have to keep their lists consistent with their physics
  configuration.
- **Overloaded name syntax.** The `:` syntax overloads the field name. Any
  future field name that contains `:` would conflict with it.
- **One list per var_array per stream.** Adding the same var_array to a
  stream a second time, with a different list, triggers the existing
  "already in stream" warning, and the second list is ignored.


