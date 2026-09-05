# MPAS-Embedded CoLM2024

This directory contains the CoLM2024 land-surface physics used by
MPAS-Atmosphere when `config_lsm_scheme = 'sf_colm2024'`.

Build CoLM2024 through MPAS:

- CMake: configure with `-DMPAS_COLM2024=ON`.
- Make: build atmosphere with `COLM2024=true`.

Run with `config_num_soil_layers = 10`. CoLM keeps its native 10-layer soil
column and its patch/PFT state; MPAS cells map one-to-one to CoLM elements.

The embedded clock and atmosphere feedback require:

- `config_calendar_type = 'gregorian'`;
- an active PBL and surface-layer scheme;
- `config_pbl_interval = 'none'`, so every CoLM advance uses the current MPAS
  physics timestep; and
- `config_radt_sw_scheme = 'rrtmg_sw'`, which supplies the required VIS/NIR
  direct and diffuse radiation components.

Initialization rejects land data whose element ID or center does not match the
MPAS cell, whose pixel-union area differs from `areaCell` by more than 5%, or
whose dominant CoLM land/water patch class contradicts the static MPAS mesh `landmask`.

CoLM supplies its native land `T2/Q2`, `U10/V10`, `CHS2/CQS2`, and `CD10`
diagnostics to MPAS. Nonlinear 2 m/10 m quantities are formed for each PFT
before area aggregation. MPAS `CK` is left to the active surface-layer scheme
because CoLM does not expose the 10 m moisture profile needed to replace it
without a roughness-length approximation.

The coupling transfers CoLM's four native surface-albedo components (VIS/NIR,
direct/diffuse) to RRTMG shortwave over CoLM-owned land cells; ocean and sea-ice
cells retain their scheme-specific MPAS albedo. CoLM and RRTMG also share the
same monthly atmospheric CO2 volume-mixing ratio, updated after each successful
CoLM step, so radiation and land photosynthesis do not follow different CO2
clocks.

MPAS initializes MPI and passes its domain communicator to CoLM. CoLM does not
start a separate process pool or require extra ranks. The MPAS LSM driver owns a
duplicated communicator context that isolates CoLM river-routing, remapping, and
restart messages while using the same MPAS processes. Distributed CoLM code still
issues the collectives and point-to-point exchanges required by those algorithms,
but only on this borrowed MPAS-owned context; it never owns an MPI runtime.

Standalone CoLM run cases, forcing namelist examples, and evaluation scripts
are intentionally not kept in this embedded source tree.

## Source provenance

The vendored source baseline is pinned to CoLM202X commit
`e1d854793c3ac19132128fb389005a74eb34c313` from
`https://github.com/CoLM-SYSU/CoLM202X.git`. MPAS-specific adaptation starts
at MPAS-Model commit `7c1441df36d4a2523461fd45d49a3927fe561820`.
These full hashes are the offline comparison baseline; later changes under
this directory are local MPAS integration patches and must not be treated as
an unmodified upstream checkout. Update both hashes deliberately when the
vendored source is rebased.

## Restart ownership

MPAS and CoLM write one synchronized checkpoint, but they do not use one
physical file. The MPAS restart stream contains the atmosphere state. CoLM
writes its patch/PFT and river state below
`DEF_dir_output/DEF_CASE_NAME/restart/<date>/`, using rank-sharded NetCDF
files plus a `.mpas_complete` marker. Preserve the complete CoLM restart tree
together with the matching MPAS restart file; a marker without every shard,
or shards without their marker, is an incomplete checkpoint and is rejected.
CoLM writes this state only when the MPAS restart stream alarm rings; normal
timesteps and finalization do not create independent CoLM checkpoints.

Completion markers use the strict `distributed-pixelset-v2` protocol. Each
marker records its format version, case/run identity, a decomposition-stable
fingerprint of each global MPAS cell's ID, latitude, longitude, and area, the
checkpoint-family ID and role, the patch/PFT/river companion basenames, the
exact logical CoLM restart basename, checkpoint time, and writer rank count.
Every patch/PFT rank shard carries the same family ID, role, and logical parent;
the river file carries the same family manifest as global NetCDF attributes.
Restart reads validate the complete family before loading state and reject
legacy or malformed metadata and any generation, run, grid, role, filename, or
time mismatch. The grid fingerprint is a stable integrity identifier, not a
UUID or content checksum. MPAS currently does not expose a checkpoint UUID to
this LSM interface or persist a shared UUID in both physical restart products,
so this validation prevents accidental mixing inside the CoLM restart family
but is not an atomic two-file transaction with the MPAS restart file. Operators
must still archive the matching MPAS file and CoLM restart tree together.

`DEF_dir_restart` accepts paths up to 1024 characters; derived checkpoint,
block-shard, and marker names are carried in longer or automatically sized
buffers so they are not silently truncated at the former 256-character limit.

All MPAS ranks must see the configured land-data and restart paths through a
consistent shared filesystem. CoLM serial metadata and indexed restart reads
do not provide a node-local file staging layer.

## Embedded feature boundaries

MPAS streams own history output, so set `DEF_HIST_FREQ = 'none'` and disable
CoLM history write-back. MPAS currently has no 14-component aerosol-deposition
flux matching CoLM SNICAR, so embedded runs must set
`DEF_Aerosol_Readin = .false.`; SNICAR then runs without external aerosol
deposition rather than inventing a surrogate flux.

The bundled embedded build intentionally rejects CoLM BGC, crop, LULCC, urban,
data-assimilation, hyperspectral, sediment, split-AI, external-lake, and
non-unstructured configurations until their sources and MPAS-owned interfaces
are integrated and tested.
