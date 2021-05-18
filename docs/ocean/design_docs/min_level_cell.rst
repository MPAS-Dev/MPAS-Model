Inactive Cells at the Top of the Water Column
=============================================

date: 2021/02/11

Contributors: Xylar Asay-Davis, Carolyn Begeman



Summary
-------

For increased flexibility in representing ice-shelf cavities in MPAS-Ocean,
we require a mechanism for inactivating cells in the water column that are
above the ice draft (the depth of the ice shelf-ocean interface).  This
capability would be similar to the disabled cells below the bathymetry.
Currently, ice-shelf cavities use a vertical coordinate where top layer follows
the ice draft, but pressure-gradient errors caused by steep, tilted layers
place many restrictions on this approach (e.g. layers are thick and bathymetry
is not well represented).  As a step toward a more flexible vertical coordinate
that can better adapt to changing ice topography in time, we require a
capability to disable top cells and thus mitigate the constraints imposed by
the current terrain-following top coordinate.  A successful implementation will
be able to run an MPAS-Ocean test case with ice-shelf cavities using a
prescribed vertical coordinate with inactive cells at the top of the water
column.  The test case will compute "surface" fluxes at the top active layer
and distribute them from the top down into active layers.  All test cases where
no top layers are inactive must produce results that remain bit-for-bit
identical to the current implementation.


Requirements
------------

Requirement: Inactive top cells
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2021/02/11

Contributors: Xylar Asay-Davis

We require a mechanism for indicating that top cells in the water column are
inactive and preventing computation on these cells.

Requirement: Surface fields and fluxes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2021/02/11

Contributors: Xylar Asay-Davis

MPAS-Ocean includes several fields that represent the surface values of 3D
fields.  These fields must use the top active cell in a column rather than the
first (potentially inactive) cell.  Similarly, the distribution of surface
fluxes into the water column must begin from the top with the first active
cell, not the first (potentially inactive) cell.

A separate design process, we will want to look at whether surface
parameterizations (notably KPP) are appropriate is implemented when they are
used under ice shelves.  It is important that masking of surface fluxes
(e.g. with ``landIceMask``) is kept consistent with inactive cells, so that
atmospheric is not applied under ice shelves.  This is a consideration we
already must account for with the current, terrain-following approach but
discrepancies between the boundary of ``landIceMask`` and the calving front
as seen in the vertical coordinate will become much more obvious with the new
coordinate.

Requirement: Correct boundary conditions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2021/02/11

Contributors: Phil Jones, Xylar Asay-Davis,

Both horizontal and vertical Boundary conditions need be applied correctly,
given that inactive layers introduce vertical faces that were previously not
present. In particular, higher-order vertical interpolants in routines like
tracer advection may need special care. Not only are loop limits changed, but
some new edge cases appear if max-min is not large enough.

Requirement: Inactivate cells during simulation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2021/02/11

Contributors: Xylar Asay-Davis

In the longer term, we require the capability to activate/inactivate top cells
as a simulation progresses and the vertical coordinate evolves.  We do not plan
to include this capability in this particular design but we want to keep it in
mind, so that we do not implement the "static-in-time" version of this
capability now in a way that makes the dynamic version more difficult.

Requirement: Bit-for-bit when no inactive top cells
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2021/02/11

Contributors: Xylar Asay-Davis

In setups without any inactive top cells, results should be unchanged
(bit-for-bit).

Requirement: No significant loss of performance
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2021/02/11

Contributors: Xylar Asay-Davis

The implementation should have a negligible impact on model performance.  This
is especially critical when there are no inactive top cells, but we also do not
want inclusion of inactive top cells to slow the model down unless absolutely
necessary.


Implementation
--------------

Implementation: Inactive top cells
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2021/04/21

Contributors: Xylar Asay-Davis, Carolyn Begeman

We will add ``minLevelCell``, analogous to ``maxLevelCell``.  In
``ocn_init_routines_compute_max_level()`` in the ``ocn_init_routines`` module,
several other derivative fields are initialized based on ``maxLevelCell``.
We will rename this function ``ocn_init_routines_compute_min_max_level()``
and will initialized the following new variables:

* ``minLevelEdgeTop`` - the minimum of ``minLevelCell`` on adjacent cells

* ``minLevelEdgeBot`` - the maximum of ``minLevelCell`` on adjacent cells

* ``minLevelVertexTop`` - the minimum of ``minLevelCell`` on adjacent cells

* ``minLevelVertexBot`` - the maximum of ``minLevelCell`` on adjacent cells

The definitions of the following variables currently in this routine will be
updated to include ``minLevelCell``:

* ``boundaryEdge``

* ``edgeMask``

* ``cellMask``

* ``vertexMask``

A very large number of vertical loops will need to be altered in MPAS-Ocean.
Thus, this work will be divided into at least two phases. 

The first phase consists of subroutines that are involved in any of the test cases for
the ``sub_ice_shelf_2D`` configuration in legacy COMPASS.  This will give us
a testbed for ensuring that the new functionality behaves as expected without
requiring us to work through routines related to parameterizations, such as
KPP and GM, that are not used in these test cases.

What follows are prioritized checklists of the modules with variables that need
to be added and subroutines where loops or other vertical indexing needs to be
updated.  First are those related to ``init`` mode, then those used in
``forward`` mode.

`Phase 1 <https://github.com/MPAS-Dev/MPAS-Model/pull/825>`_

Init:

* [X] ``ocn_init_sub_ice_shelf_2D``:

  * [X] ``ocn_init_setup_sub_ice_shelf_2D()``

* [X] ``ocn_init_ssh_and_landIcePressure``:

  * [X] ``ocn_init_ssh_and_landIcePressure_balance()``

* [X] ``ocn_init_vertical_grids``:

  * [X] ``ocn_init_vertical_grid()``
  
  * [X] ``ocn_compute_z_level_layerThickness()``
  
  * [X] ``ocn_compute_zMid_from_layerThickness()``

  * [X] ``ocn_compute_z_star_layerThickness()``

  * [X] ``ocn_alter_bottomDepth_for_pbcs()``

  * [X] ``ocn_alter_ssh_for_ptcs()``

  * [X] ``ocn_compute_Haney_number()``

* [X] ``ocn_init_isomip``:

  * [X] ``ocn_init_setup_isomip()``

Forward:

* [X] ``ocn_init_routines``:

  * [X] ``ocn_init_routines_compute_min_max_level()``

  * [X] ``ocn_init_routines_vert_coord()``

  * [X] ``ocn_init_routines_block()``

* [X] ``ocn_time_integration_split``:

  * [X] ``ocn_time_integration_split_init()``

  * [X] ``ocn_time_integrator_split()``

* [X] ``ocn_mesh``:

  * [X] ``minLevelCell``

  * [X] ``minLevelEdgeTop``

  * [X] ``minLevelEdgeBot``

  * [X] ``minLevelVertexTop``

  * [X] ``minLevelVertexBot``

  * [X] ``ocn_meshCreate()``

  * [X] ``ocn_meshUpdateFields()``

  * [X] ``ocn_meshDestroy()``

* [X] ``ocn_forcing``:

  * [X] ``ocn_forcing_build_fraction_absorbed_array``

* [X] ``ocn_thick_hadv``:

  * [X] ``ocn_hadv_thick_tend``

* [X] ``ocn_thick_vadv``:

  * [X] ``ocn_vadv_thick_tend``

* [X] ``ocn_thick_surface_flux``:

  * [X] ``ocn_thick_surface_flux_tend``

* [X] ``ocn_tracer_advection``:

  * [X] ``ocn_tracer_advection_tend``

* [X] ``ocn_tracer_advection_mono``:

  * [X] ``ocn_tracer_advection_mono_tend``

* [X] ``ocn_vel_forcing_surface_stress``:

  * [X] ``ocn_vel_forcing_surface_stress_tend``

* [X] ``ocn_vel_hmix_del2``:

  * [X] ``ocn_vel_hmix_del2_tend``

* [X] ``ocn_vel_hadv_coriolis``:

  * [X] ``ocn_vel_hadv_coriolis_tend``

* [X] ``ocn_vel_vadv``:

  * [X] ``ocn_vel_vadv_tend``

* [X] ``ocn_vmix_cvmix``:

  * [X] ``ocn_vmix_coefs_cvmix_build``

* [X] ``ocn_diagnostics``:

  * [X] ``ocn_relativeVorticity_circulation()``

  * [X] ``ocn_diagnostic_solve_layerThicknessEdge()``

  * [X] ``ocn_diagnostic_solve_vorticity()``

  * [X] ``ocn_diagnostic_solve_richardson()``

  * [X] ``ocn_diagnostic_solve_surfaceLayer()``

  * [X] ``ocn_diagnostic_solve_vortVel()``

  * [X] ``ocn_diagnostic_solve_z_coordinates()``

  * [X] ``ocn_diagnostic_solve_pressure()``

  * [X] ``ocn_vert_transport_velocity_top()``

  * [X] ``ocn_fuperp()``

  * [X] ``ocn_compute_land_ice_flux_input_fields()``

  * [X] ``ocn_validate_state()``

* [X] ``ocn_thick_ale``:

  * [X] ``ocn_ALE_thickness``

* [X] ``ocn_vel_pressure_grad``:

  * [X] ``ocn_vel_pressure_grad_tend``

* [X] ``ocn_vmix``:

  * [X] ``ocn_vmix_implicit``

  * [X] ``ocn_vel_vmix_tend_implicit``

  * [X] ``ocn_tracer_vmix_tend_implicit``

* [X] ``ocn_vmix_cvmix``:

  * [X] ``ocn_vmix_coefs_cvmix_build``


`Phase 2 <https://github.com/MPAS-Dev/MPAS-Model/pull/840>`_
(enabling other configuration options):

Init:

* [X] ``ocn_init_isomip_plus``:

  * [X] ``ocn_init_setup_isomip_plus()``

Forward:

* [X] ``ocn_tracer_surface_flux_to_tend``:

  * [X] ``ocn_tracer_surface_flux_tend``

* [X] ``ocn_diagnostics``:

  * [X] ``ocn_filter_btr_mode_tend_vel()``

* [X] ``ocn_tendency``:

  * [X] ``ocn_tend_freq_filtered_thickness``

* [X] ``ocn_tracer_advection_std``:

  * [X] ``ocn_tracer_advection_std_tend``

* [X] ``ocn_tracer_DMS``:

  * [X] ``ocn_tracer_DMS_compute``

  * [X] ``ocn_tracer_DMS_surface_flux_compute``

* [X] ``ocn_tracer_ecosys``:

  * [X] ``ocn_tracer_ecosys_compute``

  * [X] ``ocn_tracer_ecosys_surface_flux_compute``

* [X] ``ocn_tracer_MacroMolecules``:

  * [X] ``ocn_tracer_MacroMolecules_compute``

* [X] ``ocn_tracer_short_wave_absorption_jerlov``:

  * [X] ``ocn_tracer_short_wave_absorption_jerlov_tend``

* [X] ``ocn_tracer_short_wave_absorption_variable``:

  * [X] ``ocn_tracer_short_wave_absorption_variable_tend``

* [X] ``ocn_tracer_interior_restoring``:

  * [X] ``ocn_tracer_interior_restoring_compute``

* [X] ``ocn_frazil_forcing``:

  * [X] ``ocn_frazil_forcing_layer_thickness``

  * [X] ``ocn_frazil_forcing_active_tracers``

  * [X] ``ocn_frazil_forcing_build_arrays``

* [X] ``ocn_diagnostics``:

  * [X] ``ocn_compute_KPP_input_fields()``

* [X] ``ocn_vmix``:

  * [X] ``ocn_compute_kpp_rhs``

  * [X] ``ocn_vel_vmix_tend_implicit_spatially_variable``

  * [X] ``ocn_vel_vmix_tend_implicit_spatially_variable_mannings``

  * [X] ``ocn_vel_vmix_tend_implicit_rayleigh``

* [X] ``ocn_gm``:

  * [X] ``ocn_GM_compute_Bolus_velocity()``

* [X] ``ocn_tracer_hmix_del2``:

  * [X] ``ocn_tracer_hmix_del2_tend``

* [X] ``ocn_tracer_hmix_del4``:

  * [X] ``ocn_tracer_hmix_del4_tend``

* [X] ``ocn_tracer_hmix_Redi``:

  * [X] ``ocn_tracer_hmix_Redi_tend``: Incomplete

* [X] ``ocn_tracer_nonlocalflux``:

  * [X] ``ocn_tracer_nonlocalflux_tend``

* [X] ``ocn_high_freq_thickness_hmix_del2``:

  * [X] ``ocn_high_freq_thickness_hmix_del2_tend``

* [X] ``ocn_vel_hmix_del4``:

  * [X] ``ocn_vel_hmix_del4_tend``

* [X] ``ocn_vel_hmix_leith``:

  * [X] ``ocn_vel_hmix_leith_tend``

* [X] ``ocn_tracer_exponential_decay``:

  * [X] ``ocn_tracer_exponential_decay_compute``

* [X] ``ocn_tracer_ideal_age``:

  * [X] ``ocn_tracer_ideal_age_compute``

* [X] ``ocn_tidal_forcing``:

  * [X] ``ocn_tidal_forcing_build_array``

  * [X] ``ocn_tidal_forcing_layer_thickness``

  * [X] ``ocn_compute_tidal_potential_forcing``

* [X] ``ocn_vel_tidal_potential``:

  * [X] ``ocn_vel_tidal_potential_tend``

* [X] ``ocn_wetting_drying``:

  * [X] ``ocn_wetting_drying_verify``

  * [X] ``ocn_prevent_drying_rk4``

  * [X] ``ocn_wetting_drying_wettingVelocity``

.. note::

  ``minLevelCell`` changes to ``ocn_tracer_short_wave_absorption_variable`` have not been fully 
  tested as this option requires initialization files not included in existing test cases and this 
  option has not been used for production runs.

.. note::

  ``minLevelCell`` changes to ``ocn_tracer_hmix_Redi_tend`` are currently incomplete and will be 
  completed in a later bugfix due to their effects on some ecosystem tracers.

Phase 3 (changes to initialization):

Init:

* [ ] ``ocn_init_global_ocean``:

  * [ ] ``ocn_init_setup_global_ocean()``

  * [ ] ``ocn_init_setup_global_ocean_create_model_topo()``

  * [ ] ``ocn_init_setup_global_ocean_deepen_critical_passages()``

  * [ ] ``ocn_init_setup_global_ocean_interpolate_land_ice_topography()``

  * [ ] ``ocn_init_setup_global_ocean_modify_temp_under_land_ice()``

  * [ ] ``ocn_init_setup_global_ocean_cull_inland_seas()``

  * [ ] ``ocn_init_setup_global_ocean_interpolate_tracers()``

* [ ] ``ocn_init_cell_markers``:

  * [ ] ``ocn_mark_maxlevelcell()``

Forward:

* [ ] ``ocn_init_routines``:

  * [ ] ``ocn_init_routines_compute_min_max_level()``: add timers

.. note::

  May need to reconsider nVertLevels argument to ``ocn_sea_ice_init``

.. note::

  ``ocn_mark_maxlevelcell`` doesn't need to be changed if the dry cells are 
  assigned ``maxLevelCell = 0``, in which case they will be culled.

Outside the scope of this development:

* ``ocn_equation_of_state``

.. note::

  ``ocn_equation_of_state_jm`` currently doesn't include any reference to
  ``maxLevel*`` but this is mildly concerning.  T and S are clipped to the
  valid range before density is computed, meaning that contamination with
  invalid values could go unnoticed.  Still, it does appear that
  ``displacedDensity`` is currently only used starting at index ``k = 2``, so
  no invalid values should be getting used.


Implementation: Surface fields and fluxes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2021/04/21

Contributors: Xylar Asay-Davis, Carolyn Begeman, Luke Van Roekel

The subroutines  ``ocn_thick_surface_flux_tend()`` and
``ocn_tracer_surface_flux_tend()`` already distribute surface fluxes over
multiple vertical levels in a loop, so surface fluxes will simply require the
same treatment as any other loops.

It will be a little trickier to make sure we perform proper indexing of all
3D variables to get their "surface" values. The trickiness is in finding these
variables in the code. It's easy to search for ``maxLevel`` and find relevant
loops, but it's a bit harder to usefully search for an index value of ``1`` or
``2``, particularly if it's not associated with a loop over k. The surface
variables of concern (listed below) are variables where the index over
``nVertLevels`` is ``1`` without it being in a loop, so it would be easy to
miss this and leave it as ``1`` instead of ``minLevelCell(iCell)`` or
equivalent.

Here, the plan is to make sure
that variables are set to the NetCDF fill value (a large, negative number)
when they are invalid so that contamination should be obvious.

Here is a checklist of variables or subroutines requiring special care because
of top indexing that might not be easy to find (e.g. by searching for
``maxLevel``):

* [ ] ``ocn_diagnostics``:

  * [X] ``tracersSurfaceValue``

  * [X] ``normalVelocitySurfaceLayer``

  * [ ] ``ssh``

  * [X] ``fracAbsorbed``

  * [X] ``fracAbsorbedRunoff``

  * [X] ``nonLocalSurfaceTracerFlux``

  * [X] ``surfaceBuoyancyForcing``

  * [X] ``topDrag``

  * [X] ``topDragMag``

  * [X] ``landIceFrictionVelocity``

...


For KPP there are a bunch of hard coded ``1`` indices in the construction of
the depth coordinate and surface layer averaging
(`see this example <https://github.com/MPAS-Dev/MPAS-Model/blob/ocean/develop/src/core_ocean/shared/mpas_ocn_vmix_cvmix.F#L506-L534>`_).
This could be missed if the focus is just switching loop bounds, but should be
easy to implement:

.. code-block:: fortran

    do i=1,nEdgesOnCell(iCell)
      iEdge = edgesOnCell(iCell)
      deltaVelocitySquared(minLevelEdge(iEdge))
      ...
      do kIndexOBL = minLevelEdge(iEdge)+1,maxLevelelCell(iCell)

The same would likely hold for GM routines that have this type of structure.

Implementation: Correct boundary conditions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2021/02/11

Contributors: Xylar Asay-Davis,

I am not set on the implementation here, but my suggestion would be that we
retain the requirement that there are a minimum of 3 layers.  I believe the
higher-order interpolants were one reason for this.  In POP2x, it was more
practical to prevent the worst kinds of pathological edge cases as part of
mesh update, rather than trying to build it into the forward model.  That may
not be a good option in MPAS-Ocean, particularly with a dynamic boundary.  But
we may still be able to include constrains that prevent us from hitting the
worst cases (e.g. adjacent cells that have a shared edge but no or too few
layers in common to have any flow between them).

While I think we definitely need to explore these issue, maybe this is too much
for the current design document.

Implementation: Inactivate cells during simulation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2021/02/11

Contributors: Xylar Asay-Davis

The main considerations here will be:

* How time-consuming will it be to call
  ``ocn_init_routines_compute_min_max_level()`` each time ``minLevelCell`` has
  changed?  Is there anything we want to do now to make sure it is efficient?

* Do we notice any other potential problem areas as we are going through the
  code to modify loops?

Implementation: Bit-for-bit when no inactive top cells
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2021/04/21

Contributors: Xylar Asay-Davis, Carolyn Begeman

We will set ``minLevelCell`` to all ``1`` by default.  We will take care not
to reorder computations in a way that would likely lead to non-bit-for-bit
changes.

A few non-bit-for-bit changes were unavoidable on some compilers or were preferred
to clunky work-arounds:

* `Loop limits <https://github.com/MPAS-Dev/MPAS-Model/blob/233da699cf7bd9f6e40812d8594a95f1c69de984/src/core_ocean/shared/mpas_ocn_diagnostics.F#L678>`_
  in ``ocn_vorticity`` to solve for ``normalizedRelativeVorticityVertex`` 
  and ``normalizedPlanetaryVorticityVertex`` could not be changed from 
  ``1,maxLevelVertexBot`` to ``minLevelVertexTop,maxLevelVertexBot`` without introducing
  non bit-for-bit changes in the MPAS nightly regression suite.

* Redi: non-bit-for-bit results introduced to 6 ecosystem tracers on 
  ``QU240/bgc_ecosys_test``. See 
  `this comment <https://github.com/MPAS-Dev/MPAS-Model/pull/840#discussion_r612523174>`_.

* Jacobian from TS pressure gradient: non-bit-for-bit results introduced on 2 E3SM tests
  on Compy with PGI optimized-mode. See
  `this discussion <https://github.com/E3SM-Project/E3SM/pull/4171#issuecomment-804725955>`_.

Implementation: No significant loss of performance
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2021/04/21

Contributors: Xylar Asay-Davis, Carolyn Begeman

We will take care not to introduce unnecessary ``if`` statements or equivalents
that were not present before.

The only new ``if`` statements introduced are located in:

* ``ocn_init_vertical_grids: ocn_alter_ssh_for_ptcs``

* ``ocn_init_routines: ocn_init_routines_compute_min_max_level``

Testing
-------

Testing: Inactive top cells
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2021/04/21

Contributors: Xylar Asay-Davis, Carolyn Begeman, Mark Petersen

We will ensure that all 3D, prognostic variables have been initialized to the
NetCDF fill value in inactive cells at the top of the water column.  We will
attempt to do the same for 3D diagnostic variables.  This should expose any
computations involving these invalid cells.

We have modified the ``sub_ice_shelf_2D`` configuration in legacy COMPASS to
support a z-level initial coordinate in the ice-shelf cavity (including writing
out ``minLevelCell`` in the initial condition).  The implementation is in a
`add_z_level_sub_ice_shelf_2D COMPASS branch <https://github.com/xylar/compass/tree/add_z_level_sub_ice_shelf_2D>`_
and a corresponding `add_z_level_sub_ice_shelf_2D MPAS-Model branch <https://github.com/xylar/MPAS-Model/tree/ocean/add_z_level_sub_ice_shelf_2D>`_.


To begin wth, these tests also have melt fluxes disabled along with all
tendency terms.

The test cases include "partial top cells", analogous to "partial bottom
cells". When constructing your initial domain with variable ``minLevelCell``,
we have generalized the namelist options for partial bottom cells to refer to
both bottom and top cells, so that we can specify either full or partial
(bottom and top) cells. The eventual goal is always partial top cells, to model
the ice draft more realistically. But it is useful to have a full cell option
in your standard test suite.

When initialize with constant T and S horizontally and no surface forcing, a
zero initial velocity should remain exactly zero with full top and bottom
cells. Pressure gradient errors with partial top/bottom cells will mean that
the velocity will be nonzero but should remain small. This is similar to the
sea mount test, where the velocity is a measure of the error.

We will progress through the following 3 test cases:
These define 3 new test cases:

1. ``z_level_full_cells_const_S`` -  T and S are uniform in 3D. Full top and
   bottom cells. Verify min and max T and S remain exactly constant, velocity
   remains exactly zero.

2. ``z_level_full_cells`` - S is horizontally uniform (T remains uniform in
   3D), stably stratified in vertical. Disable vertical tracer mixing. Full top
   and bottom cells. Verify T and S  remain unchanged, velocity remains exactly
   zero.

3. ``z_level`` - Same as 2. but partial top and bottom cells. Maximum velocity
   is a measure of the error and should grow slowly at top and bottom(e.g.
   1e-6m/s after a day).

To begin with, these tests also have melt fluxes disabled along with all
tendency terms.  As part of debugging, we will gradually turn on tendencies
and check the behavior.

We will run on multiple machines (Ubuntu laptop, Anvil, Grizzly, Cori, Compy)
with a mix of Gnu and Intel compilers.  We will plot the resulting T, S and KE
fields to make sure they look comparable to the results with the current
terrain-following coordinate.

We will test with linear, JM and Wright equations of state:

* [X] linear
* [X] JM
* [ ] Wright

The change in velocity over 3, 5-minute timesteps for each of the test cases is 
as follows with the JM equation of state:

* ``z_level_full_cells_const_S``: O(1e-14) m/s
* ``z_level_full_cells``: O(1e-11) m/s
* ``z_level``: O(1e-10) m/s

Over a 6-month simulation, the velocity error plateaus at O(1e-3) m/s for the ``z_level`` 
test case.

Testing: Surface fields and fluxes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2021/04/21

Contributors: Xylar Asay-Davis, Carolyn Begeman

The testing in ``sub_ice_shelf_2D`` will account for the surface fluxes and
fields that we most anticipate being affected by ``minLevel*``.  We will also run 
these configurations with GM activated. Testing of other surface fields will 
likely require running tests that include KPP and other parameterizations that 
are not part of this configuration by default.

KPP and GM will be tested in existing global ocean test cases for which 
``minLevelCell = 1``. KPP will also be tested with the spherical and planar
``single_column_test``. We will consider producing a modified global test case
which has an identical number of vertical levels but with ``minLevelCell = 2``.
This should allow us to identify any missed surface fields as well. 

Testing: Correct boundary conditions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2021/02/25

Contributors: Mark Petersen and Xylar Asay-Davis

The 3 z-level versions of ``sub_ice_shelf_2D`` described above should provide
sufficient testing of the boundary conditions related to tendency terms (e.g.
higher-order tracer reconstruction). More sophisticated parameterizations
(KPP and Redi) will be addressed in the second phase of this work.

Testing: Inactivate cells during simulation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2021/02/11

Contributors: Xylar Asay-Davis

Since this is just a consideration, no testing is necessary.  We will add
timers around ``ocn_init_routines_compute_min_max_level()`` (if they are not
already present) to see if the timing is significant, though a global test
may be necessary to get a realistic feel.

Testing: Bit-for-bit when no inactive top cells
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2021/04/21

Contributors: Xylar Asay-Davis, Carolyn Begeman

We will run the legacy COMPASS nightly and land-ice-fluxes regression suites
on multiple machines (Ubuntu laptop, Anvil, Grizzly, Compy) with a mix of Gnu
and Intel compilers to make sure nothing has changed when we run with
``minLevelCell = 1`` everywhere.

We will also run the following E3SM tests before and after the changes:

* ``SMS_D.T62_oQU120_ais20.MPAS_LISIO_TEST.cori-knl_intel``
* ``SMS_D.T62_oQU120_ais20.MPAS_LISIO_TEST.cori-knl_gnu``
* ``PEM_Ln9.ne30_oECv3.A_WCYCL1850S.cori-knl_intel``
* ``PET_Ln9.ne30_oECv3.A_WCYCL1850S.cori-knl_gnu``
* ``SMS_Ld1.ne30pg2_r05_EC30to60E2r2.A_WCYCL1850S_CMIP6.compy_intel.allactive-wcprod``
* ``SMS_PS.northamericax4v1pg2_WC14to60E2r3.A_WCYCL1850S_CMIP6.compy_intel.allactive-wcprodrrm``
* ``SMS_D.T62_oQU120_ais20.MPAS_LISIO_TEST.compy_pgi``
* ``SMS.T62_oQU120_ais20.MPAS_LISIO_TEST.compy_intel``
* ``PET_Ln9_P1024.ne30_oECv3.A_WCYCL1850S.compy_intel``
* ``SMS.T62_oQU120_ais20.MPAS_LISIO_TEST.anvil_gnu``
* ``SMS.T62_oQU120_ais20.MPAS_LISIO_TEST.anvil_intel``
* ``PEM_Ln9_P1024.ne30_oECv3.A_WCYCL1850S.anvil_intel``
* ``PET_Ln9_P1024.ne30_oECv3.A_WCYCL1850S.anvil_gnu``
* ``SMS_Ld1.ne30pg2_r05_EC30to60E2r2.A_WCYCL1850S_CMIP6.chrysalis_intel.allactive-wcprod`` 
* ``SMS.T62_oQU120_ais20.MPAS_LISIO_TEST.chrysalis_intel``
* ``PEM_Ln9_P1024.ne30_oECv3.A_WCYCL1850S.chrysalis_intel``
* ``PET_Ln9_P1024.ne30_oECv3.A_WCYCL1850S.chrysalis_intel``
* ``SMS_PS.northamericax4v1pg2_WC14to60E2r3.A_WCYCL1850S_CMIP6.chrysalis_intel.allactive-wcprodrrm``

We will run the following E3SM tests after changes:

* ``PET_Ln9.T62_oQU240.GMPAS-IAF.cori-knl_intel``
* ``PET_Ln3.T62_oEC60to30v3wLI.GMPAS-DIB-IAF-ISMF.cori-haswell_intel``
* ``PEM_Ln9.T62_oQU240.GMPAS-IAF.cori-haswell_gnu``
* ``PET_Ln3.T62_oEC60to30v3wLI.GMPAS-DIB-IAF-ISMF.compy_pgi``
* ``PET_Ln9.T62_oQU240.GMPAS-IAF.compy_pgi``
* ``PEM_Ln9.T62_oQU240.GMPAS-IAF.compy_pgi``
* ``PET_Ln9.T62_oQU240.GMPAS-IAF.anvil_gnu``
* ``PEM_Ln9.T62_oQU240.GMPAS-IAF.anvil_gnu``
* ``PET_Ln3.T62_oEC60to30v3wLI.GMPAS-DIB-IAF-ISMF.anvil_intel``


Testing: No significant loss of performance
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2021/04/21

Contributors: Xylar Asay-Davis, Carolyn Begeman

The timing of E3SM test cases (``SMS_P2560x1.T62_oRRS18to6v3.GMPAS-IAF.chrysalis_intel``, 
``PEM_Ln9_P1024.ne30_oECv3.A_WCYCL1850S.cori-knl_intel``) is not significantly increased 
by Phase 1 developments, and is at the +1-2% level on grizzly/intel.

The COMPASS regression suites also include timers.  We will ensure that
performance changes are negligible (within the variability from running the
same test multiple times).
