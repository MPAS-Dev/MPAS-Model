Inactive Cells at the Top of the Water Column
=============================================

date: 2021/02/11

Contributors: Xylar Asay-Davis



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

In setups without ay inactive top cells, results should be unchanged
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

Date last modified: 2021/02/11

Contributors: Xylar Asay-Davis

We will add ``minLevelCell``, analogous to ``maxLevelCell``.  In
``ocn_init_routines_compute_max_level()`` in the ``ocn_init_routines`` module,
several other derivative fields are initialized based on ``maxLevelCell``.
We will rename this function ``ocn_init_routines_compute_min_max_level()``
and will initialized the following new variables:

* ``minLevelEdgeTop``

* ``minLevelEdgeBot``

* ``minLevelVertexTop``

* ``minLevelVertexBot``

The definitions of the following variables currently in this routine will be
updated to include ``minLevelCell``:

* ``boundaryEdge``

* ``edgeMask``

* ``cellMask``

* ``vertexMask``

A very large number of vertical loops will need to be altered in MPAS-Ocean.
We will prioritize subroutines that are involved in any of the test cases for
the ``sub_ice_shelf_2D`` configuration in legacy COMPASS.  This will give us
a testbed for ensuring that the new functionality behaves as expected without
requiring us to work through routines related to parameterizations, such as
KPP and GM, that are not used in these test cases.

What follows are prioritized checklists of the modules with variables that need
to be added and subroutines where loops or other vertical indexing needs to be
updated.  First are those related to ``init`` mode, then those used in
``forward`` mode.

Init:

* [ ] ``ocn_init_sub_ice_shelf_2D``:

  * [ ] ``ocn_init_setup_sub_ice_shelf_2D()``

* [ ] ``ocn_init_ssh_and_landIcePressure``:

  * [ ] ``ocn_init_ssh_and_landIcePressure_vertical_grid()``

  * [ ] ``ocn_init_ssh_and_landIcePressure_balance()``

* [ ] ``ocn_init_vertical_grids``:

  * [ ] ``ocn_compute_layerThickness_zMid_from_bottomDepth()``

  * [ ] ``ocn_alter_bottomDepth_for_pbcs()``

  * [ ] ``ocn_compute_Haney_number()``

  * [ ] ``ocn_init_vertical_grid_with_max_rx1()``

  * [ ] ``constrain_rx1_layer()``

Forward:

* [ ] ``ocn_init_routines``:

  * [ ] ``ocn_init_routines_compute_min_max_level()``

  * [ ] ``ocn_init_routines_vert_coord()``

  * [ ] ``ocn_init_routines_block()``

* [ ] ``ocn_time_integration_split``:

  * [ ] ``ocn_time_integration_split_init()``

  * [ ] ``ocn_time_integrator_split()``

* [ ] ``ocn_mesh``:

  * [ ] ``minLevelCell``

  * [ ] ``minLevelEdgeTop``

  * [ ] ``minLevelEdgeBot``

  * [ ] ``minLevelVertexTop``

  * [ ] ``minLevelVertexBot``

  * [ ] ``ocn_meshCreate()``

  * [ ] ``ocn_meshUpdateFields()``

  * [ ] ``ocn_meshDestroy()``

* [ ] ``ocn_diagnostics``:

  * [ ] ``ocn_relativeVorticity_circulation()``

  * [ ] ``ocn_diagnostic_solve_layerThicknessEdge()``

  * [ ] ``ocn_diagnostic_solve_vorticity()``

  * [ ] ``ocn_diagnostic_solve_richardson()``

  * [ ] ``ocn_diagnostic_solve_surfaceLayer()``

  * [ ] ``ocn_diagnostic_solve_vortVel()``

  * [ ] ``ocn_diagnostic_solve_z_coordinates()``

  * [ ] ``ocn_diagnostic_solve_pressure()``

  * [ ] ``ocn_vert_transport_velocity_top()``

  * [ ] ``ocn_fuperp()``

  * [ ] ``ocn_compute_land_ice_flux_input_fields()``

  * [ ] ``ocn_validate_state()``

...

.. note::

  ``ocn_equation_of_state_jm`` currently doesn't include any reference to
  ``maxLevel*`` but this is mildly concerning.  T and S are clipped to the
  valid range before density is computed, meaning that contamination with
  invalid values could go unnoticed.  Still, it does appear that
  ``displacedDensity`` is currently only used starting at index ``k = 2``, so
  no invalid values should be getting used.

Here is a (by no means complete) checklist of routines not used by
``sub_ice_shelf_2D`` test cases that are a lower priority to update:

Init:

* [ ] ``ocn_init_cell_markers``:

  * [ ] ``ocn_mark_maxlevelcell()``

* [ ] ``ocn_init_global_ocean``:

  * [ ] ``ocn_init_setup_global_ocean()``

  * [ ] ``ocn_init_setup_global_ocean_create_model_topo()``

  * [ ] ``ocn_init_setup_global_ocean_deepen_critical_passages()``

  * [ ] ``ocn_init_setup_global_ocean_interpolate_land_ice_topography()``

  * [ ] ``ocn_init_setup_global_ocean_modify_temp_under_land_ice()``

  * [ ] ``ocn_init_setup_global_ocean_cull_inland_seas()``

  * [ ] ``ocn_init_setup_global_ocean_interpolate_tracers()``

* [ ] ``ocn_init_isomip``:

  * [ ] ``ocn_init_setup_isomip()``

* [ ] ``ocn_init_isomip_plus``:

  * [ ] ``ocn_init_setup_isomip_plus()``

Forward:

* [ ] ``ocn_diagnostics``:

  * [ ] ``ocn_filter_btr_mode_tend_vel()``

* [ ] ``ocn_gm``:

  * [ ] ``ocn_GM_compute_Bolus_velocity()``

...


Implementation: Surface fields and fluxes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2021/02/11

Contributors: Xylar Asay-Davis

The subroutines  ``ocn_thick_surface_flux_tend()`` and
``ocn_tracer_surface_flux_tend()`` already distribute surface fluxes over
multiple vertical levels in a loop, so surface fluxes will simply require the
same treatment as any other loops.

It will be a little trickier to make sure we perform proper indexing of all
3D variables to get their "surface" values.  Here, the plan is to make sure
that variables are set to the NetCDF fill value (a large, negative number)
when they are invalid so that contamination should be obvious.

Here is a checklist of variables or subroutines requiring special care because
of top indexing that might not be easy to find (e.g. by searching for
``maxLevel``):

* [ ] ``ocn_diagnostics``:

  * [ ] ``tracersSurfaceValue``

  * [ ] ``normalVelocitySurfaceLayer``

  * [ ] ``ssh``

  * [ ] ``fracAbsorbed``

  * [ ] ``fracAbsorbedRunoff``

  * [ ] ``nonLocalSurfaceTracerFlux``

  * [ ] ``surfaceBuoyancyForcing``

  * [ ] ``topDrag``

  * [ ] ``topDragMag``

  * [ ] ``landIceFrictionVelocity``

...

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

Date last modified: 2021/02/11

Contributors: Xylar Asay-Davis

We will set ``minLevelCell`` to all ``1`` by default.  We will take care not
to reorder computations in a way that would likely lead to non-bit-for-bit
changes.

Implementation: No significant loss of performance
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2021/02/11

Contributors: Xylar Asay-Davis

We will take care not to introduce unnecessary ``if`` statements or equivalents
that were not present before.


Testing
-------

Testing: Inactive top cells
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2021/02/11

Contributors: Xylar Asay-Davis

We will ensure that all 3D, prognostic variables have been initialized to the
NetCDF fill value in inactive cells at the top of the water column.  We will
attempt to do the same for 3D diagnostic variables.  This should expose any
computations involving these invalid cells.

We will modify the ``sub_ice_shelf_2D`` configuration in legacy COMPASS to
support a z-level initial coordinate in the ice-shelf cavity (including writing
out ``minLevelCell`` in the initial condition).  Then, we will run the
following test cases in this configuration with the new coordinate:

* ``default``

* ``restart_test``

* ``iterative_init``

We will run on multiple machines (Ubuntu laptop, Anvil, Grizzly, Compy) with a
mix of Gnu and Intel compilers.  We will plot the resulting T, S and KE fields
to make sure they look comparable to the results with the current
terrain-following coordinate.

We will test with both the linear (I believe the default) and JM equations of
state.

Testing: Surface fields and fluxes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2021/02/11

Contributors: Xylar Asay-Davis

The testing in ``sub_ice_shelf_2D`` will account for the surface fluxes and
fields that we most anticipate being affected by ``minLevel*``.  Testing of
other surface fields will likely require running tests that include GM, KPP
and other parameterizations that are not part of this configuration by default.

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

Date last modified: 2021/02/11

Contributors: Xylar Asay-Davis

We will run the legacy COMPASS nightly and land-ice-fluxes regression suites
on multiple machines (Ubuntu laptop, Anvil, Grizzly, Compy) with a mix of Gnu
and Intel compilers to make sure nothing has changed when we run with
``minLevelCell = 1`` everywhere.

We will also run the following E3SM tests before and after the changes:

* ``SMS.T62_oQU120_ais20.MPAS_LISIO_TEST.anvil_intel``
* ``<<help>>``


Testing: No significant loss of performance
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2021/02/11

Contributors: Xylar Asay-Davis

The COMPASS regression suites also include timers.  We will ensure that
performance changes are negligible (within the variability from running the
same test multiple times).
