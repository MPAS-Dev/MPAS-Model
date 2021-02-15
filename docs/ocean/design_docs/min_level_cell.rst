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

* [ ] ``ocn_tendency``:
  
  * [ ] ``ocn_tend_tracer``

* [ ] ``ocn_forcing``:

  * [ ] ``ocn_forcing_build_fraction_absorbed_array``

* [ ] ``ocn_thick_hadv``:
  
  * [ ] ``ocn_hadv_thick_tend``
  
* [ ] ``ocn_thick_vadv``:
  
  * [ ] ``ocn_vadv_thick_tend``

* [ ] ``ocn_thick_surface_flux``:
  
  * [ ] ``ocn_thick_surface_flux_tend``
  
* [ ] ``ocn_tracer_advection``:
  
  * [ ] ``ocn_tracer_advection_tend``

* [ ] ``ocn_tracer_advection_mono``:
  
  * [ ] ``ocn_tracer_advection_mono_tend``

* [ ] ``ocn_vel_forcing_surface_stress``:
  
  * [ ] ``ocn_vel_forcing_surface_stress_tend``
  
* [ ] ``ocn_vel_hmix_del2``:
  
  * [ ] ``ocn_vel_hmix_del2_tend``
  
* [ ] ``ocn_vel_hadv_coriolis``:
  
  * [ ] ``ocn_vel_hadv_coriolis_tend``

* [ ] ``ocn_vel_vadv``:
  
  * [ ] ``ocn_vel_vadv_tend``

* [ ] ``ocn_vmix_cvmix``:
  
  * [ ] ``ocn_vmix_coefs_cvmix_build``

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

* [ ] ``ocn_thick_ale``:

  * [ ] ``ocn_ALE_thickness``

* [ ] ``ocn_vel_pressure_grad``:
  
  * [ ] ``ocn_vel_pressure_grad_tend``

* [ ] ``ocn_vmix``:
  
  * [ ] ``ocn_vmix_implicit``
  
  * [ ] ``ocn_vel_vmix_tend_implicit``
  
  * [ ] ``ocn_tracer_vmix_tend_implicit``

...

.. note::

  ``ocn_equation_of_state_jm`` currently doesn't include any reference to
  ``maxLevel*`` but this is mildly concerning.  T and S are clipped to the
  valid range before density is computed, meaning that contamination with
  invalid values could go unnoticed.  Still, it does appear that
  ``displacedDensity`` is currently only used starting at index ``k = 2``, so
  no invalid values should be getting used.

.. note::

  ``ocn_forcing_build_fraction_absorbed_array`` is currently only called once 
  by ``ocn_init_routines`` and would need to be called multiple times to correctly
  distribute surface fluxes unless we use an alternative approach where the vertical
  index of transmissionCoeff is number of cells from minLevelCell rather than k-levels
  
  
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

  * [ ] ``ocn_compute_KPP_input_fields()``

* [ ] ``ocn_gm``:

  * [ ] ``ocn_GM_compute_Bolus_velocity()``

* [ ] ``ocn_tendency``:
  
  * [ ] ``ocn_tend_freq_filtered_thickness``

* [ ] ``ocn_tidal_forcing``:
  
  * [ ] ``ocn_tidal_forcing_build_array``
  
  * [ ] ``ocn_tidal_forcing_layer_thickness``

* [ ] ``ocn_vel_tidal_potential``:
  
  * [ ] ``ocn_vel_tidal_potential_tend``
  
* [ ] ``ocn_tracer_advection_std``:
  
  * [ ] ``ocn_tracer_advection_std_tend``

* [ ] ``ocn_wetting_drying``:
  
  * [ ] ``ocn_wetting_drying_verify``

  * [ ] ``ocn_prevent_drying_rk4``

  * [ ] ``ocn_wetting_drying_wettingVelocity``

* [ ] ``ocn_vmix``:

  * [ ] ``ocn_vel_vmix_tend_implicit_spatially_variable``
  
  * [ ] ``ocn_vel_vmix_tend_implicit_spatially_variable_mannings``
             
  * [ ] ``ocn_vel_vmix_tend_implicit_rayleigh``
  
  * [ ] ``ocn_compute_kpp_rhs``

* [ ] ``ocn_tracer_exponential_decay``:
  
  * [ ] ``ocn_tracer_exponential_decay_compute``

* [ ] ``ocn_tracer_DMS``:
  
  * [ ] ``ocn_tracer_DMS_compute``
  
  * [ ] ``ocn_tracer_DMS_surface_flux_compute``: iLevelSurface
  
* [ ] ``ocn_tracer_ecosys``:
  
  * [ ] ``ocn_tracer_ecosys_compute``
   
  * [ ] ``ocn_tracer_ecosys_surface_flux_compute``: iLevelSurface

  * [ ] ``ocn_compute_tidal_potential_forcing``

* [ ] ``ocn_frazil_forcing``:
  
  * [ ] ``ocn_frazil_forcing_layer_thickness``
  
  * [ ] ``ocn_frazil_forcing_active_tracers``
  
  * [ ] ``ocn_frazil_forcing_build_arrays``

* [ ] ``ocn_sea_ice``:
  
  * [ ] ``ocn_sea_ice_formation``
  
  * [ ] ``ocn_sea_ice_init``

* [ ] ``ocn_tracer_hmix_del2``:
  
  * [ ] ``ocn_tracer_hmix_del2_tend``
  
* [ ] ``ocn_tracer_hmix_del4``:
  
  * [ ] ``ocn_tracer_hmix_del4_tend``
  
* [ ] ``ocn_tracer_hmix_Redi``:
  
  * [ ] ``ocn_tracer_hmix_Redi_tend``
  
* [ ] ``ocn_high_freq_thickness_hmix_del2``:
  
  * [ ] ``ocn_high_freq_thickness_hmix_del2_tend``
  
* [ ] ``ocn_vel_hmix_del4``:
  
  * [ ] ``ocn_vel_hmix_del4_tend``
  
* [ ] ``ocn_vel_hmix_leith``:
  
  * [ ] ``ocn_vel_hmix_leith_tend``
  
* [ ] ``ocn_tracer_ideal_age``:

  * [ ] ``ocn_tracer_ideal_age_compute``

* [ ] ``ocn_tracer_MacroMolecules``:

  * [ ] ``ocn_tracer_MacroMolecules_compute``

* [ ] ``ocn_tracer_nonlocalflux``:

  * [ ] ``ocn_tracer_nonlocalflux_tend``

* [ ] ``ocn_tracer_short_wave_absorption_jerlov``:

  * [ ] ``ocn_tracer_short_wave_absorption_jerlov_tend``

* [ ] ``ocn_tracer_short_wave_absorption_variable``:

  * [ ] ``ocn_tracer_short_wave_absorption_variable_tend``

  * [ ] ``ocn_get_variable_sw_fraction``: or only change depth input to this function

* [ ] ``ocn_tracer_surface_flux_to_tend``:

  * [ ] ``ocn_tracer_surface_flux_tend``

* [ ] ``ocn_tracer_interior_restoring``:
  
  * [ ] ``ocn_tracer_interior_restoring_compute``

...

.. note::

  May need to reconsider nVertLevels argument to ``ocn_sea_ice_init``
  

Implementation: Surface fields and fluxes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2021/02/11

Contributors: Xylar Asay-Davis, Luke Van Roekel

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

Testing: Correct boundary conditions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2021/02/11

Contributors: ???

<<<Need to think about this...>>>


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
