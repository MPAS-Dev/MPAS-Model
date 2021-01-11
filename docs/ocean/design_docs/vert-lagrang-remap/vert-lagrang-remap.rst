.. role:: red

Vertical Lagrangian-remap method
================================

date: 2020/12/10

Contributors: Darren Engwirda, Xylar Asay-Davis, Carolyn Begeman



Summary
-------

We propose to implement the vertical Lagrangian-remap method to support the 
choice of hybrid vertical coordinates, which may be required for evolving ice-
shelf cavity geometries. It can be seen as an extension of the Arbitrary 
Lagrangian-Eulerian method which is already implemented in MPAS-Ocean, a key 
difference being that the solution of diasurface transport is coordinate-free. 
This has the advantage of removing the vertical CFL constraint, which is of 
particular concern for an evolving ice shelf geometry in terrain-following 
coordinate schemes. This development may also reduce numerical diffusion in the
vertical, but we do not require this result for this implementation to be 
successful.

Generalization of the vertical coordinate is not in the scope of this development. 
Additionally, the infrastructure for specifying a target grid that differs from 
the existing z-star and z-tilde options is reserved for future development.
The minimum criteria for success are not degrading the accuracy of the solution 
or the performance by more than [an acceptable threshold] for several basic test 
cases. 
*[not quite sure how to write the success statement if not "meets requirements"]*


Requirements
------------

Requirement: Ability to perform vertical Lagrangian-remapping
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2020/12/04

Contributors: Darren Engwirda, Xylar Asay-Davis, Carolyn Begeman

The core of the proposed development is the ability to perform vertical 
Lagrangian-remapping (VLR). When VLR is performed, the momentum, volume and 
tracer conservation equations are first solved in a Lagrangian sense; that is, 
layer thicknesses evolve such that there is no vertical transport across layer 
interfaces. A target grid is then specified. For this stage of development, 
we only require that the algorithm be able to use the existing grid 
specifications. However, the implementation should be general enough that 
a different grid could be used for remapping. This regridding step is followed 
by a conservative remapping of velocity and scalars to the target grid. 

Requirement: Grid is sufficiently conditioned for accuracy and stability
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2020/12/04

Contributors: Darren Engwirda, Xylar Asay-Davis, Carolyn Begeman

A severely deformed grid can lead to issues with the accuracy and stability of 
the numerical methods used for the dynamics and thermodynamics. If layers are 
allowed to become very thin they can also degrade the accuracy and stability of 
the solution. Thus, a requirement of the VLR implementation is that there are 
safeguards to prevent layers from becoming too thin or deformed. 

Requirement: Ability to specify remapping method and its options
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2020/12/04

Contributors: Darren Engwirda, Xylar Asay-Davis, Carolyn Begeman

There are several choices to make for the remapping operation to balance accuracy, 
stability and computational cost. To the extent that it is feasible, the user 
should have the ability to control the order of accuracy of the remapping method,
[Darren, other options here?] and other remapping options (e.g., via namelist options).

Requirement: Support for existing time stepping schemes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2020/12/04

Contributors: Darren Engwirda, Xylar Asay-Davis, Carolyn Begeman

Vertical Lagrangian remapping should be supported (eventually) in both the RK4 
and the split-explicit time stepping schemes currently implemented in MPAS-Ocean.
The scheme will also have a straightforward interface so it can be easily added
to new time stepping schemes.

Requirement: not climate changing
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2020/12/15

Contributors: Darren Engwirda, Xylar Asay-Davis, Carolyn Begeman

For the same target grid, time-stepping scheme and problem, the solution for 
layer thicknesses and key prognostic variables using VLR is within a non-
climate-changing threshold value of the solution when VLR is not performed.
Tests should span from idealized tests of simplified dynamics to full climate
simulations. Acceptable thresholds specified for each test could be similar to
those from changing time stepping schemes.

Requirement: tracer and volume conservation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2020/12/04

Contributors: Darren Engwirda, Xylar Asay-Davis, Carolyn Begeman, Mark Petersen

The column-wise tracer content should be conserved over the remapping step to a
specified threshold, and using a specified method to integrate the tracer. The
column-integrated volume must remain unchanged over the remapping step, as
expected.

Requirement: performance
^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2020/12/04

Contributors: Darren Engwirda, Xylar Asay-Davis, Carolyn Begeman, Mark Petersen

Performance is not degraded below an acceptable level. Compute time for
stand-alone ocean, without i/o, on full primitive equations, is expected to
remain very similar - within 5% of compute time when using vertical
advection method but likely less because remapping is a column-wise, in-cache
operation.

Requirement: no interference
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2020/12/04

Contributors: Darren Engwirda, Xylar Asay-Davis, Carolyn Begeman

No calculations are made outside of remapping itself using prognostic (or 
diagnostic) variables both before and after remapping to avoid introducing 
additional errors.

Requirement: modularity
^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2020/12/15

Contributors: Darren Engwirda, Xylar Asay-Davis, Carolyn Begeman

To the degree possible, the code for determining the target grid and performing 
remapping should be kept in its own Fortran module(s) for better readability.



Algorithm Design (optional)
---------------------------

Algorithm Design: Ability to perform vertical Lagrangian-remapping
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2020/12/15

Contributors: Darren Engwirda, Xylar Asay-Davis, Carolyn Begeman

The conservation of momentum, volume, and tracer equations in MPAS-Ocean (
`Ringler et al. 2013 <https://www.sciencedirect.com/science/article/abs/pii/S1463500313000760>`_; 
`Petersen et al. 2014 <https://www.sciencedirect.com/science/article/abs/pii/S1463500314001796>`_) are:

.. math::

   \frac{\partial u_k}{\partial t} + q_k h_k u_k^{normal} + \overline{w^t \delta z^t u} = -\frac{1}{\rho_0} \nabla p_k - \frac{rho_k g}{\rho_0} \nabla z_k - \nabla K_k + [D_h^u]_k + [D_{\nu}^u]_k + F_k^u
   
   \frac{\partial h_k}{\partial t} + \nabla \cdot (h_k \mathbf{u}_k) + w_k^t - w_{k+1}^t = 0

   \frac{\partial (h_k \phi_k)}{\partial t} + \nabla \cdot (h_k \mathbf{u}_k \phi_k) + \overline{\phi}_k^t w_k^t - \overline{\phi}_{k+1}^t w_{k+1}^t = [D_h^{\phi}]_k + [D_v^{\phi}]_k + F_k^{\phi}
   
For the Lagrangian step, the vertical velocity through the top of the cell, :math:`w_k^t`, is set to zero in all of the above equations. Thus, these equations simplify to:

.. math::

   \frac{\partial u_k}{\partial t} + q_k h_k u_k^{\perp} = -\frac{1}{\rho_0} \nabla p_k - \frac{\rho_k g}{\rho_0} \nabla z_k - \nabla K_k + [D_h^u]_k + [D_v^u]_k + F_k^u
   
   \frac{\partial h_k}{\partial t} + \nabla \cdot (h_k \mathbf{u}_k) = 0

   \frac{\partial (h_k \phi_k)}{\partial t} + \nabla \cdot (h_k \mathbf{u}_k \phi_k) = [D_h^{\phi}]_k + [D_v^{\phi}]_k + F_k^{\phi}
   
The time-stepping algorithm (RK4 or split-explicit) yields the updated 
variables :math:`u_k^{lg},h_k^{lg},\phi_k^{lg}`, where the superscript
*lg* is used to designate the values after the Lagrangian step.

Note that the vertical mixing terms :math:`D_v^h, D_v^{\phi}` 
are retained here. We opt to compute these terms prior to remapping as this 
allow for future development in which the dynamics are subcycled relative to 
the thermodynamics and remapping is scheduled on the thermodynamic timestep. 
This computation of vertical mixing terms prior to remapping is similar to 
both MOM6 and HYCOM. We anticipate that there could be a trade-off between (a)
loss of accuracy of vertical mixing terms when their computation precedes 
remapping due to grid deformation and (b) loss of accuracy when their 
computation follows remapping due to remapping errors in vertical gradients of 
prognostic variables. We do not intend to test this at this time.

The target grid needs to be determined after the solution for prognostic 
variables so that the vertical Lagrangian-remapping method is general enough to
be used with coordinate systems that depend on the ocean state (this includes 
the z-star coordinate system in which SSH perturbations are vertically 
distributed between layers). We do not present an algorithmic design for 
regridding to coordinate systems not already supported in MPAS-Ocean, as this 
will be the subject of future development. For now, the target grid is based on a 
constant set of z-levels that are specified at initialization.

For the regridding step, layer thicknesses are set according to the target 
grid, conserving volume:

.. math::

   h_k^{t+1} = h_k^{target}
   
   \sum_{k=1}^{kmax}h_k^{t+1} = \sum_{k=1}^{kmax}h_k^{lg}


For the remapping step, velocities (edge-normal) and scalars are remapped to 
the target grid, conserving volume flux and scalar concentration:

.. math::

   \sum_{k=1}^{kmax} u_k^{t+1} h_k^{t+1} = \sum_{k=1}^{kmax} u_k^{lg} h_k^{lg}
   
   \sum_{k=1}^{kmax} \phi_k^{t+1} h_k^{t+1} = \sum_{k=1}^{kmax} \phi_k^{lg} h_k^{lg}

The vertical velocity across layer interfaces may be computed anytime after 
regridding. It can be computed as 

.. math::

   w = - \nabla \cdot (h_k \mathbf{u}_k) - (h_k^{t+1} - h_k^t)/dt

or

.. math::

   w = (h_k^{t+1} - h_k^{lg})/dt

The choice between the two is discussed in the Implementation section.


Implementation
--------------

Implementation: Ability to perform vertical Lagrangian-remapping
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2020/12/15

Contributors: Darren Engwirda, Xylar Asay-Davis, Carolyn Begeman

Namelist options:

- To turn VLR on/off: 
  :code:`ALE_vertical_grid, config_vert_lagrangian_remap = .true. or .false.`
- *Something related to target grid, for now just z_initial*

Namelist options specific to PPR are discussed later.

Lagrangian step:

The solution for prognostic variables in RK4 and split-explicit remains
largely the same. The main difference is that the vertical velocity through 
the top of layers is set to zero in the routine 
:code:`ocn_vert_transport_velocity_top`. This is similar to what is done when 
:code:`config_vert_coord_movement` is :code:`impermeable_interfaces`, except 
rather than exit the routine, we proceed with computations needed for the z-star and
z-tilde coordinate choices.

*Other modifications to ocn_vert_transport_velocity_top are not yet determined.*
We will likely need to bypass the :code:`ocn_ALE_thickness` call in 
:code:`ocn_vert_transport_velocity_top` so that the adjustments of layer 
thickness for SSH perturbations occur during the regridding step.


Regridding step(s):

#. :math:`z_k^1`, the depth of the top of the layer, is determined based on 
   an analytical expression for the grid. 
   The simplest case is constant z-levels, :math:`z_k^1 = z_k^{init}`.
   Since :math:`z_k^1` can be a function of the ocean state (e.g., :math:`\rho` 
   for isopycnal coordinates, regridding doesn't begin until after the solution 
   for prognostic variables.
#. Superimpose SSH perturbations according to one of the existing depth-
   dependent functions, :math:`z_k^2 = z_k^1 + c(z) \: \eta`. As in 
   :code:`ocn_ALE_thickness`, layer thicknesses are adjusted from the seafloor 
   upwards. Ideally, there is a single function that is used for both ALE
   implementations, with and without VLR.
#. Apply conditioning steps outlined in the following section.
#. Update :code:`layerThicknessEdge` from the updated :code:`layerThickness`
   using routine :code:`ocn_diagnostic_solve_layerThicknessEdge`. Note that 
   :code:`zTop, zMid` are updated later when :code:`ocn_diagnostic_solve` is 
   called.

All of the regridding steps will be performed from a separate module.
This topic is further addressed in section Implementation: modularity.


Remapping step:

Layer thickness has already been updated to reflect Lagrangian motion when 
:code:`ocn_tend_thick` is called. This is stored in 
:code:`layerThickness(tlev=2)`.

There is a new remapping routine with inputs:

- :code:`layerThickness(tlev=2)` for scalars
- :code:`layerThicknessEdge(tlev=2)` for velocity
- :code:`bottomDepth`
- :code:`layerThicknessTarget`
- :code:`layerThicknessEdgeTarget`
- Remapped and updated: :code:`normalVelocity`
- Remapped and updated: All members of :code:`tracerPool` unless 
  :code:`activeTracersOnly`, in which case only the :code:`activeTracers`
- *This may not be a complete list*

This routine makes calls to the PPR library

*More details here*

Some implementation considerations for PPR:
 
- Error-checking in PPR: make consistent with MPAS errors, consider additional
  error checks
- *Add more here*

:code:`layerThickness(tlev=2)` is then overwritten with the new layer thickness.

After remapping, :code:`ocn_diagnostic_solve` is called. This is needed to 
compute the density and pressure fields based on the remapped ocean state and
the diagnostic field :code:`vertVelocityTop` which is the vertical velocity 
through the top of the layer. This is only used as a diagnostic variable for 
computing the MOC streamfunction. None of the mixing parameterizations require
a vertical velocity (Eulerian or diasurface velocity).

Note: if `vertVelocityTop` is computed between regridding and remapping then it 
can be computed as 

.. code::
   
   vertVelocityTop(k) = vertVelocityTop(k+1) - div_hu(k) - 
                        (layerThickness(k,tlev=2) - layerThickness(k,tlev=1))/dt

If `vertVelocityTop` is computed after remapping, then :code:`div_hu` is no
longer appropriate as it has been remapped. In this case, the Lagrangian layer 
thickness should be stored in a scratch variable and then the vertical velocity 
through the top of the layer can be computed:

.. code::

   layerThicknessALE = layerThickness(tlev=2)
   
   layerThickness(tlev=2) = layerThicknessTarget
   
   vertVelocityTop = (layerThickness(tlev=2) - layerThicknessALE)/dt

The computation of :code:`vertTransportVelocityTop` and 
:code:`vertGMBolusVelocitytop` is unchanged as these fields represent Eulerian 
velocities.

Implementation: Grid is sufficiently conditioned for accuracy and stability
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2020/12/15

Contributors: Darren Engwirda, Xylar Asay-Davis, Carolyn Begeman

After determining the target grid, perform the following steps:

#. Optional: Assign :math:`h_k^{t+1}` to :math:`h_k^{lg}` if 
   :math:`h_k^{t+1} - h_k^{lg}` is less than a minimum thickness alteration. 
   This motivated by accuracy considerations, as each remapping may introduce 
   errors. *Darren, would this improve PPR computational performance?*
#. Apply minimum layer thickness criterion. 

Smoothing layers in space and time is left for a future design document in 
which we implement support for additional coordinate systems including hybrid 
coordinates.

Namelist options:

- Minimum layer thickness
- Optional: minimum thickness change for remapping to occur


Implementation: Ability to specify remapping method and its options
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2020/12/15

Contributors: Darren Engwirda, Xylar Asay-Davis, Carolyn Begeman

Namelist options:

- frequency with which remapping should be performed (on which timestep)
- order of the remapping
- order of edge slope estimates
- monotone limiter
- boundary condition option
- option to output some diagnostics?
- *Some other remapping options here*


Implementation: Support for existing time stepping schemes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2020/12/15

Contributors: Darren Engwirda, Xylar Asay-Davis, Carolyn Begeman

:code:`vertAleTransportTop` is set to zero for both time stepping schemes from 
:code:`ocn_vert_transport_velocity_top`.

*Some details here about how to treat ALE_thickness*

Implementation: performance
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2020/12/15

Contributors: Darren Engwirda, Xylar Asay-Davis, Carolyn Begeman

Options for improving performance:

- Using the split-explicit scheme
- Splitting the scalar and momentum timesteps
- Only remapping when the change in thickness exceeds given threshold
- Optimizing/parallelizing PPR?

Implementation: no interference
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2020/12/15

Contributors: Darren Engwirda, Xylar Asay-Davis, Carolyn Begeman

Ensure that no calculations are made outside of remapping itself using 
prognostic (or diagnostic) variables both before and after remapping to avoid 
introducing additional errors.

Look for places in the code where prognostic variables are used at the previous 
timestep.

Implementation: modularity
^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2020/12/15

Contributors: Darren Engwirda, Xylar Asay-Davis, Carolyn Begeman

Remapping operations (PPR) are performed in a separate routine. 

Target grid levels should be determined in a separate routine.


Testing
-------

Testing and Validation: Ability to perform vertical Lagrangian-remapping
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2020/12/15

Contributors: Darren Engwirda, Xylar Asay-Davis, Carolyn Begeman

Ability to handle strong vertical velocities: 

- baroclinic channel test case (?)

Evaluating spurious mixing due to remapping: Compare with and without VLR

- Internal wave test case
- Dense overflow test case

Tests for nightly regression suite:

- *TBD*

Testing and Validation: Grid is sufficiently conditioned for accuracy and stability
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2020/12/15

Contributors: Darren Engwirda, Xylar Asay-Davis, Carolyn Begeman


Testing and Validation: Support for existing time stepping schemes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2020/12/15

Contributors: Darren Engwirda, Xylar Asay-Davis, Carolyn Begeman

Internal wave test case. Set the target grid equal both with and without VLR.

Results from RK4 with and without VLR: 

Results from split-explicit with and without VLR: 

Testing and Validation: not climate changing
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2020/12/15

Contributors: Darren Engwirda, Xylar Asay-Davis, Carolyn Begeman

Global ocean test case (?)

Testing and Validation: performance
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2020/12/15

Contributors: Darren Engwirda, Xylar Asay-Davis, Carolyn Begeman

*Choose which test case(s) to evaluate performance with*

Testing and Validation: conservation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2020/12/15

Contributors: Darren Engwirda, Xylar Asay-Davis, Carolyn Begeman

Tests of PPR alone and embedded.

Vertical resolution convergence test: 

Nightly regression suite test:

- *TBD*

Testing and Validation: no interference
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2020/12/15

Contributors: Darren Engwirda, Xylar Asay-Davis, Carolyn Begeman

Temporarily set prognosticVariable(tlev=1) to unrealistic value after remapping 
so that any errors due to interference will be detectable?
