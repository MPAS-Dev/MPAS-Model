
Vertical Lagrangian-remap method
================================

date: 2020/11/24

Contributors: Darren Engwirda, Xylar Asay-Davis, Carolyn Begeman



Summary
-------

The vertical Lagrangian-remap method is to be implemented to support the choice 
of hybrid vertical coordinates. It can be seen as an extension of the Arbitrary 
Lagrangian-Eulerian method which is already implemented in MPAS-Ocean, a key 
difference being that the solution of diasurface transport is coordinate-free. 
This has the advantage of removing the vertical CFL constraint, which is of 
particular concern for an evolving ice shelf geometry in terrain-following 
coordinate schemes. Generalization of the vertical coordinate in the horizontal 
equations of motion is not in the scope of this development. This development 
will include some infrastructure for specifying the target grid for regridding. 
The minimum criteria for success are not degrading the accuracy of the solution 
or the performance by more than [an acceptable threshold] for several basic test 
cases... [hmm, not quite sure how to write the success statement if not "meets requirements"].


Requirements
------------

Requirement: remapping
^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2020/12/04

Contributors: Darren Engwirda, Xylar Asay-Davis, Carolyn Begeman

Requirement: A grid that relaxes to an analytical expression for a target grid with non-negative layer thicknesses.

Requirement: hybrid coordinates
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2020/12/04

Contributors: Darren Engwirda, Xylar Asay-Davis, Carolyn Begeman

Requirement: The ability to implement hybrid coordinate systems.

Requirement: user specifications
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2020/12/04

Contributors: Darren Engwirda, Xylar Asay-Davis, Carolyn Begeman

Requirement: To the extent that it is feasible, the user should have the ability to control the order of accuracy and other parameters of the remapping (e.g. via namelist options).

Requirement: multiple time stepping schemes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2020/12/04

Contributors: Darren Engwirda, Xylar Asay-Davis, Carolyn Begeman

Requirement: Remapping should be supported (eventually) in both the RK4 and the split-explicit time stepping schemes. 

Requirement: not climate changing
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2020/12/04

Contributors: Darren Engwirda, Xylar Asay-Davis, Carolyn Begeman

Requirement: For the same target grid, the solution for layer thicknesses and key prognostic variables using VLR is within a non-climate-changing threshold value [presumably greater than machine precision?] of the solution using split-explicit and RK4 after a few timesteps.

Requirement: performance
^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2020/12/04

Contributors: Darren Engwirda, Xylar Asay-Davis, Carolyn Begeman

Requirement: Performance is not degraded below an acceptable level.

Requirement: conservation
^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2020/12/04

Contributors: Darren Engwirda, Xylar Asay-Davis, Carolyn Begeman

Requirement: Remapping operation is conservative in the layer-wise sense.

Requirement: no interference
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2020/12/04

Contributors: Darren Engwirda, Xylar Asay-Davis, Carolyn Begeman

Requirement: No calculations are made outside of remapping itself using prognostic (or diagnostic) variables both before and after remapping to avoid introducing additional errors.

Requirement: modularity
^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2020/12/04

Contributors: Darren Engwirda, Xylar Asay-Davis, Carolyn Begeman

Requirement: To the degree possible, the code for performing remapping should be kept in its own Fortran module(s) for better readability.

*Each requirement is to be listed under a "section" heading, as there will be a
one-to-one correspondence between requirements, design, proposed implementation
and testing. Requirements should not discuss technical software issues, but
rather focus on model capability. To the extent possible, requirements should
be relatively independent of each other, thus allowing a clean design solution,
implementation and testing plan.*


Algorithm Design (optional)
---------------------------

Algorithm Design: remapping
^^^^^^^^^^^^^^^^^^^^^^^^^^^
Date last modified: 2020/12/04

Contributors: Darren Engwirda, Xylar Asay-Davis, Carolyn Begeman

The vertical Lagrangian-remap can be characterized by the following equations, as written in Griffies and Adcroft (2020):

Lagrangian layer motion
.. math::
   w^{grid}_{k,*} = -D^n_{k,*}

Horizontal advection thickness update
.. math::
   h^{tem} = h^n + \Delta t \Delta_s w^{grid}_{k,*}

Horizontal advection tracer update
.. math::
   [hC]^{tem} = [hC]^n - \Delta t \nabla_h \cdot [hCu]^n

Update layer thicknesses (regrid)
.. math::
   h^{n+1} = h^{target}

Diasurface velocity
.. math::
   \Delta_s w^{tem} = -(h^{target} - h^{tem})/\Delta t

Remap tracer and velocities
.. math::
   [hC]^{n+1} = [hC]^{tem} - \Delta t \Delta_s (w^{tem}C^{tem})

Specification of the target grid:
- [Some design re. flexibility in user's specification of the grid]
- [Some design re. converting these specs into an analytical expression]
- Allowable coordinate systems: z-star (current), z-tilde (current), isopycnal (new)[?]

*Discuss damping function here*


Algorithm Design: conservation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


Implementation
--------------

Implementation: remapping
^^^^^^^^^^^^^^^^^^^^^^^^^
Lagrangian grid motion:
Horizontal divergence computation remains unchanged *link to code here*
Scratch variable stores provisional layer thickness consistent with lagrangian motion.

Tracer update following Lagrangian grid motion:
Scratch variable stores tracer concentration.

Remapping of both velocities and scalars is performed after the lagrangian step and regridding. 
Assume that the edges of the velocity cells are the midpoints of scalar cells, as already specified by layerThicknessEdge.

The remapping operation detailed in \ref{eq:remap} is performed in several steps:
Reconstruction :math:'C^{n+1} = f(C^{n},h^{n},h^{n+1})'
Enforce conservation by correction :math:'[hC]^{n+1} = [hC]^{tem} - \Delta t \Delta_s (w^{tem}C^{tem})'

Conservation details are discussed in implementation section "conservation."
*Is PPR's conservation implementation equivalent to this equation?*

*Do we need to change PPR's error flags to be consistent with MPAS?*
*Do we need to add any additional error checks?*

Specification of target grid:
- Default is current vertical grid
- Should be a function of z_surface and z_bottom
- Damping function to limit the rate of grid movement in one timestep. 
*Identify parameters in damping function that should be namelist options*
- Maximum/minimum thickness alteration that must occur before remapping: as described in Petersen et al. (2015) for ALE?

Implementation: hybrid coordinates
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Namelist options:
- for horizontally smoothing the grid prior to remapping
*Do we have to worry about consistency with initial thickness conditions*

Smoothing/conditioning the grid prior to remapping:
- Volume-conserving

Implementation: user specifications
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Namelist options relevant to remapping:
- order of the remapping
- order of edge slope estimates
- monotone limiter
- boundary condition option
- option to output some diagnostics?
*Some other remapping options here*

Implementation: multiple time stepping schemes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Namelist options
- config_time_integrator = 'vertical_lagrangian_remap'

A new function will be called instead of ocn_time_integrator_rk4 when VLR is active (e.g., ocn_time_integrator_rk4_vlr). This is motivated by the fact that the vertical velocity, thickness and tracer updates are performed in a different sequence. Some of the functions it calls (ocn_vert_transport_velocity_top, ocn_tend_thick, ocn_tend_tracer, etc.) may have substitutes or have embedded case-clauses for VLR. 

Implementation: not climate changing
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
*I don't know if there are specific implementation details to discuss* 

Implementation: performance
^^^^^^^^^^^^^^^^^^^^^^^^^^^
Options for improving performance:
- Using the split-explicit scheme
- Splitting the scalar and momentum timesteps
- Only remapping when the change in thickness exceeds given threshold
- Optimizing/parallelizing PPR?

Implementation: conservation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^
*PPR details here*

Implementation: no interference
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Ensure that no calculations are made outside of remapping itself using prognostic (or diagnostic) variables both before and after remapping to avoid introducing additional errors.

Look for places in the code where prognostic variables are used at the previos timestep.
Temporarily set X_prev to unrealistic value after remapping so that any errors generated will be detectable?

Implementation: modularity
^^^^^^^^^^^^^^^^^^^^^^^^^^
Remapping operations (PPR) are performed in a separate routine. 
Target grid levels should be determined in a separate routine.
Where to determine timestep for remapping?

*This section should detail the plan for implementing the design solution for
requirement XXX. In general, this section is software-centric with a focus on
software implementation. Pseudo code is appropriate in this section. Links to
actual source code are appropriate. Project management items, such as git
branches, timelines and staffing are also appropriate.*

Testing
-------

Testing and Validation: remapping
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Ability to handle strong vertical velocities: maybe baroclinic channel test case
Evaluating spurious mixing due to remapping: Internal wave test case. Compare mixing between cases with and without VLR for RK4.

Testing and Validation: hybrid coordinates
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Seamount test case for dropping bottom cells?
Ice shelf test case for dropping top cells

Testing and Validation: user specifications
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Testing and Validation: multiple time stepping schemes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
RK4 with and without VLR: internal wave test case. Set the target grid equal both with and without VLR.

Split-explicit with and without VLR: internal wave test case. Shouldn't lead to remapping because motion is high frequency.

Testing and Validation: not climate changing
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Global ocean test case?

Testing and Validation: performance
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Testing and Validation: conservation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Sufficient to test that PPR is conservative?
Vertical convergence test: 

Testing and Validation: no interference
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Some evaluation of sub-grid momentum and scalar fluxes

*How will XXX be tested, i.e., how will be we know when we have met requirement
XXX? Which tests from the regression suites are appropriate?  How would they
need to be configured or modified to test that the new software is working
properly?  What additions or modifications to the nightly regression suite might
be made to ensure that the new capability continues to work as expected?*
