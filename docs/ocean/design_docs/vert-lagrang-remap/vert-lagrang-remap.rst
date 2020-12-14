.. role:: red

Vertical Lagrangian-remap method
================================

date: 2020/12/10

Contributors: Darren Engwirda, Xylar Asay-Davis, Carolyn Begeman



Summary
-------

The vertical Lagrangian-remap method is to be implemented to support the choice 
of hybrid vertical coordinates. It can be seen as an extension of the Arbitrary 
Lagrangian-Eulerian method which is already implemented in MPAS-Ocean, a key 
difference being that the solution of diasurface transport is coordinate-free. 
This has the advantage of removing the vertical CFL constraint, which is of 
particular concern for an evolving ice shelf geometry in terrain-following 
coordinate schemes. 

Generalization of the vertical coordinate in the horizontal 
equations of motion is not in the scope of this development. Additionally, the 
infrastructure for specifying a target grid that differs from the existing 
z-star and z-tilde options is reserved for future development.
*The minimum criteria for success are not degrading the accuracy of the solution 
or the performance by more than [an acceptable threshold] for several basic test 
cases... [hmm, not quite sure how to write the success statement if not "meets requirements"].*


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

Requirement: not climate changing
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2020/12/04

Contributors: Darren Engwirda, Xylar Asay-Davis, Carolyn Begeman

For the same target grid and problem, the solution for layer thicknesses and key 
prognostic variables using VLR is within a non-climate-changing threshold value 
[presumably greater than machine precision?] of the solution using split-explicit 
and RK4 after a few timesteps.

Requirement: performance
^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2020/12/04

Contributors: Darren Engwirda, Xylar Asay-Davis, Carolyn Begeman

Performance is not degraded below an acceptable level.

Requirement: no interference
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2020/12/04

Contributors: Darren Engwirda, Xylar Asay-Davis, Carolyn Begeman

No calculations are made outside of remapping itself using prognostic (or 
diagnostic) variables both before and after remapping to avoid introducing 
additional errors.

Requirement: modularity
^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2020/12/04

Contributors: Darren Engwirda, Xylar Asay-Davis, Carolyn Begeman

To the degree possible, the code for performing remapping should be kept in its 
own Fortran module(s) for better readability.



Algorithm Design (optional)
---------------------------

Algorithm Design: remapping
^^^^^^^^^^^^^^^^^^^^^^^^^^^
Date last modified: 2020/12/04

Contributors: Darren Engwirda, Xylar Asay-Davis, Carolyn Begeman

The conservation of momentum, volume, and tracer equations in MPAS-Ocean are:

.. math::

   \frac{\partial u_k}{\partial t} + q_k h_k u_k^{normal} + \overline{w^t \delta z^t u} = -\frac{1}{\rho_0} \nabla p_k - \frac{rho_k g}{\rho_0} \nabla z_k - \nabla K_k + [D_h^u]_k + [D_{\nu}^u]_k + F_k^u
   
   \frac{\partial h_k}{\partial t} + \nabla \cdot (h_k \mathbf{u}_k) + w_k^t - w_{k+1}^t = 0

   \frac{\partial (h_k \phi_k)}{\partial t} + \nabla \cdot (h_k \mathbf{u}_k \phi_k) + \overline{\phi}_k^t w_k^t - \overline{\phi}_{k+1}^t w_{k+1}^t = [D_h^{\phi}]_k + [D_{\nu}^{\phi}]_k + F_k^{\phi}
   
For the Lagrangian step, the vertical velocity through the top of the cell, :math:`w_k^t`, is set to zero in all of the above equations. Thus, these equations simplify to:

.. math::

   \frac{\partial u_k}{\partial t} + q_k h_k u_k^{normal} = -\frac{1}{\rho_0} \nabla p_k - \frac{rho_k g}{\rho_0} \nabla z_k - \nabla K_k + [D_h^u]_k + [D_{\nu}^u]_k + F_k^u
   
   \frac{\partial h_k}{\partial t} + \nabla \cdot (h_k \mathbf{u}_k) = 0

   \frac{\partial (h_k \phi_k)}{\partial t} + \nabla \cdot (h_k \mathbf{u}_k \phi_k) = [D_h^{\phi}]_k + [D_{\nu}^{\phi}]_k + F_k^{\phi}
   
The time-stepping algorithm (RK4 or split-explicit) yields the updated 
variables :math:`u_k^{lg},h_k^{lg},\phi_k^{lg}`.

For the regridding step, layer thicknesses are set according to the target 
grid, conserving volume:

.. math::

   h_k^{t+1} = h_k^{target}
   
   \sum_{k=1}^{kmax}h_k^{t+1} = \sum_{k=1}^{kmax}h_k^{lg}

Specification of the target grid:
- [Some design re. flexibility in user's specification of the grid]
- [Some design re. converting these specs into an analytical expression]
- Allowable coordinate systems: z-star (current), z-tilde (current), isopycnal (new)[?]

For the remapping step, velocities (edge-normal) and scalars are remapped to 
the target grid, conserving volume flux and scalar concentration:

.. math::

   \sum_{k=1}^{kmax} u_k^{t+1} h_k^{t+1} = \sum_{k=1}^{kmax} u_k^{lg} h_k^{lg}
   
   \sum_{k=1}^{kmax} \phi_k^{t+1} h_k^{t+1} = \sum_{k=1}^{kmax} \phi_k^{lg} h_k^{lg}



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

[Some PPR details here]

Conservation details are discussed in implementation section "conservation."
*Is PPR's conservation implementation equivalent to this equation?*

*Do we need to change PPR's error flags to be consistent with MPAS?*
*Do we need to add any additional error checks?*

Specification of target grid:

- Default is current vertical grid
- Should be a function of z_surface and z_bottom



Implementation: Grid is sufficiently conditioned for accuracy and stability
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

*Call the regrid/remapping function before timestepping begins in order to 
condition the grid and make adjustments for the initial ocean state in case 
coordinate system is isopycnal?*

After determining the target grid, perform the following steps:

#. Apply damping function to limit the rate of grid movement in one timestep 
   [and enforce a maximum movement limit?]. 
#. Optional: Assign :math:`h_k^{t+1}` to `h_k^{lg}` if :math:`h_k^{t+1} - h_k^{lg}` is 
   less than a minimum thickness alteration. This motivated by accuracy 
   considerations, as each remapping may introduce errors; however, it also 
   improves performance [is this true for PPR?].
#. Apply minimum layer thickness criterion.
#. Smooth the layer interfaces to reduce horizontal gradients in layer 
   thickness while conserving volume.

*Discuss damping function here or in algorithm design*

*Discuss smoothing function here or in algorithm design*

Namelist options:

- Minimum layer thickness
- Maximum thickness change during remapping
- Optional: minimum thickness change for remapping to occur
- Parameters in damping function


Implementation: hybrid coordinates
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Namelist options:

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
