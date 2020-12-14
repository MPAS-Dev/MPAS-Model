
Vertical Lagrangian-remap method
======================

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

Requirement: name-of-requirement-here
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2020/11/24

Contributors: Darren Engwirda, Xylar Asay-Davis, Carolyn Begeman

Requirement: A grid that relaxes to an analytical expression for a target grid.

Requirement: The ability to implement hybrid coordinate systems.

Requirement: XX order accuracy of the solution.

Requirement: Performance is not degraded below an acceptable level.

Requirement: Layer thicknesses are non-negative (and above a minimum thickness).

*Possible additional requirements:
- re. development of instabilities
- re. conservation/monotonicity of interpolants
*


*Each requirement is to be listed under a "section" heading, as there will be a
one-to-one correspondence between requirements, design, proposed implementation
and testing. Requirements should not discuss technical software issues, but
rather focus on model capability. To the extent possible, requirements should
be relatively independent of each other, thus allowing a clean design solution,
implementation and testing plan.*


Algorithmic Formulations (optional)
-----------------------------------

Requirement: A grid that relaxes to an analytical expression for a target grid.
Design solution: Vertical lagrangian-remapping
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: 2020/11/24

Contributors: Darren Engwirda, Xylar Asay-Davis, Carolyn Begeman

The vertical Lagrangian-remap can be characterized by the following equations, as written in Griffies and Adcroft (2020):

*Math likely needs to be edited from latex version*
.. math::
\begin{eqnarray}
\mbox{\textcolor{blue}{lagrangian layer motion}}&&\label{w_grid}
\Delta_s w^{grid}_{k,*} = -D^n_{k,*}\\
\mbox{\textcolor{blue}{hor advec h update}}&&\label{dh1}h^{tem} = h^n + \Delta t \Delta_s w^{grid}_{k,*}\\
\mbox{\textcolor{blue}{hor advec tracer update}}&&[hC]^{tem} = [hC]^n - \Delta t \nabla_h \cdot [hCu]^n\\
\mbox{\textcolor{blue}{Update h, regrid}}&&h^{n+1} = h^{target}\\
\mbox{\textcolor{blue}{Diasurface velocity}}&&\Delta_s w^{tem} = -(h^{target} - h^{tem})/\Delta t\\
\mbox{\textcolor{blue}{Remap tracer and velocities}}&&[hC]^{n+1} = [hC]^{tem} - \Delta t \Delta_s (w^{tem}C^{tem})\label{eq:remap}
\end{eqnarray}

Specification of the target grid:
- [Some design re. flexibility in user's specification of the grid]
- [Some design re. converting these specs into an analytical expression]
- Allowable coordinate systems: z-star (current), z-tilde (current), isopycnal (new)[?]

*For each requirement, there is a design solution that is intended to meet that
requirement. Design solutions can include detailed technical discussions of
PDEs, algorithms, solvers and similar, as well as technical discussion of
performance issues. In general, this section should steer away from a detailed
discussion of low-level software issues such as variable declarations,
interfaces and sequencing.*


Design and Implementation
-------------------------

Implementation: short-description-of-implementation-here
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: YYYY/MM/DD

Contributors: (add your name to this list if it does not appear)

The remapping operation detailed in \ref{eq:remap} is performed in several steps:
.. math::
\begin{eqnarray}
\mbox{\textcolor{blue}{Reconstruction}}&&C^{n+1} = f(C^{n},h^{n},h^{n+1})
\mbox{\textcolor{blue}{Enforce conservation by correction}}&&[hC]^{n+1} = [hC]^{tem} - \Delta t \Delta_s (w^{tem}C^{tem})\label{eq:remap}
\end{eqnarray}
*Does conservation implementation actually take this form? Is it equivalent?*

Specification of target grid:
- Default is current vertical grid
Should be a function of z_surface and z_bottom
Damping function to limit the rate of grid movement in one timestep
Maximum/minimum thickness alteration: as described in Petersen et al. (2015)for ALE?

*This section should detail the plan for implementing the design solution for
requirement XXX. In general, this section is software-centric with a focus on
software implementation. Pseudo code is appropriate in this section. Links to
actual source code are appropriate. Project management items, such as git
branches, timelines and staffing are also appropriate. Pseudo code can be
included via blocks like*

.. code-block:: python

   def example_function(foo):
       return foo**2.0


Testing
-------

Testing and Validation: short-description-of-testing-here
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Date last modified: YYYY/MM/DD

Contributors: (add your name to this list if it does not appear)

*How will XXX be tested, i.e., how will be we know when we have met requirement
XXX? Which tests from the regression suites are appropriate?  How would they
need to be configured or modified to test that the new software is working
properly?  What additions or modifications to the nightly regression suite might
be made to ensure that the new capability continues to work as expected?*
