.. _compass_driver_script:

driver\_script
==============

A ``driver_script`` file is used to generate a driver script that can be used to
automate several steps, including running multiple cases, and comparing output.
This file contains information describing how to create a script
that can be used to perform multiple steps in the process of running a test.
This includes running multiple run scripts within case directories, or
performing standard actions, such as comparing multiple output files.

Below, you will see text describing the various XML tags available in a
``driver_script`` file. Each will describe the tag itself, any attributes the
tag can have, and what children can be placed below the tag.

``<driver_script>`` - This tag defines a driver script for a set of case directories.

    - Attributes:
        * ``name``: The name of the driver script that will be generated

    - Children:
        * ``<case>``

        * ``<step>``

        * ``<define_env_var>``

        * ``<validation>``

``<case>`` - This tag defines the case that will be used for part of a driver
script. It implies the driver script should ``cd`` into the case
directory before executing the steps and arguments defined within.

    - Attributes:
        * ``name``: The name of the case directory that will be used for this
          portion of the driver script.

    - Children:
        * ``<step>``

        * ``<define_env_var>``

``<step>`` - This tag defines a step in a driver script

    - Attributes:
        * ``executable``: The base executable for this step of the script. e.g.
          test_model

        * ``executable_name``: The name of the executable that has been defined
          in the configuration file to be used for this step of the script.

    - Children:
        * ``<argument>``

``<argument>`` - This tag defines arguments for the executable in a specific step of
a script.

    - Attributes:
        * ``flag``: A flag that will come before the argument. e.g. ``-n``

    - Text:
        * The text between the ``<argument>`` and ``</argument>`` tags will be
          used as the argument after the flag. In the example ``mpirun -np 4``
          the flag would be ``-np``, and the text would be ``4``.

``<define_env_var>`` - This tag is used to define an environment variable which
might be needed when running the model. For example,
setting the value of ``OMP_NUM_THREADS`` to ensure OpenMP
threading is used.

    - Attributes:
        * ``name``: The name of the variable that will be set

        * ``value``: The value that will be given to the variable

``<validation>`` - This tag is used to define a block of validations that should
happen within the driver script. These validations are standard
operations that multiple driver scripts can use easily, such as
comparing fields in multiple files or against baselines. Test
case specific validations might happen in separate steps.

    - Children:
        * ``<compare_fields>``

        * ``<compare_timers>``

``<compare_fields>`` - This tag is used to define a comparison of specified
fields in two NetCDF files.

    - Attributes:
        * ``file1``: This defines the first file in the comparison.
          Relative to the ``<core>/<configuration>/<resolution>`` directory.
          Will also cause file1 to be compared against file1 in the baseline directory.

        * ``file2``: This defines the second file in the comparison.
          Relative to the ``<core>/<configuration>/<resolution>`` directory.
          Will also cause file2 to be compared against file2 in the baseline directory.

        * NOTE: If only one of file1 or file2 is specified, the testcase will
          only compare against baselines.

    - Children:
        * ``<field>``

        * ``<template>``

``<field>`` - This tag is used to define a field that will be compared within a
``<compare_fields>`` tag. Any norm thresholds that are specified all must pass to
have the comparison pass.

    - Attributes:
        * ``name``: This attribute defines the name of the field that will be compared

        * ``l1_norm``: This attribute defines the threshold for an L1 norm, to
          define a pass. If not specified, the L1 norm will not be used in
          determining if the comparison passes or fails.

        * ``l2_norm``: This attribute defines the threshold for an L2 norm, to
          define a pass. If not specified, the L2 norm will not be used in
          determining if the comparison passes or fails.

        * ``linf_norm``: This attribute defines the threshold for an L-Infinity norm, to
          define a pass. If not specified, the L-Infinity norm will not be used in
          determining if the comparison passes or fails.

``<template>`` - This tag defines a template that should be applied to define a list of field comparisions.

    - Attributes:
        * ``file``: The file that contains the template that should be expanded here.

        * ``path_base``: The base that the path attribute should be used relative
          to. Can be a pre-defined paths (see :ref:`compass_config`)

        * path: The path that the file lives in, relative to path_base.

``<compare_timers>`` - This tag is used to define a comparison of timers in two run directories.
The comparison will work with native or gptl timers automatically.

    - Attributes:
        * ``rundir1``: This is the first run directory to compare. If it is the
          only one specified timers in it will be compared only against it's
          baseline.

        * ``rundir2``: This is the second run directory to compare. If it is the
          only one specified timers in it will be compared only against it's
          baseline.

    - Children:
        * ``<timer>``

        * ``<template>``

``<timer>`` - This tag is used to define a timer that should be compared between two run directories.

    - Attributes:
        * ``name``: This is the name of the timer to compare. It should be the full
          expected name of the timer, not the printed name from the timer
          library.
