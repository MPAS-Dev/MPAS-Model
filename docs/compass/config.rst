.. _compass_config:

config
======

A config file is used to setup a case directory.
This file contains information describing how to configure a case
directory, including files that the case depends on, executables that are
required for the case, namelists and streams files the case requires, and run
scripts which can be used to automate running a case.

How to use pre-defined paths
----------------------------

This testing infrastructure has several predefined paths available as
attributes to several XML tags. Attributes that can use these will have the
line "Can use pre-defined paths" in their description.

In order to help you make use of these pre-defined paths, this section will
describe what they are, and how to use them.

To begin, there are two standard paths. These are referred to as ``<work_dir>``
and ``<script_path>``.

    - ``<work_dir>`` is the location where the test cases are setup to run.
    - ``<script_path>`` is the location where the testing infrastructure scripts live.

Additionally, there are 4 sub-paths:

    - ``<core_dir>`` - This is the core directory that contains the test case
    - ``<configuration_dir>`` - This is the configuration directory that contains the test case
    - ``<resolution_dir>`` - This is the resolution directory that contains the test case
    - ``<test_dir>`` - This is the test directory that contains the test case
    - ``<case_dir>`` - This is the case directory that is generated from an XML config file

Now, all attributes that can use pre-defined paths can build a path using the
following syntax::

  {base}_{sub}

Where ``{base}`` can be either ``work`` or ``script``, and ``{sub}`` can be any of
``core_dir``, ``configuration_dir``, ``resolution_dir``, ``test_dir``, and ``case_dir``.

Note however, ``case_dir`` isn't valid when {base} is ``script`` as a case
directory isn't typically generated in the script path if it's different from
the work path.

As an example:

 - ``script_test_dir`` would point to the location that the XML files exist to
   setup a testcase
 - ``work_test_dir`` would point to the location that the testcase will be setup,
   and will not include the case directory created from an XML file.


Description of XML file
-----------------------

Below, you will see text describing the various XML tags available in a config
file. Each will describe the tag itself, any attributes the tag can have, and
what children can be placed below the tag.

``<config>`` - This is the overarching parent tag of a config file that describes the setup for a case.

    - Attributes:
        * ``case``: The name of the case directory that will be created from this
          config tag.

    - Children:
        * ``<get_file>``

        * ``<add_executable>``

        * ``<add_link>``

        * ``<namelist>``

        * ``<streams>``

        * ``<run_script>``

``<get_file>`` - This tag defines the need for ensuring a required file is available, and the
appropriate ways of acquiring the file.

    - Attributes:
        * ``hash``: (Optional) The expected hash of the mesh file. The acquired
          mesh file will be validated using this. If this attribute is omitted,
          the resulting file will not be validated.

        * ``dest_path``: The path the resulting file should be placed in. Should be
          the name of a path defined in the config file, or optionally 'case'
          which is expanded to be the case directory generated from the XML
          file containing the get_file tag. Can additionally take the values of
          pre-defined paths

        * ``file_name``: The name of the file that will be downloaded and placed in dest_path.

    - Children:
        * ``<mirror>``

``<mirror>`` - This tag defined the different methods of acquiring a required file.

    - Attributes:
        * ``protocol``: A description of how the mesh should be retrieved.
          Currently supports ``wget``.

        * ``url``: Only used if ``protocol == wget``. The url (pre-filename) portion of
          the ``wget`` command.

``<add_executable>`` - This tag defined the need to link an executable defined in a
configuration file (e.g. general.config) into a case directory.

    - Attributes:
        * ``source``: The name of the executable, defined in the configuration file
          (e.g. ``general.config``). This name is a short name, and will be
          expanded to executables.source

        * ``dest``: The name of the link that will be generated from the executable.

``<add_link>`` - This tag defined the need to link a file into a case directory.

    - Attributes:
        * ``source_path``: The path variable from a configure file to find the
          source file in. If it is empty, source is assumed to
          have the full path to the file. Additionally, it can
          take the values of:

          - Can use pre-defined paths

        * ``source``: The source to generate a symlink from. Relative to the case
          directory that will be generated from the parent ``<config>`` tag.

        * ``dest``: The name of the resulting symlink.

``<namelist>`` - This tag defines a namelist that should be generated from a template.

    - Attributes:
        * ``name``: The name of the namelist file that will be generated from the
          template namelist pointed to by its mode attribute.

        * ``mode``: The name of the mode to use from the template input files
          Each core can define these arbitrarily

    - Children:
        * ``<template>``

        * ``<option>``

``<streams>`` - This tag defines a streams file that should be generated from a template.

    - Attributes:
        * ``name``: The name of the streams file that will be generated from the
          template streams file pointed to by its mode.

        * ``mode``: The name of the mode to use from the template input files
          Each core can define these arbitrarily

        * ``keep``: A definition of which streams to keep from the template. Values are:

            - ``all``: keep all streams from the template

            - ``immutable``: keep all immutable streams, and discard any mutable
              streams from the template

            - ``mutable``: keep all mutable streams, and discard any immutable
              streams from the template

            - ``none``: discard all streams from teh template

    - Children:
        * ``<template>``
        * ``<stream>``

``<template>`` - This tag defines a template that should be applied to a set of configurations.

    - Attributes:
        * ``file``: The file that contains the template that should be expanded here. When
          used within a ``<namelist>`` tag, the namelist portion of the template
          will be applied. When used within a ``<stream>`` tag, the streams portion
          of the template will be applied. Additionally, ``<template>`` tags
          can be used within ``<compare_fields>`` and ``<compare_timers>`` tags
          to define template fields and timers to compare.

        * ``path_base``: The base that the path attribute should be used relative
          to. Can have a value of pre-defined paths

        * ``path``: The path that the file lives in, relative to path_base.

``<option>`` - This tag defines an option that should be modified in the generated
namelist.

    - Attributes:
        * ``name``: The name of the option that should be modifed

    - Text:
        * The text within <option> and </option> tags will be used to set the
          value of the namelist option.

``<stream>`` - This tag defines a stream that should be modified / created in the
generated streams file.

    - Attributes:
        * ``name``: The name of the stream that should be modified / created

    - Children:
        * ``<attribute>``

        * ``<add_contents>``

        * ``<remove_contents>``

``<attribute>`` - This tag defines an attribute that should be created / modified
in a stream definition.

    - Attributes:
        * ``name``: The name of the stream attribute to modify / define

    - Text:
        * The text in between the <attribute> and </attribute> tags will be
          used to set the value of the attribute.

``<add_contents>`` - This tag defines a list of members to add to a stream definition

    - Children:
        * ``<member>``

``<remove_contents>`` - This tag defines a list of members to remove from a stream definition

    - Children:
        * ``<member>``

``<member>`` - This tag defines a member that should be added or removed from a stream definition.

    - Attributes:
        * ``name``: The name of the member that will be defined. If this is in an
          ``<add_contents>`` tag, it will be added to the stream, if it is in a
          ``<remove_contents>`` tag, it will be removed from the stream.

        * ``type``: The type of the member to add (This is ignored if it's within a
          ``<remove_contents>`` tag). Example values are var, var_array,
          ``var_struct``, and stream.

``<run_script>`` - This tag defines a new run script that should be generated.

    - Attributes:
        * ``name``: The name of the script that will be generated

    - Children:
        * ``<step>``

        * ``<define_env_var>``

        * ``<model_run>``

``<step>`` - This tag defines a step in a run script

    - Attributes:
        * ``executable``: The base executable for this step of the run script. e.g. mpirun

        * ``executable_name``: The name of the executable that has been defined in
          the configuration file to be used for this step of the run script.

    - Children:
        * ``<argument>``

``<argument>`` - This tag defines arguments for the executable in a specific step of
a run script.

    - Attributes:
        * ``flag``: A flag that will come before the argument. e.g. ``-n``

    - Text:
        * The text between the ``<argument>`` and ``</argument>`` tags will be used as
          the argument after the flag. In the example ``mpirun -n 4`` the flag
          would be -n, and the text would be 4.

``<define_env_var>`` - This tag is used to define an environment variable which
might be needed when running the model. For example,
setting the value of OMP_NUM_THREADS to ensure OpenMP
threading is used.

    - Attributes:
        * ``name``: The name of the variable that will be set

        * ``value``: The value that will be given to the variable

``<model_run>`` - This tag is used to define a run of the model, as configured by
some set of attributes.

    - Attributes:
        * This tag is unique, in that it can take a variety of attributes. The
          attributes available depend on the batch system. Within the
          definition of the batch system, any attribute that has a value with
          the given format ``attr_{name}`` represents an attribute that is required
          when using this tag. An example of attributes that most batch systems
          would require is:

            - ``procs``: The number of MPI tasks to spawn
            - ``threads``: The number of OpenMP threads to use in the run
            - ``namelist``: The namelist file to use when performing the run
            - ``streams``: The streams file to use when performing the run
            - ``executable``: (Optional) The name of the executble to use from the
              config file. If this is not specified, it defaults to 'model'.
