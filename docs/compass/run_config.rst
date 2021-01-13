.. _compass_run_config:

run\_config
===========

A ``run_config`` file is used to define an environment to run the model in.
This file will describe the steps that convert a line of ``<model_run>``
from a run_script (defined in a config file) into an execution of the model.

Below is a description of the XML tags available within a run_config file:

``<run_config>`` - The parent tag of a run_config file. This is used to define a
run_config definition.

    - Children:
        * ``<define_env_var>``

        * ``<step>``

``<define_env_var>`` - This tag is used to specify the definition of an environment
variable. For example, OMP_NUM_THREADS.

    - Attributes:
        * ``name``: This attribute defines the name of the environment variable
          that will be set.

        * ``value``: This attribute defines the value that will be given to the
          environment variable. This attribute can take the value
          ``attr_{name}`` to use the value of the attribute in the
          ``<model_run>`` tag that is generating the model run, rather than
          having it's value hard coded.

``<step>`` - This tag defines a step in a run script
    - Attributes:
        * ``executable``: The base executable for this step of the run script.
          e.g. ``mpirun``

        * ``executable_name``: The name of the executable that has been defined in
          the configuration file to be used for this step of the run script.

    - Children:
        * ``<argument>``

``<argument>`` - This tag defines arguments for the executable in a specific step of
a run script.

    - Attributes:
        * flag: A flag that will come before the argument. e.g. ``-n``

    - Text:
        * The text between the ``<argument>`` and ``</argument>`` tags will be used as
          the argument after the flag. In the example ``mpirun -np 4`` the flag
          would be ``-np``, and the text would be ``4``.
          Additionally, the text of this tag can take the following keyword values:

            - ``model``: Use the model executable in place of an actual argument

            - ``attr_{name}``: Use the attribute of the ``<model_run>`` tag that
              generates a model run named ``{name}`` for the value.

Examples
--------

As an example, suppose a case for the test core had the following ``<model_run>``
tag::

  <model_run procs="1" threads="1" namelist="namelist.test" streams="streams.test"/>

And you wanted this to produce a set of lines that looked like::

    export OMP_NUM_THREADS=1
    mpirun -n 1 ./test_model -n namelist.test -s streams.test

A ``<run_config>`` file would look as follows for this::

    <run_config>
        <define_env_var name="OMP_NUM_THREADS" value="attr_threads"/>
        <step executable="mpirun">
            <argument flag="-n">attr_procs</argument>
            <argument flag="">model</argument>
            <argument flag="-n">attr_namelist</argument>
            <argument flag="-s">attr_streams</argument>
        </step>
    </run_config>

