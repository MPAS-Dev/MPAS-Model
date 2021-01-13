.. _compass_template:

template
========

This document describes the format of a template file, which is used to define
a set of namelist and streams configurations that will be used multiple times.

A template file contains information describing how to configure a namelist
and/or a streams file in a standard way. A template file can be applied to
multiple cases, and allows config files for cases to be shorter since the
options are defined within the template instead of in the config file.

Below, you will see text describing the various XML tags available in a template
file. Each will describe the tag itself, any attributes the tag can have, and
what children can be placed below the tag.

``<template>`` - This is the overarching parent tag within a template file. It
begins a template definition.

    Children:
        * ``<namelist>``

        * ``<streams>``

``<namelist>`` - This tag defines a set of namelist modifications that will be
applied if this template is used within a config file's ``<namelist>`` tag.

    - Children:
        * ``<option>``

        * ``<template>`` (child template)

``<option>`` - This tag defines an option that should be modified in the namelist file.

    - Attributes:
        * ``name``: The name of the option that should be modified

    - Text:
        * The text within ``<option>`` and ``</option>`` tags will be used to set the
          value of the namelist option.

``<streams>`` - This tag defines a set of streams modifications that will be
applied if this template is used within a config file's ``<streams>`` tag.

    - Children:
        * ``<stream>``

``<stream>`` - This tag defines a stream that should be modified / created in the
streams file.

    - Children:
        * ``<attribute>``

        * ``<add_contents>``

        * ``<remove_contents>``

``<attribute>`` - This tag defines an attribute that should be created / modified
in a stream definition.

    - Attributes:
        * ``name``: The name of the stream attribute to modify / define

    - Text:
        * The text in between the ``<attribute>`` and ``</attribute>`` tags will be
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
          ``<remove_contents>`` tag). Example values are ``var``, ``var_array``,
          ``var_struct``, and ``stream``.

``<validation>`` - This tag defines a set of templated validations that can be applied from a template file.
The validation template tag can only be applied to driver script files.

    - Children:
        * ``<compare_fields>``

        * ``<compare_timers>``

``<compare_fields>`` - This tag contains a list of fields that should be compared
if the template is used within a ``<compare_fields>`` tag.

    - Children:
        * ``<field>``

        * ``<template>``

``<field>`` - This tag defines a specific field that should be compared if this
template is used within a ``<compare_fields>`` tag. See
:ref:`compass_driver_script` for configuration information.

``<compare_timers>`` - This tag contains a list of timers that should be compared
if the template is used within a ``<compare_timers>`` tag.

    - Children:
        * ``<timer>``

        * ``<template>``

``<template>`` (child template) - This tag allows another template to be applied
from within a template file. WARNING: A template can include any other template
(including it self) so it is possible to create an infinite loop of template
application.

    - Attributes:
        * ``file``: The file that contains the template that should be expanded
          here. When used within a ``<namelist>`` tag, the namelist portion of
          the template will be applied. When used within a ``<stream>`` tag,
          the streams portion of the template will be applied. Additionally,
          ``<template>`` tags can be used within ``<compare_fields>`` and
          ``<compare_timers>`` tags to define template fields and timers to
          compare.

        * ``path_base``: The base that the path attribute should be used relative
          to. Can be a pre-defined paths (see :ref:`compass_config` for more
          information)

        * ``path``: The path that the file lives in, relative to ``path_base``.

