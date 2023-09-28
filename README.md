A collection of libraries for configuration parsing for applications.

- The libraries are *independent* from one another. They are designed to work
  together, but each library solves a separate problem and may be used
  independently (they're all hosted in one repo to make development easier).

- All libraries are built for at least Scala on the JVM and Scala Native, since
  they are intended to be used for long-running services as well as short-lived
  CLI tools.

## Clam (command-line argument parsing)

The **c**ommand-**l**ine **a**rgument **m**apper.

## Confuse (configuration parsing)

Read configuration from multiple formats, and map it to scala types. A **fus**ion
of **con**figuration formats.

- YAML
- INI
- JSON
- HOCON (aka the typesafe/lightbend config library)
- JVM system properties
- env vars

## Basedirs

*TODO*

Standard types of directories.
