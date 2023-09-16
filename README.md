A collection of libraries for configuration parsing in CLI applications.

- The libraries are *independent* from one another. They may work together, but
  each library solves one problem and may be used independently (they're all
  hosted in one repo to make development easier).

- All libraries are built for at least Scala on the JVM and Scala Native, since
  they are intended to be used for long-running services as well as short-lived
  CLI tools.

## Clam

The **c**ommand-**l**ine **a**rgument **m**apper.

## Confuze

Configuration from multiple formats.
- YAML
- INI
- JSON
- HOCON (aka typesafe's config)
- JVM system properties
- env vars

## Basedirs

Standard types of directories.
