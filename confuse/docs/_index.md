---
title: Confuse
---

Parse and map configuration from multiple formats

## Example

```scala
{{%snippet src="src/confuse/example/src/main.scala" name="example"%}}
```

## Maven Coordinates

Scala 3, JVM and Native

```scala
ivy"io.crashbox::confuse::{{<version>}}"
```

## Code Structure

Tthis the intended dependency graph between packages:

```mermaid
flowchart LR
  confuse .-> |export| confuse.api;
  confuse.api --> confuse.model;
  confuse.api --> confuse.formats;
  confuse.formats --> confuse.model;
  confuse.model --> confuse.parsers;
  confuse.api --> confuse.parsers;
```

## Control flow
```mermaid
flowchart LR
  files --> |read| model;
  env --> |read| model;
  args --> |read| model;
  model --> |parse| scala-type;
```
