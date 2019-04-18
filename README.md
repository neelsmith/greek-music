# `greekmusic`

## What it is


`greekmusic` is a library for working with ancient Greek music encoded in a notation inspired by the Virgapes notation for neumes. It currently builds on the JVM and Javascript.



## Current version: 2.1.3

Status: active development. Release notes

## License

[GPL 3.0](https://opensource.org/licenses/gpl-3.0.html)

## Documentation

Forthcoming...


## Using, building, testing

`greekmusic` can be built using any version of Scala 2.11 or higher. Binaries are available from jcenter built with Scala 2.11.8 and 2.12.8. To use the binaries in an sbt project, include `Resolver.jcenterRepo` in your list of resolvers

    resolvers += Resolver.jcenter

and add this to your library dependencies:

    "edu.holycross.shot.cite" %% "virgapes" % VERSION

For maven, ivy or gradle equivalents, refer to <https://bintray.com/neelsmith/maven/greekmusic>.
