Scala Bindings for SimpleDB (Amazon Web Services)
=================================================

This is a fork of <http://github.com/rbarooah/simpledb-scala-binding> to clean up the code and port to Scala 2.8.0.

### Building ###

You need Apache Buildr 1.3.x or higher.

    # compile, test and package .jars
    buildr package

### Examples ###

Launch your Scala REPL and see the examples under <src/examples>.

    # launch the sample stopwatch server on port 9999
    scala -jar target/simpledb-scala-binding-1.0.0.jar

### Target platform ###

* Scala 2.8.0+
* JVM 1.5+

### License ###

simpledb-scala-binding is is licensed under the terms of the Apache Software License v2.0.
<http://www.apache.org/licenses/LICENSE-2.0.html>
