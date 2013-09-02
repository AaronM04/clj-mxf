clj-mxf
=======

A Clojure library for parsing and manipulating MXF (Media eXchange Format)
files, especially those created with Avid Media Composer.

Installation
------------

Run make, which will copy the Leiningen shell script ("lein") to /usr/local/bin
if it's not already there (requires sudo), and runs 'lein uberjar' to produce
the standalone .jar mentioned in the usage section. Leiningen will download
itself to ~/.lein the first time it is executed, and will download all the
dependencies for clj-mxf as well.

Run make install, which will install the .jar file and shell scripts to
/usr/local/bin (FIXME: shell scripts not implemented yet).

REPL Usage
----------

Run 'lein repl' from the clj-mxf directory.

This is a useful function for REPL work:
    (defn reload []
      (load-file "src/clj_mxf/slingshot_workaround.clj")
      (load-file "src/clj_mxf/bits.clj")
      (load-file "src/clj_mxf/ul.clj")
      (load-file "src/clj_mxf/core.clj"))

Command-line Usage
------------------

FIXME: shell script usage

Examples
--------

...

