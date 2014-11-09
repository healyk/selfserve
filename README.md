SelfServe
===

Self Serve is a self contained webserver.  It is made to run on the
local host and not be exposed to an outside network.  It's purpose is
to easily allow development and testing of prototype and as an easily
distributable to desktop machines.

## Config

A configuration file will be generated the first time main.scm is run.
This config file looks something like this:

```scheme
(make-server-config 
  port: 81
  log-filename: "log/server.log"
  root-dir: "public")
```

Most of it is self-explainitory.

  * port - Which port the http server should run on
  * log-filename - Relative location and filename of the file to write
    logs to.
  * root-dir - Where the http root directory exists.

## What is done

Right now any file in the public directory will be served up as a
regular file.  So ```public/foo.html``` can be accessed with a web
browser hitting the endpoint ```localhost:3456/foo.html```

