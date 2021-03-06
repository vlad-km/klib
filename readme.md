# Status

**The project is closed**


# Kernel library for minimal JSCL environment 

JSCL is a Common Lisp to Javascript compiler, which is bootstrapped
from Common Lisp and executed from the browser.
[https://github.com/jscl-project]


## Compile

In the 'function bootstrap (jscl.lisp)' to write the following:

```lisp
(compile-application (append (directory "klib/attic.lisp")
                             (directory "klib/jsom.lisp")
                             (directory "klib/string.lisp")
                             (directory "klib/dom.lisp")
                             (directory "klib/load.lisp")
                             (directory "klib/res-loader.lisp")
                             (directory "klib/print.lisp")
                             (directory "klib/dom-stream-output.lisp")
                             (directory "klib/klib.lisp")
                             (directory "klib/trivial-ws.lisp") )
                     (merge-pathnames "klib.js" "moren/static/js"))
```

and call in you Lisp:

```lisp
(jscl:bootstrap)
```

## Previous versions

1. March, 2017 - see branch klib-master-03-2017


Copyleft, 2017, mvk
As is, with no guarantees

*Have a fun*
