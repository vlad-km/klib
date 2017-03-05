# Kernel library for minimal JSCL console ide

JSCL is a Common Lisp to Javascript compiler, which is bootstrapped
from Common Lisp and executed from the browser.
[https://github.com/jscl-project]


## Include files

- **src/attic.lisp**     different functions that are used regularly

- **src/jsom.lisp**      functions to manipulate the JS objects 

- **src/dom.lisp**       functions to manipulate the DOM objects

- **src/resman.lisp**    primitive resource manager.

- **src/load.lisp**      Common lisp function LOAD release for browser.
This implementation doesn't match CLHS 

- **src/res-load.lisp**  some functions for load and hot plugin resources (.js .css) 
from host (localhost/extension). 

- **lib/klib.js**        compiled library

- **lib/rtjscl.js**      js functions `opNew`, `opEval`, `reqXHRsendNull`
for the desired interaction jscl with js environment. See **bof.md** for more information. 
**Important!** the file must be load after jscl.js and  before **klib.js** 
(see Install section for example)


## Compile

In the 'bootstrap' to write the following:

    (compile-application (append (directory "klib/attic.lisp")
                                 (directory "klib/jsom.lisp")
                                 (directory "klib/dom.lisp")
                                 (directory "klib/resman.lisp")
                                 (directory "klib/load.lisp")
                                 (directory "klib/res-loader.lisp"))
                         (merge-pathnames "klib.js" "moren/static/js"))

and call in you Lisp: 

    (jscl:bootstrap)

## Install

```html
<script src="moren/static/js/jscl.js"></script>
<script src="moren/static/js/rtjscl.js"></script>
<script src="moren/static/js/klib.js"></script>
<script ... other .... </script>
```


## Documentation

Will with time. Till, please see comments to each function on each file.


## 

Copyright, 2016, mvk
As is, with no guarantees

*Have a fun*
