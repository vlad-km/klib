# Bug or feature (working notes)

## rtjsck.js

`reqXHRsendNull`:  
>wrapper for call  **xhr.send** method with null as the argument. 
Need for correct **XMLHttpRequest**. As an example: xhr.send(null).
Another case, when out `JS` get an object containing` null`. In this case, an interrupt occurs:
`>> ERROR: Cannot use 'in' operator to search for 'length' in null`
                   

`opNew`: 
>js `new` wrapper. Unfortunately `make-new` not correctly create objects. 
As an example:

```lisp
(jscl::make-new #j:WebSocket (#j:String "ws://127.0.0.1:40000")) 
=> ERROR: Failed to construct 'WebSocket': The URL 'w,s,:,/,/,1,2,7,.,0,.,0,.,1,:,4,0,0,0,0' is invalid
```

`opEval`: 
>for correct ops with window.eval in a case where `undefined` returns. 
As an example:

```js                 
(setf *dd (#j:window:eval "var *dd"))
(if *dd ..)
=> ERROR: Variable *DD is unbound 
``` 

Possible around:

```lisp
(setf *dd (#j:window:eval "var *dd"))
(if (not (boundp '*dd)) (setf *dd nil))
(if *dd .... ...)
```
or
```lisp
(setf *dd (#j:opEval "var *dd"))
(if (equal *dd "undefined") 
     ...
     ...)
```

## Other cases

### Object.keys and jscl string

```lisp
(setf *obj1 (make-js-object "aa" 1))
(setf *keys1 (#j:Object:keys *obj1))
;
(equal (aref *key1s 0) "aa")
=> nil
(string (aref *keys1 0))
;=> "aa"
;
(equal (string (aref *keys1 0)) "aa"))
;=> nil
(equal (funcall ((jscl::oget (aref *keys1 0) "toString" "bind") (aref *keys1 0))) "aa")
;=> t
;
(setf *s1 (aref k1 0))
(equal *s1 "aa")
;=> t
(equal (string *s1) "aa")
;=> t

``` 


### Communication JSCL with browser API's

We want to see the browser's history.
API chrome.history provides, for this, method **search** [https://developer.chrome.com/extensions/history].

When call the method, take off the three timestamp (functions time-start time-stop):

- Time1 beginning of lisp code
- Time2 completion time lambda function, which we passed in `chrome.history.search` as a callback
- Time3 time finalize our lisp code

It looks like this:

```lisp 
(let ((hi))  
    (time-start)   
        (#j:chrome:history:search  
           ;; query
           (make-js-object "text" "" "startTime" (historyStartTime) "maxResults" 10)  
           ;; callback
           (lambda (historyItems) 
                 (time-stop "Time1")  
                     (setf hi historyItems)   
                     (time-stop "Time2")))  
    (time-stop "Time3" ) 
   hi)
=>Time3: 0.001 ms
  NIL
  Time1: 0.107 ms
  Time2: 0.115 ms
```

These timestamps, indicate the following:

- at the time "Time3" execution of the lambda functions in `chrome.history.search`
has not yet started. It will happen at a time "Time1: 0.107 ms"

- assigning a value of the variable 'hi' will happen at a time "Time2: 0.115 ms",
after completion `let` in which the variable 'hi' was defined

- block `let` will be completed at the time "Time3: 0.001 ms"



** Possible conclusion **
> Due to asynchronous Javascript and using familiar to lisp
programming style, we will not achieve our goal. Go to the asynchronous style.

One way of might be:

```lisp
(#j:chrome:history:search
      (make-js-object "text" "" "startTime" (historyStartTime) "maxResults" 10)
         (lambda (historyItems)
              (dotimes (i (length historyItems))
                    (print (oget (aref historyItems i) "title")))))
```

Or, use, `Promise`. We define two functions `get-hist-promise` and` then` such that:

`Get-hist-promise` returns the promise of access to historical data.

```lisp
(defun get-hist-promise ()
       (#j:opNew #j:window "Promise" 
             (lambda (resolve rejected)
                  (#j:chrome:history:search
                       (make-js-object "text" "" "startTime" (historyStartTime) "maxResults" 10)
                             (lambda (historyItems)
                                  (funcall resolve historyItems))))))

```                    

`then`, its wrapper for `Promise` method ` then`:

```lisp
(defun then (prom handler)
      (funcall ((oget prom "then" "bind") prom handler)))
```

Then the our decision will look like this:

```lisp
(then (get-hist-promise)
      (lambda (x)
          (dotimes (i (length x))
                 (print (oget (aref x i) "title")))))
```

Ok.







