// jscl xhr-receive
//
// Problem: 1) It is impossible to pass js:null from the jscl to js
//          2) Impossible return back js:null. jscl raise error condition
//
// reqXHRsendNull - workaround for (1) case
//
var reqXHRsendNull = function (req) {
    req.send(null);
}


// #j:opNew
//
// (jscl::make-new #j:WebSocket "ws://192.0.0.1/40000")
// ERROR: Failed to construct 'WebSocket': The URL 'w,s,:,/,/,1,9,2,.,0,.,0,.,1,/,4,0,0,0,0' is invalid.
//
// Solved
//          Use lisp-to-js for this cases
//          (make-new #j:WebSocket (lisp-to-js "ws://192.0.0.1/40000"))
//          Ok
//
//
// (#j:opNew #j:window "WebSocket" "ws://192.0.0.1/40000")
// => #<JS-OBJECT [object WebSocket]>
//
// (defun make-js-instance (root-addr &rest  args)
//     (apply #j:opNew root-addr args))
//
// (make-js-instance #j:window "WebSocket" ""ws://192.0.0.1/40000"")
//
//
// Status: Will be deprecate
//
var opNew =  function () {
    var args = [].concat(null,Array.prototype.slice.call(arguments,2));
    var fn = arguments[0][arguments[1]];
    return new (Function.prototype.bind.apply(fn,args))();
};

// Eval
//
//  return res !== void 0;
//
//var opEval = function (s) {
//    var res = window.eval(s);
//    if (res === undefined) {
//        res = "undefined"; }
//    return res;
//};

// eval("var dd=11;") => undefined
// now
//     eval("var dd=11;") => false
//     eval("dd = 99;") => 99
//
var opEval = function (s) {
    var res = window.eval(s);
    if (res !== void 0) {return res;} {return false;}
};



// js object manipulation
//

// (setf *ar (make-new #j:Array))
// (#j:setObjectValue *ar 0 "name")
// => #("name") or ["name"]
//                or
// (setf *obj (new))
// (#j:setObjectValue *ar "value" (lisp-to-js "name"))
// => {value: ["name"]}
//
// See klib/src/jsom.lisp comments for more detail
//
var setObjectValue = function (obj,idx,val) {return obj[idx]=val;};
