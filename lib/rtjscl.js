// jscl xhr-receive
var reqXHRsendNull = function (req) {
    req.send(null);
}


// #j:opNew
//
// (jscl::make-new #j:WebSocket "ws://192.0.0.1/40000")
// ERROR: Failed to construct 'WebSocket': The URL 'w,s,:,/,/,1,9,2,.,0,.,0,.,1,/,4,0,0,0,0' is invalid.
//
// (#j:opNew #j:window "WebSocket" "ws://192.0.0.1/40000")
// => #<JS-OBJECT [object WebSocket]>
//
// (defun make-js-instance (root-addr &rest  args)
//     (apply #j:opNew root-addr args))
//
// (make-js-instance #j:window "WebSocket" ""ws://192.0.0.1/40000"")
//

var opNew =  function () {
    var args = [].concat(null,Array.prototype.slice.call(arguments,2));
    var fn = arguments[0][arguments[1]];
    return new (Function.prototype.bind.apply(fn,args))();
};

//
//
//
var opEval = function (s) {
    var res = window.eval(s);
    if (res === undefined) {
        res = "undefined"; }
    return res;
};

// js object manipulation
//

// (setf *ar (#j:makeArray))
// (#j:setObjectValue *ar 0 (#j:returnString "name"))
// => #("name") or ["name"]
//                or
// (setf *obj (new))
// (#j:setObjectValue *ar "value" (#j:returnString "name"))
// => {value: ["name"]}
//
var setObjectValue = function (obj,idx,val) {return obj[idx]=val;};

// make js array
//
// (setf *ar (#j:makeArray))
// => #()
//
// (setf (aref *ar 0) 111)
//=> Error: Out of range.
//
// (funcall ((oget *ar "push" "bind") *ar 111))
// => #(111)
var makeArray = function(){return new Array();};


// jscl-js string compatibilite
//
// (equal (#j:returntString "11") (#j:returnString 11))
// => t
// (equal (#j:returnString "11") (#j:returnString "11"))
// => t
// (equal (#j:returnString "11") "11")
// => t
var returnString = function(x) {if (typeof x === 'string') {return x;} {return String(x);};};
