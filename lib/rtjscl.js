// jscl xhr-receive
//
// Problem: 1) It is impossible to pass js:null from the jscl to js
//
// reqXHRsendNull - workaround for (1) case
//
var reqXHRsendNull = function (req) {
    req.send(null);
}


