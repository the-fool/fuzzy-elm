var _user$project$Native_Buffer = {
  'buffer': buffer,
  'set': set,
  'get': get
}

var buffer = function(sz) {
  return new Array(sz);
}

var set = function(ar, i, val) {
  ar[i] = val;
  return ar;
}

var get = function(ar, i) {
  return ar[i];
}

// make is a function that takes an instance of the
// elm runtime
// returns an object where:
//      keys are names to be accessed in pure Elm
//      values are either functions or values
var make = function make(elm) {
    // If Native isn't already bound on elm, bind it!
    elm.Native = elm.Native || {};
    // then the same for our module
    elm.Native.Buffer = elm.Native.Buffer || {};

    // `values` is where the object returned by make ends up internally
    // return if it's already set, since you only want one definition of
    // values for speed reasons
    if (elm.Native.Buffer.values) return elm.Native.Buffer.values;

    // return the object of your module's stuff!
    return elm.Native.Buffer.values = {
      'buffer': buffer,
      'set': set,
      'get': get
    };
};

exports.make = make;


// setup code for MyModule
// Elm.Native.MyModule should be an object with
// a property `make` which is specified above