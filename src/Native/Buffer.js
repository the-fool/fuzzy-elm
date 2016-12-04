
var buffer = function(sz) {
  console.log('new buffer');
  return new Array(sz);
}

var set = function(i, val, ar) {
  if (!ar) { console.log("why is ar undefined?");}
  ar = ar || [];
  ar[i] = val;
  return ar;
}


var get = function(i, ar) {
  return ar[i];
}


var _user$project$Native_Buffer = {
  'buffer': buffer,
  'set': F3(set),
  'get': F2(get),
}
