const buffer = (sz) => new Array(sz);

const set = (i, val, ar) => {
  if (!ar) { console.log("why is ar undefined?");}
  ar = ar || [];
  ar[i] = val;
  return ar;
}

const get = (i, ar) => ar[i];

const _user$project$Native_Buffer = {
  'buffer': buffer,
  'set': F3(set),
  'get': F2(get),
}
