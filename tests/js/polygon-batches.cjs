// Regression for Arrow IPC streams containing multiple record batches.
// Uses the shipped Arrow runtime and the actual private binding function.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');
const root = path.resolve(__dirname, '../..');
const {arrow: a} = require(path.join(root, 'inst/htmlwidgets/lib/deckgl/deckgl-bundle.js'));
const binding = fs.readFileSync(path.join(root, 'inst/htmlwidgets/deckgl.js'), 'utf8');
const start = binding.indexOf('  function arrowPolygonToBinary(');
const end = binding.indexOf('\n  // Process layers:', start);
assert(start >= 0 && end > start);
const convert = vm.runInNewContext('('+binding.slice(start,end).trim()+')', {
  performance, console: {log(){}, warn(){}, error(){}}, Uint32Array, Float32Array, Float64Array
});
const type = new a.List(new a.Field('rings', new a.List(new a.Field('points',
  new a.Struct([new a.Field('x',new a.Float64()),new a.Field('y',new a.Float64())])))));
const batch = (start, n) => a.tableFromArrays({geometry:a.vectorFromArray(
  Array.from({length:n},(_,j)=>[[{x:start+j,y:0},{x:start+j+1,y:0},{x:start+j,y:1},{x:start+j,y:0}]]), type)});
const first = batch(0,2048), second = batch(2048,2048), last = batch(4096,904);
for (const [name, table, offset] of [
  ['single batch',first,0], ['multiple batches',first.concat(second,last),0],
  ['sliced batches',first.concat(second,last).slice(1000,4500),1000]
]) {
  const result=convert(table,'geometry');
  assert(result, name+': conversion failed');
  assert.equal(result.length,table.numRows,name+': rows');
  assert.equal(result.startIndices.length,table.numRows+1,name+': offsets');
  assert.equal(result.positions.length,table.numRows*4*2,name+': positions');
  for(let i=0;i<table.numRows;i++) {
    assert.equal(result.startIndices[i],i*4,name+': vertex offset');
    assert.equal(result.positions[i*8],offset+i,name+': row coordinate');
  }
  assert.equal(result.startIndices[table.numRows],result.positions.length/2);
  console.log('PASS '+name+' ('+table.numRows+' polygons)');
}
