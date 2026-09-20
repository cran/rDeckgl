// Binary attribute conversion for standard layers fed by Arrow tables with
// several record batches. Uses the shipped Arrow runtime and the actual
// private conversion function from the widget binding.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');
const root = path.resolve(__dirname, '../..');
const {arrow: a} = require(path.join(root, 'inst/htmlwidgets/lib/deckgl/deckgl-bundle.js'));
const binding = fs.readFileSync(path.join(root, 'inst/htmlwidgets/deckgl.js'), 'utf8');
const start = binding.indexOf('  function arrowTableToBinaryAttributes(');
const end = binding.indexOf('\n  // Load binary payloads', start);
assert(start >= 0 && end > start);
const warnings = [];
const convert = vm.runInNewContext('(' + binding.slice(start, end).trim() + ')', {
  console: {log() {}, warn(message) { warnings.push(message); }, error() {}},
  Uint8Array, Float32Array, Float64Array, Int32Array, BigInt64Array
});

// Three batches of 2048 + 2048 + 904 rows, as DuckDB emits them.
const sizes = [2048, 2048, 904];
let offset = 0;
const batches = sizes.map(n => {
  const rows = Array.from({length: n}, (_, j) => offset + j);
  offset += n;
  const batch = a.tableFromArrays({
    x: Float64Array.from(rows, i => i * 0.5),
    y: Float64Array.from(rows, i => -i),
    r: Int32Array.from(rows, i => i % 256),
    g: Int32Array.from(rows, i => (i * 7) % 256),
    b: Int32Array.from(rows, i => 3),
    radius: Float64Array.from(rows, i => i + 0.25),
    id: BigInt64Array.from(rows, i => BigInt(i))
  });
  return batch;
});
const table = batches[0].concat(batches[1], batches[2]);
const n = table.numRows;
assert.equal(n, 5000);
assert.equal(table.getChild('x').data.length, 3, 'table must keep three record batches');

const probes = [0, 2047, 2048, 2500, 4095, 4096, n - 1];
function checkPositions(result, z, label) {
  assert(result.binary, label + ': expected binary attributes');
  assert.equal(result.binary.length, n, label + ': length');
  const position = result.binary.attributes.getPosition;
  assert.equal(position.size, 3, label + ': position size');
  assert(position.value instanceof Float32Array, label + ': position typed array');
  assert.equal(position.value.length, n * 3, label + ': position length');
  for (const i of probes) {
    assert.equal(position.value[i * 3], Math.fround(i * 0.5), label + ': x at ' + i);
    assert.equal(position.value[i * 3 + 1], Math.fround(-i), label + ': y at ' + i);
    assert.equal(position.value[i * 3 + 2], z, label + ': z at ' + i);
  }
}

// String accessors with whitespace variations, colour without alpha, radius column.
const full = convert(table, {
  '@@type': 'ScatterplotLayer', id: 'pts', data: {__arrow_url: 'x.arrows'},
  getPosition: '@@=[ x ,y]', getFillColor: '@@=[r, g, b]', getRadius: '@@=radius',
  radiusUnits: 'pixels', pickable: true
});
checkPositions(full, 0, 'strings');
const color = full.binary.attributes.getFillColor;
assert.equal(color.size, 4);
assert(color.value instanceof Uint8Array);
assert.equal(color.value.length, n * 4);
for (const i of probes) {
  assert.deepEqual(Array.from(color.value.subarray(i * 4, i * 4 + 4)), [i % 256, (i * 7) % 256, 3, 255], 'colour at ' + i);
}
const radius = full.binary.attributes.getRadius;
assert.equal(radius.size, 1);
assert(radius.value instanceof Float32Array);
assert.equal(radius.value.length, n);
for (const i of probes) assert.equal(radius.value[i], Math.fround(i + 0.25), 'radius at ' + i);
assert.equal(warnings.length, 0, 'no fallback warnings: ' + warnings.join(' | '));
console.log('PASS string accessors across ' + sizes.length + ' batches (' + n + ' rows)');

// Constant z as a literal third element; constant colour and radius stay plain props.
const constantZ = convert(table, {
  '@@type': 'ScatterplotLayer', id: 'z', getPosition: '@@=[x,y,7.5]',
  getFillColor: [255, 0, 0], getRadius: 4
});
checkPositions(constantZ, 7.5, 'constant z');
assert.deepEqual(Object.keys(constantZ.binary.attributes), ['getPosition']);
console.log('PASS constant z and constant colour/radius props');

// {fields: [...]} form, including four colour channels and a BigInt column.
const fields = convert(table, {
  '@@type': 'ScatterplotLayer', id: 'f', getPosition: {fields: ['x', 'y']},
  getFillColor: {fields: ['r', 'g', 'b', 'id']}, getRadius: {fields: ['radius']}
});
checkPositions(fields, 0, 'fields');
for (const i of probes) {
  assert.deepEqual(Array.from(fields.binary.attributes.getFillColor.value.subarray(i * 4, i * 4 + 4)),
    [i % 256, (i * 7) % 256, 3, i % 256], 'fields colour at ' + i);
  assert.equal(fields.binary.attributes.getRadius.value[i], Math.fround(i + 0.25));
}
assert.equal(warnings.length, 0);
console.log('PASS {fields: [...]} accessors');

// Any other row-referencing accessor forces row objects and one warning.
const fallback = convert(table, {
  '@@type': 'ScatterplotLayer', id: 'rows', getPosition: '@@=[x, y]', getLineColor: '@@=[r, g, b]'
});
assert(!fallback.binary && Array.isArray(fallback.rows));
assert.equal(fallback.rows.length, n);
// Spread into this realm: vm objects carry the sandbox's Object prototype.
assert.deepEqual({...fallback.rows[4096]}, {x: 2048, y: -4096, r: 0, g: (4096 * 7) % 256, b: 3, radius: 4096.25, id: 4096});
assert.equal(warnings.length, 1);
assert.match(warnings[0], /getLineColor/);
console.log('PASS row-object fallback names the accessor');

// Expressions and missing columns also fall back rather than rendering nothing.
for (const spec of [
  {id: 'expr', getPosition: '@@=[x * 2, y]'},
  {id: 'missing', getPosition: '@@=[x, nope]'},
  {id: 'none', getFillColor: '@@=[r, g, b]'}
]) {
  const result = convert(table, spec);
  assert(Array.isArray(result.rows) && result.rows.length === n, spec.id + ': rows fallback');
}
assert.equal(warnings.length, 4);
console.log('PASS unsupported accessors fall back to rows');

// A NULL in a bound column must not be read from the raw values buffer: the
// converter refuses binary binding and the row fallback keeps the null.
const nullable = a.tableFromArrays({
  x: Float64Array.from({length: 8}, (_, i) => i),
  y: Float64Array.from({length: 8}, (_, i) => -i),
  r: Int32Array.from({length: 8}, () => 10),
  g: Int32Array.from({length: 8}, () => 20),
  b: Int32Array.from({length: 8}, () => 30),
  radius: Float64Array.from({length: 8}, () => 1)
});
const withNull = a.tableFromIPC(a.tableToIPC(new a.Table({
  x: a.vectorFromArray([0, null, 2, 3, 4, 5, 6, 7], new a.Float64()),
  y: nullable.getChild('y'),
  r: nullable.getChild('r'),
  g: nullable.getChild('g'),
  b: nullable.getChild('b'),
  radius: nullable.getChild('radius')
}), 'stream'));
assert(Number(withNull.getChild('x').nullCount) > 0, 'fixture must carry a null');
const before = warnings.length;
const nullResult = convert(withNull, {
  '@@type': 'ScatterplotLayer', id: 'nulls',
  getPosition: '@@=[x, y]', getFillColor: '@@=[r, g, b]', getRadius: '@@=radius'
});
assert(!nullResult.binary, 'a null column must not be bound as binary attributes');
assert.equal(nullResult.rows.length, 8);
assert.equal(nullResult.rows[1].x, null, 'the row fallback keeps the null');
assert.equal(nullResult.rows[2].x, 2);
assert.equal(warnings.length, before + 1);
assert.match(warnings[before], /NULL/);
console.log('PASS NULL columns fall back to rows and stay null');

// DuckDB returns DECIMAL for ordinary aggregates over integers. Arrow stores
// those as multi-word objects, which deck.gl cannot use as a number. The
// converter must refuse to bind such a column, and the row fallback must still
// yield finite numbers rather than raw Arrow objects.
const decimalWords = new Uint32Array(4 * 4);
for (let i = 0; i < 4; i++) decimalWords[i * 4] = (i + 1) * 15;
const decimalVector = new a.Vector([a.makeData({
  type: new a.Decimal(1, 18, 128), data: decimalWords, length: 4, nullCount: 0
})]);
const decimalTable = new a.Table({
  x: a.vectorFromArray(Float64Array.from([0, 1, 2, 3])),
  y: a.vectorFromArray(Float64Array.from([0, -1, -2, -3])),
  r: a.vectorFromArray(Int32Array.from([10, 10, 10, 10])),
  g: a.vectorFromArray(Int32Array.from([20, 20, 20, 20])),
  b: a.vectorFromArray(Int32Array.from([30, 30, 30, 30])),
  radius: decimalVector
});
assert.match(String(decimalTable.getChild('radius').type), /Decimal/, 'fixture must carry a DECIMAL column');
assert.equal(typeof decimalTable.getChild('radius').get(0), 'object', 'DECIMAL values are Arrow objects');
const beforeDecimal = warnings.length;
const decimalResult = convert(decimalTable, {
  '@@type': 'ScatterplotLayer', id: 'decimals',
  getPosition: '@@=[x, y]', getFillColor: '@@=[r, g, b]', getRadius: '@@=radius'
});
assert(!decimalResult.binary, 'a DECIMAL column must not be bound as a binary attribute');
assert.equal(decimalResult.rows.length, 4);
for (const row of decimalResult.rows) {
  assert(row.radius === null || Number.isFinite(row.radius),
    'radius must be a finite number or null, got ' + String(row.radius) + ' (' + typeof row.radius + ')');
  assert.notEqual(typeof row.radius, 'object', 'radius must not be a raw Arrow object');
}
assert(warnings.length > beforeDecimal, 'the DECIMAL column must produce a warning');
console.log('PASS DECIMAL columns reach the layer as numbers, never Arrow objects');
