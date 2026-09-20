// End to end: R exports 100,000 DuckDB points with file transport, the saved
// widget is served over HTTP, and headless Chromium must bind the table as
// binary attributes and draw it. Set RDECKGL_LIB to reuse an installed
// library, PLAYWRIGHT_DIR to point at a directory whose node_modules has
// playwright, and RSCRIPT to override the R binary.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const http = require('node:http');
const os = require('node:os');
const path = require('node:path');
const {spawnSync} = require('node:child_process');
const root = path.resolve(__dirname, '../..');

function resolvePlaywright() {
  const paths = [process.env.PLAYWRIGHT_DIR, root,
    path.resolve(root, '../../DBviz/geometry-benchmark-repro')].filter(Boolean);
  try {
    return require(require.resolve('playwright', {paths}));
  } catch (err) {
    return null;
  }
}

const playwright = resolvePlaywright();
if (!playwright) {
  console.log('SKIP scatter-binary-browser: Playwright cannot be resolved; set PLAYWRIGHT_DIR to a directory whose node_modules contains it');
  process.exit(0);
}

const rscript = process.env.RSCRIPT || '/usr/local/bin/Rscript';
const rBinary = path.join(path.dirname(rscript), 'R');
const work = fs.mkdtempSync(path.join(os.tmpdir(), 'rdeckgl-browser-'));
const exportDir = path.join(work, 'export');
const rows = 100000;
const probes = [0, 50000, rows - 1];

function run(command, args, options = {}) {
  const result = spawnSync(command, args, {encoding: 'utf8', ...options});
  if (result.status !== 0) {
    throw new Error(`${command} ${args.join(' ')} failed:\n${result.stdout}\n${result.stderr}`);
  }
  return result;
}

let lib = process.env.RDECKGL_LIB;
if (!lib) {
  lib = path.join(work, 'lib');
  fs.mkdirSync(lib);
  console.log('Installing rDeckgl into ' + lib);
  run(rBinary, ['CMD', 'INSTALL', '--no-docs', '--no-multiarch', '--library=' + lib, root]);
}

const script = `
.libPaths(c(${JSON.stringify(lib)}, .libPaths()))
library(rDeckgl)
export_dir <- ${JSON.stringify(exportDir)}
dir.create(export_dir)
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = file.path(${JSON.stringify(work)}, "points.duckdb"))
DBI::dbExecute(con, paste(
  "CREATE TABLE points AS SELECT i AS id,",
  "((i * 7919) % 1000)::DOUBLE + 0.125 AS x, ((i * 104729) % 1000)::DOUBLE + 0.375 AS y,",
  "((i * 37) % 256)::INTEGER AS r, ((i * 91) % 256)::INTEGER AS g, ((i * 53) % 256)::INTEGER AS b,",
  "(2 + i % 3)::DOUBLE AS radius FROM range(${rows}) t(i)"))
spec <- list(
  views = list(list(\`@@type\` = "OrthographicView", controller = TRUE)),
  initialViewState = list(target = c(500, 500, 0), zoom = -1),
  layers = list(list(
    \`@@type\` = "ScatterplotLayer", id = "points",
    data = list(type = "duckdb", format = "arrow",
                query = "SELECT id, x, y, r, g, b, radius FROM points ORDER BY id"),
    getPosition = "@@=[x, y]", getFillColor = "@@=[r, g, b]",
    getRadius = "@@=radius", radiusUnits = "pixels")))
w <- deckgl(spec, con = con, data_transport = "file", data_dir = export_dir)
htmlwidgets::saveWidget(w, file.path(export_dir, "index.html"), selfcontained = FALSE)
node <- w$x$spec$layers[[1]]$data
expected <- DBI::dbGetQuery(con, sprintf(
  "SELECT id, x, y, r, g, b, radius FROM points WHERE id IN (%s) ORDER BY id",
  paste(c(${probes.join(', ')}), collapse = ", ")))
jsonlite::write_json(list(node = node, expected = expected),
  file.path(export_dir, "expected.json"), auto_unbox = TRUE, digits = NA)
DBI::dbDisconnect(con, shutdown = TRUE)
`;
fs.writeFileSync(path.join(work, 'export.R'), script);
console.log('Exporting ' + rows + ' points with R');
run(rscript, [path.join(work, 'export.R')]);

const expected = JSON.parse(fs.readFileSync(path.join(exportDir, 'expected.json'), 'utf8'));
const dataFile = expected.node.__arrow_url || expected.node.__parquet_url;
assert(dataFile, 'data node must reference an exported file');
assert(['copy_arrows', 'record_batch_stream', 'copy_parquet'].includes(expected.node.__export_method));
assert(fs.existsSync(path.join(exportDir, dataFile)), 'exported data file must sit beside index.html');
console.log('Export method ' + expected.node.__export_method + ' wrote ' + dataFile +
  ' (' + fs.statSync(path.join(exportDir, dataFile)).size + ' bytes)');

const mime = {'.html': 'text/html', '.js': 'text/javascript', '.css': 'text/css',
  '.wasm': 'application/wasm', '.json': 'application/json',
  '.arrows': 'application/vnd.apache.arrow.stream', '.parquet': 'application/octet-stream'};
const server = http.createServer((req, res) => {
  const file = path.resolve(exportDir, '.' + decodeURIComponent(new URL(req.url, 'http://localhost').pathname));
  if (!file.startsWith(exportDir + path.sep) || !fs.existsSync(file) || fs.statSync(file).isDirectory()) {
    res.writeHead(404);
    res.end();
    return;
  }
  res.setHeader('Content-Type', mime[path.extname(file)] || 'application/octet-stream');
  res.end(fs.readFileSync(file));
});

async function main() {
  await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
  const browser = await playwright.chromium.launch({
    headless: true,
    args: ['--use-gl=angle', '--use-angle=swiftshader', '--enable-unsafe-swiftshader', '--ignore-gpu-blocklist']
  });
  try {
    const page = await browser.newPage({viewport: {width: 1000, height: 800}});
    const errors = [];
    page.on('pageerror', e => errors.push(String(e)));
    page.on('requestfailed', r => errors.push(r.url() + ': ' + (r.failure() || {}).errorText));
    page.on('console', m => { if (m.type() === 'error') errors.push(m.text()); });
    await page.goto(`http://127.0.0.1:${server.address().port}/index.html`, {waitUntil: 'load', timeout: 120000});
    await page.waitForFunction(n => {
      const el = [...document.querySelectorAll('div')].find(d => d.__deckInstance);
      const layer = el && el.__deckInstance.props.layers[0];
      return Boolean(layer && layer.isLoaded && layer.getNumInstances() === n);
    }, rows, {timeout: 120000});
    // Points are drawn after the attributes upload; poll the drawing buffer,
    // which luma.gl keeps (preserveDrawingBuffer) between frames.
    await page.waitForFunction(() => {
      const el = [...document.querySelectorAll('div')].find(d => d.__deckInstance);
      const gl = el.querySelector('canvas').getContext('webgl2');
      const pixels = new Uint8Array(gl.drawingBufferWidth * gl.drawingBufferHeight * 4);
      gl.readPixels(0, 0, gl.drawingBufferWidth, gl.drawingBufferHeight, gl.RGBA, gl.UNSIGNED_BYTE, pixels);
      for (let i = 3; i < pixels.length; i += 4) if (pixels[i] === 255) return true;
      return false;
    }, null, {timeout: 60000, polling: 250});
    const state = await page.evaluate(probes => {
      const el = [...document.querySelectorAll('div')].find(d => d.__deckInstance);
      const layer = el.__deckInstance.props.layers[0];
      const data = layer.props.data;
      const positions = data.attributes.getPosition.value;
      const gl = el.querySelector('canvas').getContext('webgl2');
      const pixels = new Uint8Array(gl.drawingBufferWidth * gl.drawingBufferHeight * 4);
      gl.readPixels(0, 0, gl.drawingBufferWidth, gl.drawingBufferHeight, gl.RGBA, gl.UNSIGNED_BYTE, pixels);
      let opaque = 0;
      for (let i = 3; i < pixels.length; i += 4) if (pixels[i] === 255) opaque++;
      return {
        instances: layer.getNumInstances(), dataLength: data.length,
        attributes: Object.keys(data.attributes), positionLength: positions.length,
        typedArray: positions.constructor.name,
        sampled: probes.map(i => [positions[i * 3], positions[i * 3 + 1], positions[i * 3 + 2]]),
        color: probes.map(i => Array.from(data.attributes.getFillColor.value.subarray(i * 4, i * 4 + 4))),
        radius: probes.map(i => data.attributes.getRadius.value[i]),
        opaque, total: pixels.length / 4, layerType: layer.constructor.layerName
      };
    }, probes);
    assert.equal(state.layerType, 'ScatterplotLayer');
    assert.equal(state.instances, rows, 'getNumInstances');
    assert.equal(state.dataLength, rows, 'props.data.length');
    assert.deepEqual(state.attributes, ['getPosition', 'getFillColor', 'getRadius']);
    assert.equal(state.typedArray, 'Float32Array');
    assert.equal(state.positionLength, rows * 3);
    expected.expected.forEach((row, k) => {
      assert.equal(row.id, probes[k]);
      assert(state.sampled[k].every(Number.isFinite), 'finite position at ' + row.id);
      assert.deepEqual(state.sampled[k], [Math.fround(row.x), Math.fround(row.y), 0], 'position of row ' + row.id);
      assert.deepEqual(state.color[k], [row.r, row.g, row.b, 255], 'colour of row ' + row.id);
      assert.equal(state.radius[k], Math.fround(row.radius), 'radius of row ' + row.id);
    });
    assert(state.opaque > 0, 'canvas must contain opaque pixels');
    assert.deepEqual(errors, [], 'browser errors');
    console.log('PASS ' + rows + ' points bound as binary attributes via ' + expected.node.__export_method +
      '; ' + state.opaque + ' of ' + state.total + ' pixels opaque; browser ' + browser.version());
  } finally {
    await browser.close();
    server.close();
    if (!process.env.RDECKGL_KEEP) fs.rmSync(work, {recursive: true, force: true});
  }
}

main().catch(err => {
  console.error(err);
  process.exit(1);
});
