// The RStudio Viewer / saveWidget scenario: the DuckDB export is written into
// a data_dir that is NOT the directory the page is saved into, and that
// directory is deleted before the page is served. The widget must still find
// its data through the html dependency attachment. Set RDECKGL_LIB to reuse an
// installed library, PLAYWRIGHT_DIR to point at a directory whose node_modules
// has playwright, and RSCRIPT to override the R binary.
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
  console.log('SKIP attachment-transport-browser: Playwright cannot be resolved; set PLAYWRIGHT_DIR');
  process.exit(0);
}

const rscript = process.env.RSCRIPT || '/usr/local/bin/Rscript';
const rBinary = path.join(path.dirname(rscript), 'R');
const work = fs.mkdtempSync(path.join(os.tmpdir(), 'rdeckgl-attachment-'));
const rows = 20000;

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

// Two pages: one saved with saveWidget(selfcontained = FALSE), one printed the
// way the RStudio Viewer prints a widget (htmltools::save_html + libdir).
const script = `
.libPaths(c(${JSON.stringify(lib)}, .libPaths()))
library(rDeckgl)
work <- ${JSON.stringify(work)}
data_dir <- file.path(work, "exported-data")
dir.create(data_dir)
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = file.path(work, "points.duckdb"))
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
w <- deckgl(spec, con = con, data_transport = "file", data_dir = data_dir)
saved <- file.path(work, "saved"); dir.create(saved)
htmlwidgets::saveWidget(w, file.path(saved, "index.html"), selfcontained = FALSE)
viewer <- file.path(work, "viewer"); dir.create(viewer)
htmltools::save_html(htmltools::as.tags(w), file = file.path(viewer, "index.html"), libdir = "lib")
node <- w$x$spec$layers[[1]]$data
jsonlite::write_json(list(node = node), file.path(work, "expected.json"), auto_unbox = TRUE, digits = NA)
DBI::dbDisconnect(con, shutdown = TRUE)
`;
fs.writeFileSync(path.join(work, 'export.R'), script);
console.log('Exporting ' + rows + ' points with R');
run(rscript, [path.join(work, 'export.R')]);

const expected = JSON.parse(fs.readFileSync(path.join(work, 'expected.json'), 'utf8'));
const dataFile = expected.node.__arrow_url || expected.node.__parquet_url;
assert(dataFile, 'data node must reference an exported file');

// The page directories must already hold the data file inside their dependency
// directory, and nothing beside the HTML.
for (const dir of ['saved', 'viewer']) {
  const base = path.join(work, dir);
  assert(!fs.existsSync(path.join(base, dataFile)), dir + ': data file must not sit beside index.html');
  const found = run('find', [base, '-name', dataFile]).stdout.trim().split('\n').filter(Boolean);
  assert.equal(found.length, 1, dir + ': exactly one copy of the data file in the dependency directory');
  console.log(dir + ': ' + path.relative(base, found[0]));
}
// Delete data_dir: the pages must not depend on it any more.
fs.rmSync(path.join(work, 'exported-data'), {recursive: true, force: true});

const mime = {'.html': 'text/html', '.js': 'text/javascript', '.css': 'text/css',
  '.wasm': 'application/wasm', '.json': 'application/json',
  '.arrows': 'application/vnd.apache.arrow.stream', '.parquet': 'application/octet-stream'};

function serve(dirName) {
  const base = path.join(work, dirName);
  return http.createServer((req, res) => {
    const file = path.resolve(base, '.' + decodeURIComponent(new URL(req.url, 'http://localhost').pathname));
    if (!file.startsWith(base + path.sep) || !fs.existsSync(file) || fs.statSync(file).isDirectory()) {
      res.writeHead(404);
      res.end();
      return;
    }
    res.setHeader('Content-Type', mime[path.extname(file)] || 'application/octet-stream');
    res.end(fs.readFileSync(file));
  });
}

async function check(browser, dirName) {
  const server = serve(dirName);
  await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
  try {
    const page = await browser.newPage({viewport: {width: 800, height: 600}});
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
    const state = await page.evaluate(() => {
      const el = [...document.querySelectorAll('div')].find(d => d.__deckInstance);
      const layer = el.__deckInstance.props.layers[0];
      const data = layer.props.data;
      return {
        instances: layer.getNumInstances(),
        binary: Boolean(data && data.attributes),
        attachments: [...document.querySelectorAll('link[rel="attachment"]')].map(l => l.getAttribute('href'))
      };
    });
    assert.equal(state.instances, rows, dirName + ': rendered rows');
    assert(state.binary, dirName + ': rows must be bound as binary attributes');
    // The page also carries the parquet-wasm attachments of the deck.gl bundle.
    const dataLinks = state.attachments.filter(href => href.endsWith('/' + dataFile));
    assert.equal(dataLinks.length, 1, dirName + ': one attachment link for the data file');
    assert(dataLinks[0].includes('deckgl-data-'), dirName + ': attachment sits in the widget data dependency');
    assert.deepEqual(errors, [], dirName + ': browser errors');
    console.log('PASS ' + dirName + ': ' + rows + ' rows via ' + dataLinks[0]);
    await page.close();
  } finally {
    server.close();
  }
}

async function main() {
  const browser = await playwright.chromium.launch({
    headless: true,
    args: ['--use-gl=angle', '--use-angle=swiftshader', '--enable-unsafe-swiftshader', '--ignore-gpu-blocklist']
  });
  try {
    await check(browser, 'saved');
    await check(browser, 'viewer');
  } finally {
    await browser.close();
    if (!process.env.RDECKGL_KEEP) fs.rmSync(work, {recursive: true, force: true});
  }
}

main().catch(err => {
  console.error(err);
  process.exit(1);
});
