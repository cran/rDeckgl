// deckgl.js - Deck.gl htmlwidget for R

// Polyfill for Node.js 'process' global (required by some bundled dependencies)
if (typeof process === 'undefined') {
  window.process = {
    env: {},
    version: '',
    versions: {},
    browser: true
  };
}

// Helper function to decode base64 to Uint8Array
function base64ToUint8Array(base64) {
  try {
    const binary_string = atob(base64);
    const len = binary_string.length;
    const bytes = new Uint8Array(len);
    for (let i = 0; i < len; i++) {
      bytes[i] = binary_string.charCodeAt(i);
    }
    return bytes;
  } catch (e) {
    console.error("[deckgl] Failed to decode base64 string:", base64.substring(0, 100) + "...", e);
    throw e;
  }
}

const deckGlModule = { promise: null };
let parquetWasmModule = null;

// Load parquet-wasm from local files or CDN (fallback list) for Parquet decoding in the browser
async function loadParquetWasm(baseUrl) {
  if (parquetWasmModule) return parquetWasmModule;

  // Construct local path based on htmlwidgets dependency structure
  // baseUrl would be something like "deckgl_files/parquet-wasm-0.6.0" in rendered HTML
  const localPaths = [];

  if (baseUrl) {
    localPaths.push(`${baseUrl}/parquet_wasm.js`);
  }

  // Try various possible local paths
  localPaths.push(
    "./parquet-wasm-0.6.0/parquet_wasm.js",
    "parquet-wasm-0.6.0/parquet_wasm.js",
    "./lib/parquet-wasm/parquet_wasm.js",
    "lib/parquet-wasm/parquet_wasm.js"
  );

  const candidates = [
    ...localPaths,
    // CDN fallbacks
    "https://cdn.jsdelivr.net/npm/parquet-wasm@0.6.0/esm/parquet_wasm.js",
    "https://cdn.jsdelivr.net/npm/parquet-wasm@0.6.1/esm/parquet_wasm.js",
    "https://unpkg.com/parquet-wasm@0.6.0/esm/parquet_wasm.js",
    "https://unpkg.com/parquet-wasm@0.6.1/esm/parquet_wasm.js"
  ];

  let lastError = null;
  for (const url of candidates) {
    try {
      console.log(`[deckgl] Attempting to load parquet-wasm from: ${url}`);
      const mod = await import(url);
      const hasReadParquet = typeof mod.readParquet === "function";
      if (!hasReadParquet) {
        console.warn(`[deckgl] parquet-wasm from ${url} missing readParquet; trying next candidate`, Object.keys(mod));
        continue;
      }
      parquetWasmModule = mod;
      console.log(`[deckgl] ✅ Loaded parquet-wasm from ${url}`);
      return parquetWasmModule;
    } catch (err) {
      console.warn(`[deckgl] Failed to load parquet-wasm from ${url}:`, err.message);
      lastError = err;
    }
  }

  throw lastError || new Error("Failed to load parquet-wasm from any source");
}

// Helper to wait for React to load
async function waitForReact(maxRetries = 100, delayMs = 50) {
  for (let i = 0; i < maxRetries; i++) {
    if (window.React && typeof window.React.useLayoutEffect === 'function') {
      if (i > 0) {
        console.log('[deckgl] React loaded after', i * delayMs, 'ms');
      }
      return true;
    }
    await new Promise(resolve => setTimeout(resolve, delayMs));
  }
  console.error('[deckgl] React failed to load after', maxRetries * delayMs, 'ms');
  return false;
}

// Helper to wait for bundle to load with retries
async function waitForBundle(maxRetries = 50, delayMs = 100) {
  // First wait for React (required by bundle)
  const reactLoaded = await waitForReact();
  if (!reactLoaded) {
    console.error('[deckgl] Cannot proceed without React');
    return false;
  }
  
  // Then wait for bundle to fully initialize
  for (let i = 0; i < maxRetries; i++) {
    // Check if bundle has fully loaded (deck.Deck should exist)
    const hasDeck = window.deck && window.deck.Deck;
    
    if (hasDeck) {
      console.log('[deckgl] Bundle loaded successfully after', i * delayMs, 'ms');
      return true;
    }
    if (i === 0) {
      console.log('[deckgl] Waiting for deckgl-bundle to initialize...');
    }
    await new Promise(resolve => setTimeout(resolve, delayMs));
  }
  console.error('[deckgl] Bundle failed to load after', maxRetries * delayMs, 'ms');
  console.error('[deckgl] React available:', !!window.React);
  console.error('[deckgl] window.deck:', window.deck);
  console.error('[deckgl] window.deck.Deck:', window.deck && window.deck.Deck);
  console.error('[deckgl] window.loaders:', window.loaders);
  console.error('[deckgl] window.arrow:', window.arrow);
  console.error('[deckgl] window.geoarrowDeck:', window.geoarrowDeck);
  return false;
}

async function ensureDeckGlModules() {
  if (!deckGlModule.promise) {
    deckGlModule.promise = (async () => {
      // Wait for bundle to load
      const bundleLoaded = await waitForBundle();
      if (!bundleLoaded) {
        throw new Error('Deck.gl libraries are not loaded. Ensure deckgl-bundle dependency is registered.');
      }

      // Setup maplibre as mapboxgl compatibility (deck.gl expects window.mapboxgl)
      if (!window.mapboxgl && window.maplibregl) {
        window.mapboxgl = window.maplibregl;
        console.log('[deckgl] Set up maplibregl as mapboxgl for deck.gl compatibility');
      }

      // Setup window.deck from DeckGLBundle export
      if (!window.deck && window.DeckGLBundle) {
        window.deck = window.DeckGLBundle;
        console.log('[deckgl] Set up window.deck from DeckGLBundle');
      }

      const deck = window.deck;
      if (!deck || !deck.Deck) {
        throw new Error('Deck.gl libraries are not loaded. Ensure deckgl-bundle dependency is registered.');
      }

      const JSONConverter = deck.JSONConverter || (window.deck__json && window.deck__json.JSONConverter);
      const JSONConfiguration = deck.JSONConfiguration || (window.deck__json && window.deck__json.JSONConfiguration);

      if (!JSONConverter || !JSONConfiguration) {
        throw new Error('Deck.gl JSONConverter is unavailable. Ensure @deck.gl/json is accessible via deckgl-bundle.');
      }

      const loadersGlobal = window.loaders;
      const csvLoader = window.CSVLoader || (loadersGlobal && loadersGlobal.CSVLoader);
      if (loadersGlobal && typeof loadersGlobal.registerLoaders === 'function' && csvLoader) {
        try {
          loadersGlobal.registerLoaders(csvLoader);
        } catch (err) {
          console.warn('[deckgl] Failed to register CSVLoader:', err);
        }
      }

      // GeoArrow and Arrow support
      const geoarrowDeck = window.geoarrowDeck;
      const arrow = window.arrow;
      const ArrowLoader = loadersGlobal?.ArrowLoader;

      if (!geoarrowDeck) {
        console.warn('[deckgl] @geoarrow/deck.gl-layers not available. GeoArrow layers will not work.');
      }
      if (!ArrowLoader) {
        console.warn('[deckgl] ArrowLoader not available. Arrow data format will not work.');
      }

      return { 
        deck, 
        JSONConverter, 
        JSONConfiguration,
        geoarrowDeck,
        arrow,
        ArrowLoader
      };
    })();
  }
  return deckGlModule.promise;
}

function collectDeckGlTokens(spec) {
  const classNames = new Set();
  const enumNames = new Set();

  const walk = (node) => {
    if (node === null || node === undefined) return;
    if (Array.isArray(node)) {
      node.forEach(walk);
      return;
    }
    if (typeof node === 'object') {
      if (typeof node['@@type'] === 'string') {
        classNames.add(node['@@type']);
      }
      Object.keys(node).forEach((key) => walk(node[key]));
      return;
    }
    if (typeof node === 'string' && node.startsWith('@@#')) {
      const enumToken = node.slice(3);
      const enumName = enumToken.split('.')[0];
      if (enumName) enumNames.add(enumName);
    }
  };

  walk(spec);
  return {
    classNames: Array.from(classNames),
    enumNames: Array.from(enumNames)
  };
}

function resolveDeckExport(deck, name, geoarrowDeck = null) {
  // First check GeoArrow layers (for GeoArrowPolygonLayer, GeoArrowSolidPolygonLayer, etc.)
  if (geoarrowDeck && geoarrowDeck[name]) {
    return geoarrowDeck[name];
  }
  
  // Then check standard deck.gl exports
  if (!deck || typeof name !== 'string') return null;
  
  // Special case: DeckGL in spec should use deck.Deck (pure JS class), not deck.DeckGL (React component)
  if (name === 'DeckGL' && deck.Deck) return deck.Deck;
  
  if (deck[name]) return deck[name];
  for (const key of Object.keys(deck)) {
    const candidate = deck[key];
    if (candidate && typeof candidate === 'object' && candidate[name]) {
      return candidate[name];
    }
  }
  return null;
}

async function renderDeckGlView(el, payload) {
  const { deck, JSONConverter, JSONConfiguration, geoarrowDeck, arrow, ArrowLoader } = await ensureDeckGlModules();
  const spec = payload.spec || {};
  const userProvidedViewState = Boolean(spec.viewState);
  const { classNames, enumNames } = collectDeckGlTokens(spec);
  console.log('[deckgl] Collected class names from spec:', Array.from(classNames));
  console.log('[deckgl] Collected enum names from spec:', Array.from(enumNames));

  // Helper: Check if a layer type is a GeoArrow layer
  function isGeoArrowLayerType(typeName) {
    return typeName && typeName.startsWith('GeoArrow');
  }

  // Helper: Parse Arrow IPC from base64
  async function parseArrowIPC(base64Data) {
    if (!base64Data || base64Data === '') return null;
    if (!arrow) {
      console.warn('[deckgl] Apache Arrow not available');
      return null;
    }
    try {
      const bytes = base64ToUint8Array(base64Data);
      // Use apache-arrow directly for proper RecordBatch parsing
      const table = arrow.tableFromIPC(bytes);
      console.log('[deckgl] Parsed Arrow table:', table.numRows, 'rows,', table.numCols, 'columns');
      // Detailed schema inspection for debugging GeoArrow
      for (const field of table.schema.fields) {
        const typeStr = field.type ? field.type.toString() : 'unknown';
        const typeId = field.type ? field.type.typeId : 'unknown';
        let metaObj = null;
        let extName = null;
        let extMeta = null;
        
        if (field.metadata && typeof field.metadata.get === 'function') {
          try {
            metaObj = Object.fromEntries(field.metadata);
            extName = field.metadata.get('ARROW:extension:name');
            extMeta = field.metadata.get('ARROW:extension:metadata');
          } catch (e) {
            console.log(`[deckgl] Error reading metadata for ${field.name}:`, e);
          }
        }
        
        console.log(`[deckgl] Field "${field.name}": type=${typeStr}, typeId=${typeId}, metadata=`, metaObj);
        
        if (extName) {
          console.log(`[deckgl]   -> GeoArrow extension: ${extName}`);
        } else {
          console.log(`[deckgl]   -> No GeoArrow extension metadata found`);
        }
        if (extMeta) {
          console.log(`[deckgl]   -> Extension metadata: ${extMeta}`);
        }
      }
      return table;
    } catch (err) {
      console.error('[deckgl] Failed to parse Arrow IPC:', err);
      return null;
    }
  }

  // Helper: Fetch Parquet from URL and parse to Arrow table
  async function parseParquetFromUrl(url) {
    if (!url || url === '') return null;
    if (!arrow) {
      console.warn('[deckgl] Apache Arrow not available');
      return null;
    }
    try {
      console.log('[deckgl] 📥 Fetching Parquet from URL:', url);

      const response = await fetch(url);
      if (!response.ok) {
        console.error('[deckgl] ❌ Failed to fetch Parquet:', response.status, response.statusText);
        return null;
      }

      const arrayBuffer = await response.arrayBuffer();
      const bytes = new Uint8Array(arrayBuffer);
      console.log('[deckgl] ✅ Fetched', bytes.byteLength, 'bytes');

      const parquetModule = await loadParquetWasm();
      if (!parquetModule) {
        console.error('[deckgl] ❌ parquet-wasm module not loaded');
        return null;
      }
      console.log('[deckgl] ✅ parquet-wasm module loaded');

      // Initialize WASM module
      if (typeof parquetModule.default === 'function') {
        console.log('[deckgl] Initializing WASM...');
        await parquetModule.default();
        console.log('[deckgl] ✅ parquet-wasm WASM initialized');
      }

      const readParquet = parquetModule.readParquet;
      if (!readParquet) {
        console.error('[deckgl] ❌ parquet-wasm readParquet function not available');
        return null;
      }
      console.log('[deckgl] ✅ readParquet function available');

      console.log('[deckgl] Reading Parquet with WASM...');
      const wasmTable = readParquet(bytes);
      console.log('[deckgl] ✅ WASM table created');

      let ipc = null;
      if (wasmTable && typeof wasmTable.intoIPCStream === 'function') {
        console.log('[deckgl] Converting WASM table to IPC stream...');
        ipc = wasmTable.intoIPCStream();
        console.log('[deckgl] ✅ IPC stream created, length:', ipc ? ipc.byteLength : 0);
        if (typeof wasmTable.drop === 'function') {
          wasmTable.drop();
          console.log('[deckgl] ✅ WASM memory freed');
        }
      } else {
        ipc = wasmTable;
        console.warn('[deckgl] ⚠️  WASM table missing intoIPCStream method, using directly');
      }

      console.log('[deckgl] Parsing IPC stream to Arrow table...');
      const table = arrow.tableFromIPC(ipc);
      console.log('[deckgl] ✅ Arrow table created:', table.numRows, 'rows,', table.numCols, 'columns');
      console.log('[deckgl] Schema fields:', table.schema.fields.map(f => f.name).join(', '));
      return table;
    } catch (err) {
      console.error('[deckgl] ❌ Failed to parse Parquet from URL:', err);
      console.error('[deckgl] Error stack:', err.stack);
      return null;
    }
  }

  // Helper: Fetch Arrow IPC from URL and parse to Arrow table
  async function parseArrowFromUrl(url) {
    if (!url || url === '') return null;
    if (!arrow) {
      console.warn('[deckgl] Apache Arrow not available');
      return null;
    }
    try {
      console.log('[deckgl] 📥 Fetching Arrow IPC from URL:', url);
      const response = await fetch(url);
      if (!response.ok) {
        console.error('[deckgl] ❌ Failed to fetch Arrow IPC:', response.status, response.statusText);
        return null;
      }
      const arrayBuffer = await response.arrayBuffer();
      const bytes = new Uint8Array(arrayBuffer);
      console.log('[deckgl] ✅ Fetched', bytes.byteLength, 'bytes of Arrow IPC');
      const table = arrow.tableFromIPC(bytes);
      console.log('[deckgl] ✅ Parsed Arrow table:', table.numRows, 'rows,', table.numCols, 'columns');
      return table;
    } catch (err) {
      console.error('[deckgl] ❌ Failed to parse Arrow from URL:', err);
      return null;
    }
  }

  // Helper: Parse Parquet (base64) using parquet-wasm, returning an Arrow table
  async function parseParquetBase64(base64Data) {
    if (!base64Data || base64Data === '') return null;
    if (!arrow) {
      console.warn('[deckgl] Apache Arrow not available');
      return null;
    }
    try {
      console.log('[deckgl] Starting Parquet decode...');
      console.log('[deckgl] Base64 data length:', base64Data ? base64Data.length : 0, 'characters');

      const parquetModule = await loadParquetWasm();
      if (!parquetModule) {
        console.error('[deckgl] ❌ parquet-wasm module not loaded');
        return null;
      }
      console.log('[deckgl] ✅ parquet-wasm module loaded');

      // Initialize WASM module - the default export is the init function
      // After calling it, the functions become available on the MODULE (not on the return value)
      if (typeof parquetModule.default === 'function') {
        console.log('[deckgl] Initializing WASM...');
        await parquetModule.default();
        console.log('[deckgl] ✅ parquet-wasm WASM initialized');
      }

      // readParquet is on the module itself
      const readParquet = parquetModule.readParquet;
      if (!readParquet) {
        console.error('[deckgl] ❌ parquet-wasm readParquet function not available');
        console.log('[deckgl] Available functions:', Object.keys(parquetModule));
        return null;
      }
      console.log('[deckgl] ✅ readParquet function available');

      console.log('[deckgl] Decoding base64...');
      const bytes = base64ToUint8Array(base64Data);
      console.log('[deckgl] ✅ Decoded to', bytes.byteLength, 'bytes');

      console.log('[deckgl] Reading Parquet with WASM...');
      const wasmTable = readParquet(bytes);
      console.log('[deckgl] ✅ WASM table created');

      let ipc = null;
      if (wasmTable && typeof wasmTable.intoIPCStream === 'function') {
        console.log('[deckgl] Converting WASM table to IPC stream...');
        ipc = wasmTable.intoIPCStream();
        console.log('[deckgl] ✅ IPC stream created, length:', ipc ? ipc.byteLength : 0);
        if (typeof wasmTable.drop === 'function') {
          wasmTable.drop();
          console.log('[deckgl] ✅ WASM memory freed');
        }
      } else {
        ipc = wasmTable;
        console.warn('[deckgl] ⚠️  WASM table missing intoIPCStream method, using directly');
      }

      console.log('[deckgl] Parsing IPC stream to Arrow table...');
      const table = arrow.tableFromIPC(ipc);
      console.log('[deckgl] ✅ Arrow table created:', table.numRows, 'rows,', table.numCols, 'columns');
      console.log('[deckgl] Schema fields:', table.schema.fields.map(f => f.name).join(', '));
      return table;
    } catch (err) {
      console.error('[deckgl] ❌ Failed to parse Parquet:', err);
      console.error('[deckgl] Error stack:', err.stack);
      return null;
    }
  }

  // Check if an Arrow field has proper GeoArrow polygon extension metadata
  function hasGeoArrowPolygonMetadata(field) {
    if (!field || !field.metadata) return false;
    const extName = field.metadata.get('ARROW:extension:name');
    return extName === 'geoarrow.polygon' || extName === 'geoarrow.multipolygon';
  }

  // Detect polygon-like field structure
  function isPolygonLikeField(field) {
    if (!field || !field.type) return false;
    const t = field.type;
    // List<List<Struct<{x,y}>>> pattern
    if (t.typeId === 12 && t.children && t.children.length === 1) {
      const inner = t.children[0];
      if (inner.type && inner.type.typeId === 12 && inner.type.children && inner.type.children.length === 1) {
        const coord = inner.type.children[0];
        if (coord.type && coord.type.typeId === 13) { // Struct
          return true;
        }
      }
    }
    return false;
  }

  // Detect WKB by inspecting binary data content
  // WKB format: byte 0 = byte order (0x00 big-endian, 0x01 little-endian)
  // bytes 1-4 = geometry type (3 = Polygon, 6 = MultiPolygon)
  function detectWKBFromBinaryColumn(column) {
    try {
      // Check first few values to detect WKB pattern
      const samplesToCheck = Math.min(5, column.length);
      let wkbCount = 0;

      for (let i = 0; i < samplesToCheck; i++) {
        const data = column.get(i);
        if (!data || data.byteLength < 5) continue;

        const view = new DataView(data.buffer, data.byteOffset, data.byteLength);
        const byteOrder = view.getUint8(0);

        // Byte order must be 0 (big-endian) or 1 (little-endian)
        if (byteOrder !== 0 && byteOrder !== 1) continue;

        const littleEndian = byteOrder === 1;
        const geomType = view.getUint32(1, littleEndian);

        // Check for polygon-like types: 3 = Polygon, 6 = MultiPolygon
        // Also check with SRID flag (0x20000000) masked off
        const baseType = geomType & 0xFF;
        if (baseType === 3 || baseType === 6) {
          wkbCount++;
        }
      }

      // If majority of samples look like WKB, consider it WKB
      return wkbCount > 0 && wkbCount >= samplesToCheck / 2;
    } catch (e) {
      console.warn('[deckgl] Error detecting WKB:', e);
      return false;
    }
  }

  // WKB to Arrow Polygon Vector Converter
  // Parses WKB binary data directly and constructs native Arrow polygon vectors
  // Based on rDeckgl-geoarrow-deckgl implementation
  function wkbToPolygonVector(binaryVector) {
    try {
      const Arrow = window.arrow;
      if (!Arrow) {
        console.error('[deckgl] Apache Arrow library not available');
        return null;
      }

      console.log(`[deckgl] Converting ${binaryVector.length} WKB geometries to native GeoArrow polygon format`);

      let totalPolys = binaryVector.length;
      let totalRings = 0;
      let totalPoints = 0;

      // Pass 1: Count rings and points
      for (let i = 0; i < totalPolys; i++) {
        const wkb = binaryVector.get(i);
        if (!wkb) continue;

        const view = new DataView(wkb.buffer, wkb.byteOffset, wkb.byteLength);
        const littleEndian = view.getUint8(0) === 1;
        const type = view.getUint32(1, littleEndian);

        if (type !== 3) { // Polygon type = 3
          console.warn(`[deckgl] Skipping non-polygon WKB (type ${type}) at index ${i}`);
          continue;
        }

        const numRings = view.getUint32(5, littleEndian);
        totalRings += numRings;

        let offset = 9;
        for (let r = 0; r < numRings; r++) {
          const numPoints = view.getUint32(offset, littleEndian);
          totalPoints += numPoints;
          offset += 4 + (numPoints * 16); // 16 bytes per point (2 doubles)
        }
      }

      // Allocate Arrow arrays
      const polyOffsets = new Int32Array(totalPolys + 1);
      const ringOffsets = new Int32Array(totalRings + 1);
      const coords = new Float64Array(totalPoints * 2);

      let currentPolyOffset = 0;
      let currentRingOffset = 0;
      let currentCoordIndex = 0;

      polyOffsets[0] = 0;
      ringOffsets[0] = 0;

      // Pass 2: Fill arrays with data
      let ringIndex = 0;

      for (let i = 0; i < totalPolys; i++) {
        const wkb = binaryVector.get(i);
        if (!wkb) {
          polyOffsets[i + 1] = polyOffsets[i];
          continue;
        }

        const view = new DataView(wkb.buffer, wkb.byteOffset, wkb.byteLength);
        const littleEndian = view.getUint8(0) === 1;
        const type = view.getUint32(1, littleEndian);

        if (type !== 3) {
          polyOffsets[i + 1] = polyOffsets[i];
          continue;
        }

        const numRings = view.getUint32(5, littleEndian);

        let offset = 9;
        for (let r = 0; r < numRings; r++) {
          const numPoints = view.getUint32(offset, littleEndian);
          offset += 4;

          for (let p = 0; p < numPoints; p++) {
            coords[currentCoordIndex++] = view.getFloat64(offset, littleEndian);
            coords[currentCoordIndex++] = view.getFloat64(offset + 8, littleEndian);
            offset += 16;
          }

          currentRingOffset += numPoints;
          ringOffsets[++ringIndex] = currentRingOffset;
        }

        currentPolyOffset += numRings;
        polyOffsets[i + 1] = currentPolyOffset;
      }

      // Construct Arrow Data structures
      // Point: FixedSizeList<2, Float64>
      const pointDataType = new Arrow.FixedSizeList(2, new Arrow.Field('xy', new Arrow.Float64()));
      const pointData = Arrow.makeData({
        type: pointDataType,
        length: totalPoints,
        child: Arrow.makeData({
          type: new Arrow.Float64(),
          length: totalPoints * 2,
          data: coords
        })
      });

      // Ring: List<Point>
      const ringDataType = new Arrow.List(new Arrow.Field('points', pointDataType));
      const ringData = Arrow.makeData({
        type: ringDataType,
        length: totalRings,
        valueOffsets: ringOffsets,
        child: pointData
      });

      // Polygon: List<Ring>
      const polyDataType = new Arrow.List(new Arrow.Field('rings', ringDataType));
      const polyData = Arrow.makeData({
        type: polyDataType,
        length: totalPolys,
        valueOffsets: polyOffsets,
        child: ringData
      });

      const vec = Arrow.makeVector(polyData);

      console.log(`[deckgl] Successfully converted ${totalPolys} WKB polygons to native GeoArrow format`);
      console.log(`[deckgl]   Total rings: ${totalRings}, Total points: ${totalPoints}`);

      return vec;
    } catch (err) {
      console.error('[deckgl] WKB to Arrow conversion failed:', err);
      return null;
    }
  }

  // Assign geoarrow.polygon metadata to a Table's schema and return new Table
  function assignGeoArrowExtensionToTable(table, geomColumnName) {
    try {
      if (!arrow || !table || !table.schema) return table;
      const fields = table.schema.fields;
      const idx = fields.findIndex(f => f.name === geomColumnName);
      if (idx === -1) return table;
      const geomField = fields[idx];
      const existing = geomField.metadata && geomField.metadata.get('ARROW:extension:name');
      if (existing === 'geoarrow.polygon' || existing === 'geoarrow.multipolygon') return table;

      const md = new Map(geomField.metadata || []);
      md.set('ARROW:extension:name', 'geoarrow.polygon');
      md.set('ARROW:extension:metadata', '{}');

      const newField = new arrow.Field(geomField.name, geomField.type, geomField.nullable, md);
      const newFields = fields.map((f, i) => (i === idx ? newField : f));
      const newSchema = new arrow.Schema(newFields, table.schema.metadata);

      // Create new table with updated schema
      const newTable = new arrow.Table(newSchema, table.data);
      console.log(`[deckgl] Assigned geoarrow.polygon metadata to "${geomColumnName}"`);

      // Debug: inspect geometry column structure
      const geomCol = newTable.getChild(geomColumnName);
      if (geomCol) {
        console.log(`[deckgl] Geometry column type:`, geomCol.type.toString());
        console.log(`[deckgl] Geometry column numChildren:`, geomCol.numChildren);
        if (geomCol.numChildren > 0) {
          const rings = geomCol.getChildAt(0);
          console.log(`[deckgl] Rings type:`, rings?.type?.toString());
          console.log(`[deckgl] Rings numChildren:`, rings?.numChildren);
          if (rings && rings.numChildren > 0) {
            const coords = rings.getChildAt(0);
            console.log(`[deckgl] Coords type:`, coords?.type?.toString());
            console.log(`[deckgl] Coords numChildren:`, coords?.numChildren);
          }
        }
      }

      return newTable;
    } catch (err) {
      console.warn('[deckgl] Failed to assign GeoArrow extension:', err);
      return table;
    }
  }

  // Convert a GeoArrow polygon column to binary attrs for SolidPolygonLayer
  function arrowPolygonToBinary(table, geomColumnName) {
    const startTime = performance.now();
    console.log(`[deckgl] arrowPolygonToBinary: Starting conversion for ${table.numRows} rows`);
    try {
      const geomCol = table.getChild(geomColumnName);
      if (!geomCol || !geomCol.data || geomCol.data.length === 0) {
        console.warn('[deckgl] Geometry column missing or empty for binary fallback');
        return null;
      }

      // Use first chunk (dataset is single chunk)
      const chunk = geomCol.data[0];
      if (!chunk || !chunk.valueOffsets) {
        console.warn('[deckgl] Geometry column has no valueOffsets');
        return null;
      }
      const polyOffsets = chunk.valueOffsets;

      const rings = geomCol.getChildAt(0);
      if (!rings || !rings.data || rings.data.length === 0) {
        console.warn('[deckgl] Rings column missing or empty');
        return null;
      }
      const ringOffsets = rings.data[0].valueOffsets;

      const coordStruct = rings.getChildAt(0);
      if (!coordStruct) {
        console.warn('[deckgl] Coordinate struct missing');
        return null;
      }

      console.log('[deckgl] coordStruct type:', coordStruct.type?.toString(), 'numChildren:', coordStruct.numChildren);

      // Try to get x,y as separate columns (Struct)
      const xCol = coordStruct.numChildren >= 2 ? coordStruct.getChildAt(0) : null;
      const yCol = coordStruct.numChildren >= 2 ? coordStruct.getChildAt(1) : null;

      let coordX, coordY;

      if (xCol && yCol) {
        // Struct<x, y> format
        console.log('[deckgl] Using Struct<x,y> format');
        coordX = xCol.data ? xCol.data[0].values : xCol.values || xCol;
        coordY = yCol.data ? yCol.data[0].values : yCol.values || yCol;
      } else {
        // FixedSizeList format - interleaved coordinates
        console.log('[deckgl] Trying FixedSizeList / interleaved format');

        // For FixedSizeList, the values are in the child column
        const valuesCol = coordStruct.getChildAt(0);
        const values = valuesCol ? (valuesCol.data?.[0]?.values || valuesCol.values) : null;

        if (values && values.length > 0) {
          console.log('[deckgl] Found', values.length, 'interleaved coordinate values');
          // Deinterleave x,y coordinates
          coordX = new Float64Array(values.length / 2);
          coordY = new Float64Array(values.length / 2);
          for (let i = 0; i < values.length / 2; i++) {
            coordX[i] = values[i * 2];
            coordY[i] = values[i * 2 + 1];
          }
        } else {
          console.warn('[deckgl] Could not extract coordinates from structure');
          return null;
        }
      }

      if (!coordX || !coordY || !coordX.length || !coordY.length) {
        console.warn('[deckgl] Coordinate values are invalid or empty');
        return null;
      }

      const startIndices = new Uint32Array(polyOffsets.length);
      for (let i = 0; i < polyOffsets.length; i++) {
        startIndices[i] = ringOffsets[polyOffsets[i]];
      }

      const positions = new Float32Array(coordX.length * 2);
      let minX = Infinity, maxX = -Infinity, minY = Infinity, maxY = -Infinity;
      for (let i = 0; i < coordX.length; i++) {
        const x = coordX[i];
        const y = coordY[i];
        positions[2 * i] = x;
        positions[2 * i + 1] = y;
        if (x < minX) minX = x;
        if (x > maxX) maxX = x;
        if (y < minY) minY = y;
        if (y > maxY) maxY = y;
      }

      const elapsed = performance.now() - startTime;
      console.log(`[deckgl] Converted ${table.numRows} polygons to binary format: ${positions.length/2} coordinates in ${elapsed.toFixed(0)}ms`);
      console.log(`[deckgl] Coordinate bounds: X=[${minX.toFixed(4)}, ${maxX.toFixed(4)}], Y=[${minY.toFixed(4)}, ${maxY.toFixed(4)}]`);
      console.log(`[deckgl] Center: [${((minX + maxX) / 2).toFixed(4)}, ${((minY + maxY) / 2).toFixed(4)}]`);

      return {
        length: table.numRows,
        startIndices,
        positions
      };
    } catch (err) {
      console.error('[deckgl] Binary fallback conversion failed:', err);
      return null;
    }
  }

  // Process layers: handle GeoArrow layers (WKB conversion + layer creation)
  // Returns created GeoArrow layer instances
  async function processGeoArrowLayers(layerSpecs) {
    if (!Array.isArray(layerSpecs)) return [];

    const layers = [];

    for (const layerSpec of layerSpecs) {
      const typeName = layerSpec['@@type'];

      if (isGeoArrowLayerType(typeName)) {
        const dataNode = layerSpec.data;
        if (dataNode && (dataNode.__arrow || dataNode.__arrow_url || dataNode.__parquet || dataNode.__parquet_url)) {
          let arrowTable = null;

          if (dataNode.__arrow) {
            console.log('[deckgl] Decoding Arrow IPC payload for GeoArrow layer');
            arrowTable = await parseArrowIPC(dataNode.__arrow);
          } else if (dataNode.__arrow_url) {
            console.log('[deckgl] Fetching Arrow IPC from URL for GeoArrow layer');
            arrowTable = await parseArrowFromUrl(dataNode.__arrow_url);
          } else if (dataNode.__parquet) {
            console.log('[deckgl] Decoding Parquet payload for GeoArrow layer');
            arrowTable = await parseParquetBase64(dataNode.__parquet);
          } else if (dataNode.__parquet_url) {
            console.log('[deckgl] Fetching Parquet from URL for GeoArrow layer');
            arrowTable = await parseParquetFromUrl(dataNode.__parquet_url);
          }

          if (arrowTable && arrowTable.numRows > 0) {
            console.log('[deckgl] Processing GeoArrow layer with', arrowTable.numRows, 'rows');

            // Identify geometry column and check for WKB encoding
            let geomColumnName = layerSpec.geometryColumn || 'geometry';
            let hasMetadata = false;
            let isWKB = false;
            const fieldNames = arrowTable.schema.fields.map(f => f.name);
            console.log('[deckgl] Schema fields:', fieldNames);

            // Honor preferred geometry column if present (case-sensitive then case-insensitive)
            let preferredField = arrowTable.schema.fields.find(f => f.name === geomColumnName);
            if (!preferredField) {
              preferredField = arrowTable.schema.fields.find(
                f => typeof f.name === 'string' && f.name.toLowerCase() === geomColumnName.toLowerCase()
              );
              if (preferredField) {
                geomColumnName = preferredField.name;
              }
            }

            if (preferredField) {
              if (hasGeoArrowPolygonMetadata(preferredField)) {
                hasMetadata = true;
              } else if (isPolygonLikeField(preferredField)) {
                geomColumnName = preferredField.name;
                arrowTable = assignGeoArrowExtensionToTable(arrowTable, geomColumnName);
                hasMetadata = true;
              }
            }

            for (const f of arrowTable.schema.fields) {
              if (hasGeoArrowPolygonMetadata(f)) {
                geomColumnName = f.name;
                hasMetadata = true;
                break;
              }
            }

            // If no metadata, try to detect native GeoArrow structure by shape (any field)
            if (!hasMetadata) {
              for (const f of arrowTable.schema.fields) {
                if (isPolygonLikeField(f)) {
                  geomColumnName = f.name || geomColumnName;
                  arrowTable = assignGeoArrowExtensionToTable(arrowTable, geomColumnName);
                  hasMetadata = true;
                  break;
                }
              }
            }

            // If requested geometry column not present, fallback to first polygon-like or first field
            if (!arrowTable.schema.fields.find(f => f.name === geomColumnName)) {
              const polyField = arrowTable.schema.fields.find(isPolygonLikeField);
              if (polyField) {
                geomColumnName = polyField.name;
                arrowTable = assignGeoArrowExtensionToTable(arrowTable, geomColumnName);
                hasMetadata = hasMetadata || hasGeoArrowPolygonMetadata(polyField) || isPolygonLikeField(polyField);
              } else if (arrowTable.schema.fields.length > 0) {
                geomColumnName = arrowTable.schema.fields[0].name;
              }
              console.warn('[deckgl] geometryColumn not found; using', geomColumnName);
            }

            console.log('[deckgl] Using geometry column:', geomColumnName);
            console.log('[deckgl] Layer spec requested geometryColumn:', layerSpec.geometryColumn);

            // Check for WKB encoding
            if (!hasMetadata) {
              const geomNames = ['geometry', 'geom', 'wkb_geometry', 'the_geom', 'GEOMETRY', 'GEOM', 'WKB_GEOMETRY', 'THE_GEOM'];
              for (const name of geomNames) {
                const f = arrowTable.schema.fields.find(ff => ff.name === name);
                if (f) {
                  geomColumnName = name;

                  // Check if this is WKB encoded via metadata
                  const extName = f.metadata && f.metadata.get('ARROW:extension:name');
                  if (extName === 'geoarrow.wkb') {
                    console.log(`[deckgl] Detected WKB encoding on field "${name}" via metadata`);
                    isWKB = true;
                    break;
                  }

                  // Check if it's native GeoArrow structure
                  if (isPolygonLikeField(f)) {
                    geomColumnName = f.name || geomColumnName;
                    arrowTable = assignGeoArrowExtensionToTable(arrowTable, geomColumnName);
                    hasMetadata = true;
                    break;
                  }

                  // Auto-detect WKB from binary data content (for R arrow which doesn't support extension metadata)
                  const column = arrowTable.getChild(name);
                  if (column && detectWKBFromBinaryColumn(column)) {
                    console.log(`[deckgl] Auto-detected WKB encoding on field "${name}" from binary content`);
                    isWKB = true;
                    break;
                  }
                }
              }
            }

            // Handle WKB encoding - convert to native GeoArrow polygon format
            if (isWKB) {
              console.log(`[deckgl] Converting WKB to native GeoArrow polygon format`);

              // Get the WKB geometry column
              const wkbColumn = arrowTable.getChild(geomColumnName);
              if (wkbColumn) {
                // Convert WKB to native Arrow polygon vector
                const polygonVector = wkbToPolygonVector(wkbColumn);

                if (polygonVector) {
                  // Create new schema with GeoArrow metadata on geometry field
                  const metadata = new Map();
                  metadata.set('ARROW:extension:name', 'geoarrow.polygon');
                  metadata.set('ARROW:extension:metadata', '{}');

                  // Build new fields array with metadata on geometry
                  const newFields = arrowTable.schema.fields.map(field => {
                    if (field.name === geomColumnName) {
                      return new arrow.Field(
                        geomColumnName,
                        polygonVector.type,
                        field.nullable,
                        metadata
                      );
                    }
                    return field;
                  });

                  const newSchema = new arrow.Schema(newFields, arrowTable.schema.metadata);

                  // Collect vectors for RecordBatch
                  const vectors = arrowTable.schema.fields.map(field => {
                    if (field.name === geomColumnName) {
                      return polygonVector;
                    }
                    return arrowTable.getChild(field.name);
                  });

                  // Flatten any multi-chunk non-geometry vector into a single
                  // contiguous Data, otherwise vectors.map(v => v.data[0])
                  // would silently drop all rows past the first chunk while
                  // the struct still claims arrowTable.numRows in length -
                  // rows past the chunk boundary then read undefined values
                  // and the layer falls back to its default opaque-black
                  // fill (and disables picking) for those rows.
                  function flattenVectorData(vec) {
                    if (!vec || !vec.data) return null;
                    if (vec.data.length <= 1) return vec.data[0];
                    // Rebuild as a single contiguous chunk by iterating
                    // every value and constructing a fresh single-chunk
                    // Vector via vectorFromArray. Cross-type and reliable;
                    // slower than zero-copy but only runs on the rare
                    // multi-chunk export path.
                    try {
                      const arr = new Array(vec.length);
                      for (let i = 0; i < vec.length; i++) arr[i] = vec.get(i);
                      const rebuilt = arrow.vectorFromArray
                        ? arrow.vectorFromArray(arr, vec.type)
                        : null;
                      if (rebuilt && rebuilt.data && rebuilt.data.length === 1) {
                        return rebuilt.data[0];
                      }
                      console.warn(
                        '[deckgl] flattenVectorData: vectorFromArray returned',
                        rebuilt && rebuilt.data ? rebuilt.data.length : 'null',
                        'chunks; falling back to chunk 0 (data past chunk 0 will be lost)'
                      );
                    } catch (e) {
                      console.warn('[deckgl] flattenVectorData failed:', e);
                    }
                    return vec.data[0];
                  }

                  const multiChunkCols = vectors
                    .map((v, i) => ({ name: newFields[i].name, n: v && v.data ? v.data.length : 0 }))
                    .filter(x => x.n > 1);
                  if (multiChunkCols.length > 0) {
                    console.log(
                      '[deckgl] Flattening multi-chunk columns before RecordBatch reconstruction:',
                      multiChunkCols
                    );
                  }
                  const flatChildren = vectors.map((v, i) =>
                    newFields[i].name === geomColumnName ? v.data[0] : flattenVectorData(v)
                  );

                  // Try to create RecordBatch with explicit schema
                  try {
                    // Get the data arrays from vectors
                    const structData = arrow.makeData({
                      type: new arrow.Struct(newFields),
                      children: flatChildren,
                      length: arrowTable.numRows
                    });

                    const batch = new arrow.RecordBatch(newSchema, structData);
                    arrowTable = new arrow.Table(batch);

                    console.log(`[deckgl] Created Arrow table with RecordBatch and explicit schema`);
                    console.log(`[deckgl]   Table has ${arrowTable.numRows} rows, ${arrowTable.numCols} columns`);
                    console.log(`[deckgl]   Geometry field metadata:`, arrowTable.schema.fields.find(f => f.name === geomColumnName)?.metadata);
                    hasMetadata = true;
                  } catch (batchErr) {
                    console.error('[deckgl] RecordBatch creation failed:', batchErr);

                    // Fallback: Store polygonVector separately and pass to layer
                    // Keep original table but we'll pass getPolygon separately
                    console.log('[deckgl] Using fallback: storing polygon vector for explicit getPolygon');
                    arrowTable.__convertedPolygonVector = polygonVector;
                    hasMetadata = true;
                  }
                } else {
                  console.error('[deckgl] WKB to native GeoArrow conversion failed');
                }
              }
            }

            // Helper: parse "@@=[col_r, col_g, col_b(, col_a)]" expressions
            // into the list of referenced column names. Returns null if the
            // expression isn't a simple color array of Arrow column refs.
            function parseColorAccessorColumns(expr) {
              if (typeof expr !== 'string' || !expr.startsWith('@@=')) return null;
              const body = expr.substring(3).trim();
              if (!body.startsWith('[') || !body.endsWith(']')) return null;
              const inner = body.substring(1, body.length - 1);
              const parts = inner.split(',').map(s => s.trim()).filter(Boolean);
              if (parts.length < 3 || parts.length > 4) return null;
              // Each part must be a bare identifier (column name)
              const idRe = /^[A-Za-z_][A-Za-z0-9_]*$/;
              if (!parts.every(p => idRe.test(p))) return null;
              return parts;
            }

            // Helper: build a packed per-feature RGBA Uint8Array from named
            // Int32-like Arrow columns. Returns null on any missing column.
            function buildColorBufferFromArrow(table, columnNames, numRows) {
              const cols = columnNames.map(n => table.getChild(n));
              if (cols.some(c => !c)) return null;
              const buf = new Uint8Array(numRows * 4);
              for (let i = 0; i < numRows; i++) {
                buf[4 * i]     = cols[0].get(i) & 0xff;
                buf[4 * i + 1] = cols[1].get(i) & 0xff;
                buf[4 * i + 2] = cols[2].get(i) & 0xff;
                buf[4 * i + 3] = cols.length >= 4 ? (cols[3].get(i) & 0xff) : 255;
              }
              return buf;
            }

            // SolidPolygonLayer in binary mode interprets attribute buffers
            // as per-vertex, not per-feature. Expand a per-feature RGBA
            // buffer to per-vertex using startIndices.
            // startIndices has length numFeatures+1; positions has
            // numVertices*2 entries (XY pairs).
            function expandPerFeatureColorsToVertices(
              perFeatureColors, startIndices, numVertices
            ) {
              const numFeatures = startIndices.length - 1;
              const out = new Uint8Array(numVertices * 4);
              for (let f = 0; f < numFeatures; f++) {
                const vStart = startIndices[f];
                const vEnd = startIndices[f + 1];
                const r = perFeatureColors[4 * f];
                const g = perFeatureColors[4 * f + 1];
                const b = perFeatureColors[4 * f + 2];
                const a = perFeatureColors[4 * f + 3];
                for (let v = vStart; v < vEnd; v++) {
                  out[4 * v]     = r;
                  out[4 * v + 1] = g;
                  out[4 * v + 2] = b;
                  out[4 * v + 3] = a;
                }
              }
              return out;
            }

            // Route WKB-converted GeoArrowPolygonLayer through the binary
            // SolidPolygonLayer path too. The @geoarrow/deck.gl-layers
            // function-accessor path resolves objectInfo.data.data.getChild
            // unreliably for some rows (Table.data is an array of batches in
            // modern Arrow JS), causing a subset of polygons to fall back to
            // deck.gl's default opaque-black fill + picking off. Going
            // through the binary path with a precomputed color buffer
            // bypasses that lookup entirely.
            const geomCol = arrowTable.getChild(geomColumnName);
            const useBinarySolidPath =
              (typeName === 'GeoArrowSolidPolygonLayer' ||
               typeName === 'GeoArrowPolygonLayer') &&
              deck && deck.SolidPolygonLayer;
            if (useBinarySolidPath) {
              console.log('[deckgl] 🔧 Processing GeoArrowSolidPolygonLayer...');
              console.log('[deckgl] Converting Arrow table to binary format...');
              const binary = arrowPolygonToBinary(arrowTable, geomColumnName);
              if (binary) {
                console.log('[deckgl] ✅ Binary conversion successful');
                console.log('[deckgl]    Polygons:', binary.length);
                console.log('[deckgl]    Start indices length:', binary.startIndices ? binary.startIndices.length : 0);
                console.log('[deckgl]    Positions length:', binary.positions ? binary.positions.length : 0);
                console.log('[deckgl]    Total coordinates:', binary.positions ? binary.positions.length / 2 : 0);

                // Extract properties from Arrow table for tooltips
                const properties = [];
                const schema = arrowTable.schema;
                const propertyFields = schema.fields.filter(f =>
                  f.name !== geomColumnName &&
                  f.name !== 'geometry' &&
                  f.name !== 'GEOMETRY' &&
                  f.name !== 'geom'
                );

                for (let i = 0; i < arrowTable.numRows; i++) {
                  const obj = {};
                  for (const field of propertyFields) {
                    const column = arrowTable.getChild(field.name);
                    obj[field.name] = column.get(i);
                  }
                  properties.push(obj);
                }

                console.log('[deckgl] Extracted properties for', properties.length, 'features');
                if (properties.length > 0) {
                  console.log('[deckgl] Sample properties:', properties[0]);
                }

                // Build attributes; add precomputed per-feature fill color
                // buffer if the spec passes an @@= column-ref expression.
                const polyAttributes = {
                  getPolygon: { value: binary.positions, size: 2 }
                };
                let staticFillColor = null;
                const fillCols = parseColorAccessorColumns(layerSpec.getFillColor);
                if (fillCols) {
                  const perFeatureColors = buildColorBufferFromArrow(
                    arrowTable, fillCols, arrowTable.numRows
                  );
                  if (perFeatureColors) {
                    const numVerts = binary.positions.length / 2;
                    const perVertexColors = expandPerFeatureColorsToVertices(
                      perFeatureColors, binary.startIndices, numVerts
                    );
                    polyAttributes.getFillColor = { value: perVertexColors, size: 4 };
                    console.log(
                      '[deckgl] Built per-vertex fill color buffer from columns:',
                      fillCols,
                      `(${arrowTable.numRows} features → ${numVerts} vertices)`
                    );
                  } else {
                    console.warn(
                      '[deckgl] Could not build color buffer from columns',
                      fillCols, '- falling back to default fill'
                    );
                    staticFillColor = [24, 190, 140, 160];
                  }
                } else if (Array.isArray(layerSpec.getFillColor)) {
                  staticFillColor = layerSpec.getFillColor;
                } else {
                  staticFillColor = [24, 190, 140, 160];
                }

                const polyLayerProps = {
                  id: (layerSpec.id || `geoarrow-${Date.now()}`) + '-binary',
                  data: {
                    length: binary.length,
                    startIndices: binary.startIndices,
                    attributes: polyAttributes,
                    properties: properties  // Store properties for tooltips
                  },
                  _normalize: false,
                  _windingOrder: 'CCW',
                  getElevation: layerSpec.getElevation || 0,
                  extruded: layerSpec.extruded || false,
                  pickable: layerSpec.pickable || false,
                  autoHighlight: layerSpec.autoHighlight || false,
                  stroked: layerSpec.stroked || false,
                  filled: layerSpec.filled !== false,
                  wireframe: layerSpec.wireframe || false
                };
                if (staticFillColor !== null) {
                  polyLayerProps.getFillColor = staticFillColor;
                }

                console.log('[deckgl] 🎨 Creating SolidPolygonLayer with props:', {
                  id: polyLayerProps.id,
                  dataLength: polyLayerProps.data.length,
                  startIndicesLength: polyLayerProps.data.startIndices?.length,
                  positionsLength: polyLayerProps.data.attributes?.getPolygon?.value?.length,
                  extruded: polyLayerProps.extruded,
                  pickable: polyLayerProps.pickable,
                  windingOrder: polyLayerProps._windingOrder,
                  fillColor: polyLayerProps.getFillColor
                });
                
                // Debug: Show first few positions
                const pos = polyLayerProps.data.attributes?.getPolygon?.value;
                if (pos && pos.length >= 10) {
                  console.log('[deckgl] First 5 coordinate pairs:', 
                    `[${pos[0]}, ${pos[1]}], [${pos[2]}, ${pos[3]}], [${pos[4]}, ${pos[5]}], [${pos[6]}, ${pos[7]}], [${pos[8]}, ${pos[9]}]`
                  );
                }

                const layer = new deck.SolidPolygonLayer(polyLayerProps);
                layers.push(layer);
                console.log('[deckgl] ✅ SolidPolygonLayer created and added to layers array');
                continue;
              } else {
                console.error('[deckgl] ❌ Binary conversion failed for GeoArrowSolidPolygonLayer');
              }
            }

            // Create GeoArrow layer manually (not through JSONConverter) for other GeoArrow layer types
            const useGeoArrowLayer = (hasMetadata || !isWKB);
            if (useGeoArrowLayer) {
              const LayerClass = geoarrowDeck && geoarrowDeck[typeName];
              if (LayerClass) {
                // Build layer props
                const layerProps = {
                  id: layerSpec.id || `geoarrow-${Date.now()}`,
                  data: arrowTable,
                  geometryColumn: geomColumnName
                };

                console.log(`[deckgl] Creating GeoArrow layer with ${arrowTable.numRows} rows`);
                console.log(`[deckgl] Layer props geometryColumn:`, layerProps.geometryColumn);

                // Copy non-data props and parse accessor expressions
                const schema = arrowTable.schema;
                for (const [key, value] of Object.entries(layerSpec)) {
                  if (key === '@@type' || key === 'data' || key === 'geometryColumn') continue;

                  // Parse @@= accessor expressions
                  if (typeof value === 'string' && value.startsWith('@@=')) {
                    try {
                      const expr = value.substring(3);
                      // GeoArrow layers use a different accessor signature:
                      // Accessor function receives: (objectInfo: AccessorContext) => value
                      // where objectInfo = { index, data, target }
                      // We need to extract column values by index from the Arrow table

                      // Get field names (excluding geometry)
                      const fieldNames = schema.fields.map(f => f.name).filter(n => n !== 'geometry' && n !== 'GEOMETRY' && n !== 'geom');

                      // Create column accessors
                      // objectInfo.data.data is the RecordBatch
                      const columnAccessors = fieldNames.map(name => {
                        return `const ${name} = objectInfo.data.data.getChild('${name}').get(objectInfo.index);`;
                      }).join(' ');

                      // Create the accessor function with proper signature
                      // objectInfo has: { index, data: { data, length, attributes }, target }
                      const accessorFunc = new Function('objectInfo', `
                        ${columnAccessors}
                        return ${expr};
                      `);

                      layerProps[key] = accessorFunc;
                      console.log(`[deckgl] Created accessor for ${key}: ${expr}`);
                    } catch (err) {
                      console.warn(`[deckgl] Failed to parse accessor ${key}:`, err);
                      layerProps[key] = value;
                    }
                  } else {
                    layerProps[key] = value;
                  }
                }

                try {
                  // If we have a stored polygon vector from fallback, use it explicitly
                  if (arrowTable.__convertedPolygonVector) {
                    // Get the polygon data (first chunk)
                    const polygonData = arrowTable.__convertedPolygonVector.data[0];
                    layerProps.getPolygon = polygonData;
                    console.log('[deckgl] Using explicit getPolygon from converted vector');
                  }

                  // Create layer
                  const layer = new LayerClass(layerProps);
                  layers.push(layer);
                  console.log('[deckgl] Created GeoArrow layer:', typeName);
                } catch (err) {
                  console.error(`[deckgl] Failed to create GeoArrow layer:`, err);
                  // Fallback to SolidPolygonLayer with binary attributes
                  if (deck && deck.SolidPolygonLayer) {
                    const binary = arrowPolygonToBinary(arrowTable, geomColumnName);
                    if (binary) {
                      const polyLayerProps = {
                        id: (layerSpec.id || `geoarrow-${Date.now()}`) + '-binary',
                        data: {
                          length: binary.length,
                          startIndices: binary.startIndices,
                          attributes: {
                            getPolygon: { value: binary.positions, size: 2 }
                          }
                        },
                        _normalize: false,
                        _windingOrder: 'CCW',
                        getFillColor: layerSpec.getFillColor || [24, 190, 140, 160],
                        extruded: layerSpec.extruded || false,
                        pickable: layerSpec.pickable || false
                      };
                      layers.push(new deck.SolidPolygonLayer(polyLayerProps));
                      console.warn('[deckgl] Falling back to SolidPolygonLayer with binary attributes');
                    }
                  }
                }
              } else {
                console.warn(`[deckgl] GeoArrow layer class not found for ${typeName}`);
                // Fallback to SolidPolygonLayer if class missing
                if (deck && deck.SolidPolygonLayer) {
                  const binary = arrowPolygonToBinary(arrowTable, geomColumnName);
                  if (binary) {
                    const polyLayerProps = {
                      id: (layerSpec.id || `geoarrow-${Date.now()}`) + '-binary',
                      data: {
                        length: binary.length,
                        startIndices: binary.startIndices,
                        attributes: {
                          getPolygon: { value: binary.positions, size: 2 }
                        }
                      },
                      _normalize: false,
                      _windingOrder: 'CCW',
                      getFillColor: layerSpec.getFillColor || [24, 190, 140, 160],
                      extruded: layerSpec.extruded || false,
                      pickable: layerSpec.pickable || false
                    };
                    layers.push(new deck.SolidPolygonLayer(polyLayerProps));
                    console.warn('[deckgl] Falling back to SolidPolygonLayer with binary attributes (no GeoArrow class)');
                  }
                }
              }
            } else if (useBinaryFallback) {
              // Binary fallback for large datasets with Struct coordinates
              if (deck && deck.SolidPolygonLayer) {
                const binary = arrowPolygonToBinary(arrowTable, geomColumnName);
                if (binary) {
                  const polyLayerProps = {
                    id: (layerSpec.id || `geoarrow-${Date.now()}`) + '-binary',
                    data: {
                      length: binary.length,
                      startIndices: binary.startIndices,
                      attributes: {
                        getPolygon: { value: binary.positions, size: 2 }
                      }
                    },
                    _normalize: false,
                    _windingOrder: 'CCW',
                    getFillColor: layerSpec.getFillColor || [24, 190, 140, 160],
                    getElevation: layerSpec.getElevation || 0,
                    extruded: layerSpec.extruded || false,
                    pickable: layerSpec.pickable || false,
                    autoHighlight: layerSpec.autoHighlight || false
                  };
                  layers.push(new deck.SolidPolygonLayer(polyLayerProps));
                  console.log('[deckgl] Created SolidPolygonLayer with binary attributes (large dataset fallback)');
                } else {
                  console.error('[deckgl] Binary fallback conversion failed');
                }
              }
            }
          } else {
            console.warn(`[deckgl] No valid Arrow data for ${typeName}`);
          }
        } else {
          console.warn(`[deckgl] GeoArrow layer ${typeName} requires Arrow data format (__arrow, __parquet, or __parquet_url)`);
        }
      }
    }
    return layers;
  }

  // Process GeoArrow layers (convert WKB, create layer instances)
  const geoArrowLayers = await processGeoArrowLayers(spec.layers || []);

  // Prepare spec for JSONConverter - only standard (non-GeoArrow) layers
  const standardLayerSpecs = (spec.layers || []).filter(l => !isGeoArrowLayerType(l['@@type']));
  const specForConverter = {
    ...spec,
    layers: standardLayerSpecs
  };
  console.log('[deckgl] Spec for JSONConverter:', {
    hasViews: !!specForConverter.views,
    hasInitialViewState: !!specForConverter.initialViewState,
    layerCount: standardLayerSpecs.length
  });

  const classes = {};
  classNames.forEach((name) => {
    // Include GeoArrow layers in the configuration so JSONConverter can parse their accessors
    const resolved = resolveDeckExport(deck, name, geoarrowDeck);
    if (resolved) {
      if (typeof resolved === 'function') {
        classes[name] = resolved;
      } else {
        console.warn(`[deckgl] Resolved ${name} is not a constructor function:`, typeof resolved, resolved);
      }
    } else {
      console.warn(`[deckgl] Missing class binding for ${name}.`);
    }
  });

  const enumerations = {};
  enumNames.forEach((name) => {
    const resolved = resolveDeckExport(deck, name);
    if (resolved) {
      enumerations[name] = resolved;
    } else if (deck[name]) {
      enumerations[name] = deck[name];
    } else {
      console.warn(`[deckgl] Missing enumeration binding for ${name}.`);
    }
  });

  console.log('[deckgl] Collected classes:', Object.keys(classes));
  console.log('[deckgl] GeoArrow layers created:', geoArrowLayers.length);

  const configuration = new JSONConfiguration({
    classes,
    enumerations
  });

  const converter = new JSONConverter({ configuration });

  let props;
  try {
    props = converter.convert(specForConverter) || {};
    console.log('[deckgl] Props after JSONConverter:', {
      hasViews: !!props.views,
      hasInitialViewState: !!props.initialViewState,
      hasController: !!props.controller,
      layerCount: props.layers ? props.layers.length : 0
    });
  } catch (err) {
    console.error('[deckgl] JSONConverter.convert failed:', err);
    console.error('[deckgl] Spec being converted:', JSON.stringify(specForConverter, null, 2).substring(0, 500));
    console.error('[deckgl] Classes available:', Object.keys(classes));
    throw err;
  }

  // Fallback: if no layers were created, try converting each standard layer individually
  if (!props.layers || props.layers.length === 0) {
    const fallbackLayers = (standardLayerSpecs || [])
      .map((layerSpec) => {
        try {
          return converter.convert(layerSpec);
        } catch (e) {
          const layerId = layerSpec?.id || layerSpec?.['@@type'];
          console.warn('[deckgl] Failed to convert layer spec in fallback:', layerId, e);
          return null;
        }
      })
      .filter(Boolean);
    if (fallbackLayers.length > 0) {
      console.warn('[deckgl] JSONConverter returned no layers; applied fallback conversion.');
      props.layers = fallbackLayers;
    }
  }

  // Views: use JSONConverter to build view instances, then ensure controller is enabled for MapView
  if (!props.views && spec.views) {
    console.log('[deckgl] Processing views from spec');
    if (Array.isArray(spec.views)) {
      props.views = spec.views.map(viewSpec => {
        if (viewSpec && viewSpec['@@type']) {
          try {
            const convertedView = converter.convert(viewSpec);
            if (convertedView && convertedView.constructor && convertedView.constructor.name === 'MapView') {
              if (convertedView.controller == null) {
                convertedView.controller = true;
              }
            }
            return convertedView;
          } catch (err) {
            console.warn('[deckgl] Failed to convert view spec, using raw spec:', err);
            return viewSpec;
          }
        }
        return viewSpec;
      });
    } else {
      props.views = spec.views;
    }
  }

  // initialViewState and viewState can be copied directly (they're plain objects)
  if (!props.initialViewState && spec.initialViewState) {
    console.log('[deckgl] Manually preserving initialViewState from spec');
    props.initialViewState = spec.initialViewState;
  }
  if (!props.viewState && spec.viewState) {
    console.log('[deckgl] Manually preserving viewState from spec');
    props.viewState = spec.viewState;
  }

  // Avoid disabling controller: drop whichever view state was not explicitly set by the spec.
  if (props.viewState && props.initialViewState) {
    if (userProvidedViewState) {
      console.warn('[deckgl] Both viewState and initialViewState supplied; using viewState and dropping initialViewState.');
      delete props.initialViewState;
    } else {
      console.warn('[deckgl] Dropping generated viewState to rely on initialViewState for interaction.');
      delete props.viewState;
    }
  }

  // Normalize view state objects - handle both MapView and OrthographicView formats
  const coerceViewState = (vs, label) => {
    if (!vs || typeof vs !== 'object') return vs;
    
    // Check if this is OrthographicView-style (has 'target' array)
    if (Array.isArray(vs.target)) {
      // OrthographicView uses target: [x, y, z], zoom
      const normalized = { ...vs };
      if (!Number.isFinite(normalized.zoom)) {
        normalized.zoom = 0;
      }
      console.log(`[deckgl] ${label} using OrthographicView format:`, normalized);
      return normalized;
    }
    
    // Check if this is MapView-style (has longitude/latitude)
    if ('longitude' in vs || 'latitude' in vs) {
      const normalized = { ...vs };
      const safe = (v, fallback) => (Number.isFinite(v) ? v : fallback);
      
      if (!Number.isFinite(normalized.longitude)) normalized.longitude = 0;
      if (!Number.isFinite(normalized.latitude)) normalized.latitude = 0;
      if (!Number.isFinite(normalized.zoom)) normalized.zoom = 0;
      normalized.pitch = safe(normalized.pitch, 0);
      normalized.bearing = safe(normalized.bearing, 0);
      
      console.log(`[deckgl] ${label} using MapView format:`, normalized);
      return normalized;
    }
    
    // Unknown format - return as is
    console.log(`[deckgl] ${label} using custom format:`, vs);
    return vs;
  };

  props.initialViewState = coerceViewState(props.initialViewState, 'initialViewState');
  props.viewState = coerceViewState(props.viewState, 'viewState');
  
  // Debug: Show exact view state values
  console.log('[deckgl] 📍 Final initialViewState:', JSON.stringify(props.initialViewState));
  if (props.initialViewState) {
    console.log('[deckgl]    longitude:', props.initialViewState.longitude);
    console.log('[deckgl]    latitude:', props.initialViewState.latitude);
    console.log('[deckgl]    zoom:', props.initialViewState.zoom);
  }

  // Combine standard layers with GeoArrow layers
  const allLayers = [...(props.layers || []), ...geoArrowLayers];
  props.layers = allLayers;
  console.log('[deckgl] Total layers:', allLayers.length);
  console.log('[deckgl] GeoArrow layers count:', geoArrowLayers.length);
  console.log('[deckgl] Standard layers count:', (props.layers || []).length - geoArrowLayers.length);
  
  // Debug: log each layer
  allLayers.forEach((layer, idx) => {
    console.log(`[deckgl] Layer ${idx}:`, {
      id: layer?.id,
      type: layer?.constructor?.name || layer?.constructor?.layerName,
      hasProps: !!layer?.props,
      propsData: layer?.props?.data ? 'present' : 'missing'
    });
  });

  // Fallback: if no layers came through JSONConverter but we created GeoArrow layers, use them
  if ((!props.layers || props.layers.length === 0) && geoArrowLayers.length > 0) {
    props.layers = geoArrowLayers;
    console.warn('[deckgl] Using GeoArrow layers as sole layer set (no layers from JSONConverter)');
  }

  // Early return if no layers - prevents deck.gl initialization errors
  if (!props.layers || props.layers.length === 0) {
    console.warn('[deckgl] No layers to render; skipping Deck initialization. Display will remain empty.');
    el.__deckInstance = null;
    // Add a message to the element so users know what's happening
    el.innerHTML = '<div style="display: flex; align-items: center; justify-content: center; height: 100%; color: #666; font-family: sans-serif;">No layers to display</div>';
    return;
  }

  // Ensure we have valid views
  const ensureViews = (views) => {
    if (!views) return null;
    if (!Array.isArray(views)) views = [views];
    return views.map((v) => {
      try {
        if (v && typeof v === 'object') {
          // Already a View instance - return as is
          if (deck && deck.View && v instanceof deck.View) {
            return v;
          }
          
          // Convert view descriptors to actual View instances
          const viewType = v['@@type'];
          if (v.controller == null) {
            v.controller = true;
          }
          if (!v.id) {
            v.id = viewType ? viewType.toLowerCase() + '-view' : 'default-view';
          }
          
          // Create the appropriate view instance
          // Remove @@type before passing to constructor
          const viewProps = { ...v };
          delete viewProps['@@type'];
          
          if (viewType === 'OrthographicView' && deck.OrthographicView) {
            console.log('[deckgl] Creating OrthographicView with props:', viewProps);
            return new deck.OrthographicView(viewProps);
          } else if (viewType === 'MapView' && deck.MapView) {
            // MapView - but note this may have issues without proper basemap setup
            console.log('[deckgl] Creating MapView with props:', viewProps);
            return new deck.MapView(viewProps);
          } else if (viewType === 'FirstPersonView' && deck.FirstPersonView) {
            return new deck.FirstPersonView(viewProps);
          } else {
            // Default to OrthographicView for unknown types
            console.warn('[deckgl] Unknown view type:', viewType, '- using OrthographicView');
            return new deck.OrthographicView(viewProps);
          }
        }
      } catch (e) {
        console.warn('[deckgl] Failed to create view instance:', e);
      }
      return v;
    });
  };

  props.views = ensureViews(props.views || spec.views);
  if (!props.views || props.views.length === 0) {
    // Check if we have geographic view state (longitude/latitude)
    const hasGeoViewState = props.initialViewState && 
      ('longitude' in props.initialViewState || 'latitude' in props.initialViewState);
    
    if (hasGeoViewState) {
      // Use MapView for geographic coordinates - but without basemap
      // This gives proper mercator projection for lon/lat data
      console.log('[deckgl] No views specified, using MapView for geographic coordinates');
      props.views = [new deck.MapView({ controller: true, id: 'default-map-view' })];
    } else {
      // Use OrthographicView for non-geographic data
      console.log('[deckgl] No views specified, using OrthographicView');
      props.views = [new deck.OrthographicView({ controller: true, id: 'default-view' })];
    }
  }

  if (!props.parent) {
    props.parent = el;
  }

  // Enforce non-zero size on container and props
  const defaultWidth = 800;
  const defaultHeight = 600;
  const minWidth = props.width || defaultWidth;
  const minHeight = props.height || defaultHeight;

  function clampSize() {
    if (el) {
      if (!el.style.width && el.clientWidth === 0) {
        el.style.width = `${minWidth}px`;
      }
      if (!el.style.height && el.clientHeight === 0) {
        el.style.height = `${minHeight}px`;
      }
    }
  }
  clampSize();

  const measuredWidth = (el && el.clientWidth) || minWidth;
  const measuredHeight = (el && el.clientHeight) || minHeight;
  const toNumber = (value, fallback) => {
    const num = typeof value === 'string' ? Number.parseFloat(value) : value;
    return Number.isFinite(num) ? num : fallback;
  };
  
  // Ensure we have valid dimensions - if height is 0, use default
  let finalWidth = toNumber(measuredWidth, defaultWidth);
  let finalHeight = toNumber(measuredHeight, defaultHeight);
  
  // Critical: If height is 0, deck.gl will fail assertions
  if (finalHeight <= 0) {
    console.warn('[deckgl] Height is 0, using default height:', defaultHeight);
    finalHeight = defaultHeight;
    el.style.height = `${defaultHeight}px`;
  }
  if (finalWidth <= 0) {
    console.warn('[deckgl] Width is 0, using default width:', defaultWidth);
    finalWidth = defaultWidth;
    el.style.width = `${defaultWidth}px`;
  }
  
  props.width = finalWidth;
  props.height = finalHeight;
  console.log(`[deckgl] Using size ${props.width}x${props.height} for Deck instance`);

  // Set up controller - use spec's controller if provided, otherwise enable it
  if (spec.controller !== undefined) {
    props.controller = spec.controller;
    console.log('[deckgl] Using controller from spec:', spec.controller);
  } else if (!props.controller) {
    // Default to enabling controller
    props.controller = true;
    console.log('[deckgl] Enabling default controller');
  }
  
  console.log('[deckgl] Controller config:', props.controller);
  console.log('[deckgl] Views config:', props.views);
  console.log('[deckgl] Initial view state:', props.initialViewState);

  // Log detailed view information
  if (Array.isArray(props.views)) {
    props.views.forEach((view, idx) => {
      console.log(`[deckgl] View ${idx}:`, view);
      if (view) {
        console.log(`[deckgl] View ${idx} controller:`, view.controller);
        const isViewInstance = !!(deck && deck.View && view instanceof deck.View);
        console.log(`[deckgl] View ${idx} is View instance:`, isViewInstance);
      }
    });
  }

  // Debug snapshot before Deck creation to diagnose assertion failures
  try {
    if (Array.isArray(props.layers)) {
      const layerSummaries = props.layers.map((l) => {
        const isLayerInstance = !!(l && deck && deck.Layer && l instanceof deck.Layer);
        const sample = Array.isArray(l?.props?.data) && l.props.data.length > 0
          ? l.props.data[0]
          : (Array.isArray(l?.data) && l.data.length > 0 ? l.data[0] : null);
        return {
          id: l?.id,
          type: l?.constructor?.layerName || l?.name || l?.id,
          isLayerInstance,
          dataLength: Array.isArray(l?.props?.data) ? l.props.data.length
            : (Array.isArray(l?.data) ? l.data.length
              : (l?.data && typeof l.data.length === 'number' ? l.data.length : 'n/a')),
          sample
        };
      });
      console.log('[deckgl] Layer summary before Deck init:', JSON.stringify(layerSummaries, null, 2));
    }
  } catch (e) {
    console.warn('[deckgl] Failed to summarize layers before init', e);
  }

  // Note: Layer count check moved earlier in the code to prevent view initialization issues

  if (!props.container) {
    props.container = el;
  }

  // Add default tooltip handler if not provided
  if (!props.getTooltip) {
    props.getTooltip = (info) => {
      if (!info.object && info.index === undefined) return null;

      // For binary layers (GeoArrowSolidPolygonLayer), check if properties are stored separately
      let tooltipData = info.object;
      if (!tooltipData && info.index !== undefined && info.layer?.props?.data?.properties) {
        tooltipData = info.layer.props.data.properties[info.index];
      }

      if (!tooltipData) return null;

      const propsList = [];
      for (const [key, value] of Object.entries(tooltipData)) {
        if (key !== 'geometry' && key !== 'polygon' && value !== null && value !== undefined) {
          propsList.push(`${key}: ${value}`);
        }
      }
      return propsList.length > 0 ? {
        html: `<div style="background: rgba(0,0,0,0.8); color: white; padding: 8px; border-radius: 4px; font-family: monospace; font-size: 12px;">${propsList.join('<br>')}</div>`
      } : null;
    };
  }

  el.classList.add('deckgl-view');
  el.style.position = 'relative';
  el.style.width = '100%';
  el.style.height = '100%';
  el.style.margin = '0';
  el.style.padding = '0';
  el.style.pointerEvents = 'auto';
  el.style.touchAction = 'auto';
  el.style.userSelect = 'auto';

  if (el.__deckInstance) {
    console.log('[deckgl] Updating existing Deck instance with new props');
    // Only update, don't recreate
    try {
      el.__deckInstance.setProps(props);
      // Force a redraw to ensure updates are applied
      el.__deckInstance.redraw();
    } catch (err) {
      console.error('[deckgl] Failed to update Deck instance:', err);
      // If update fails, recreate
      el.__deckInstance.finalize();
      delete el.__deckInstance;
      el.innerHTML = '';
      el.__deckInstance = new deck.Deck(props);
    }
  } else {
    console.log('[deckgl] Creating new Deck instance');
    console.log('[deckgl] Final props for Deck:', {
      width: props.width,
      height: props.height,
      controller: props.controller,
      viewsCount: props.views?.length,
      layersCount: props.layers?.length,
      initialViewState: props.initialViewState
    });
    el.innerHTML = '';
    try {
      el.__deckInstance = new deck.Deck(props);

      // Ensure container and canvas allow pointer interactions
      el.style.pointerEvents = 'auto';
      el.style.touchAction = 'auto';
      const canvases = el.getElementsByTagName('canvas');
      if (canvases && canvases.length > 0) {
        const c = canvases[0];
        c.style.pointerEvents = 'auto';
        c.style.position = 'absolute';
        c.style.inset = '0';
        c.style.width = '100%';
        c.style.height = '100%';
      }

      // If resize events report height 0, ignore them
      const userOnResize = props.onResize;
      el.__deckInstance.setProps({
        onResize: (size) => {
          if (!size || size.height === 0) {
            console.warn('[deckgl] Ignoring resize with height=0; keeping previous size');
            return;
          }
          if (typeof userOnResize === 'function') {
            userOnResize(size);
          }
        }
      });
    } catch (err) {
      console.error('[deckgl] Failed to create Deck instance:', err);
      // Swallow the error to allow onRender or later hooks to recover/recreate
      return;
    }
  }
}

HTMLWidgets.widget({
  name: "deckgl",
  type: "output",
  factory: function(el, width, height) {
    console.log("[deckgl] — factory() called — element, size:", el, width, height);
    const pending = {};
    let widgetIdInstance = null;
    let handlerRegistered = false;

    function shinyConnector(wid) {
      return {
        query: function(q) {
          console.log(`[deckgl][${wid}] → shinyConnector sending query:`, q);
          return new Promise((resolve, reject) => {
            const reqId = "q" + Math.random().toString(36).substr(2, 9);
            pending[reqId] = { resolve, reject, queryType: q.type || "json" };
            Shiny.setInputValue(
              `${wid}_deckgl_query`,
              { request: reqId, sql: q.sql, type: q.type || "json" },
              { priority: "event" }
            );
          });
        }
      };
    }

    function registerHandler(wid) {
      if (handlerRegistered) {
        console.log(`[deckgl][${wid}] Handler already registered.`);
        return;
      }
      Shiny.addCustomMessageHandler(`${wid}_deckgl_response`, async message => {
        console.log(`[deckgl][${wid}] ← shinyConnector received response:`, message);
        const cbEntry = pending[message.request];
        if (!cbEntry) {
          console.warn(`[deckgl][${wid}] Received response for unknown request:`, message.request);
          return;
        }

        if (message.error) {
          console.error(`[deckgl][${wid}] Error from Shiny for request ${message.request}:`, message.error);
          cbEntry.reject(new Error(message.error));
        } else {
          // Check if data is Arrow format
          if ((message.dataFormat === 'arrow' || message.dataFormat === 'geoarrow') &&
              (typeof message.data === 'string' || typeof message.dataUrl === 'string')) {
            try {
              const { ArrowLoader } = await ensureDeckGlModules();
              if (!ArrowLoader) {
                throw new Error('ArrowLoader not available');
              }
              let bytes;
              if (typeof message.dataUrl === 'string') {
                const response = await fetch(message.dataUrl);
                if (!response.ok) {
                  throw new Error(`Failed to fetch Arrow data: ${response.status} ${response.statusText}`);
                }
                bytes = new Uint8Array(await response.arrayBuffer());
              } else {
                // Decode base64 to Uint8Array
                const binaryString = atob(message.data);
                bytes = new Uint8Array(binaryString.length);
                for (let i = 0; i < binaryString.length; i++) {
                  bytes[i] = binaryString.charCodeAt(i);
                }
              }
              // Parse Arrow IPC stream
              const arrowTable = await ArrowLoader.parse(bytes);
              cbEntry.resolve(arrowTable);
            } catch (err) {
              console.error(`[deckgl][${wid}] Failed to parse Arrow data:`, err);
              cbEntry.reject(err);
            }
          } else {
            // Legacy JSON format
            cbEntry.resolve(message.data);
          }
        }
        delete pending[message.request];
      });
      handlerRegistered = true;
      console.log(`[deckgl][${wid}] ✓ Custom message handler registered.`);
    }

    const widgetInstance = {
      renderValue: async function(x) {
        widgetIdInstance = x.widgetId;
        const wid = widgetIdInstance;

        console.log(`[deckgl][${wid}] — renderValue() invoked:`, x);

        if (!handlerRegistered && typeof Shiny !== 'undefined') {
          registerHandler(wid);
        }

        // Store the current spec for potential updates
        el.__lastSpec = x;

        try {
          await renderDeckGlView(el, x);
        } catch (err) {
          console.error(`[deckgl][${wid}] Deck.gl rendering failed:`, err);
          el.innerHTML = `<div style="color:red; padding:10px; font-family:sans-serif;">
            <h4>Deck.gl Rendering Error</h4>
            <p><strong>Message:</strong> ${err.message || err}</p>
          </div>`;
        }
      },
      resize: function(w, h) {
        const wid = widgetIdInstance;
        console.log(`[deckgl][${wid || 'unknown'}] — resize() called: ${w}×${h}.`);
        
        // If height is 0, ignore the resize to prevent assertion failures
        if (h === 0 || h === null || h === undefined) {
          console.warn(`[deckgl][${wid || 'unknown'}] Ignoring resize with height=0`);
          return;
        }
        
        const currentW = el.clientWidth || el.__lastWidth || w || 800;
        const currentH = el.clientHeight || el.__lastHeight || h || 600;

        const safeW = w && w > 0 ? w : currentW;
        const safeH = h && h > 0 ? h : currentH;

        // Apply to container to prevent collapse
        el.style.width = `${safeW}px`;
        el.style.height = `${safeH}px`;
        el.__lastWidth = safeW;
        el.__lastHeight = safeH;

        if (el.__deckInstance) {
          try {
            el.__deckInstance.setProps({ width: safeW, height: safeH });
          } catch (err) {
            console.warn(`[deckgl][${wid || 'unknown'}] Deck.gl resize failed:`, err);
          }
        }
      }
    };
    return widgetInstance;
  }
});
