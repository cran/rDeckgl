# rDeckgl 0.2.0

* Never disconnect a caller-supplied DuckDB connection. A Shiny session used to
  close every connection it had seen when it ended, which killed the shared
  connection an app opens once at top level and broke every later session in the
  same R process. Only connections `deckgl()` opens itself are closed now.
* Convert Arrow values that have no direct JavaScript number in the row
  fallback. A DECIMAL column, which DuckDB produces for ordinary aggregates such
  as `SUM` over integers, reached the layer as a raw Arrow object that
  stringifies to "0" and throws on `valueOf()`. Convertible values become
  numbers, the rest become `NULL` with one warning naming the column and type.
* Report the real cause when a page saved with `selfcontained = TRUE` cannot
  resolve a file-transport payload, instead of rendering an empty layer.
* Scope an auto-created `data_dir` to the Shiny session and remove it when the
  session ends, rather than leaving one temporary directory per render.

* Ship `data_transport = "file"` payloads as an html dependency attachment of
  the widget, so the relative Arrow/Parquet URL resolves in the RStudio Viewer,
  in Shiny and after `htmlwidgets::saveWidget(selfcontained = FALSE)` into any
  directory, not only when the page is served from `data_dir`. `data_dir` now
  defaults to a session temporary directory.
* Refuse a DuckDB data node whose query holds more than one SQL statement. The
  `COPY` rung could not parse it and the record-batch rung would have executed
  every statement on the caller's connection and exported the last result.
* Skip binary attribute binding when a bound column contains NULL values, and
  read such columns element-wise in the row fallback: `toArray()` ignores the
  Arrow validity bitmap and would have rendered raw buffer bytes.

* Export `format = "arrow"` DuckDB data nodes straight from the database
  engine instead of materialising rows in R: `COPY (FORMAT ARROWS)` when the
  nanoarrow extension loads, otherwise Arrow record-batch streaming, otherwise
  `COPY (FORMAT PARQUET)`. File transport writes into `data_dir`; inline
  transport embeds the exported bytes. Nodes record the `__export_method`.
* Bind Arrow and Parquet payloads of standard layers such as ScatterplotLayer
  as deck.gl binary attributes for column-reference `getPosition`,
  `getFillColor` and `getRadius` accessors across all record batches. Other
  row-referencing accessors fall back to row objects with a console warning.

* Preallocate hydrated rows for ordinary atomic columns while retaining named
  assignment for nested, attributed and unusually named data.
* Use primitive column extraction and validation during plain-row export;
  validate scalar shapes, attributes and exact types before concatenation.
  Preserve editable rows, mixed logical/numeric fallbacks and serializer options.

* Avoid temporary one-row data frames for live Shiny JSON query responses with
  ordinary atomic columns. Preserve the existing row values and wire JSON;
  attributed, list, matrix and unusual data frames retain base R subsetting.
* Reuse validated prototype types and row attributes during static export,
  avoiding redundant per-cell type-set lookups and per-row attribute lists.
  Preserve mixed-type, missing-value, JavaScript and editability guards.

* Preserve JSON booleans in edited SQL rows mixing logical and numeric cells,
  using the original serializer when scalar types differ within a column.

* Encode plain SQL rows before htmlwidgets scans for JavaScript evaluations,
  avoiding a named traversal of every data cell. Preserve custom serializers.
* Avoid per-scalar data-frame method dispatch while constructing the same
  editable row lists, including nested and attributed column values.

* Serialize plain SQL-result rows through column vectors to avoid per-cell JSON
  dispatch. The emitted JSON and editable R row lists are preserved; nested,
  attributed and incompatible rows retain their existing serialization path.

* Preserve all polygon rows across Arrow record batches in the binary
  SolidPolygonLayer path. Ignore padded offsets and rebase sliced buffers;
  this fixes truncated geometry and allocation failures without changing APIs.
* Add browser-binding regression checks for single, multiple and sliced batches
  (`node tests/js/polygon-batches.cjs`).

# rDeckgl 0.1.0

## Initial Release

* Initial CRAN submission
* Core deck.gl visualization functionality
* DuckDB-backed data hydration
* Shiny integration with `deckglOutput()` and `renderDeckgl()`
* Support for JSON and YAML specifications
* Automatic format detection
* Examples for common deck.gl layer types

## Features

* **Visualization:** Full deck.gl 9.2.2 support via htmlwidgets
* **Data Backend:** Server-side SQL queries via DuckDB
* **Formats:** JSON, YAML, and R list specifications
* **Shiny:** Reactive bindings for interactive applications
* **Performance:** Efficient handling of large datasets (millions of rows)

## Documentation

* Added comprehensive README
* Created getting-started vignette
* Documented all exported functions
* Added examples for scatterplot, hexagon, and polygon layers
