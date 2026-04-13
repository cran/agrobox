# agrobox 0.3.0

## New features

- Added support for combined ordering and relabeling through:
  - `orden_factor`
  - `grupo1_orden`
  - `grupo2_orden`
  
  These arguments now accept both named and unnamed vectors:
  - Unnamed vectors reorder levels.
  - Named vectors reorder and relabel simultaneously.

- Added two real-world datasets:
  - `nitrogeno_liberacion`: nitrogen release dynamics from fertilizers.
  - `pimiento_hibridacion`: hybrid seed production dynamics in pepper.

- Added new outputs to `agrobox()`:
  - `$data`: summarized dataset used in the analysis (means, sd, n).
  - `$stats`: ANOVA diagnostics per cluster (p-values, CV, Power).

## Improvements

- Faceting now correctly respects factor level order defined by:
  - `grupo1_orden`
  - `grupo2_orden`
  
  This resolves previous inconsistencies with `facet_grid()` ordering.

- Improved handling of factor levels across all internal steps:
  - Consistent propagation of levels from raw data → analysis → plotting.
  - Prevents unintended reordering in ggplot facets.

- Cluster construction is now more robust:
  - Uses `interaction()` when both grouping variables are present.
  - Preserves factor levels in all grouping scenarios.

- Enhanced statistical robustness:
  - Improved handling of missing values (NA) in post-hoc comparisons.
  - Automatic suppression of letters when statistical assumptions fail.
  
- Improved compatibility with ggplot2 faceting behavior by enforcing factor levels prior to plotting.

## Internal changes

- Refactored factor handling through `aplicar_orden_labels()`:
  - Unified logic for ordering and relabeling across all variables.

- Reworked cluster generation logic to support:
  - Single grouping
  - Dual grouping
  - No grouping (single panel)

- Improved reconstruction of facet variables for `geom_text()`:
  - Ensures correct placement of labels within panels.

- Cleaner separation between:
  - data preprocessing
  - statistical analysis
  - visualization
  - reporting

---

# agrobox 0.2.1

## Minor fixes

- Resubmission to CRAN after incoming checks.
- Moved `magick`, `kableExtra`, and `tinytex` from Imports to Suggests.
- No user-facing changes.
---

# agrobox 0.2.0

## New features

- Added `agrosintesis()`, a high-level wrapper to automate multi-variable ANOVA
  summaries using `agrobox()` as the core engine.
  - Supports multiple response variables.
  - Supports optional clustering through a formula interface
    (e.g. `Variedad ~ Localidad`).
  - Automatically iterates over cluster combinations and returns
    publication-ready summary tables.
  - When `estructura = NULL`, the analysis is performed globally
    (no clustering).

- Added `agroexcel()` to export agrobox and agrosintesis results
  to Excel (`.xlsx`) files.
  - Works with single tables and lists of tables.
  - Automatically creates one worksheet per cluster.
  - Sheet names are sanitized to comply with Excel and Windows limitations.

- Added `agrotabla()` to export result tables as high-resolution PNG images.
  - Uses LaTeX for high-quality table rendering.
  - Supports both single tables and lists of tables.
  - File names are sanitized for LaTeX and filesystem compatibility.
  - Designed for fast reporting and presentation-ready outputs.

## Improvements

- Cluster labels now preserve original factor levels instead of numeric codes.
- Improved handling of factor levels when clustering is enabled.
- Column names are automatically escaped to avoid LaTeX compilation issues.
- File and sheet name sanitization improves Windows compatibility.

## Internal changes

- Refactored reporting logic to separate analysis from export utilities.
- Avoided side effects in core analytical functions.
- Improved CRAN compliance by:
  - Using fully qualified namespace calls.
  - Avoiding `library()` calls inside functions.
  - Wrapping file-generating examples in `\dontrun{}`.

---

# agrobox 0.1.1

## Changes

- Improved main function performance and robustness.
