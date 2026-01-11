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
