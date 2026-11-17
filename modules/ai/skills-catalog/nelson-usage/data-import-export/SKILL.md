---
name: nelson-usage-data-import-export
audience: user
modules: spreadsheet, json, hdf5, matio
description: Read and write data in Nelson - text/CSV, tables, MAT, HDF5, JSON, and plain files. Use when the user wants to load a file into Nelson or save results to disk.
triggers: csv, fichier, file, lire, read, write, ecrire, charger, load, enregistrer, save, import, export, table, readtable, writetable, json, hdf5, h5, mat, matfile, texte, text, colonne, column
functions: readtable, writetable, readmatrix, writematrix, readcell, readlines, writelines, csvread, csvwrite, dlmread, dlmwrite, fileread, filewrite, load, save, matfile, jsondecode, jsonencode, h5read, h5write, h5create
---

# Import and Export Data

Load a file into Nelson, or write results back to disk, with the right reader for the format.

## When to use
- Reading a CSV / delimited / text file, a MAT, HDF5, JSON, or a table.
- Writing results (a matrix, a table, variables) to a file.

## When NOT to use
- Pure number crunching with no file (use the analysis guide).
- Building a plot (use the plotting guide).

## Key functions
Tabular text: `readtable` / `writetable`, `readmatrix` / `writematrix`, `readcell`. Lines: `readlines` / `writelines`, `fileread` / `filewrite`. Delimited: `dlmread` / `dlmwrite`, `csvread` / `csvwrite`. Binary: `load` / `save`, `matfile`, `h5read` / `h5write` / `h5create`. JSON: `jsondecode` / `jsonencode`. (Signatures are in the Verified Nelson API block.)

## Idioms & workflow
1. Pick the reader by format: a header-and-columns file reads cleanly with `readtable`; a pure numeric grid with `readmatrix`; mixed content with `readcell`.
2. Check existence first (`isfile(path)`) and fail with a clear message rather than letting a low-level error surface.
3. For a big MAT file, open it with `matfile(path)` and read only the variables you need instead of `load`-ing everything.
4. Prefer a path under `tempdir()` for generated outputs in examples.
5. Report shape after loading (`size`) so the user can confirm the import.

## Gotchas
- A character array uses single quotes `'...'`; a string uses double quotes `"..."` - some writers treat them differently.
- `save` without a variable list writes the whole workspace; name the variables to keep the file small.
- JSON round-trips: `jsondecode(jsonencode(x))` may change a struct/cell shape; verify before relying on it.

## Done when
The file is read into a variable of the expected shape (or written and re-read to confirm), with paths explicit and existence checked.
