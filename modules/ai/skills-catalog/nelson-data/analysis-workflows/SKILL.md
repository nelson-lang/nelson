---
name: nelson-data-analysis-workflows
description: Build reproducible Nelson data workflows for loading files, validating shapes and missing values, computing summaries, creating plots, exporting artifacts, and returning concise numerical reports.
---

# Nelson Data Analysis Workflows

Create small, reproducible analysis scripts that separate data loading, computation, visualization, and reporting.

## When To Use

- User asks for a Nelson script that analyzes data.
- A task loads files, computes statistics, or produces plots.
- An MCP agent needs concise numerical summaries and artifact paths.
- A workflow should be rerunnable from a clean folder.
- A smoke test is needed for an analysis pipeline.

## When Not To Use

- User asks only for module internals: use `nelson-core-module-work`.
- User asks only for graphics behavior: use plotting guidelines or graphics module patterns.
- User asks to optimize an existing slow workflow: use `nelson-core-performance`.

## Workflow

1. Identify inputs, file formats, units, and expected shapes.
2. Validate file existence and key dimensions before computation.
3. Keep loading, cleaning, computation, plotting, and export in separate blocks or helpers.
4. Use existing table, statistics, plotting, and file I/O functions before adding new helpers.
5. Return concise summaries and artifact paths.
6. Add a small validation case or smoke test when the workflow is committed.

## Data Checks

| Check | Why It Matters |
| --- | --- |
| File exists | Fail early with a clear message |
| Shape matches expectation | Prevent silent dimension mistakes |
| Missing values are handled | Make summaries reproducible |
| Units are explicit | Avoid wrong interpretation |
| Column names are checked | Prevent fragile positional assumptions |
| Randomness is controlled | Keep outputs reproducible |

## Plotting Rules

- Create plots only when they answer a question.
- Label axes and units.
- Use stable output paths under the project or temp folder.
- Call drawing/export helpers explicitly when an agent must inspect an image.
- Return the image path instead of embedding large binary data in text.

## Reporting Rules

- Report counts, ranges, means, or key metrics in compact structs or text.
- Avoid dumping large arrays.
- Include assumptions that affect interpretation.
- Keep generated files under predictable names.
- Make rerun behavior clear: overwrite, version, or fail if output exists.

## Done When

- The workflow runs from a clean folder.
- Inputs and outputs are explicit.
- Data assumptions are validated.
- Summaries are concise.
- Any generated artifact path is returned or printed.
