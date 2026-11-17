---
name: nelson-usage-data-analysis-statistics
audience: user
modules: statistics, data_analysis, table
description: Summarize and analyze data in Nelson - statistics, grouping, tables and categoricals, missing-value handling. Use when the user wants means, spreads, correlations, group summaries, or to clean data.
triggers: moyenne, mean, mediane, median, ecart, std, variance, var, statistique, statistics, correlation, covariance, quantile, percentile, histogramme, histogram, groupe, group, table, categorical, categorie, manquant, missing, nan, resume, summary, analyse, analysis, tendance, moving
functions: mean, median, std, var, mode, min, max, sum, cumsum, sort, sortrows, unique, quantile, prctile, corrcoef, cov, movmean, histogram, histc, table, array2table, struct2table, categorical, summary, groupsummary, varfun, fillmissing, rmmissing, ismissing, isnan
---

# Analyze Data

Turn a dataset into summaries, correlations, or a cleaned table - with the operation applied along the right dimension.

## When to use
- Means, spreads, quantiles, correlations, histograms of numeric data.
- Grouping / per-category summaries, or building and querying a `table`.
- Cleaning missing values before computing.

## When NOT to use
- Only loading or saving a file (use the import/export guide).
- Linear-algebra solves or decompositions (use the linear-algebra guide).

## Key functions
Summaries: `mean`, `median`, `std`, `var`, `mode`, `min`, `max`, `sum`, `cumsum`, `quantile`, `prctile`. Relationships: `corrcoef`, `cov`. Moving: `movmean`. Tables: `table`, `array2table`, `struct2table`, `summary`, `groupsummary`, `varfun`. Categoricals: `categorical`. Missing: `fillmissing`, `rmmissing`, `ismissing`, `isnan`. (Signatures in the Verified Nelson API block.)

## Idioms & workflow
1. State the dimension: `mean(A, 1)` reduces columns, `mean(A, 2)` rows. Never rely on the default when the shape matters.
2. Handle missing values deliberately: drop with `rmmissing`, or impute with `fillmissing`, before a summary - and say which you did.
3. For per-group results, prefer a `table` + `groupsummary` / `varfun` over hand-written loops.
4. Use `categorical` for a fixed set of labels so counts and groupings are stable.
5. Return compact results (a struct or small table), not a dump of large arrays.

## Gotchas
- A `NaN` propagates through `mean`/`sum`; clean first or the result is `NaN`.
- `std`/`var` use the sample normalization by default; pass the flag when you need the population form.
- Indexing is 1-based; `A(end,:)` is the last row.

## Done when
The requested statistic or summary is computed along the correct dimension, missing values are handled explicitly, and the result is reported compactly.
