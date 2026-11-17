---
name: nelson-usage-plotting-graphics
audience: user
modules: graphics, graphics_io
description: Create and save figures in Nelson - line, scatter, bar, surface and image plots, with labels, legends, limits, and export. Use when the user wants to plot data or produce a figure file.
triggers: plot, tracer, trace, graphe, graph, graphique, figure, courbe, curve, nuage, scatter, barre, bar, histogramme, histogram, surface, surf, mesh, contour, image, imagesc, legende, legend, axe, axis, titre, title, couleur, color, colormap, exporter, export, saveas, subplot
functions: plot, plot3, figure, hold, xlabel, ylabel, title, legend, grid, axis, subplot, surf, mesh, contour, scatter, bar, imagesc, colorbar, colormap, xlim, ylim, semilogx, loglog, errorbar, stairs, stem, quiver, patch, fill, drawnow, saveas, print
---

# Plotting and Graphics

Produce a clear, labelled figure and, when asked, save it to a file.

## When to use
- Plotting data: lines, scatter, bars, surfaces, images, error bars.
- Adding labels/legend/limits, using subplots, or exporting a figure.

## When NOT to use
- Computing the data itself (use the analysis guide first).
- Building an interactive app window (use the app/UI guide).

## Key functions
2-D: `plot`, `scatter`, `bar`, `stairs`, `stem`, `errorbar`, `semilogx`, `loglog`, `quiver`, `fill`, `patch`. 3-D / fields: `plot3`, `surf`, `mesh`, `contour`, `imagesc`. Decoration: `xlabel`, `ylabel`, `title`, `legend`, `grid`, `axis`, `xlim`, `ylim`, `colorbar`, `colormap`. Layout/flow: `figure`, `subplot`, `hold`, `drawnow`. Export: `saveas`, `print`. (Signatures in the Verified Nelson API block.)

## Idioms & workflow
1. `figure()` to start a fresh window; `hold('on')` to overlay several series, `hold('off')` when done.
2. Always label: `xlabel`, `ylabel`, `title`, and `legend` when there is more than one series.
3. Set limits with `xlim`/`ylim` only when the auto range hides the point; otherwise leave them automatic.
4. For several panels use `subplot(r, c, k)`; keep each panel labelled.
5. To save, use `saveas(gcf(), path)` (or `print`), writing to a path under `tempdir()` in examples.

## Gotchas
- Without `hold('on')`, a second `plot` replaces the first.
- Vectors must share length; a column/row mismatch plots nothing or errors.
- A figure may need `drawnow` before a capture/export in a non-interactive run.

## Done when
The figure shows the data with axes labelled (and a legend for multiple series), and is saved to the requested file when export was asked.
