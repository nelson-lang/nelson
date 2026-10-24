//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { UiBlock, Helpers } from '../types.js';
//=============================================================================
export function scope(block: UiBlock, helpers: Helpers): void {
  const { svgRect, createSvgElement, svgText } = helpers;
  const group = block.group;
  const body = svgRect!(0, 0, block.width, block.height, 'nf-block');
  group.appendChild(body);
  const title = svgText!(10, 20, 'Scope');
  group.appendChild(title);
  block.bodyRect = body;
  block.scopeTitle = title;
  // Initialize the top-left type label to include the configured block name
  // so the title is visible even before the simulation runs.
  try {
    const showTitle = (block.params && (block.params.showTitle as boolean | undefined)) !== false;
    const nameText = String(
      (block.params && (block.params.name as string | undefined)) || ''
    ).trim();
    const combined = nameText && showTitle ? `Scope - ${nameText}` : 'Scope';
    try {
      title.textContent = combined;
      title.setAttribute('display', 'block');
    } catch (e) {}
  } catch (e) {}
  const plotHeight = block.height - 40;
  const plot = svgRect!(10, 30, block.width - 20, plotHeight, 'scope-plot');
  group.appendChild(plot);
  // Apply the saved backgroundColor so loaded diagrams show it immediately.
  try {
    const bg = (block.params && (block.params.backgroundColor as string | undefined)) || undefined;
    if (bg && String(bg).trim()) {
      try {
        try {
          (plot as SVGElement).style.setProperty('fill', String(bg), 'important');
        } catch (e) {}
      } catch (e) {}
      try {
        plot.setAttribute('fill', String(bg));
      } catch (e) {}
    }
  } catch (e) {}
  const defs = createSvgElement!('defs');
  const clipId = `scope-clip-${block.id}`;
  const clipRect = createSvgElement!('rect', {
    x: String(plot.getAttribute('x') ?? ''),
    y: String(plot.getAttribute('y') ?? ''),
    width: String(plot.getAttribute('width') ?? ''),
    height: String(plot.getAttribute('height') ?? ''),
  });
  const clipPath = createSvgElement!('clipPath', { id: clipId });
  clipPath.appendChild(clipRect);
  defs.appendChild(clipPath);
  group.appendChild(defs);

  const gridLinesGroup = createSvgElement!('g', {
    class: 'scope-grid-lines',
    'clip-path': `url(#${clipId})`,
  });
  const xGridLines = Array.from({ length: 9 }, () =>
    createSvgElement!('line', { class: 'scope-grid-line', display: 'none' })
  );
  const yGridLines = Array.from({ length: 9 }, () =>
    createSvgElement!('line', { class: 'scope-grid-line', display: 'none' })
  );
  xGridLines.forEach((l) => gridLinesGroup.appendChild(l));
  yGridLines.forEach((l) => gridLinesGroup.appendChild(l));
  group.appendChild(gridLinesGroup);

  const axesGroup = createSvgElement!('g', { class: 'scope-axes' });
  const xAxis = createSvgElement!('line', { class: 'scope-axis' });
  const yAxis = createSvgElement!('line', { class: 'scope-axis' });
  axesGroup.appendChild(xAxis);
  axesGroup.appendChild(yAxis);
  const xTicks = Array.from({ length: 9 }, () =>
    createSvgElement!('line', { class: 'scope-tick' })
  );
  const yTicks = Array.from({ length: 9 }, () =>
    createSvgElement!('line', { class: 'scope-tick' })
  );
  const xTickLabels = Array.from({ length: 9 }, () =>
    createSvgElement!('text', { class: 'scope-tick-label scope-tick-label-x' })
  );
  const yTickLabels = Array.from({ length: 9 }, () =>
    createSvgElement!('text', { class: 'scope-tick-label scope-tick-label-y' })
  );
  xTicks.forEach((t) => axesGroup.appendChild(t));
  yTicks.forEach((t) => axesGroup.appendChild(t));
  xTickLabels.forEach((l) => axesGroup.appendChild(l));
  yTickLabels.forEach((l) => axesGroup.appendChild(l));
  group.appendChild(axesGroup);
  const pathsGroup = createSvgElement!('g', {
    class: 'scope-paths',
    'clip-path': `url(#${clipId})`,
  });
  group.appendChild(pathsGroup);
  const colors = ['scope-path-1', 'scope-path-2', 'scope-path-3'];
  block.scopePaths = colors.map((cls: string) => {
    const p = createSvgElement!('path', { class: `scope-path ${cls}` });
    pathsGroup.appendChild(p);
    return p;
  });
  block.scopePlot = plot;
  block.scopeAxes = { xAxis, yAxis, xTicks, yTicks, xTickLabels, yTickLabels };
  block.scopeClipRect = clipRect;
  block.scopeGridLines = { xGridLines, yGridLines };

  const xLabelEl = createSvgElement!('text', {
    class: 'scope-axis-label scope-xlabel',
    display: 'none',
  });
  const yLabelEl = createSvgElement!('text', {
    class: 'scope-axis-label scope-ylabel',
    display: 'none',
  });
  group.appendChild(xLabelEl);
  group.appendChild(yLabelEl);
  block.scopeAxisLabels = { xLabel: xLabelEl, yLabel: yLabelEl };

  const legendGroup = createSvgElement!('g', { class: 'scope-legend' });
  const legendItems = Array.from({ length: 3 }, () =>
    createSvgElement!('text', { class: 'scope-legend-item', display: 'none' })
  );
  legendItems.forEach((el) => legendGroup.appendChild(el));
  group.appendChild(legendGroup);
  block.scopeLegendItems = legendItems;

  const blockTitleEl = createSvgElement!('text', {
    class: 'scope-block-title',
    x: String(block.width / 2),
    y: '28',
    display: 'none',
  });
  group.appendChild(blockTitleEl);
  block.scopeBlockTitle = blockTitleEl;

  const hintColors = ['#f6d63b', '#d35cff', '#35d1ff'];
  block.scopeInputHints = hintColors.map((color: string) => {
    const h = createSvgElement!('circle', {
      cx: 8,
      cy: 40,
      r: 3,
      fill: color,
      class: 'scope-input-hint',
    });
    group.appendChild(h);
    return h;
  });
}
//=============================================================================
export default scope;
//=============================================================================
