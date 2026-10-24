//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import { createSvgElement } from '../svg-elements.js';
import * as FieldNames from '../../FieldNames.js';
//=============================================================================
export function svgRect(x: number, y: number, w: number, h: number, cls: string) {
  return createSvgElement('rect', { x, y, width: w, height: h, class: cls });
}
//=============================================================================
export function svgText(x: number, y: number, text: string) {
  return createSvgElement('text', { x, y, class: 'block-text upright' }, text);
}
//=============================================================================
export function renderSourcePlot(group: any, width: number, height: number, plotPath: any) {
  group.appendChild(svgRect(0, 0, width, height, 'nf-block'));
  const axisX = 14;
  const axisY = height - 16;
  const axisTop = 14;
  const axisRight = width - 14;
  group.appendChild(
    createSvgElement('line', {
      x1: axisX,
      y1: axisY,
      x2: axisRight,
      y2: axisY,
      class: 'source-axis',
      stroke: '#3c3c3c',
      'stroke-width': 2,
      fill: 'none',
    })
  );
  group.appendChild(
    createSvgElement('line', {
      x1: axisRight,
      y1: axisY,
      x2: axisRight - 6,
      y2: axisY - 3,
      class: 'source-axis',
      stroke: '#3c3c3c',
      'stroke-width': 2,
      fill: 'none',
    })
  );
  group.appendChild(
    createSvgElement('line', {
      x1: axisRight,
      y1: axisY,
      x2: axisRight - 6,
      y2: axisY + 3,
      class: 'source-axis',
      stroke: '#3c3c3c',
      'stroke-width': 2,
      fill: 'none',
    })
  );
  group.appendChild(
    createSvgElement('line', {
      x1: axisX,
      y1: axisY,
      x2: axisX,
      y2: axisTop,
      class: 'source-axis',
      stroke: '#3c3c3c',
      'stroke-width': 2,
      fill: 'none',
    })
  );
  group.appendChild(
    createSvgElement('line', {
      x1: axisX,
      y1: axisTop,
      x2: axisX - 3,
      y2: axisTop + 6,
      class: 'source-axis',
      stroke: '#3c3c3c',
      'stroke-width': 2,
      fill: 'none',
    })
  );
  group.appendChild(
    createSvgElement('line', {
      x1: axisX,
      y1: axisTop,
      x2: axisX + 3,
      y2: axisTop + 6,
      class: 'source-axis',
      stroke: '#3c3c3c',
      'stroke-width': 2,
      fill: 'none',
    })
  );
  group.appendChild(
    createSvgElement('path', {
      d: plotPath,
      class: 'source-plot',
      stroke: '#3c3c3c',
      'stroke-width': 2,
      fill: 'none',
    })
  );
}
//=============================================================================
export function renderCenteredAxesPlot(group: any, width: number, height: number, plotPath: any) {
  group.appendChild(svgRect(0, 0, width, height, 'nf-block'));
  const axisLeft = 14;
  const axisRight = width - 14;
  const axisTop = 14;
  const axisBottom = height - 14;
  const midX = (axisLeft + axisRight) / 2;
  const midY = (axisTop + axisBottom) / 2;
  group.appendChild(
    createSvgElement('line', {
      x1: axisLeft,
      y1: midY,
      x2: axisRight,
      y2: midY,
      class: 'source-axis',
      stroke: '#3c3c3c',
      'stroke-width': 2,
      fill: 'none',
    })
  );
  group.appendChild(
    createSvgElement('line', {
      x1: axisRight,
      y1: midY,
      x2: axisRight - 6,
      y2: midY - 3,
      class: 'source-axis',
      stroke: '#3c3c3c',
      'stroke-width': 2,
      fill: 'none',
    })
  );
  group.appendChild(
    createSvgElement('line', {
      x1: axisRight,
      y1: midY,
      x2: axisRight - 6,
      y2: midY + 3,
      class: 'source-axis',
      stroke: '#3c3c3c',
      'stroke-width': 2,
      fill: 'none',
    })
  );
  group.appendChild(
    createSvgElement('line', {
      x1: midX,
      y1: axisBottom,
      x2: midX,
      y2: axisTop,
      class: 'source-axis',
      stroke: '#3c3c3c',
      'stroke-width': 2,
      fill: 'none',
    })
  );
  group.appendChild(
    createSvgElement('line', {
      x1: midX,
      y1: axisTop,
      x2: midX - 3,
      y2: axisTop + 6,
      class: 'source-axis',
      stroke: '#3c3c3c',
      'stroke-width': 2,
      fill: 'none',
    })
  );
  group.appendChild(
    createSvgElement('line', {
      x1: midX,
      y1: axisTop,
      x2: midX + 3,
      y2: axisTop + 6,
      class: 'source-axis',
      stroke: '#3c3c3c',
      'stroke-width': 2,
      fill: 'none',
    })
  );
  if (!plotPath) return;
  const paths = Array.isArray(plotPath) ? plotPath : [plotPath];
  paths.forEach((d) => {
    group.appendChild(
      createSvgElement('path', {
        d,
        class: 'source-plot',
        stroke: '#3c3c3c',
        'stroke-width': 2,
        fill: 'none',
      })
    );
  });
}
//=============================================================================
export function renderRectBlock(
  block: any,
  title: string,
  lines: string[] = [],
  iconType: any = null
) {
  const group = block.group;
  group.appendChild(svgRect(0, 0, block.width, block.height, 'nf-block'));
  if (iconType) {
    const icon = drawIcon(iconType, block.width - 28, 8);
    if (icon) group.appendChild(icon);
  }
  group.appendChild(svgText(10, 22, title));
  lines.forEach((line, index) => {
    group.appendChild(svgText(10, 42 + index * 16, line));
  });
}
//=============================================================================
export function drawIcon(type: string, x: number, y: number, sizeOverride: number | null = null) {
  const g = createSvgElement('g', {
    class: 'block-icon upright',
    transform: `translate(${x}, ${y})`,
  });
  const size = sizeOverride ?? 22;
  const mid = size / 2;

  const appendLine = (x1: number, y1: number, x2: number, y2: number) =>
    g.appendChild(createSvgElement('line', { x1, y1, x2, y2 }));
  const appendPath = (d: string) =>
    g.appendChild(
      createSvgElement('path', {
        d,
        class: 'source-plot',
        stroke: '#3c3c3c',
        'stroke-width': 2,
        fill: 'none',
      })
    );
  const appendPoly = (pts: string) => g.appendChild(createSvgElement('polyline', { points: pts }));
  const appendRect = (rx: number, ry: number, w: number, h: number) =>
    g.appendChild(createSvgElement('rect', { x: rx, y: ry, width: w, height: h }));
  const addText = (txt: string) => g.appendChild(createSvgElement('text', { x: mid, y: mid }, txt));
  const addMath = (tex: string) => {
    const foreign = createSvgElement('foreignObject', {
      x: 0,
      y: 0,
      width: size,
      height: size,
      class: 'upright',
    });
    const div = document.createElement('div');
    div.setAttribute('xmlns', 'http://www.w3.org/1999/xhtml');
    div.className = 'icon-math';
    const span = document.createElement('span');
    span.className = 'mathjax-tex';
    span.textContent = `\\(${tex}\\)`;
    div.appendChild(span);
    foreign.appendChild(div);
    g.appendChild(foreign);
    (globalThis as any).queueMathJaxTypeset?.();
  };

  const renderers: Record<string, () => void> = {
    constant: () => appendLine(2, mid, size - 2, mid),
    step: () => appendPath(`M2 ${size - 4} H${mid} V4 H${size - 2}`),
    ramp: () => appendLine(2, size - 4, size - 2, 4),
    impulse: () => appendLine(mid, size - 2, mid, 2),
    sine: () =>
      appendPath(
        `M2 ${mid} C6 4, 10 4, 14 ${mid} C18 ${size - 4}, 20 ${size - 4}, ${size - 2} ${mid}`
      ),
    chirp: () =>
      appendPoly(
        `2 ${mid} 6 10 10 ${mid} 12 10 14 ${mid} 15 10 16 ${mid} 17 10 18 ${mid} 20 10 22 ${mid}`
      ),
    noise: () => appendPoly(`2 12 6 8 10 14 14 6 18 16 22 10`),
    file: () => {
      appendRect(4, 2, 14, 18);
      g.appendChild(createSvgElement('polyline', { points: `14 2 18 6 14 6` }));
    },
    scope: () => appendPoly(`2 16 6 10 10 14 14 6 18 12 22 8`),
    saturation: () => appendPath(`M2 ${size - 4} H8 L14 6 H20`),
    rate: () => appendPath(`M2 ${size - 4} L12 6 H20`),
    backlash: () => appendPoly(`2 16 8 16 10 10 16 10 18 4`),
    lpf: () => appendPath(`M2 6 H10 V16 H20`),
    hpf: () => appendPath(`M2 16 H10 V6 H20`),
    pid: () => addText('PID'),
    tf: () => addText('TF'),
    dtf: () => addMath('z^{-1}'),
    zoh: () => addText('ZOH'),
    foh: () => addText('FOH'),
    integrator: () => addText('1/s'),
    derivative: () => addText('d/dt'),
    ddelay: () => addMath('z^{-1}'),
    stateSpace: () => addText('SS'),
    dstateSpace: () => addText('SSd'),
  };

  // mux/demux need bespoke handling
  if (type === 'mux') {
    const s = size;
    const m = s / 2;
    const h = Math.max(4, Math.floor(s * 0.25));
    const left = 3;
    const right = s - 3;
    g.appendChild(
      createSvgElement('polygon', { points: `${left} ${m - h} ${left} ${m + h} ${right} ${m}` })
    );
    const gap = Math.max(2, Math.floor(h / 1.2));
    const inYs = [m - gap, m, m + gap];
    for (const yy of inYs) {
      g.appendChild(createSvgElement('line', { x1: 0, y1: yy, x2: left + 2, y2: yy }));
    }
  } else if (type === 'demux') {
    const s = size;
    const m = s / 2;
    const h = Math.max(4, Math.floor(s * 0.25));
    const left = 3;
    const right = s - 3;
    g.appendChild(
      createSvgElement('polygon', { points: `${right} ${m - h} ${right} ${m + h} ${left} ${m}` })
    );
    const gap = Math.max(2, Math.floor(h / 1.2));
    const outYs = [m - gap, m, m + gap];
    for (const yy of outYs) {
      g.appendChild(createSvgElement('line', { x1: right - 2, y1: yy, x2: s, y2: yy }));
    }
  } else if (type === FieldNames.kDelay) {
    addText('e^-sT');
  } else {
    const fn = renderers[type];
    if (fn) fn();
    else addText('fx');
  }
  return g;
}
//=============================================================================
