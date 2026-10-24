//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Indirection: keeps `new URL(rel, base)` resolved at runtime instead of
// being rewritten by Vite's build-time asset analysis.
const MODULE_URL: string = import.meta.url;
/* eslint-disable @typescript-eslint/no-explicit-any */
import { Block, Helpers } from '../types.js';
//=============================================================================
const FMU_WIDTH = 96;
// Recompute the FMU block geometry: one output port per FMU Float64 output
// (params.numOutputs, filled by applyFmuInspection). No inputs are exposed yet.
export function fmuResizer(blockDef: any): void {
  const params = blockDef.params ?? {};
  const numOutputs = Math.max(1, parseInt(String(params.numOutputs ?? 1), 10) || 1);
  const numInputs = Math.max(0, parseInt(String(params.numInputs ?? 0), 10) || 0);
  const rows = Math.max(numOutputs, numInputs, 1);
  const height = Math.max(48, 20 * (rows + 1));
  blockDef.width = FMU_WIDTH;
  blockDef.height = height;
  // Ports sit on the block edge like every other block, with no lead stub: a stub would
  // offset the marker from the body and leave the wire crossing it.
  blockDef.dynamicInputs = Array.from({ length: numInputs }, (_, i) => ({
    x: 0,
    y: height / 2 + (i - (numInputs - 1) / 2) * 20,
    side: 'left',
  }));
  blockDef.dynamicOutputs = Array.from({ length: numOutputs }, (_, i) => ({
    x: FMU_WIDTH,
    y: height / 2 + (i - (numOutputs - 1) / 2) * 20,
    side: 'right',
  }));
}
//=============================================================================
export function fmuRenderer(block: Block, helpers: Helpers): void {
  const { createSvgElement } = helpers;
  const group = block.group as Element & { appendChild: (child: Node) => void };

  group.appendChild(
    createSvgElement!('rect', {
      x: 0,
      y: 0,
      width: block.width,
      height: block.height,
      rx: 6,
      class: 'nf-block',
      fill: '#eef4ff',
      stroke: '#2b6cb0',
      'stroke-width': 2,
    })
  );

  const inputs = Array.isArray((block as any).dynamicInputs) ? (block as any).dynamicInputs : [];
  const outputs = Array.isArray((block as any).dynamicOutputs) ? (block as any).dynamicOutputs : [];

  // Port labels: the FMU variable names, with the variable description as a
  // hover tooltip (<title>).
  const params = (block as any).params ?? {};
  const inNames: string[] = Array.isArray(params.inputNames) ? params.inputNames : [];
  const inDescs: string[] = Array.isArray(params.inputDescriptions) ? params.inputDescriptions : [];
  const outNames: string[] = Array.isArray(params.outputNames) ? params.outputNames : [];
  const outDescs: string[] = Array.isArray(params.outputDescriptions)
    ? params.outputDescriptions
    : [];
  const addPortLabel = (name: string, desc: string, x: number, y: number, anchor: string) => {
    if (!name) {
      return;
    }
    const label = createSvgElement!('text', {
      x,
      y: y + 3,
      'text-anchor': anchor,
      'font-size': 9,
      fill: '#4a5568',
      class: 'fmu-port-label',
    }) as Element & { appendChild: (n: Node) => void };
    label.appendChild(document.createTextNode(name));
    if (desc) {
      const title = createSvgElement!('title', {});
      title.appendChild(document.createTextNode(desc));
      label.appendChild(title);
    }
    group.appendChild(label);
  };
  inputs.forEach((port: any, i: number) =>
    addPortLabel(inNames[i], inDescs[i] ?? '', 5, port.y, 'start')
  );
  outputs.forEach((port: any, i: number) =>
    addPortLabel(outNames[i], outDescs[i] ?? '', block.width - 5, port.y, 'end')
  );

  // Prefer the FMU's own icon (model.png/model.svg embedded as a data URI by
  // the inspect step); otherwise show the "FMU" label.
  const iconUri = (block as any).params?.iconUri;
  if (typeof iconUri === 'string' && iconUri) {
    const pad = 10;
    group.appendChild(
      createSvgElement!('image', {
        href: iconUri,
        x: pad,
        y: pad,
        width: Math.max(0, block.width - 2 * pad),
        height: Math.max(0, block.height - 2 * pad),
        preserveAspectRatio: 'xMidYMid meet',
        class: 'fmu-model-icon',
      })
    );
  } else {
    group
      .appendChild(
        createSvgElement!('text', {
          x: block.width / 2,
          y: block.height / 2 + 5,
          'text-anchor': 'middle',
          'font-size': 14,
          'font-weight': 700,
          fill: '#2b6cb0',
          class: 'fmu-label',
        })
      )
      .appendChild(document.createTextNode('FMU'));
  }

  try {
    const iconUrl = new URL('./exports/fmu.svg', MODULE_URL).href;
    const size = 16;
    const img = createSvgElement!('image', {
      href: iconUrl,
      x: String(Math.round(block.width - size - 4)),
      y: String(4),
      width: String(size),
      height: String(size),
      class: 'block-icon',
    });
    group.appendChild(img);
  } catch {}
}
//=============================================================================
