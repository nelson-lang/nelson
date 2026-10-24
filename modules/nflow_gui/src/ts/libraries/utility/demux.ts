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
const DEMUX_WIDTH = 44;
export function demuxRenderer(block: Block, helpers: Helpers): void {
  const { createSvgElement } = helpers;
  const group = block.group as Element & { appendChild: (child: Node) => void };

  group.appendChild(
    createSvgElement!('rect', {
      x: 0,
      y: 0,
      width: block.width,
      height: block.height,
      class: 'nf-block',
      fill: '#000000',
    })
  );

  group.appendChild(
    createSvgElement!('line', {
      x1: -16,
      y1: block.height / 2,
      x2: 0,
      y2: block.height / 2,
      class: 'demux-stub',
      stroke: '#444441',
      'stroke-width': 1.5,
    })
  );

  const outputs = Array.isArray((block as any).dynamicOutputs) ? (block as any).dynamicOutputs : [];
  for (const port of outputs) {
    group.appendChild(
      createSvgElement!('line', {
        x1: block.width,
        y1: port.y,
        x2: block.width + 16,
        y2: port.y,
        class: 'demux-stub',
        stroke: '#444441',
        'stroke-width': 1.5,
      })
    );
  }

  try {
    const iconUrl = new URL('./exports/demux.svg', MODULE_URL).href;
    const img = createSvgElement!('image', {
      href: iconUrl,
      x: String(
        Math.round(
          (block.width - Math.min(18, Math.floor(Math.min(block.width, block.height) - 8))) / 2
        )
      ),
      y: String(
        Math.round(
          (block.height - Math.min(18, Math.floor(Math.min(block.width, block.height) - 8))) / 2
        )
      ),
      width: String(Math.min(18, Math.floor(Math.min(block.width, block.height) - 8))),
      height: String(Math.min(18, Math.floor(Math.min(block.width, block.height) - 8))),
      class: 'block-icon',
    });
    group.appendChild(img);
  } catch {}
}
//=============================================================================
export function demuxResizer(blockDef: any): void {
  const params = blockDef.params ?? {};
  const numOutputs = Math.max(2, parseInt(String(params.Outputs ?? 2), 10) || 2);
  const height = Math.max(40, numOutputs * 20);
  blockDef.width = DEMUX_WIDTH;
  blockDef.height = height;
  blockDef.dynamicInputs = [{ x: 0, y: height / 2, side: 'left' }];
  blockDef.dynamicOutputs = Array.from({ length: numOutputs }, (_, i) => ({
    x: DEMUX_WIDTH,
    y: (i + 0.5) * (height / numOutputs),
    side: 'right',
  }));
}
//=============================================================================
