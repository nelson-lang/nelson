//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { computeSubsystemPortLabelFrame } from './helpers.js';
import { Block, Helpers } from '../types.js';
//=============================================================================
export function subsystem(block: Block, helpers: Helpers): void {
  const { createSvgElement, renderTeXMath, formatLabelTeX } = helpers;
  const formatPortLabelTeX = (label: unknown) => {
    const text = String(label || '').trim();
    if (!text) return '';
    return typeof formatLabelTeX === 'function' ? formatLabelTeX(text) : text;
  };
  const group = block.group as Element & { appendChild: (child: Node) => void };
  const body = createSvgElement!('rect', {
    x: 0,
    y: 0,
    width: block.width,
    height: block.height,
    class: 'nf-block',
  });
  group.appendChild(body);
  const inNames = Array.isArray(block.params?.externalInputs) ? block.params.externalInputs : [];
  const outNames = Array.isArray(block.params?.externalOutputs) ? block.params.externalOutputs : [];
  const inYs = Array.isArray(block.dynamicInputs)
    ? block.dynamicInputs.map((p: { y: number }) => p.y)
    : [block.height / 2];
  const outYs = Array.isArray(block.dynamicOutputs)
    ? block.dynamicOutputs.map((p: { y: number }) => p.y)
    : [block.height / 2];
  const portLabelLayer = createSvgElement!('g', { class: 'subsystem-port-labels' });
  const labelScale = 0.8;
  inYs.forEach((y: number, idx: number) => {
    const name = String(inNames[idx]?.name || `in${idx + 1}`);
    const frame = computeSubsystemPortLabelFrame({
      blockWidth: block.width,
      blockHeight: block.height,
      portY: y,
      side: 'left',
    });
    const mg = createSvgElement!('g', {
      class: 'label-math subsystem-port-label subsystem-port-label--left',
      transform: `translate(${frame.x},${frame.y})`,
    });
    mg.dataset!.scale = String(labelScale);
    portLabelLayer.appendChild(mg);
    renderTeXMath!(mg, formatPortLabelTeX!(name), frame.width, frame.height);
  });
  outYs.forEach((y: number, idx: number) => {
    const name = String(outNames[idx]?.name || `out${idx + 1}`);
    const frame = computeSubsystemPortLabelFrame({
      blockWidth: block.width,
      blockHeight: block.height,
      portY: y,
      side: 'right',
    });
    const mg = createSvgElement!('g', {
      class: 'label-math subsystem-port-label subsystem-port-label--right',
      transform: `translate(${frame.x},${frame.y})`,
    });
    mg.dataset!.scale = String(labelScale);
    portLabelLayer.appendChild(mg);
    renderTeXMath!(mg, formatPortLabelTeX!(name), frame.width, frame.height);
  });
  group.appendChild(portLabelLayer);
  const name = String(block.params?.name || 'Subsystem');
  group.appendChild(
    createSvgElement!(
      'text',
      {
        x: block.width / 2,
        y: block.height / 2,
        class: 'block-text upright',
        'text-anchor': 'middle',
        'dominant-baseline': 'middle',
      },
      name
    )
  );

  // Lock icon shown when block.params.masked === true
  try {
    const isMasked = Boolean(
      (block.params && (block.params as Record<string, unknown>).masked) === true
    );
    const maskGroup = createSvgElement!('g', {
      class: 'masked-indicator',
      'data-visible': isMasked ? 'true' : 'false',
      transform: `translate(${Math.max(6, block.width - 24)}, 6)`,
    });

    // Unicode lock glyph in an SVG text element so it renders consistently
    maskGroup.appendChild(
      createSvgElement!(
        'text',
        {
          x: 7,
          y: 12,
          class: 'mask-lock-char',
        },
        '🔒'
      )
    );

    group.appendChild(maskGroup);
  } catch (_e) {}
}
//=============================================================================
export default subsystem;
//=============================================================================
