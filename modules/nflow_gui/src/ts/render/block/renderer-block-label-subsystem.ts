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
import { renderTeXMath, createSvgElement } from '../svg-elements.js';
import { formatLabelTeX, computeLabelMathWidth } from './label.js';
import { computeSubsystemPortLabelFrame } from '../../libraries/utility/helpers.js';
import * as FieldNames from '../../FieldNames.js';
//=============================================================================
export function applySubsystemBlockLabel(block: any, ctx: any) {
  if (block.type === FieldNames.kLabelSource || block.type === FieldNames.kLabelSink) {
    const mathGroup = block.group.querySelector(
      '.label-math, .label-source-math, .nf-label-sink-formula'
    );
    if (mathGroup) {
      const tex = formatLabelTeX(block.params[FieldNames.kGotoTag] || '');
      const mathWidth = computeLabelMathWidth(tex, block.width);
      const offsetX = (block.width - mathWidth) / 2;
      mathGroup.setAttribute('transform', `translate(${offsetX},-24)`);
      renderTeXMath(mathGroup, tex, mathWidth, block.height);
    }
    if (block.type === FieldNames.kLabelSink) {
      const showNode = block.params[FieldNames.kShowNode] !== false;
      const circle = block.group.querySelector('circle.label-node');
      const line = block.group.querySelector('line.label-node');
      if (circle) circle.style.display = showNode ? 'block' : 'none';
      if (line) line.style.display = showNode ? 'block' : 'none';
    }
    return;
  }

  if (block.type !== FieldNames.kSubsystem) return;

  const title = block.group.querySelector('.block-text');
  if (title) {
    title.textContent = String(block.params?.name || 'Subsystem');
    title.setAttribute('x', String(block.width / 2));
    title.setAttribute('y', String(block.height / 2));
  }
  // Manifest-rendered subsystems carry a math group: refresh it on rename.
  try {
    const mathGroup =
      (block.group.querySelector('.nf-generated-subsystem-formula') as Element) ||
      (block.group.querySelector('.nf-block-formula') as Element) ||
      null;
    if (mathGroup) {
      const tex = formatLabelTeX(String(block.params?.name || 'Subsystem'));
      try {
        renderTeXMath(mathGroup, tex, block.width, block.height);
      } catch (err) {
        try {
          // eslint-disable-next-line no-console
          if (typeof console !== 'undefined' && typeof console.warn === 'function') {
          }
        } catch {}
      }
    }
  } catch (_e) {
    // ignore
  }
  const labelLayer = block.group.querySelector('.subsystem-port-labels');
  if (!labelLayer) return;
  while (labelLayer.firstChild) labelLayer.removeChild(labelLayer.firstChild);
  const inNames = Array.isArray(block.params?.externalInputs) ? block.params.externalInputs : [];
  const outNames = Array.isArray(block.params?.externalOutputs) ? block.params.externalOutputs : [];
  const inputPorts = block.ports
    .filter((p: any) => p.type === 'in')
    .sort((a: any, b: any) => a.index - b.index);
  const outputPorts = block.ports
    .filter((p: any) => p.type === 'out')
    .sort((a: any, b: any) => a.index - b.index);
  const labelScale = 0.8;
  const debugLines: string[] = [];
  inputPorts.forEach((port: any, idx: number) => {
    const name = String(inNames[idx]?.name || `in${idx + 1}`);
    const tex = formatLabelTeX(name);
    const frame = computeSubsystemPortLabelFrame({
      blockWidth: block.width,
      blockHeight: block.height,
      portY: port.y,
      side: 'left',
    });
    const mathGroup = createSvgElement('g', {
      class: 'label-math subsystem-port-label subsystem-port-label--left',
      transform: `translate(${frame.x},${frame.y})`,
    });
    mathGroup.dataset.scale = String(labelScale);
    labelLayer.appendChild(mathGroup);
    renderTeXMath(mathGroup, tex, frame.width, frame.height);
    debugLines.push(
      `in${idx + 1}: raw="${name}" tex="${tex}" box=[x:${frame.x}, y:${frame.y}, w:${
        frame.width
      }, h:${frame.height}, scale:${labelScale}]`
    );
  });
  outputPorts.forEach((port: any, idx: number) => {
    const name = String(outNames[idx]?.name || `out${idx + 1}`);
    const tex = formatLabelTeX(name);
    const frame = computeSubsystemPortLabelFrame({
      blockWidth: block.width,
      blockHeight: block.height,
      portY: port.y,
      side: 'right',
    });
    const mathGroup = createSvgElement('g', {
      class: 'label-math subsystem-port-label subsystem-port-label--right',
      transform: `translate(${frame.x},${frame.y})`,
    });
    mathGroup.dataset.scale = String(labelScale);
    labelLayer.appendChild(mathGroup);
    renderTeXMath(mathGroup, tex, frame.width, frame.height);
    debugLines.push(
      `out${idx + 1}: raw="${name}" tex="${tex}" box=[x:${frame.x}, y:${frame.y}, w:${
        frame.width
      }, h:${frame.height}, scale:${labelScale}]`
    );
  });
  if ((ctx as any).debugLog) {
    (ctx as any).debugLog.textContent = [
      `[subsystem-label-debug] block=${block.id} name="${String(
        block.params?.name || 'Subsystem'
      )}"`,
      ...debugLines,
    ].join('\n');
  }
}
//=============================================================================
