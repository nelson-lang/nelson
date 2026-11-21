//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { comment } from './comment.js';
import { switchRenderer } from './switch.js';
import { toggleSwitchRenderer } from './toggle-switch.js';
import { subsystem } from './subsystem.js';
import { muxRenderer, muxResizer } from './mux.js';
import { demuxRenderer, demuxResizer } from './demux.js';
import utilityManifest from '../../../../../nflow_blocks/libraries/utility/library.json';
import { buildRegistryFromManifest } from '../generic-block-renderer.js';
import type { LibraryManifest } from '../block-descriptor.js';
//=============================================================================
// multiportSwitch: control input on top (port 0), N data inputs on the left.
// eslint-disable-next-line @typescript-eslint/no-explicit-any
export function multiportSwitchResizer(blockDef: any): void {
  const n = Math.max(1, parseInt(String(blockDef.params?.DataPortCount ?? 3), 10) || 3);
  const width = 40;
  // Data ports on the shared 20 px pitch, centred on the block.
  const height = Math.max(60, 20 * (n + 1));
  blockDef.width = width;
  blockDef.height = height;
  const data = Array.from({ length: n }, (_, i) => ({
    x: 0,
    y: height / 2 + (i - (n - 1) / 2) * 20,
    side: 'left',
  }));
  blockDef.dynamicInputs = [{ x: width / 2, y: 0, side: 'top' }, ...data];
  blockDef.dynamicOutputs = [{ x: width, y: height / 2, side: 'right' }];
}
//=============================================================================
// eslint-disable-next-line @typescript-eslint/no-explicit-any
export function busCreatorResizer(block: any): void {
  const names = Array.isArray(block.params?.MemberNames) ? block.params.MemberNames.length : 0;
  const n = Math.max(2, Number.parseInt(String(block.params?.Inputs ?? names), 10) || 2);
  const width = Math.max(40, Number(block.params?.width) || 80);
  const height = Math.max(n * 16, Number(block.params?.height) || Math.max(70, n * 20));
  block.width = width;
  block.height = height;
  block.dynamicInputs = Array.from({ length: n }, (_, i) => ({
    x: 0,
    y: ((i + 0.5) * height) / n,
    side: 'left',
  }));
  block.dynamicOutputs = [{ x: width, y: height / 2, side: 'right' }];
}
//=============================================================================
// eslint-disable-next-line @typescript-eslint/no-explicit-any
export function busSelectorResizer(block: any): void {
  const paths = Array.isArray(block.params?.SelectedSignals) ? block.params.SelectedSignals : [];
  const n = block.params?.OutputAsBus ? 1 : Math.max(1, paths.length);
  const width = Math.max(40, Number(block.params?.width) || 85);
  const height = Math.max(n * 16, Number(block.params?.height) || Math.max(70, n * 20));
  block.width = width;
  block.height = height;
  block.dynamicInputs = [{ x: 0, y: height / 2, side: 'left' }];
  block.dynamicOutputs = Array.from({ length: n }, (_, i) => ({
    x: width,
    y: ((i + 0.5) * height) / n,
    side: 'right',
  }));
}
//=============================================================================
const utilityManifestDefs = buildRegistryFromManifest({
  ...utilityManifest,
  _dir: 'utility',
} as LibraryManifest).getDefinitions();
export const blockDefinitions = {
  comment: { render: comment },
  switch: { render: switchRenderer },
  toggleSwitch: { render: toggleSwitchRenderer },
  subsystem: { render: subsystem },
  mux: { render: muxRenderer, resize: muxResizer },
  demux: { render: demuxRenderer, resize: demuxResizer },
  busCreator: { render: utilityManifestDefs['busCreator']?.render, resize: busCreatorResizer },
  busSelector: { render: utilityManifestDefs['busSelector']?.render, resize: busSelectorResizer },
  multiportSwitch: {
    render: utilityManifestDefs['multiportSwitch']?.render,
    resize: multiportSwitchResizer,
  },
};
//=============================================================================
