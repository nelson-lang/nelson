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
const utilityManifestDefs = buildRegistryFromManifest(utilityManifest as any).getDefinitions();
export const blockDefinitions = {
  comment: { render: comment },
  switch: { render: switchRenderer },
  toggleSwitch: { render: toggleSwitchRenderer },
  subsystem: { render: subsystem },
  mux: { render: muxRenderer, resize: muxResizer },
  demux: { render: demuxRenderer, resize: demuxResizer },
  multiportSwitch: {
    render: utilityManifestDefs['multiportSwitch']?.render,
    resize: multiportSwitchResizer,
  },
};
//=============================================================================
