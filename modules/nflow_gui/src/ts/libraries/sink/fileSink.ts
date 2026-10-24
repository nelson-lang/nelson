//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { Block, Helpers } from '../types.js';
//=============================================================================
export function fileSink(block: Block, helpers: Helpers): void {
  const { svgRect, createSvgElement } = helpers;
  const group = block.group;
  group.appendChild(svgRect!(0, 0, block.width, block.height, 'nf-block'));
  const iconW = 44,
    iconH = 54;
  const x = (block.width - iconW) / 2;
  const y = (block.height - iconH) / 2;
  const fold = 12;
  group.appendChild(
    createSvgElement!('path', {
      d: [
        `M${x} ${y}`,
        `H${x + iconW - fold}`,
        `L${x + iconW} ${y + fold}`,
        `V${y + iconH}`,
        `H${x}`,
        'Z',
      ].join(' '),
      class: 'file-icon-stroke',
    })
  );
  group.appendChild(
    createSvgElement!('polyline', {
      points: `${x + iconW - fold},${y} ${x + iconW - fold},${y + fold} ${x + iconW},${y + fold}`,
      class: 'file-icon-stroke',
    })
  );
  group.appendChild(
    createSvgElement!(
      'text',
      {
        x: block.width / 2,
        y: block.height / 2 + 4,
        class: 'file-icon-label upright',
        'text-anchor': 'middle',
      },
      'OUT'
    )
  );
}
//=============================================================================
export default fileSink;
//=============================================================================
