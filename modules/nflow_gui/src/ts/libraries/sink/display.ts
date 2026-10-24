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
export function display(block: UiBlock, helpers: Helpers): void {
  const { svgRect, createSvgElement } = helpers;
  const group = block.group;

  const body = svgRect!(0, 0, block.width, block.height, 'nf-block');
  group.appendChild(body);
  block.bodyRect = body;

  group.appendChild(
    createSvgElement!('rect', {
      x: 0,
      y: 0,
      width: block.width,
      height: 16,
      class: 'display-title-bar',
    })
  );

  const titleText = createSvgElement!(
    'text',
    {
      x: block.width / 2,
      y: 11,
      class: 'display-title',
      'text-anchor': 'middle',
    },
    String(block.params?.Label ?? 'Display')
  );
  group.appendChild(titleText);
  block.displayTitleEl = titleText;

  const valueArea = svgRect!(4, 20, block.width - 8, block.height - 24, 'display-value-area');
  group.appendChild(valueArea);

  const valueText = createSvgElement!(
    'text',
    {
      x: block.width / 2,
      y: 20 + Math.floor((block.height - 24) / 2) + 5,
      class: 'display-value',
      'text-anchor': 'middle',
    },
    '--'
  );
  group.appendChild(valueText);
  block.displayValueEl = valueText;
}
//=============================================================================
export default display;
//=============================================================================
