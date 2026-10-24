//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import * as FieldNames from '../../FieldNames.js';
//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
export const SUBSYSTEM_PORT_LABEL_EDGE_PAD = 6;
export const computeSubsystemPortLabelFrame = ({
  blockWidth,
  blockHeight,
  portY,
  side,
}: {
  blockWidth: number;
  blockHeight: number;
  portY: number;
  side: 'left' | 'right';
}) => {
  const width = Math.max(48, Math.floor(blockWidth * 0.36));
  const height = blockHeight;
  const x =
    side === 'right'
      ? blockWidth - width - SUBSYSTEM_PORT_LABEL_EDGE_PAD
      : SUBSYSTEM_PORT_LABEL_EDGE_PAD;
  const y = Math.round(portY - height / 2);
  return { x, y, width, height };
};
//=============================================================================
export const conditionToLatex = (condition: string, threshold: string | number) => {
  const op = condition === FieldNames.kGt ? '>' : condition === FieldNames.kNe ? '\\ne' : '\\geq';
  return `${op}\\!\\!${threshold}`;
};
//=============================================================================
export const estimateLabelWidth = (text: string | unknown, charWidth = 7.5, padding = 16) => {
  const len = Math.max(String(text || '').length, 1);
  return Math.ceil(len * charWidth + padding);
};
//=============================================================================
export const computePortYs = (count: number, height: number) => {
  if (count <= 0) return [];
  if (count === 1) return [height / 2];
  const top = 20;
  const bottom = height - 20;
  const step = (bottom - top) / (count - 1);
  return Array.from({ length: count }, (_, i) => top + step * i);
};
//=============================================================================
