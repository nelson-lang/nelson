//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { Line } from './ansi';
//=============================================================================
export type MenuState = { x: number; y: number; hasSelection: boolean };
export type PanelApi = {
  maximize: () => void;
  isMaximized: () => boolean;
  exitMaximized: () => void;
};
//=============================================================================
export type Segment = { kind: 'text'; lines: Line[] } | { kind: 'html'; html: string };
//=============================================================================
