//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { rpc } from '../rpc/client';
//=============================================================================
// Load the 'web' surface's persisted assistant conversation over the
// assistant.history RPC and coerce it into the AssistantPane's message shape.
// Shared by every web wrapper (docked pane, notebook, editor), which all drive
// the same 'web' surface. Best-effort: an unreachable RPC or a malformed reply
// resolves to [], so the pane simply opens empty.
type Turn = { role: 'user' | 'assistant'; text: string };
//=============================================================================
export function loadAssistantHistory(): Promise<Turn[]> {
  return rpc<{ turns?: unknown }>('assistant.history', {})
    .then((result) => {
      if (!Array.isArray(result.turns)) {
        return [];
      }
      return result.turns.filter(isTurn);
    })
    .catch(() => []);
}
//=============================================================================
function isTurn(value: unknown): value is Turn {
  if (typeof value !== 'object' || value === null) {
    return false;
  }
  const turn = value as { role?: unknown; text?: unknown };
  return (turn.role === 'user' || turn.role === 'assistant') && typeof turn.text === 'string';
}
//=============================================================================
