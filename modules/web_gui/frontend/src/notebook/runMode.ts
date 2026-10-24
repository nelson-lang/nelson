//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { NotebookCtx } from './context';
//=============================================================================
export type RunMode = 'all' | 'section' | 'reactive' | 'nothing';
export function runWithMode(ctx: NotebookCtx, mode: RunMode | undefined, blockId: string): void {
  if (mode === 'nothing') return;
  if (mode === 'reactive') void ctx.runReactive(blockId);
  else if (mode === 'section') void ctx.runSection(blockId);
  else void ctx.runAll();
}
//=============================================================================
/** The Nelson assignment a control contributes to the shared workspace. */
export function controlAssignment(block: { type: string; props: Record<string, unknown> }): string {
  const p = block.props as Record<string, string | number | boolean>;
  const name = String(p.varName ?? '');
  if (!name) return '';
  switch (block.type) {
    case 'slider':
    case 'numberfield':
      return `${name} = ${Number(p.value)}`;
    case 'checkbox':
      return `${name} = ${p.checked ? 'true' : 'false'}`;
    case 'statebutton':
      return `${name} = ${p.on ? 'true' : 'false'}`;
    case 'dropdown':
    case 'textfield':
      return `${name} = ${formatScalarOrString(String(p.value))}`;
    case 'varselect': {
      // Alias the picked workspace variable (an identifier), or nothing.
      const picked = String(p.value ?? '').trim();
      return picked ? `${name} = ${picked}` : '';
    }
    case 'rangeslider':
      return `${name} = [${Number(p.low)} ${Number(p.high)}]`;
    case 'colorpicker':
      return `${name} = ${hexToRgb(String(p.value))}`;
    case 'datepicker':
      return `${name} = "${String(p.value)}"`;
    default:
      return '';
  }
}
//=============================================================================
function formatScalarOrString(v: string): string {
  const n = Number(v);
  return Number.isNaN(n) ? `"${v}"` : String(n);
}
//=============================================================================
function hexToRgb(hex: string): string {
  const m = /^#?([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})$/i.exec(hex);
  if (!m) return '[0 0 0]';
  const c = (i: number) => (parseInt(m[i], 16) / 255).toFixed(3);
  return `[${c(1)} ${c(2)} ${c(3)}]`;
}
//=============================================================================
