//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Build a LIVE catalog of the blocks currently loaded in the editor, so the
// assistant is told the real, current block set (paths, parameters, ports/pins)
// instead of a hand-maintained list in the skill that drifts as libraries evolve.
// Each entry is one self-delimited line starting with "- nflow/", so it survives
// being flattened into the single-line prompt literal the bridge sends.
import { getLoadedLibraries } from './library-loader.js';
//=============================================================================
interface RawBlock {
  type?: string;
  defaultParams?: Record<string, unknown>;
  inputs?: unknown[];
  outputs?: unknown[];
  physicalPins?: Array<{ name?: string }>;
}
//=============================================================================
// Builtin libraries are registered as "builtin.<name>" but the scripted API path
// is "nflow/<name>/<type>", so drop the prefix for the path segment.
function shortLibraryName(id: string): string {
  return id.replace(/^builtin\./, '');
}
//=============================================================================
function describeBlock(lib: string, block: RawBlock): string | null {
  if (!block || typeof block.type !== 'string' || block.type === '') {
    return null;
  }
  const params =
    block.defaultParams && typeof block.defaultParams === 'object'
      ? Object.keys(block.defaultParams)
      : [];
  const paramPart = params.length ? ` {${params.join(',')}}` : '';

  let portPart: string;
  if (Array.isArray(block.physicalPins) && block.physicalPins.length) {
    const pins = block.physicalPins.map((p) => String(p?.name ?? '')).filter(Boolean);
    const outs = Array.isArray(block.outputs) ? block.outputs.length : 0;
    portPart = ` pins: ${pins.join(',')}${outs > 0 ? ` (+${outs} signal out)` : ''}`;
  } else {
    const ins = Array.isArray(block.inputs) ? block.inputs.length : 0;
    const outs = Array.isArray(block.outputs) ? block.outputs.length : 0;
    portPart = ` ${ins}in/${outs}out`;
  }
  return `- nflow/${lib}/${block.type}${paramPart}${portPart}`;
}
//=============================================================================
// A one-paragraph, self-delimited catalog of every loaded block, or '' when no
// library is loaded (the caller then sends the prompt without it).
export function buildBlockCatalog(): string {
  let libraries: Map<string, { id?: string; library?: unknown }>;
  try {
    libraries = getLoadedLibraries() as Map<string, { id?: string; library?: unknown }>;
  } catch {
    return '';
  }
  const lines: string[] = [];
  for (const entry of libraries.values()) {
    const library = entry.library as { blocks?: RawBlock[] } | undefined;
    const blocks = Array.isArray(library?.blocks) ? (library as { blocks: RawBlock[] }).blocks : [];
    if (!blocks.length) {
      continue;
    }
    const lib = shortLibraryName(String(entry.id ?? ''));
    for (const block of blocks) {
      const line = describeBlock(lib, block);
      if (line) {
        lines.push(line);
      }
    }
  }
  if (!lines.length) {
    return '';
  }
  return (
    'Live block catalog (the blocks available in this editor right now; use ONLY ' +
    'these block paths and these exact parameter names — a block or parameter not ' +
    'listed here does not exist): ' +
    lines.join('\n')
  );
}
//=============================================================================
