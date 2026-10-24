//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { getDiscoveredManifests } from './library-loader.js';
import { nflowRpc } from './native-rpc.js';
import { pushStatus } from './status-service.js';
import { t } from '../utils/i18n.js';
//=============================================================================
type BlockLike = { type?: unknown } | null | undefined;
type Manifest = { blocks?: unknown };
const BLOCK_TYPE_PATTERN = /^[A-Za-z][A-Za-z0-9_.-]*$/;
const HELP_TOPIC_PATTERN = /^(?:block:)?[A-Za-z][A-Za-z0-9_.-]*$/;
function unqualifiedBlockType(type: string): string {
  for (const [libraryId, value] of getDiscoveredManifests()) {
    const prefix = `nflow.${libraryId}.`;
    if (!type.startsWith(prefix)) continue;
    const candidate = type.slice(prefix.length);
    const manifestBlocks = (value as Manifest).blocks;
    const blocks: unknown[] = Array.isArray(manifestBlocks) ? manifestBlocks : [];
    if (
      blocks.some(
        (block) =>
          block != null &&
          typeof block === 'object' &&
          String((block as { type?: unknown }).type || '') === candidate
      )
    ) {
      return candidate;
    }
  }
  return type;
}
//=============================================================================
function blockHelpPage(type: string): string | null {
  for (const [libraryId, value] of getDiscoveredManifests()) {
    const manifest = value as Manifest & { _dir?: unknown };
    const blocks = Array.isArray(manifest.blocks) ? manifest.blocks : [];
    if (
      !blocks.some(
        (block) =>
          block != null &&
          typeof block === 'object' &&
          String((block as { type?: unknown }).type || '') === type
      )
    ) {
      continue;
    }
    // Built-in NFlow manifests are generated from the same directory names as
    // the help archive. External libraries keep the regular indexed lookup;
    // their module-specific help layout is not known by the GUI.
    const directory = typeof manifest._dir === 'string' ? manifest._dir.trim() : '';
    if (!libraryId.startsWith('builtin.') || !directory) return null;
    return `./nflow_blocks/${directory}/${type}.html`;
  }
  return null;
}
//=============================================================================
export function blockHelpTopic(block: BlockLike): string | null {
  const rawType = typeof block?.type === 'string' ? block.type.trim() : '';
  if (!rawType) return null;
  const type = unqualifiedBlockType(rawType);
  return BLOCK_TYPE_PATTERN.test(type) ? `block:${type}` : null;
}
//=============================================================================
async function requestHelp(
  topic: string,
  successMessage: string,
  errorMessage: string,
  extraParams: Record<string, unknown> = {}
): Promise<boolean> {
  if (!HELP_TOPIC_PATTERN.test(topic)) return false;
  try {
    const result = await nflowRpc<{ success?: boolean }>('nflow.help.open', {
      topic,
      ...extraParams,
    });
    if (!result?.success) throw new Error('documentation request was rejected');
    pushStatus({ level: 'success', message: t(successMessage), durationMs: 2500 });
    return true;
  } catch (error) {
    pushStatus({
      level: 'error',
      message: t(errorMessage, {
        error: error instanceof Error ? error.message : String(error),
      }),
      durationMs: 5000,
      diagnosticSource: 'application',
    });
    return false;
  }
}
//=============================================================================
export function openHelpTopic(topic: string): Promise<boolean> {
  return requestHelp(topic.trim(), 'Related help opened', 'Unable to open help: {error}');
}
//=============================================================================
export function openBlockHelp(block: BlockLike): Promise<boolean> {
  const topic = blockHelpTopic(block);
  if (!topic) return Promise.resolve(false);
  const page = blockHelpPage(topic.slice('block:'.length));
  return requestHelp(
    topic,
    'Block help opened',
    'Unable to open block help: {error}',
    page ? { page } : {}
  );
}
//=============================================================================
