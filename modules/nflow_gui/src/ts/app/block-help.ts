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
import { isNflowHelpWindowArmed, openNflowHelpWindow } from './help-window.js';
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
  // Browser build: raise the shared help window now, while the click gesture is
  // still valid, so the host round-trip only has to navigate an open window.
  if (isNflowHelpWindowArmed()) openNflowHelpWindow();
  return requestHelp(topic.trim(), 'Related help opened', 'Unable to open help: {error}');
}
//=============================================================================
export function openDocumentation(): Promise<boolean> {
  // Open the shared "nelson-help" window on the documentation home page. No topic
  // and no host round-trip: in the browser build the window is armed, so it opens
  // straight from the click gesture (an empty target loads help/<lang>/index.html).
  if (isNflowHelpWindowArmed()) {
    openNflowHelpWindow({});
    pushStatus({ level: 'success', message: t('Documentation opened'), durationMs: 2500 });
    return Promise.resolve(true);
  }
  return Promise.resolve(false);
}
//=============================================================================
export function openBlockHelp(block: BlockLike): Promise<boolean> {
  const topic = blockHelpTopic(block);
  if (!topic) return Promise.resolve(false);
  const page = blockHelpPage(topic.slice('block:'.length));
  if (isNflowHelpWindowArmed()) {
    // Browser build: open the shared "nelson-help" window straight from this
    // click (gesture preserved, no blocked popup). A built-in block's page is
    // known synchronously, so no host round-trip is needed.
    openNflowHelpWindow(page ? { open: page } : {});
    if (page) {
      pushStatus({ level: 'success', message: t('Block help opened'), durationMs: 2500 });
      return Promise.resolve(true);
    }
    // External library block: the window is now open; let the host resolve the
    // page (via doc) and the opener navigates the open window.
    return requestHelp(topic, 'Block help opened', 'Unable to open block help: {error}');
  }
  return requestHelp(
    topic,
    'Block help opened',
    'Unable to open block help: {error}',
    page ? { page } : {}
  );
}
//=============================================================================
