//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Web tools hosted as panels (the examples gallery, the package manager).
// The engine pushes `tool.show` when a tool builtin is called in the console
// (demo(), nmm_gui(...)) and `tool.event` for the events a tool emits; a panel
// subscribes to its tool's events and consumes the request it was shown with.
import { onPush } from '../rpc/client';
import { ensureCorePanel } from './desktop';
//=============================================================================
export const TOOL_PANELS: Record<string, string> = { examples: 'examples', nmm: 'packages' };
type ToolListener = (channel: string, payload: string) => void;
const listeners = new Map<string, Set<ToolListener>>();
const pendingArgs = new Map<string, Record<string, unknown>>();
function payloadText(payload: unknown): string {
  if (payload === undefined || payload === null) return '';
  return typeof payload === 'string' ? payload : JSON.stringify(payload);
}
//=============================================================================
export function emitToolEvent(tool: string, channel: string, payload: unknown): boolean {
  const set = listeners.get(tool);
  if (!set || set.size === 0) return false;
  const text = payloadText(payload);
  set.forEach((listener) => listener(channel, text));
  return true;
}
//=============================================================================
export function subscribeToolEvents(tool: string, listener: ToolListener): () => void {
  let set = listeners.get(tool);
  if (!set) {
    set = new Set();
    listeners.set(tool, set);
  }
  set.add(listener);
  return () => {
    set?.delete(listener);
  };
}
//=============================================================================
// The request a tool was shown with (a deep link), handed to the panel once.
export function takeToolArgs(tool: string): Record<string, unknown> | null {
  const args = pendingArgs.get(tool) ?? null;
  pendingArgs.delete(tool);
  return args;
}
//=============================================================================
export function showTool(tool: string, args?: Record<string, unknown>): void {
  const panel = TOOL_PANELS[tool];
  if (!panel) return;
  const request = args && typeof args === 'object' ? args : {};
  ensureCorePanel(panel);
  // An open panel takes the request as an event; a new one reads it on mount.
  if (!emitToolEvent(tool, 'deeplink', Object.keys(request).length ? request : '')) {
    if (Object.keys(request).length) pendingArgs.set(tool, request);
  }
}
//=============================================================================
export function installWebToolPushes(): () => void {
  const offShow = onPush('tool.show', (p) => {
    const push = p as { tool?: string; args?: Record<string, unknown> };
    showTool(String(push.tool ?? ''), push.args);
  });
  const offEvent = onPush('tool.event', (p) => {
    const push = p as { tool?: string; channel?: string; payload?: unknown };
    emitToolEvent(String(push.tool ?? ''), String(push.channel ?? ''), push.payload);
  });
  return () => {
    offShow();
    offEvent();
  };
}
//=============================================================================
