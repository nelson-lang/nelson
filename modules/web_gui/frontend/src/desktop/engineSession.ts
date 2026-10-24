//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Engine session: the single path a command takes to the interpreter and the
// single place its Busy/Ready state is derived (the Qt desktop polls the
// engine; here it follows the command cycle). Submitting a non-empty command
// makes the engine Busy; the next `term.prompt` push makes it Ready again and
// then runs the "evaluation finished" subscribers (workspace, history, current
// directory, figure reconcile...).
import { onPush, rpc } from '../rpc/client';
//=============================================================================
let busy = false;
let latestBusyToken = 0;
let activeBusyToken: number | null = null;
let pendingSubmittedCommands = 0;
let deferredUntokenedPrompt: unknown | undefined;
const busySubs = new Set<(busy: boolean) => void>();
const readySubs = new Set<(prompt: unknown) => void>();
function setBusy(next: boolean): void {
  if (busy === next) return;
  busy = next;
  busySubs.forEach((f) => f(busy));
}
//=============================================================================
export function isEngineBusy(): boolean {
  return busy;
}
//=============================================================================
export function subscribeEngine(cb: (busy: boolean) => void): () => void {
  busySubs.add(cb);
  return () => busySubs.delete(cb);
}
//=============================================================================
// Runs after every prompt, once the engine is Ready again.
export function onEngineReady(cb: (prompt: unknown) => void): () => void {
  readySubs.add(cb);
  return () => readySubs.delete(cb);
}
//=============================================================================
// Hands a line to the interpreter. Rejects when the transport fails, in which
// case no prompt will follow, so the engine is not left Busy.
export async function submitCommand(line: string): Promise<void> {
  const commandSubmitted = line.trim().length > 0;
  if (commandSubmitted) {
    pendingSubmittedCommands += 1;
    setBusy(true);
  }
  try {
    await rpc('term.input', { line });
  } catch (err) {
    setBusy(false);
    deferredUntokenedPrompt = undefined;
    throw err;
  } finally {
    if (commandSubmitted) {
      pendingSubmittedCommands = Math.max(0, pendingSubmittedCommands - 1);
      flushDeferredUntokenedPrompt();
    }
  }
}
//=============================================================================
// Fire-and-forget variant for menus, toolbars and panels.
export function runCommand(line: string): Promise<void> {
  return submitCommand(line).catch(() => undefined);
}
//=============================================================================
function tokenFromPayload(payload: unknown): number | null {
  const token = (payload as { token?: unknown })?.token;
  return typeof token === 'number' && Number.isFinite(token) ? token : null;
}
//=============================================================================
function acceptReadyPrompt(prompt: unknown): void {
  setBusy(false);
  readySubs.forEach((f) => f(prompt));
}
//=============================================================================
function flushDeferredUntokenedPrompt(): void {
  if (pendingSubmittedCommands > 0 || deferredUntokenedPrompt === undefined) return;
  const prompt = deferredUntokenedPrompt;
  deferredUntokenedPrompt = undefined;
  handlePrompt(prompt);
}
//=============================================================================
function handlePrompt(prompt: unknown): void {
  const token = tokenFromPayload(prompt);
  if (token === null && pendingSubmittedCommands > 0) {
    deferredUntokenedPrompt = prompt;
    return;
  }
  if (activeBusyToken !== null) {
    if (token === null || token < activeBusyToken) return;
    activeBusyToken = null;
  } else if (token !== null && token < latestBusyToken) {
    return;
  }
  acceptReadyPrompt(prompt);
}
//=============================================================================
onPush('term.prompt', handlePrompt);
// Commands may also originate outside the command window (for example from
// the examples gallery). The transport announces those evaluations so the
// desktop keeps the same Busy/Stop lifecycle as an interactive command.
onPush('term.busy', (payload) => {
  const token = tokenFromPayload(payload);
  if (token !== null) {
    latestBusyToken = Math.max(latestBusyToken, token);
    activeBusyToken = token;
  } else {
    activeBusyToken = null;
  }
  setBusy(true);
});
//=============================================================================
