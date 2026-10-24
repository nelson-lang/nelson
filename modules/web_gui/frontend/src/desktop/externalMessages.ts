//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import {
  applyStartupActions,
  onStartupActionsReady,
  startupWorkspaceFilePath,
  type StartupUrlActions,
} from './startupUrlActions';
import { nelsonStringLiteral } from './commands';
//=============================================================================
export type NelsonDesktopExternalMessage =
  | {
      nelson: 'web-desktop';
      id?: string | number;
      type: 'run';
      command: string;
    }
  | {
      nelson: 'web-desktop';
      id?: string | number;
      type: 'open';
      path: string;
    }
  | {
      nelson: 'web-desktop';
      id?: string | number;
      type: 'script';
      file?: string;
      name?: string;
      content: string;
      run?: boolean;
    };
//=============================================================================
type MessageSource = Pick<Window, 'postMessage'> | null;
function isRecord(value: unknown): value is Record<string, unknown> {
  return Boolean(value) && typeof value === 'object' && !Array.isArray(value);
}
//=============================================================================
export function postMessageReplyTargetOrigin(origin: string): string {
  return origin && origin !== 'null' ? origin : '*';
}
//=============================================================================
function toActions(message: NelsonDesktopExternalMessage): StartupUrlActions {
  if (message.type === 'run') {
    return { openPath: null, script: null, command: message.command };
  }
  if (message.type === 'open') {
    return { openPath: message.path, script: null, command: '' };
  }
  return {
    openPath: null,
    script: {
      file: message.file ?? message.name ?? 'message-script.m',
      content: message.content,
    },
    command: message.run
      ? `run(${nelsonStringLiteral(
          startupWorkspaceFilePath(message.file ?? message.name ?? 'message-script.m')
        )})`
      : '',
  };
}
//=============================================================================
export function parseNelsonDesktopExternalMessage(
  value: unknown
): NelsonDesktopExternalMessage | null {
  if (!isRecord(value) || value.nelson !== 'web-desktop') return null;
  const id = typeof value.id === 'string' || typeof value.id === 'number' ? value.id : undefined;
  if (value.type === 'run' && typeof value.command === 'string') {
    return { nelson: 'web-desktop', id, type: 'run', command: value.command };
  }
  if (value.type === 'open' && typeof value.path === 'string') {
    return { nelson: 'web-desktop', id, type: 'open', path: value.path };
  }
  if (value.type === 'script' && typeof value.content === 'string') {
    return {
      nelson: 'web-desktop',
      id,
      type: 'script',
      content: value.content,
      file: typeof value.file === 'string' ? value.file : undefined,
      name: typeof value.name === 'string' ? value.name : undefined,
      run: value.run === true,
    };
  }
  return null;
}
//=============================================================================
export function postNelsonDesktopExternalReply(
  source: MessageSource,
  origin: string,
  message: NelsonDesktopExternalMessage,
  ok: boolean,
  error = ''
): void {
  if (!source) return;
  try {
    source.postMessage(
      {
        nelson: 'web-desktop',
        id: message.id,
        type: ok ? 'ack' : 'error',
        requestType: message.type,
        ...(error ? { error } : {}),
      },
      postMessageReplyTargetOrigin(origin)
    );
  } catch {
    // The embedder may have navigated away or closed between the request and
    // the reply. The Nelson action has already completed, so the bridge should
    // not surface a secondary delivery failure as an unhandled error.
  }
}
//=============================================================================
export function installNelsonDesktopMessageBridge(): () => void {
  const listener = (event: MessageEvent<unknown>) => {
    const message = parseNelsonDesktopExternalMessage(event.data);
    if (!message) return;
    onStartupActionsReady(() => {
      void applyStartupActions(toActions(message))
        .then(() =>
          postNelsonDesktopExternalReply(event.source as MessageSource, event.origin, message, true)
        )
        .catch((error: unknown) =>
          postNelsonDesktopExternalReply(
            event.source as MessageSource,
            event.origin,
            message,
            false,
            error instanceof Error ? error.message : String(error)
          )
        );
    });
  };
  window.addEventListener('message', listener);
  return () => window.removeEventListener('message', listener);
}
//=============================================================================
