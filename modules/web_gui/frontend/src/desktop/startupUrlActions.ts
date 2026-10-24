//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { openEditor } from './desktop';
import { runCommand } from './engineSession';
import { rpc } from '../rpc/client';
//=============================================================================
interface StartupScript {
  file: string;
  content: string;
}
//=============================================================================
export interface StartupUrlActions {
  openPath: string | null;
  script: StartupScript | null;
  command: string;
}
//=============================================================================
interface StartupUrlActionDependencies {
  openEditor: (path: string) => void;
  rpc: typeof rpc;
  runCommand: typeof runCommand;
}
//=============================================================================
const DEFAULT_SCRIPT_FILE = 'startup-url.m';
const STARTUP_URL_READY_EVENT = 'nelson:desktop-ready-for-startup-url';
const STARTUP_WORKSPACE = '/workspace';
let desktopReadyForStartupUrl = false;
let consoleReadyForStartupUrl = false;
function isStartupUrlReady(): boolean {
  return desktopReadyForStartupUrl && consoleReadyForStartupUrl;
}
//=============================================================================
function notifyIfReady(): void {
  if (!isStartupUrlReady()) return;
  window.dispatchEvent(new Event(STARTUP_URL_READY_EVENT));
}
//=============================================================================
function decodeBase64UrlText(value: string): string {
  const normalized = value.replace(/-/g, '+').replace(/_/g, '/');
  const padding = normalized.length % 4 === 0 ? '' : '='.repeat(4 - (normalized.length % 4));
  const binary = globalThis.atob(`${normalized}${padding}`);
  const bytes = Uint8Array.from(binary, (character) => character.charCodeAt(0));
  return new TextDecoder().decode(bytes);
}
//=============================================================================
export function safeStartupScriptFileName(value: string | null): string {
  const leaf = (value || DEFAULT_SCRIPT_FILE).replaceAll('\\', '/').split('/').pop() || '';
  const safe = leaf.replace(/[^A-Za-z0-9._-]+/g, '_').replace(/^_+|_+$/g, '');
  const file = safe || DEFAULT_SCRIPT_FILE;
  return /\.m$/i.test(file) ? file : `${file}.m`;
}
//=============================================================================
export function startupWorkspaceFilePath(file: string): string {
  return `${STARTUP_WORKSPACE}/${safeStartupScriptFileName(file)}`;
}
//=============================================================================
function safeOpenPath(value: string | null): string | null {
  if (!value) return null;
  if (/^[a-z][a-z0-9+.-]*:/i.test(value)) return null;
  const normalized = value.replaceAll('\\', '/');
  if (
    !normalized ||
    normalized.includes('\0') ||
    normalized.split('/').some((part) => part === '..')
  ) {
    return null;
  }
  return normalized;
}
//=============================================================================
function scriptContent(params: URLSearchParams): string | null {
  const plain = params.get('script');
  if (plain !== null) return plain;
  const encoded = params.get('script64');
  return encoded === null ? null : decodeBase64UrlText(encoded);
}
//=============================================================================
export function parseStartupUrlActions(href: string): StartupUrlActions {
  const url = new URL(href, window.location.href);
  const params = url.searchParams;
  const content = scriptContent(params);
  const runs = params
    .getAll('run')
    .map((command) => command.trim())
    .filter((command) => command.length > 0);
  return {
    openPath: content === null ? safeOpenPath(params.get('open')) : null,
    script:
      content === null
        ? null
        : {
            file: safeStartupScriptFileName(params.get('file') ?? params.get('name')),
            content,
          },
    command: runs.join('\n'),
  };
}
//=============================================================================
export async function applyStartupActions(
  actions: StartupUrlActions,
  dependencies: StartupUrlActionDependencies = { openEditor, rpc, runCommand }
): Promise<void> {
  const openPath = safeOpenPath(actions.openPath);
  if (openPath) {
    dependencies.openEditor(openPath);
  }
  if (actions.script) {
    const file = startupWorkspaceFilePath(actions.script.file);
    await dependencies.rpc('fs.create', {
      path: file,
      kind: 'file',
      content: actions.script.content,
    });
    dependencies.openEditor(file);
  }
  if (actions.command) {
    await dependencies.runCommand(actions.command);
  }
}
//=============================================================================
export async function applyStartupUrlActions(
  href: string,
  dependencies: StartupUrlActionDependencies = { openEditor, rpc, runCommand }
): Promise<void> {
  await applyStartupActions(parseStartupUrlActions(href), dependencies);
}
//=============================================================================
export function onStartupActionsReady(callback: () => void): () => void {
  if (isStartupUrlReady()) {
    window.setTimeout(callback, 0);
    return () => undefined;
  }
  window.addEventListener(STARTUP_URL_READY_EVENT, callback, { once: true });
  return () => window.removeEventListener(STARTUP_URL_READY_EVENT, callback);
}
//=============================================================================
export function scheduleStartupUrlActions(href = window.location.href): void {
  const run = () => {
    void applyStartupUrlActions(href).catch((error: unknown) => {
      console.error('Nelson startup URL action failed', error);
    });
  };
  onStartupActionsReady(run);
}
//=============================================================================
export function notifyStartupUrlDesktopReady(): void {
  desktopReadyForStartupUrl = true;
  notifyIfReady();
}
//=============================================================================
export function notifyStartupUrlConsoleReady(): void {
  consoleReadyForStartupUrl = true;
  notifyIfReady();
}
//=============================================================================
