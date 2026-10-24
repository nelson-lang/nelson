//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
export interface NativeMenuItem {
  label?: string;
  action?: string;
  role?:
    | "hide"
    | "hideOthers"
    | "showAll"
    | "closeWindow"
    | "minimize"
    | "zoom";
  shortcut?: string;
  enabled?: boolean;
  checked?: boolean;
  separator?: boolean;
  items?: NativeMenuItem[];
}
//=============================================================================
export interface NativeMenu {
  label: string;
  items: NativeMenuItem[];
}
//=============================================================================
export interface NativeMenuDescriptor {
  menus: NativeMenu[];
}
//=============================================================================
interface NativeMenuMessageHandler {
  postMessage(message: NativeMenuDescriptor & { type: "set" }): void;
}
//=============================================================================
interface NativeMenuWindow extends Window {
  webkit?: {
    messageHandlers?: {
      webuiNativeMenu?: NativeMenuMessageHandler;
    };
  };
}
//=============================================================================
const ACTION_EVENT = "webui:native-menu-action";
const REFRESH_EVENT = "webui:native-menu-refresh";
function messageHandler(): NativeMenuMessageHandler | undefined {
  if (typeof window === "undefined") return undefined;
  return (window as NativeMenuWindow).webkit?.messageHandlers?.webuiNativeMenu;
}
//=============================================================================
export function isNativeMenuAvailable(): boolean {
  return typeof messageHandler()?.postMessage === "function";
}
//=============================================================================
export function publishNativeMenu(descriptor: NativeMenuDescriptor): boolean {
  const handler = messageHandler();
  if (!handler || !Array.isArray(descriptor.menus)) return false;
  try {
    handler.postMessage({ type: "set", ...descriptor });
    return true;
  } catch {
    return false;
  }
}
//=============================================================================
export function installNativeMenu(
  describe: () => NativeMenuDescriptor,
  execute: (action: string) => void
): () => void {
  if (!isNativeMenuAvailable()) return () => undefined;

  const publish = () => publishNativeMenu(describe());
  const onAction = (event: Event) => {
    const action = String(
      (event as CustomEvent<{ action?: string }>).detail?.action ?? ""
    );
    if (!action) return;
    execute(action);
    window.setTimeout(publish, 0);
  };
  const onRefresh = () => publish();

  window.addEventListener(ACTION_EVENT, onAction);
  window.addEventListener(REFRESH_EVENT, onRefresh);
  publish();

  return () => {
    window.removeEventListener(ACTION_EVENT, onAction);
    window.removeEventListener(REFRESH_EVENT, onRefresh);
  };
}
//=============================================================================
