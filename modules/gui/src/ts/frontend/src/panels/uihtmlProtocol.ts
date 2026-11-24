//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// The uihtml page contract in one place: the postMessage envelopes exchanged
// between the desktop host (uihtmlHost.ts) and the page bootstrap
// (uihtmlBootstrap.ts), and the component API a page sees in setup(). Both ends
// import these types, so a change to the protocol cannot drift between the page
// and the host without the compiler noticing.
//=============================================================================
// Host -> page.
//=============================================================================
export interface UiHtmlInitMessage {
  __uihtml: 'init';
  revision: number;
  value: unknown;
}
//=============================================================================
export interface UiHtmlApplyMessage {
  __uihtml: 'apply';
  revision: number;
  value: unknown;
}
//=============================================================================
// An engine -> page event. The same shape also travels page -> engine, so it is
// shared by both directions.
export interface UiHtmlEventMessage {
  __uihtml: 'event';
  name: string;
  data: unknown;
}
//=============================================================================
export type UiHtmlHostMessage = UiHtmlInitMessage | UiHtmlApplyMessage | UiHtmlEventMessage;
//=============================================================================
// Page -> host.
//=============================================================================
export interface UiHtmlReadyMessage {
  __uihtml: 'ready';
}
//=============================================================================
export interface UiHtmlDataMessage {
  __uihtml: 'data';
  value: unknown;
}
//=============================================================================
export type UiHtmlPageMessage = UiHtmlReadyMessage | UiHtmlDataMessage | UiHtmlEventMessage;
//=============================================================================
// The object a page receives in setup() and in every event callback.
//=============================================================================
export interface UiHtmlComponent {
  Data: unknown;
  addEventListener(name: string, handler: UiHtmlEventHandler): void;
  removeEventListener(name: string, handler: UiHtmlEventHandler): void;
  sendEventToNelson(name: string, data: unknown): void;
}
//=============================================================================
export interface UiHtmlEventArgs {
  Source: UiHtmlComponent;
  Data: unknown;
}
//=============================================================================
export type UiHtmlEventHandler = (args: UiHtmlEventArgs) => void;
//=============================================================================
// The global a page defines so the bridge can hand it its component.
export type UiHtmlSetup = (component: UiHtmlComponent) => void;
//=============================================================================
