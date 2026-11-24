//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Routes an assistant "showPanel" event to the matching GUI panel, so a host
// action (for example profile_current_file) can bring the Profiler panel to the
// front on the web surface, mirroring the Qt dock. Only a known panel id is opened,
// so an injected event can never open something arbitrary.
import { subscribeToolEvents } from './webTools';
import { ensureCorePanel } from './desktop';
//=============================================================================
const ALLOWED_PANELS: Record<string, string> = {
  profiler: 'profiler',
  callstack: 'callstack',
  breakpoints: 'breakpoints',
};
//=============================================================================
let installed = false;
//=============================================================================
export function installShowPanelRouter(): void {
  if (installed) {
    return;
  }
  installed = true;
  subscribeToolEvents('assistant', (channel, raw) => {
    if (channel !== 'showPanel') {
      return;
    }
    let panel = '';
    try {
      const payload = JSON.parse(raw) as { panel?: string };
      panel = payload.panel ?? '';
    } catch {
      return;
    }
    const id = ALLOWED_PANELS[panel];
    if (id) {
      ensureCorePanel(id);
    }
  });
}
//=============================================================================
