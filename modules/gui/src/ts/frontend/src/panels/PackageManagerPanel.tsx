//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useMemo } from 'react';
import { PackageManagerApp, type PackageManagerHost } from '@nelson/package-manager/embed';
import '@nelson/package-manager/app.css';
import { rpc } from '../rpc/client';
import { subscribeToolEvents, takeToolArgs } from '../desktop/webTools';
//=============================================================================
// The package manager (modules/nmm_gui/frontend) docked as a panel: its
// requests go through the `tool.dispatch` RPC to the nmm web tool, its
// events arrive as `tool.event` pushes, and nmm_gui(action, name) in the
// console reaches it as a deep link.
export function PackageManagerPanel(): JSX.Element {
  const host = useMemo<PackageManagerHost>(
    () => ({
      transport: (method, params) => rpc('tool.dispatch', { tool: 'nmm', method, params }),
      subscribe: (listener) => subscribeToolEvents('nmm', listener),
      deepLink: () => takeToolArgs('nmm'),
    }),
    []
  );
  return (
    <div className="packages-panel">
      <PackageManagerApp host={host} />
    </div>
  );
}
//=============================================================================
