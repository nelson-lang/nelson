//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useEffect, useMemo, useState } from 'react';
import { onPush, rpc } from '../rpc/client';
import type { WorkspaceListResult } from '../rpc/types';
import { closeVariable, selectVariable, useVariableHost } from '../desktop/variableHost';
import { VariableEditorPanel } from './VariableEditorPanel';
import { DesktopIcon } from '../components/DesktopIcon';
//=============================================================================
export function VariablesEditorHost(): JSX.Element {
  const host = useVariableHost();
  const [available, setAvailable] = useState<string[]>([]);
  const [titles, setTitles] = useState<Record<string, string>>({});
  const panelApi = useMemo(
    () => ({
      setTitle: (title: string) =>
        setTitles((current) =>
          current[host.active!] === title ? current : { ...current, [host.active!]: title }
        ),
    }),
    [host.active]
  );

  useEffect(() => {
    const refresh = () =>
      rpc<WorkspaceListResult>('workspace.list')
        .then((result) => setAvailable(result.variables.map((variable) => variable.name)))
        .catch(() => undefined);
    refresh();
    return onPush('workspace.changed', refresh);
  }, []);

  if (!host.active) {
    return (
      <div className="variables-host variables-host--empty">
        <label htmlFor="variables-host-select">Open a workspace variable</label>
        <select
          id="variables-host-select"
          value=""
          onChange={(event) => selectVariable(event.target.value)}
        >
          <option value="" disabled>
            Select a variable…
          </option>
          {available.map((name) => (
            <option key={name} value={name}>
              {titles[name] ?? name}
            </option>
          ))}
        </select>
      </div>
    );
  }

  return (
    <div className="variables-host">
      <div className="variables-host__tabs" role="tablist" aria-label="Open variables">
        {host.names.map((name) => (
          <div
            key={name}
            className={'variables-host__tab' + (name === host.active ? ' active' : '')}
          >
            <button
              type="button"
              role="tab"
              aria-selected={name === host.active}
              onClick={() => selectVariable(name)}
            >
              {titles[name] ?? name}
            </button>
            <button
              type="button"
              className="variables-host__close"
              aria-label={`Close variable ${name}`}
              onClick={() => closeVariable(name)}
            >
              <DesktopIcon name="close" size={16} />
            </button>
          </div>
        ))}
        <select
          className="variables-host__open"
          aria-label="Open another variable"
          value=""
          onChange={(event) => selectVariable(event.target.value)}
        >
          <option value="">+ Open…</option>
          {available
            .filter((name) => !host.names.includes(name))
            .map((name) => (
              <option key={name} value={name}>
                {name}
              </option>
            ))}
        </select>
      </div>
      <div className="variables-host__editor">
        <VariableEditorPanel name={host.active} panelApi={panelApi} />
      </div>
    </div>
  );
}
//=============================================================================
