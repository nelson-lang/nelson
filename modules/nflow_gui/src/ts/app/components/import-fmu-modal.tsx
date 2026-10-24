//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Preview dialog for the FMU import assistant. Listens for IMPORT_FMU_EVENT
// (dispatched by app/fmu-import.ts with a resolve callback) and resolves with
// the chosen block name, or null on cancel.
//=============================================================================
import { useEffect, useState, type MouseEvent } from 'react';
import { IMPORT_FMU_EVENT } from './modal-events.js';
import type { ImportFmuRequestDetail } from '../fmu-import.js';
//=============================================================================
export function ImportFmuModal(): JSX.Element | null {
  const [request, setRequest] = useState<ImportFmuRequestDetail | null>(null);
  const [name, setName] = useState('');

  useEffect(() => {
    const onRequest = (event: Event): void => {
      const detail = (event as CustomEvent<ImportFmuRequestDetail>).detail;
      if (!detail || typeof detail.resolve !== 'function') return;
      event.preventDefault(); // claim it so the flow shows the preview
      setRequest(detail);
      setName(detail.fmuName);
    };
    document.addEventListener(IMPORT_FMU_EVENT, onRequest);
    return () => document.removeEventListener(IMPORT_FMU_EVENT, onRequest);
  }, []);

  if (!request) return null;

  const finish = (result: string | null): void => {
    request.resolve(result);
    setRequest(null);
  };

  const onBackdropClick = (event: MouseEvent<HTMLDivElement>): void => {
    if (event.target === event.currentTarget) finish(null);
  };

  const iface = request.coSimulation
    ? 'Co-Simulation'
    : request.modelExchange
    ? 'Model Exchange'
    : 'unknown interface';
  const portList = (names: string[]): string => (names.length ? names.join(', ') : 'none');

  return (
    <div className="modal show" role="presentation" onClick={onBackdropClick}>
      <div className="modal-content" role="dialog" aria-modal="true" aria-label="Import FMU">
        <div className="modal-header">
          <h3 data-i18n="Import FMU as a block">Import FMU as a block</h3>
          <button
            className="modal-close"
            data-i18n-title="Close"
            title="Close"
            onClick={() => finish(null)}
          >
            &times;
          </button>
        </div>
        <div className="modal-body">
          <div className="control-group">
            <label htmlFor="importFmuName" data-i18n="Block name">
              Block name
            </label>
            <input
              id="importFmuName"
              className="input"
              type="text"
              value={name}
              onChange={(e) => setName(e.target.value)}
            />
          </div>
          <p className="settings-note">
            FMI {request.fmiVersion} · {iface}
          </p>
          <p className="settings-note">
            <b>Inputs</b> ({request.inputNames.length}): {portList(request.inputNames)}
          </p>
          <p className="settings-note">
            <b>Outputs</b> ({request.outputNames.length}): {portList(request.outputNames)}
          </p>
          <p className="settings-note">
            <b>Parameters</b> ({request.parameterNames.length}): {portList(request.parameterNames)}
          </p>
        </div>
        <div className="modal-footer">
          <button className="btn" data-i18n="Cancel" onClick={() => finish(null)}>
            Cancel
          </button>
          <button
            className="btn btn-primary"
            data-i18n="Add to palette"
            onClick={() => finish(name.trim() || request.fmuName)}
          >
            Add to palette
          </button>
        </div>
      </div>
    </div>
  );
}
//=============================================================================
export default ImportFmuModal;
//=============================================================================
