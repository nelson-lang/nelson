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
import { rpc } from '../rpc/client';
import type { DiagnosticsInfoResult } from '../rpc/types';
//=============================================================================
function bytes(value: number | undefined): string {
  if (!Number.isFinite(value)) return 'n/a';
  return `${(Number(value) / (1024 * 1024)).toFixed(2)} MiB`;
}
//=============================================================================
function yesNo(value: boolean | undefined): string {
  return value ? 'yes' : 'no';
}
//=============================================================================
function feature(info: DiagnosticsInfoResult | null, name: string): boolean | undefined {
  return info?.browser?.features?.[name];
}
//=============================================================================
function Row({ label, value }: { label: string; value: string | number | undefined }): JSX.Element {
  return (
    <tr>
      <th scope="row">{label}</th>
      <td>{value === undefined || value === '' ? 'n/a' : value}</td>
    </tr>
  );
}
//=============================================================================
function ListBlock({
  title,
  values,
}: {
  title: string;
  values: string[] | undefined;
}): JSX.Element {
  return (
    <section className="diagnostics__section">
      <h3>{title}</h3>
      <p className="diagnostics__list">{values?.length ? values.join(', ') : 'n/a'}</p>
    </section>
  );
}
//=============================================================================
export function DiagnosticsPanel(): JSX.Element {
  const [info, setInfo] = useState<DiagnosticsInfoResult | null>(null);
  const [error, setError] = useState('');

  const refresh = () => {
    setError('');
    rpc<DiagnosticsInfoResult>('diagnostics.info')
      .then(setInfo)
      .catch((err: unknown) => setError(err instanceof Error ? err.message : String(err)));
  };

  useEffect(refresh, []);

  const enabledFeatures = useMemo(
    () =>
      Object.entries(info?.capabilities?.features ?? {})
        .filter((entry) => entry[1])
        .map((entry) => entry[0])
        .sort(),
    [info]
  );

  return (
    <div className="browser diagnostics diagnostics-panel">
      <div className="diagnostics__bar">
        <span>Runtime diagnostics</span>
        <button type="button" onClick={refresh}>
          Refresh
        </button>
      </div>
      {error ? <div className="diagnostics__error">{error}</div> : null}
      {!info && !error ? <div className="browser__empty">Loading diagnostics...</div> : null}
      {info ? (
        <div className="diagnostics__body">
          <section className="diagnostics__section">
            <h3>Session</h3>
            <table className="diagnostics__table">
              <tbody>
                <Row label="Backend" value={info.backend.name} />
                <Row label="Version" value={info.backend.version} />
                <Row label="Launch mode" value={info.backend.launchMode} />
                <Row label="Runtime" value={info.runtime?.name} />
                <Row label="Profile" value={info.runtime?.profile} />
                <Row label="Build" value={info.runtime?.buildId} />
              </tbody>
            </table>
          </section>
          <section className="diagnostics__section">
            <h3>Delivery</h3>
            <table className="diagnostics__table">
              <tbody>
                <Row label="Files" value={info.delivery?.files} />
                <Row label="Startup" value={bytes(info.delivery?.startupBytes)} />
                <Row label="On demand" value={bytes(info.delivery?.onDemandBytes)} />
                <Row label="Total" value={bytes(info.delivery?.totalBytes)} />
              </tbody>
            </table>
          </section>
          <section className="diagnostics__section">
            <h3>Persistence</h3>
            <table className="diagnostics__table">
              <tbody>
                <Row label="Command history" value={yesNo(info.persistence?.commandHistory)} />
                <Row label="Preferences" value={info.persistence?.preferencesPath} />
                <Row label="Workspace" value={info.persistence?.workspacePath} />
              </tbody>
            </table>
          </section>
          <ListBlock title="Enabled features" values={enabledFeatures} />
          <section className="diagnostics__section">
            <h3>Build options</h3>
            <table className="diagnostics__table">
              <tbody>
                <Row
                  label="Native Wasm exceptions"
                  value={yesNo(info.capabilities?.features.nativeWasmExceptions)}
                />
                <Row
                  label="Threaded Wasm build"
                  value={yesNo(info.capabilities?.features.threadedWasm)}
                />
              </tbody>
            </table>
          </section>
          <section className="diagnostics__section">
            <h3>Browser runtime</h3>
            <table className="diagnostics__table">
              <tbody>
                <Row label="Platform" value={info.browser?.platform} />
                <Row label="Secure context" value={yesNo(feature(info, 'secureContext'))} />
                <Row
                  label="Cross-origin isolated"
                  value={yesNo(feature(info, 'crossOriginIsolated'))}
                />
                <Row label="Wasm core" value={yesNo(feature(info, 'wasmCore'))} />
                <Row label="Wasm exceptions" value={yesNo(feature(info, 'wasmExceptions'))} />
                <Row label="Wasm JSPI" value={yesNo(feature(info, 'wasmJspi'))} />
                <Row label="Wasm threads" value={yesNo(feature(info, 'wasmThreads'))} />
                <Row label="PWA cache" value={yesNo(feature(info, 'pwaInstallSupport'))} />
                <Row label="File System Access" value={yesNo(feature(info, 'fileSystemAccess'))} />
                <Row label="Drag-and-drop files" value={yesNo(feature(info, 'dragAndDropFiles'))} />
                <Row label="Web Audio" value={yesNo(feature(info, 'webAudio'))} />
                <Row label="Microphone" value={yesNo(feature(info, 'microphone'))} />
                <Row label="WebGL" value={yesNo(feature(info, 'webGl'))} />
                <Row label="WebGL2" value={yesNo(feature(info, 'webGl2'))} />
                <Row label="WebGPU" value={yesNo(feature(info, 'webGpu'))} />
              </tbody>
            </table>
          </section>
          <ListBlock title="Modules" values={info.capabilities?.modules} />
          <ListBlock title="Capabilities" values={info.capabilities?.capabilities} />
          <ListBlock title="Unavailable" values={info.capabilities?.unavailable} />
          <ListBlock title="Browser unsupported features" values={info.browser?.unsupported} />
          <ListBlock title="Browser notes" values={info.browser?.notes} />
        </div>
      ) : null}
    </div>
  );
}
//=============================================================================
