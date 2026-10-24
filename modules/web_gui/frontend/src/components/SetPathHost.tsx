//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useCallback, useEffect, useState } from 'react';
import { rpc } from '../rpc/client';
import { onEngineReady, runCommand } from '../desktop/engineSession';
import { webPrompt } from './PromptHost';
import { DesktopIcon } from './DesktopIcon';
//=============================================================================
// Set Path dialog. Mutations go through the addpath/rmpath/savepath/
// restoredefaultpath builtins and the list reloads on the next prompt.
export const OPEN_SETPATH_EVENT = 'nelson-webview.open-setpath';
export function SetPathHost(): JSX.Element | null {
  const [open, setOpen] = useState(false);
  const [paths, setPaths] = useState<string[]>([]);

  const reload = useCallback(() => {
    rpc<{ paths?: string[] }>('path.list')
      .then((r) => setPaths(r.paths ?? []))
      .catch(() => undefined);
  }, []);

  useEffect(() => {
    const onOpen = () => {
      setOpen(true);
      reload();
    };
    window.addEventListener(OPEN_SETPATH_EVENT, onOpen);
    return () => window.removeEventListener(OPEN_SETPATH_EVENT, onOpen);
  }, [reload]);

  // While open, refresh after each command settles (a prompt is pushed).
  useEffect(() => {
    if (!open) return;
    return onEngineReady(() => reload());
  }, [open, reload]);

  if (!open) return null;

  const quote = (p: string) => `'${p.replace(/'/g, "''")}'`;
  const addFolder = async () => {
    const p = await webPrompt('Folder to add to the path:');
    if (p) void runCommand(`addpath(${quote(p)})`);
  };
  const remove = (p: string) => void runCommand(`rmpath(${quote(p)})`);
  const save = () => void runCommand('savepath()');
  const restore = () => void runCommand('restoredefaultpath()');

  return (
    <div className="dialog-overlay" role="presentation" onMouseDown={() => setOpen(false)}>
      <div
        className="dialog-card setpath-card"
        role="dialog"
        aria-modal="true"
        aria-label="Set Path"
        onMouseDown={(e) => e.stopPropagation()}
        onKeyDown={(e) => {
          if (e.key === 'Escape') setOpen(false);
        }}
      >
        <div className="about-card__title">Set Path</div>
        <p className="setpath-card__hint">Folders on the Nelson search path (in order).</p>
        <ul className="setpath-list">
          {paths.map((p) => (
            <li key={p} className="setpath-list__item">
              <span className="browser__mono setpath-list__path" title={p}>
                {p}
              </span>
              <button
                className="setpath-list__x"
                title="Remove from path"
                onClick={() => remove(p)}
              >
                <DesktopIcon name="close" size={16} />
              </button>
            </li>
          ))}
          {paths.length === 0 ? <li className="setpath-list__empty">(empty)</li> : null}
        </ul>
        <div className="dialog-card__buttons setpath-card__buttons">
          <button className="dialog-btn" onClick={addFolder}>
            Add Folder…
          </button>
          <button className="dialog-btn" onClick={restore}>
            Restore Default
          </button>
          <button className="dialog-btn" onClick={save}>
            Save
          </button>
          <span style={{ flex: 1 }} />
          <button className="dialog-btn dialog-btn--default" onClick={() => setOpen(false)}>
            Close
          </button>
        </div>
      </div>
    </div>
  );
}
//=============================================================================
