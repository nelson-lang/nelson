//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useEffect, useState } from 'react';
import { runCommand } from '../desktop/engineSession';
import { DesktopIcon } from './DesktopIcon';
//=============================================================================
// Plots gallery for the selected workspace variable, filtered by its shape.
// Opened from the Workspace panel with the variable name and dimensions.
export const OPEN_PLOTS_EVENT = 'nelson-webview.open-plots';
interface Seed {
  name: string;
  rows: number;
  cols: number;
}
//=============================================================================
interface PlotType {
  cmd: string;
  label: string;
  applies: (isVector: boolean, isMatrix: boolean) => boolean;
}
//=============================================================================
const PLOTS: PlotType[] = [
  { cmd: 'plot', label: 'Line', applies: () => true },
  { cmd: 'bar', label: 'Bar', applies: (v) => v },
  { cmd: 'barh', label: 'Bar (horizontal)', applies: (v) => v },
  { cmd: 'area', label: 'Area', applies: (v) => v },
  { cmd: 'stairs', label: 'Stairs', applies: (v) => v },
  { cmd: 'stem', label: 'Stem', applies: (v) => v },
  { cmd: 'pie', label: 'Pie', applies: (v) => v },
  { cmd: 'hist', label: 'Histogram', applies: () => true },
  { cmd: 'surf', label: 'Surface', applies: (_v, m) => m },
  { cmd: 'mesh', label: 'Mesh', applies: (_v, m) => m },
  { cmd: 'contour', label: 'Contour', applies: (_v, m) => m },
  { cmd: 'contourf', label: 'Contour (filled)', applies: (_v, m) => m },
  { cmd: 'pcolor', label: 'Pseudocolor', applies: (_v, m) => m },
  { cmd: 'imagesc', label: 'Scaled image', applies: (_v, m) => m },
  { cmd: 'waterfall', label: 'Waterfall', applies: (_v, m) => m },
];
//=============================================================================
export function PlotGalleryHost(): JSX.Element | null {
  const [open, setOpen] = useState(false);
  const [seed, setSeed] = useState<Seed | null>(null);

  useEffect(() => {
    const onOpen = (e: Event) => {
      const d = (e as CustomEvent<Seed>).detail;
      if (!d?.name) return;
      setSeed(d);
      setOpen(true);
    };
    window.addEventListener(OPEN_PLOTS_EVENT, onOpen as EventListener);
    return () => window.removeEventListener(OPEN_PLOTS_EVENT, onOpen as EventListener);
  }, []);

  if (!open || !seed) return null;

  const isVector = seed.rows <= 1 || seed.cols <= 1;
  const isMatrix = seed.rows > 1 && seed.cols > 1;
  const available = PLOTS.filter((p) => p.applies(isVector, isMatrix));
  const plot = (cmd: string) => {
    void runCommand(`${cmd}(${seed.name})`);
    setOpen(false);
  };

  return (
    <div className="dialog-overlay" role="presentation" onMouseDown={() => setOpen(false)}>
      <div
        className="dialog-card plots-card"
        role="dialog"
        aria-modal="true"
        aria-label="Plots"
        onMouseDown={(e) => e.stopPropagation()}
        onKeyDown={(e) => {
          if (e.key === 'Escape') setOpen(false);
        }}
      >
        <div className="about-card__title">
          Plots - <span className="browser__mono">{seed.name}</span>{' '}
          <span className="plots-card__dims">
            {seed.rows}×{seed.cols}
          </span>
        </div>
        <div className="plots-grid">
          {available.map((p) => (
            <button key={p.cmd} className="plots-grid__item" onClick={() => plot(p.cmd)}>
              <span className="plots-grid__icon" aria-hidden>
                <DesktopIcon name="plot" size={24} />
              </span>
              <span className="plots-grid__label">{p.label}</span>
            </button>
          ))}
        </div>
        <div className="dialog-card__buttons">
          <button className="dialog-btn" onClick={() => setOpen(false)}>
            Close
          </button>
        </div>
      </div>
    </div>
  );
}
//=============================================================================
