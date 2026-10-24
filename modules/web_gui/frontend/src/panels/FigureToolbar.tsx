//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useEffect, useRef, useState } from 'react';
import type { FigureTool } from '../rpc/figureTypes';
import { DesktopIcon } from '../components/DesktopIcon';
import type { DesktopIconName } from '../components/DesktopIcon';
//=============================================================================
interface Props {
  title: string;
  is3d: boolean;
  tool: FigureTool;
  setTool: (tool: FigureTool) => void;
  view?: [number, number];
  detailsVisible: boolean;
  toggleDetails: () => void;
  // The figure ToolBar is 'none': no navigation tools, no details bar toggle.
  toolbarHidden?: boolean;
  inspectorOpen: boolean;
  toggleInspector: () => void;
  maximized: boolean;
  toggleMaximize?: () => void;
  onAction: (request: Record<string, unknown>) => void;
  exportFigure: (format: string, scale: number, preview: boolean) => void;
  exporting: boolean;
  saveAs: () => void;
  saveAsLabel?: string;
  print: () => void;
  refresh: () => void;
  detach: () => void;
  detachLabel: string;
  canUndo?: boolean;
  canRedo?: boolean;
}
//=============================================================================
export function FigureToolbar(props: Props) {
  const [menu, setMenu] = useState<'export' | 'more' | null>(null);
  const [format, setFormat] = useState('png');
  const [scale, setScale] = useState('1');
  const root = useRef<HTMLDivElement>(null);
  useEffect(() => {
    if (!menu) return;
    const outside = (event: MouseEvent) => {
      if (!root.current?.contains(event.target as Node)) setMenu(null);
    };
    const escape = (event: KeyboardEvent) => {
      if (event.key === 'Escape') setMenu(null);
    };
    document.addEventListener('mousedown', outside);
    document.addEventListener('keydown', escape);
    return () => {
      document.removeEventListener('mousedown', outside);
      document.removeEventListener('keydown', escape);
    };
  }, [menu]);
  return (
    <div className="figure-workspace-header" ref={root}>
      <div className="figure-panel__toolbar figure-workspace-heading">
        <div className="figure-workspace-title">
          {props.title}
          <span>{props.is3d ? '3D' : '2D'} plot</span>
        </div>
        {!props.toolbarHidden && <FigureNavigation {...props} />}
        <div className="figure-workspace-actions">
          <button
            aria-label="Undo figure edit"
            title="Undo (Ctrl/Cmd+Z)"
            disabled={!props.canUndo}
            onClick={() => props.onAction({ action: 'undo' })}
          >
            <DesktopIcon name="undo" size={17} />
          </button>
          <button
            aria-label="Redo figure edit"
            title="Redo (Ctrl/Cmd+Shift+Z)"
            disabled={!props.canRedo}
            onClick={() => props.onAction({ action: 'redo' })}
          >
            <DesktopIcon name="redo" size={17} />
          </button>
          <button aria-expanded={props.inspectorOpen} onClick={props.toggleInspector}>
            <DesktopIcon name="workspace" size={17} />
            Properties
          </button>
          {props.toggleMaximize && (
            <button
              onClick={props.toggleMaximize}
              title={props.maximized ? 'Restore' : 'Full window'}
              aria-label={props.maximized ? 'Restore' : 'Full window'}
            >
              <DesktopIcon name={props.maximized ? 'restore' : 'maximize'} size={17} />
            </button>
          )}
          <button
            aria-label="More figure actions"
            aria-expanded={menu === 'more'}
            onClick={() => setMenu(menu === 'more' ? null : 'more')}
          >
            •••
          </button>
          <button
            className="figure-export-primary"
            title="Export PNG, SVG or PDF to your computer as a download, without the desktop interface"
            aria-expanded={menu === 'export'}
            disabled={props.exporting}
            onClick={() => setMenu(menu === 'export' ? null : 'export')}
          >
            Export <DesktopIcon name="down" size={14} />
          </button>
        </div>
      </div>
      {menu === 'more' && <FigureMoreMenu props={props} close={() => setMenu(null)} />}
      {menu === 'export' && (
        <FigureExportMenu
          props={props}
          close={() => setMenu(null)}
          format={format}
          setFormat={setFormat}
          scale={scale}
          setScale={setScale}
        />
      )}
    </div>
  );
}
//=============================================================================
const MODE_TITLES: Record<FigureTool, string> = {
  inspect: 'Inspect: hover or click to read a data point; drag to pan (2D) or rotate (3D)',
  zoom: 'Zoom (Z): scroll, or drag a box to zoom in; click to zoom in, Shift-click to zoom out',
  pan: 'Pan (P): drag to move the view inside the axes; keep the subplot in place',
  rotate: 'Rotate (R): change the 3D orientation of the selected subplot',
};
//=============================================================================
function FigureNavigation(props: Props) {
  const modes: { tool: FigureTool; label: string; icon: DesktopIconName }[] = [
    { tool: 'inspect', label: 'Inspect', icon: 'crosshair' },
    { tool: 'zoom', label: 'Zoom', icon: 'zoom' },
    { tool: 'pan', label: 'Pan', icon: 'pan' },
    ...(props.is3d ? [{ tool: 'rotate' as const, label: 'Rotate', icon: 'rotate' as const }] : []),
  ];
  const presets: Record<string, [number, number]> = {
    '3d': [-37.5, 30],
    xy: [0, 90],
    xz: [0, 0],
    yz: [90, 0],
  };
  const preset =
    Object.entries(presets).find(
      ([, v]) =>
        props.view && Math.abs(v[0] - props.view[0]) < 0.01 && Math.abs(v[1] - props.view[1]) < 0.01
    )?.[0] ?? 'free';
  return (
    <div className="figure-workspace-tools" role="group" aria-label="Figure navigation">
      {modes.map(({ tool, label, icon }) => (
        <button
          key={tool}
          aria-pressed={props.tool === tool}
          title={MODE_TITLES[tool]}
          onClick={() => props.setTool(tool)}
        >
          <DesktopIcon name={icon} size={17} />
          {label}
        </button>
      ))}
      <span className="figure-workspace-separator" />
      <button
        title="Fit (F): fit the selected axes to data and keep the current orientation. Double-click a subplot to fit it."
        onClick={() => props.onAction({ action: 'fit' })}
      >
        <DesktopIcon name="zoomfit" size={17} />
        Fit
      </button>
      <button
        title="Restore the saved orientation, camera and limits of the selected subplot"
        onClick={() => props.onAction({ action: 'restore' })}
      >
        <DesktopIcon name="home" size={17} />
        Restore view
      </button>
      {props.is3d && (
        <label className="figure-view-preset">
          View
          <select
            aria-label="View preset"
            value={preset}
            onChange={(event) => {
              const v = presets[event.target.value];
              if (v)
                props.onAction({
                  action: 'view',
                  azimuth: v[0],
                  elevation: v[1],
                  resetCamera: true,
                });
            }}
          >
            <option value="free">Custom</option>
            <option value="3d">Default 3D</option>
            <option value="xy">Top · XY</option>
            <option value="xz">Front · XZ</option>
            <option value="yz">Side · YZ</option>
          </select>
        </label>
      )}
    </div>
  );
}
//=============================================================================
function FigureMoreMenu({ props, close }: { props: Props; close: () => void }) {
  const act = (request: Record<string, unknown>) => {
    props.onAction(request);
    close();
  };
  return (
    <div className="figure-workspace-menu" role="group" aria-label="More figure actions">
      <button aria-label="Zoom in" onClick={() => act({ action: 'zoom', factor: 1.25 })}>
        Zoom in
      </button>
      <button aria-label="Zoom out" onClick={() => act({ action: 'zoom', factor: 0.8 })}>
        Zoom out
      </button>
      {!props.toolbarHidden && (
        <button
          aria-pressed={props.detailsVisible}
          onClick={() => {
            props.toggleDetails();
            close();
          }}
        >
          Plot details bar
        </button>
      )}
      <button
        title="Replace the reference for Restore view with the selected subplot’s current view"
        onClick={() => act({ action: 'reference' })}
      >
        Use current view as reference
      </button>
      <button
        title={
          props.saveAsLabel === 'Save to workspace…'
            ? 'Save in the browser workspace (virtual filesystem). Use Export to download to your computer.'
            : 'Save the current figure to a local file on this computer'
        }
        onClick={() => {
          props.saveAs();
          close();
        }}
      >
        <DesktopIcon name="save" size={17} />
        {props.saveAsLabel ?? 'Save as…'}
      </button>
      <button
        onClick={() => {
          props.print();
          close();
        }}
      >
        <DesktopIcon name="print" size={17} />
        Print figure
      </button>
      <button
        onClick={() => {
          props.refresh();
          close();
        }}
      >
        <DesktopIcon name="refresh" size={17} />
        Refresh
      </button>
      <button
        title={props.detachLabel}
        onClick={() => {
          props.detach();
          close();
        }}
      >
        <DesktopIcon name="undock" size={17} />
        {props.detachLabel}
      </button>
    </div>
  );
}
//=============================================================================
function FigureExportMenu({
  props,
  close,
  format,
  setFormat,
  scale,
  setScale,
}: {
  props: Props;
  close: () => void;
  format: string;
  setFormat: (value: string) => void;
  scale: string;
  setScale: (value: string) => void;
}) {
  return (
    <div className="figure-workspace-menu" role="group" aria-label="Export figure">
      <strong>Export figure</strong>
      <label>
        Format
        <select
          aria-label="Format"
          value={format}
          onChange={(event) => setFormat(event.target.value)}
        >
          <option value="png">PNG</option>
          <option value="svg">SVG</option>
          <option value="pdf">PDF</option>
        </select>
      </label>
      {format === 'png' && (
        <label>
          Resolution
          <select
            aria-label="Resolution"
            value={scale}
            onChange={(event) => setScale(event.target.value)}
          >
            <option value="1">Current size</option>
            <option value="2">2× resolution</option>
            <option value="3">3× resolution</option>
          </select>
        </label>
      )}
      <span className="figure-workspace-muted">Current view, without the desktop interface.</span>
      <button
        disabled={props.exporting}
        onClick={() => props.exportFigure(format, Number(scale), true)}
      >
        Preview
      </button>
      <button
        disabled={props.exporting}
        onClick={() => {
          props.exportFigure(format, Number(scale), false);
          close();
        }}
      >
        Download {format.toUpperCase()}
      </button>
    </div>
  );
}
//=============================================================================
