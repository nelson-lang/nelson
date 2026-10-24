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
import { rpc } from '../../rpc/client';
import type { WorkspaceListResult, VarOpenResult, VarBlockResult } from '../../rpc/types';
import { useNotebook } from '../context';
import { runWithMode } from '../runMode';
import { ControlConfig } from './ControlConfig';
//=============================================================================
async function fetchVarItems(name: string): Promise<string[]> {
  const info = await rpc<VarOpenResult>('var.open', { name });
  const rows = Math.min(info.rows ?? 0, 200);
  const cols = Math.min(info.cols ?? 0, 200);
  if (rows === 0 || cols === 0) return [];
  const blk = await rpc<VarBlockResult>('var.getBlock', { name, row0: 0, col0: 0, rows, cols });
  const items: string[] = [];
  for (const row of blk.cells ?? []) for (const cell of row) items.push(String(cell));
  return items;
}
//=============================================================================
interface BlockProps {
  block: any;
  editor: any;
}
//=============================================================================
export function NumberFieldCell({ block, editor }: BlockProps) {
  const ctx = useNotebook();
  const { label, varName, value, min, max, step, runMode } = block.props;
  return (
    <div className="nls-control" contentEditable={false}>
      <span className="nls-control-label">{label || varName}</span>
      <span className="nls-var-badge">{varName}</span>
      <input
        className="nls-number-input"
        type="number"
        value={value}
        min={min}
        max={max}
        step={step}
        onChange={(e) => {
          editor.updateBlock(block, { props: { value: parseFloat(e.target.value) || 0 } });
          runWithMode(ctx, runMode ?? 'all', block.id);
        }}
      />
      <ControlConfig block={block} editor={editor} />
    </div>
  );
}
//=============================================================================
export function DropdownCell({ block, editor }: BlockProps) {
  const ctx = useNotebook();
  const { label, varName, value, options, labels, optionsVar, runMode } = block.props;
  const [varItems, setVarItems] = useState<string[] | null>(null);
  useEffect(() => {
    let alive = true;
    if (optionsVar && String(optionsVar).trim()) {
      fetchVarItems(String(optionsVar).trim())
        .then((items) => alive && setVarItems(items))
        .catch(() => alive && setVarItems([]));
    } else {
      setVarItems(null);
    }
    return () => {
      alive = false;
    };
  }, [optionsVar]);
  const values = varItems ?? splitList(options);
  const texts = varItems ?? splitList(labels);
  return (
    <div className="nls-control" contentEditable={false}>
      <span className="nls-control-label">{label || varName}</span>
      <span className="nls-var-badge">{varName}</span>
      <select
        className="nls-select"
        value={value}
        onChange={(e) => {
          editor.updateBlock(block, { props: { value: e.target.value } });
          runWithMode(ctx, runMode ?? 'all', block.id);
        }}
      >
        {values.map((v, i) => (
          <option key={v} value={v}>
            {texts[i] ?? v}
          </option>
        ))}
      </select>
      {optionsVar ? <span className="nls-from-var">from {String(optionsVar)}</span> : null}
      <ControlConfig block={block} editor={editor} dropdown />
    </div>
  );
}
//=============================================================================
export function CheckboxCell({ block, editor }: BlockProps) {
  const ctx = useNotebook();
  const { label, varName, checked, runMode } = block.props;
  return (
    <label className="nls-control nls-control-check" contentEditable={false}>
      <input
        type="checkbox"
        checked={checked}
        onChange={(e) => {
          editor.updateBlock(block, { props: { checked: e.target.checked } });
          runWithMode(ctx, runMode ?? 'all', block.id);
        }}
      />
      <span className="nls-control-label">{label || varName}</span>
      <span className="nls-var-badge">{varName}</span>
      <ControlConfig block={block} editor={editor} />
    </label>
  );
}
//=============================================================================
export function ButtonCell({ block }: BlockProps) {
  const ctx = useNotebook();
  const { caption } = block.props;
  return (
    <div className="nls-control nls-control-button" contentEditable={false}>
      <button className="nls-btn nls-btn-primary" onClick={() => void ctx.runAll()}>
        {caption || 'Run'}
      </button>
    </div>
  );
}
//=============================================================================
export function StateButtonCell({ block, editor }: BlockProps) {
  const ctx = useNotebook();
  const { label, varName, on, runMode } = block.props;
  return (
    <div className="nls-control nls-control-button" contentEditable={false}>
      <button
        className={`nls-btn nls-statebtn ${on ? 'is-on' : ''}`}
        aria-pressed={on}
        onClick={() => {
          editor.updateBlock(block, { props: { on: !on } });
          runWithMode(ctx, runMode ?? 'all', block.id);
        }}
      >
        {on ? '● ' : '○ '}
        {label || varName}
      </button>
      <span className="nls-var-badge">{varName}</span>
      <ControlConfig block={block} editor={editor} />
    </div>
  );
}
//=============================================================================
export function TextFieldCell({ block, editor }: BlockProps) {
  const ctx = useNotebook();
  const { label, varName, value, runMode } = block.props;
  return (
    <div className="nls-control" contentEditable={false}>
      <span className="nls-control-label">{label || varName}</span>
      <span className="nls-var-badge">{varName}</span>
      <input
        className="nls-text-input"
        type="text"
        defaultValue={value}
        onKeyDown={(e) => e.stopPropagation()}
        onBlur={(e) => {
          editor.updateBlock(block, { props: { value: e.target.value } });
          runWithMode(ctx, runMode ?? 'all', block.id);
        }}
      />
      <ControlConfig block={block} editor={editor} />
    </div>
  );
}
//=============================================================================
export function ColorPickerCell({ block, editor }: BlockProps) {
  const ctx = useNotebook();
  const { label, varName, value, runMode } = block.props;
  return (
    <div className="nls-control" contentEditable={false}>
      <span className="nls-control-label">{label || varName}</span>
      <span className="nls-var-badge">{varName}</span>
      <span className="nls-color-value">{value}</span>
      <input
        className="nls-color-input"
        type="color"
        value={value}
        onChange={(e) => {
          editor.updateBlock(block, { props: { value: e.target.value } });
          runWithMode(ctx, runMode ?? 'all', block.id);
        }}
      />
      <ControlConfig block={block} editor={editor} />
    </div>
  );
}
//=============================================================================
export function DatePickerCell({ block, editor }: BlockProps) {
  const ctx = useNotebook();
  const { label, varName, value, runMode } = block.props;
  return (
    <div className="nls-control" contentEditable={false}>
      <span className="nls-control-label">{label || varName}</span>
      <span className="nls-var-badge">{varName}</span>
      <input
        className="nls-date-input"
        type="date"
        value={value}
        onChange={(e) => {
          editor.updateBlock(block, { props: { value: e.target.value } });
          runWithMode(ctx, runMode ?? 'all', block.id);
        }}
      />
      <ControlConfig block={block} editor={editor} />
    </div>
  );
}
//=============================================================================
export function RangeSliderCell({ block, editor }: BlockProps) {
  const ctx = useNotebook();
  const { label, varName, low, high, min, max, step, runMode, runOn } = block.props;
  const commit = (nextLow: number, nextHigh: number, released: boolean) => {
    const lo = Math.min(nextLow, nextHigh);
    const hi = Math.max(nextLow, nextHigh);
    editor.updateBlock(block, { props: { low: lo, high: hi } });
    const onRelease = (runOn ?? 'change') === 'release';
    if (onRelease ? released : !released) runWithMode(ctx, runMode ?? 'all', block.id);
  };
  return (
    <div className="nls-slider nls-range" contentEditable={false}>
      <div className="nls-slider-head">
        <span className="nls-control-label">{label || varName}</span>
        <span className="nls-var-badge">{varName}</span>
        <span className="nls-slider-value">
          [{formatNum(low)}, {formatNum(high)}]
        </span>
        <ControlConfig block={block} editor={editor} slider />
      </div>
      <div className="nls-range-track">
        <input
          type="range"
          min={min}
          max={max}
          step={step}
          value={low}
          onInput={(e) => commit(parseFloat((e.target as HTMLInputElement).value), high, false)}
          onPointerUp={() => commit(low, high, true)}
        />
        <input
          type="range"
          min={min}
          max={max}
          step={step}
          value={high}
          onInput={(e) => commit(low, parseFloat((e.target as HTMLInputElement).value), false)}
          onPointerUp={() => commit(low, high, true)}
        />
      </div>
      <div className="nls-slider-scale">
        <span>{formatNum(min)}</span>
        <span>{formatNum(max)}</span>
      </div>
    </div>
  );
}
//=============================================================================
export function FileBrowserCell({ block, editor }: BlockProps) {
  const ctx = useNotebook();
  const { label, varName, filename, runMode } = block.props;
  const inputRef = useRef<HTMLInputElement>(null);

  const setPath = (full: string) => {
    const base = full.split(/[\\/]/).pop() || full;
    // The variable gets the full host path (quoted by controlAssignment).
    editor.updateBlock(block, { props: { filename: base, value: full } });
    runWithMode(ctx, runMode ?? 'all', block.id);
  };

  const browse = async () => {
    // Prefer the desktop's native file dialog (returns a real host path);
    // fall back to the sandbox <input type=file> in the browser.
    try {
      const r = await rpc<{ available?: boolean; canceled?: boolean; path?: string }>(
        'fs.dialog.native',
        { mode: 'open', title: 'Select a file', historyKey: 'notebook.file' }
      );
      if (r.available) {
        if (!r.canceled && r.path) setPath(r.path);
        return;
      }
    } catch {
      /* no native dialog: use the browser input */
    }
    inputRef.current?.click();
  };

  return (
    <div className="nls-control" contentEditable={false}>
      <span className="nls-control-label">{label || varName}</span>
      <span className="nls-var-badge">{varName}</span>
      <button className="nls-btn" onClick={() => void browse()}>
        Browse&hellip;
      </button>
      <span className="nls-file-name">{filename || 'no file'}</span>
      <input
        ref={inputRef}
        type="file"
        hidden
        onChange={(e) => {
          const f = e.target.files?.[0];
          if (f) setPath(f.name);
        }}
      />
      <ControlConfig block={block} editor={editor} />
    </div>
  );
}
//=============================================================================
export function VariableSelectorCell({ block, editor }: BlockProps) {
  const ctx = useNotebook();
  const { label, varName, value, runMode } = block.props;
  const [vars, setVars] = useState<string[]>([]);
  useEffect(() => {
    let alive = true;
    rpc<WorkspaceListResult>('workspace.list')
      .then((r) => {
        if (alive) setVars((r.variables ?? []).map((v) => v.name));
      })
      .catch(() => {
        /* engine not ready */
      });
    return () => {
      alive = false;
    };
  }, []);
  const options = vars.length ? vars : value ? [value] : [];
  return (
    <div className="nls-control" contentEditable={false}>
      <span className="nls-control-label">{label || varName}</span>
      <span className="nls-var-badge">{varName}</span>
      <select
        className="nls-select"
        value={value}
        onChange={(e) => {
          editor.updateBlock(block, { props: { value: e.target.value } });
          runWithMode(ctx, runMode ?? 'all', block.id);
        }}
      >
        <option value="">(none)</option>
        {options.map((v) => (
          <option key={v} value={v}>
            {v}
          </option>
        ))}
      </select>
      <ControlConfig block={block} editor={editor} />
    </div>
  );
}
//=============================================================================
function splitList(s: string): string[] {
  return String(s ?? '')
    .split(',')
    .map((x) => x.trim())
    .filter(Boolean);
}
//=============================================================================
function formatNum(n: number): string {
  return Number.isInteger(n) ? String(n) : n.toFixed(2);
}
//=============================================================================
