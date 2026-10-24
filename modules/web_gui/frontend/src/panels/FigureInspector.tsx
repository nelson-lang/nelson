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
import type {
  FigureAxesState,
  FigureInspectorState,
  InspectorModel,
  InspectorProperty,
  InspectorTreeNode,
} from '../rpc/figureTypes';
import { inspectorList } from '../rpc/figureTypes';
import { FigureInspectorResize } from './FigureInspectorResize';
//=============================================================================
export function parseFigureProperty(
  text: string,
  kind: 'text' | 'number' | 'vector' | 'color',
  count?: number
): unknown {
  if (kind === 'text') return text;
  if (!text.trim()) throw new Error('A value is required.');
  if (kind === 'color' && !/^[\s\d.,+eE\-\[\]]+$/.test(text)) return text.trim();
  const values = text
    .replace(/[\[\]]/g, '')
    .trim()
    .split(/[\s,]+/)
    .map(Number);
  if (
    !values.length ||
    values.some((value) => !Number.isFinite(value)) ||
    (count !== undefined && values.length !== count)
  )
    throw new Error(`Enter ${count ?? 'finite'} numeric values.`);
  if (kind === 'number') return values[0];
  return values;
}
//=============================================================================
function Field({
  label,
  value,
  kind = 'text',
  count,
  onCommit,
}: {
  label: string;
  value: unknown;
  kind?: 'text' | 'number' | 'vector' | 'color';
  count?: number;
  onCommit: (value: unknown) => void;
}) {
  const formatted = Array.isArray(value)
    ? value.map((v) => Number(Number(v).toPrecision(7))).join(', ')
    : String(value ?? '');
  const [draft, setDraft] = useState(formatted);
  const [error, setError] = useState('');
  const editing = useRef(false);
  useEffect(() => {
    if (!editing.current) setDraft(formatted);
  }, [formatted]);
  const commit = () => {
    editing.current = false;
    if (draft === formatted) return;
    try {
      onCommit(parseFigureProperty(draft, kind, count));
      setDraft(formatted);
      setError('');
    } catch (err) {
      setError(err instanceof Error ? err.message : String(err));
    }
  };
  return (
    <label className="figure-property-field">
      <span>{label}</span>
      {kind === 'color' && (
        <input
          type="color"
          aria-label={`${label} picker`}
          value={
            Array.isArray(value) && value.length === 3
              ? '#' +
                value
                  .map((component) =>
                    Math.round(Math.max(0, Math.min(1, Number(component))) * 255)
                      .toString(16)
                      .padStart(2, '0')
                  )
                  .join('')
              : '#000000'
          }
          onChange={(event) => {
            const hex = event.target.value;
            onCommit([1, 3, 5].map((offset) => parseInt(hex.slice(offset, offset + 2), 16) / 255));
          }}
        />
      )}
      <input
        type={kind === 'number' ? 'number' : 'text'}
        step={kind === 'number' ? '0.5' : undefined}
        aria-label={label}
        value={draft}
        aria-invalid={!!error}
        onFocus={() => {
          editing.current = true;
        }}
        onChange={(event) => setDraft(event.target.value)}
        onBlur={commit}
        onKeyDown={(event) => {
          if (event.key === 'Enter') event.currentTarget.blur();
          if (event.key === 'Escape') {
            setDraft(formatted);
            setError('');
            editing.current = false;
          }
        }}
      />
      {error && (
        <span role="alert" className="figure-property-error">
          {error}
        </span>
      )}
    </label>
  );
}
//=============================================================================
function Select({
  label,
  value,
  choices,
  onCommit,
}: {
  label: string;
  value: string;
  choices: string[];
  onCommit: (value: string) => void;
}) {
  const options = choices.includes(value) ? choices : [value, ...choices];
  return (
    <label className="figure-property-field">
      <span>{label}</span>
      <select aria-label={label} value={value} onChange={(event) => onCommit(event.target.value)}>
        {options.map((choice) => (
          <option key={choice}>{choice}</option>
        ))}
      </select>
    </label>
  );
}
//=============================================================================
function Angle({
  label,
  value,
  min,
  max,
  onCommit,
}: {
  label: string;
  value: number;
  min: number;
  max: number;
  onCommit: (value: number) => void;
}) {
  const [draft, setDraft] = useState(value);
  const active = useRef(false);
  useEffect(() => {
    if (!active.current) setDraft(value);
  }, [value]);
  const commit = () => {
    active.current = false;
    if (draft !== value) onCommit(draft);
  };
  return (
    <label className="figure-property-field">
      <span>
        {label}
        <span aria-hidden="true">{draft.toFixed(1)}°</span>
      </span>
      <input
        type="range"
        aria-label={label}
        min={min}
        max={max}
        step="0.5"
        value={draft}
        onPointerDown={() => {
          active.current = true;
        }}
        onChange={(event) => {
          active.current = true;
          setDraft(Number(event.target.value));
        }}
        onPointerUp={commit}
        onKeyUp={commit}
        onBlur={commit}
      />
    </label>
  );
}
//=============================================================================
function ViewControls({
  axes,
  action,
}: {
  axes: FigureAxesState;
  action: (request: Record<string, unknown>) => void;
}) {
  return (
    <details open>
      <summary>View</summary>
      <Angle
        label="Azimuth"
        value={axes.View[0]}
        min={-180}
        max={180}
        onCommit={(value) => action({ action: 'view', azimuth: value, elevation: axes.View[1] })}
      />
      <Angle
        label="Elevation"
        value={axes.View[1]}
        min={-90}
        max={90}
        onCommit={(value) => action({ action: 'view', azimuth: axes.View[0], elevation: value })}
      />
      {axes.CameraPositionMode === 'manual' && (
        <p className="figure-workspace-muted">
          Camera position is manual. Reset the camera to control its orientation with View.
        </p>
      )}
      <button
        onClick={() => action({ action: 'view', azimuth: -37.5, elevation: 30, resetCamera: true })}
      >
        Reset camera
      </button>
      <label className="figure-property-check">
        <input
          type="checkbox"
          checked={axes.projectionFloor}
          onChange={(event) => action({ action: 'floor', enabled: event.target.checked })}
        />
        Projection onto floor
      </label>
    </details>
  );
}
//=============================================================================
function TreeNode({
  node,
  selected,
  onSelect,
  depth,
}: {
  node: InspectorTreeNode;
  selected: number;
  onSelect: (handle: number) => void;
  depth: number;
}) {
  const children = inspectorList(node.children);
  const label = node.label && node.label !== node.type ? `${node.type} (${node.label})` : node.type;
  return (
    <div className="figure-tree-node">
      <button
        type="button"
        className={'figure-tree-label' + (node.handle === selected ? ' is-selected' : '')}
        style={{ paddingLeft: depth * 12 + 4 }}
        data-handle={node.handle}
        onClick={() => onSelect(node.handle)}
      >
        {label}
      </button>
      {children.map((child, index) => (
        <TreeNode
          key={`${child.handle}-${index}`}
          node={child}
          selected={selected}
          onSelect={onSelect}
          depth={depth + 1}
        />
      ))}
    </div>
  );
}
//=============================================================================
function PropertyRow({
  prop,
  onCommit,
}: {
  prop: InspectorProperty;
  onCommit: (value: unknown) => void;
}) {
  const { name, kind, writable, display, value, choices } = prop;
  if (!writable || kind === 'info') {
    return (
      <div className="figure-property-field figure-property-readonly">
        <span>{name}</span>
        <span className="figure-property-value" title={display}>
          {display}
        </span>
      </div>
    );
  }
  if (kind === 'enum') {
    return (
      <Select label={name} value={String(value ?? display)} choices={choices} onCommit={onCommit} />
    );
  }
  if (kind === 'bool') {
    const checked = value === true || value === 'true' || value === 1;
    return (
      <label className="figure-property-field figure-property-check">
        <span>{name}</span>
        <input
          type="checkbox"
          checked={checked}
          onChange={(event) => onCommit(event.target.checked)}
        />
      </label>
    );
  }
  if (kind === 'color') {
    return <Field label={name} value={value} kind="color" onCommit={onCommit} />;
  }
  if (kind === 'number') {
    return <Field label={name} value={value} kind="number" count={1} onCommit={onCommit} />;
  }
  if (kind === 'vector') {
    return <Field label={name} value={value} kind="vector" onCommit={onCommit} />;
  }
  return <Field label={name} value={value ?? display} kind="text" onCommit={onCommit} />;
}
//=============================================================================
interface Props {
  state: FigureInspectorState | null;
  axes: FigureAxesState | null;
  model: InspectorModel | null;
  tree: InspectorTreeNode | null;
  selectedObject: number;
  selectObject: (handle: number) => void;
  action: (request: Record<string, unknown>) => void;
  width?: number;
  onWidthChange?: (width: number) => void;
}
//=============================================================================
export function FigureInspector({
  state,
  axes,
  model,
  tree,
  selectedObject,
  selectObject,
  action,
  width = 260,
  onWidthChange,
}: Props) {
  const [filter, setFilter] = useState('');
  if (!state) {
    return (
      <aside className="figure-inspector" aria-label="Figure properties">
        Loading properties…
      </aside>
    );
  }
  const change = (name: string, value: unknown) =>
    action({
      action: 'properties',
      objectHandle: model?.handle ?? selectedObject,
      properties: { [name]: value },
    });
  const needle = filter.trim().toLowerCase();
  const properties = inspectorList(model?.properties).filter(
    (prop) =>
      !needle ||
      prop.name.toLowerCase().includes(needle) ||
      prop.display.toLowerCase().includes(needle)
  );
  return (
    <aside
      className="figure-inspector"
      aria-label="Figure properties"
      style={{ width, flexBasis: width }}
    >
      {onWidthChange && <FigureInspectorResize width={width} onChange={onWidthChange} />}
      <h3>Properties</h3>
      {axes && <ViewControls axes={axes} action={action} />}
      <details open>
        <summary>Objects</summary>
        <div className="figure-inspector-browser" role="tree" aria-label="Object browser">
          {tree ? (
            <TreeNode node={tree} selected={selectedObject} onSelect={selectObject} depth={0} />
          ) : (
            <p className="figure-workspace-muted">Loading objects…</p>
          )}
        </div>
      </details>
      <input
        className="figure-property-search"
        type="search"
        aria-label="Search properties"
        placeholder="Search properties"
        value={filter}
        onChange={(event) => setFilter(event.target.value)}
      />
      <div className="figure-inspector-list" data-object={model?.handle ?? ''}>
        {model ? (
          properties.map((prop) => (
            <PropertyRow
              key={prop.name}
              prop={prop}
              onCommit={(value) => change(prop.name, value)}
            />
          ))
        ) : (
          <p className="figure-workspace-muted">Select an object to inspect its properties.</p>
        )}
      </div>
    </aside>
  );
}
//=============================================================================
