//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { memo, useEffect, useRef, useState, type CSSProperties } from 'react';
import type { MenuNode, TreeNode, UIControl } from '../rpc/types';
import { ContextMenuPopup } from './FigureMenuBar';
import { buttonLabelPlacement } from './buttonLabelPlacement';
import { controlToSvg } from './figureSvg';
import { FigureSliderControl } from './FigureSliderControl';
//=============================================================================
type ContextState = { items: MenuNode[]; x: number; y: number };
function TreeNodes({
  nodes,
  selected,
  onSelect,
}: {
  nodes: TreeNode[];
  selected: number | null;
  onSelect: (handle: number) => void;
}): JSX.Element {
  const label = (n: TreeNode): JSX.Element => (
    <span
      style={{
        cursor: 'pointer',
        padding: '0 4px',
        borderRadius: 2,
        background: n.handle != null && n.handle === selected ? '#cde3ff' : 'transparent',
      }}
      onClick={(e) => {
        e.stopPropagation();
        if (n.handle != null) onSelect(n.handle);
      }}
    >
      {n.text}
    </span>
  );
  return (
    <ul style={{ listStyle: 'none', margin: 0, paddingLeft: 16 }}>
      {nodes.map((n, i) => (
        <li key={i}>
          {n.children && n.children.length > 0 ? (
            <details open>
              <summary style={{ cursor: 'pointer' }}>{label(n)}</summary>
              <TreeNodes nodes={n.children} selected={selected} onSelect={onSelect} />
            </details>
          ) : (
            label(n)
          )}
        </li>
      ))}
    </ul>
  );
}
//=============================================================================
function TreeControl({
  c,
  box,
  onAction,
}: {
  c: UIControl;
  box: CSSProperties;
  onAction: ActionHandler;
}): JSX.Element {
  const nodes = c.data?.nodes ?? [];
  const [selected, setSelected] = useState<number | null>(null);
  return (
    <div
      key={c.handle}
      title={c.tooltip || undefined}
      className="figure-uicontrol"
      style={{ ...box, overflow: 'auto', background: c.background || '#fff', paddingLeft: 0 }}
    >
      <TreeNodes
        nodes={nodes}
        selected={selected}
        onSelect={(handle) => {
          setSelected(handle);
          onAction(c, 0, String(handle));
        }}
      />
    </div>
  );
}
//=============================================================================
type ActionHandler = (control: UIControl, value: number, text: string) => void;
function KnobControl({
  c,
  box,
  onAction,
}: {
  c: UIControl;
  box: CSSProperties;
  onAction: ActionHandler;
}): JSX.Element {
  const cx = c.w / 2;
  const cy = c.h / 2;
  // Pointer-to-value mapping uses screen angles (y down), 135deg (lower-left) to
  // 405deg (lower-right): the same screen points as the Qt-convention face below.
  const START = 135;
  const SWEEP = 270;
  const range = c.max - c.min || 1;
  const dragging = useRef(false);

  const valueFromEvent = (e: React.PointerEvent<SVGSVGElement>): number => {
    const rect = e.currentTarget.getBoundingClientRect();
    const deg =
      (Math.atan2(e.clientY - (rect.top + cy), e.clientX - (rect.left + cx)) * 180) / Math.PI;
    let rel = deg - START;
    while (rel < 0) rel += 360;
    while (rel >= 360) rel -= 360;
    // Snap the bottom dead zone to whichever end is closer.
    if (rel > SWEEP) rel = rel > SWEEP + (360 - SWEEP) / 2 ? 0 : SWEEP;
    return c.min + (rel / SWEEP) * range;
  };

  return (
    <svg
      key={c.handle}
      className="figure-uicontrol"
      style={{
        ...box,
        background: undefined,
        overflow: 'visible',
        cursor: c.enabled ? 'pointer' : 'default',
      }}
      viewBox={`0 0 ${c.w} ${c.h}`}
      onPointerDown={(e) => {
        if (!c.enabled) return;
        dragging.current = true;
        e.currentTarget.setPointerCapture(e.pointerId);
        onAction(c, valueFromEvent(e), c.text);
      }}
      onPointerMove={(e) => {
        if (dragging.current) onAction(c, valueFromEvent(e), c.text);
      }}
      onPointerUp={(e) => {
        dragging.current = false;
        e.currentTarget.releasePointerCapture(e.pointerId);
      }}
      // Reuse the exact SVG face the export draws (ticks, labels, pointer), in
      // local coordinates.
      dangerouslySetInnerHTML={{ __html: controlToSvg({ ...c, x: 0, y: 0 }) }}
    />
  );
}
//=============================================================================
function TableControl({
  c,
  box,
  onAction,
}: {
  c: UIControl;
  box: CSSProperties;
  onAction: ActionHandler;
}): JSX.Element {
  const columns = c.data?.columns ?? [];
  const rows = c.data?.rows ?? [];
  const editable = c.data?.editable ?? [];
  const columnWidths = c.data?.columnWidths ?? [];
  const columnCount = Math.max(columns.length, ...rows.map((row) => row.length), 0);
  const fixedWidths = Array.from({ length: columnCount }, (_, i) => {
    const width = columnWidths[i];
    return typeof width === 'number' && Number.isFinite(width) && width > 0 ? width : null;
  });
  const hasFixedWidths = fixedWidths.some((width) => width != null);
  const fixedTotal = fixedWidths.reduce<number>((sum, width) => sum + (width ?? 0), 0);
  const autoCount = fixedWidths.filter((width) => width == null).length;
  const autoWidth = autoCount > 0 && fixedTotal < c.w ? (c.w - fixedTotal) / autoCount : 76;
  const renderedWidths = hasFixedWidths
    ? fixedWidths.map((width) => Math.max(1, Math.round(width ?? autoWidth)))
    : [];
  const renderedTableWidth =
    renderedWidths.length > 0 ? renderedWidths.reduce((sum, width) => sum + width, 0) : undefined;
  const cell: CSSProperties = {
    border: '1px solid #d0d0d0',
    padding: '2px 6px',
    textAlign: 'left',
    whiteSpace: 'nowrap',
  };
  // A cell edit carries its coordinates through the text channel as JSON.
  const sendEdit = (row: number, col: number, value: string) =>
    onAction(c, 0, JSON.stringify({ row, col, value }));
  return (
    <div
      key={c.handle}
      title={c.tooltip || undefined}
      className="figure-uicontrol"
      style={{ ...box, overflow: 'auto', background: c.background || '#fff' }}
    >
      <table
        style={{
          borderCollapse: 'collapse',
          width: renderedTableWidth != null ? renderedTableWidth : '100%',
          tableLayout: renderedTableWidth != null ? 'fixed' : 'auto',
          fontSize: 'inherit',
        }}
      >
        {renderedWidths.length > 0 && (
          <colgroup>
            <col style={{ width: 34 }} />
            {renderedWidths.map((width, i) => (
              <col key={i} style={{ width }} />
            ))}
          </colgroup>
        )}
        {columns.length > 0 && (
          <thead>
            <tr>
              <th style={{ ...cell, background: '#f0f0f0', width: 34 }} />
              {columns.map((h, i) => (
                <th key={i} style={{ ...cell, background: '#f0f0f0', fontWeight: 600 }}>
                  {h}
                </th>
              ))}
            </tr>
          </thead>
        )}
        <tbody>
          {rows.map((row, ri) => (
            <tr key={ri}>
              <td style={{ ...cell, background: '#f2f2f2', textAlign: 'center', color: '#555' }}>
                {ri + 1}
              </td>
              {row.map((v, ci) =>
                editable[ci] ? (
                  <td key={ci} style={{ ...cell, padding: 0 }}>
                    <input
                      defaultValue={v}
                      style={{
                        width: '100%',
                        boxSizing: 'border-box',
                        border: 'none',
                        padding: '2px 6px',
                        font: 'inherit',
                        background: 'transparent',
                      }}
                      onBlur={(e) => {
                        if (e.target.value !== v) sendEdit(ri, ci, e.target.value);
                      }}
                      onKeyDown={(e) => {
                        if (e.key === 'Enter') e.currentTarget.blur();
                      }}
                    />
                  </td>
                ) : (
                  <td key={ci} style={cell}>
                    {v}
                  </td>
                )
              )}
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
}
//=============================================================================
function GaugeControl({ c, box }: { c: UIControl; box: CSSProperties }): JSX.Element {
  return (
    <svg
      key={c.handle}
      className="figure-uicontrol"
      style={{ ...box, background: undefined, pointerEvents: 'none', overflow: 'visible' }}
      viewBox={`0 0 ${c.w} ${c.h}`}
      dangerouslySetInnerHTML={{ __html: controlToSvg({ ...c, x: 0, y: 0 }) }}
    />
  );
}
//=============================================================================
function SwitchControl({
  c,
  box,
  onAction,
}: {
  c: UIControl;
  box: CSSProperties;
  onAction: ActionHandler;
}): JSX.Element {
  const on = c.value >= 1;
  const offLabel = c.items?.[0] ?? 'Off';
  const onLabel = c.items?.[1] ?? 'On';
  const vertical = c.data?.vertical ?? false;

  // Rocker switches use a vertical authored rectangle. The SVG face is the same as
  // the figure export, so the live control has the native geometry and stays a real
  // accessible element.
  if (vertical) {
    return (
      <svg
        key={c.handle}
        className="figure-uicontrol"
        role="button"
        aria-label={`${offLabel}/${onLabel}`}
        aria-pressed={on}
        tabIndex={c.enabled ? 0 : -1}
        style={{
          ...box,
          background: undefined,
          overflow: 'visible',
          cursor: c.enabled ? 'pointer' : 'default',
        }}
        viewBox={`0 0 ${c.w} ${c.h}`}
        onClick={() => {
          if (c.enabled) onAction(c, on ? 0 : 1, c.text);
        }}
        onKeyDown={(e) => {
          if (c.enabled && (e.key === 'Enter' || e.key === ' ')) {
            e.preventDefault();
            onAction(c, on ? 0 : 1, c.text);
          }
        }}
        dangerouslySetInnerHTML={{ __html: controlToSvg({ ...c, x: 0, y: 0 }) }}
      />
    );
  }

  return (
    <div
      key={c.handle}
      title={c.tooltip || undefined}
      className="figure-uicontrol"
      style={{ ...box, display: 'flex', alignItems: 'center', gap: 6, background: undefined }}
    >
      <span style={{ position: 'absolute', right: 'calc(100% + 4px)', whiteSpace: 'nowrap' }}>
        {offLabel}
      </span>
      <button
        type="button"
        disabled={!c.enabled}
        onClick={() => onAction(c, on ? 0 : 1, c.text)}
        style={{
          position: 'relative',
          width: c.w,
          height: 20,
          flex: '0 0 auto',
          padding: 0,
          borderRadius: 10,
          border: '1px solid #8f8f8f',
          background: on ? '#0a84ff' : '#cccccc',
          cursor: c.enabled ? 'pointer' : 'default',
        }}
      >
        <span
          style={{
            position: 'absolute',
            top: 1,
            left: on ? c.w - 19 : 1,
            width: 16,
            height: 16,
            borderRadius: '50%',
            background: '#ffffff',
            transition: 'left 0.12s',
          }}
        />
      </button>
      <span style={{ position: 'absolute', left: 'calc(100% + 4px)', whiteSpace: 'nowrap' }}>
        {onLabel}
      </span>
    </div>
  );
}
//=============================================================================
// Real HTML widgets laid out over the canvas at the coordinates the engine reports.
// Other render backends blit a snapshot of the Qt widget into the picture, which
// cannot be clicked. A DOM element has focus, keyboard and accessibility like the
// rest of the desktop.
function FigureControlsImpl({
  controls,
  onAction,
  onMenuSelect,
}: {
  controls: UIControl[];
  // value/text as the user left them; the engine stores them and runs Callback.
  onAction: (control: UIControl, value: number, text: string) => void;
  // a uicontextmenu item was chosen: run its MenuSelectedFcn by handle.
  onMenuSelect: (handle: number) => void;
}): JSX.Element {
  const [ctx, setCtx] = useState<ContextState | null>(null);
  const [pendingActionHandle, setPendingActionHandle] = useState<number | null>(null);
  const previousControlsRef = useRef(new Map<number, { text: string; value: number }>());
  useEffect(() => {
    if (pendingActionHandle !== null) {
      const previous = previousControlsRef.current.get(pendingActionHandle);
      const current = controls.find((control) => control.handle === pendingActionHandle);
      if (
        !current ||
        (previous && (previous.text !== current.text || previous.value !== current.value))
      ) {
        setPendingActionHandle(null);
      }
    }
    previousControlsRef.current = new Map(
      controls.map((control) => [control.handle, { text: control.text, value: control.value }])
    );
  }, [controls, pendingActionHandle]);
  const sendButtonAction = (control: UIControl, value: number, text: string) => {
    setPendingActionHandle(control.handle);
    window.setTimeout(() => {
      setPendingActionHandle((handle) => (handle === control.handle ? null : handle));
    }, 2000);
    onAction(control, value, text);
  };
  // A single right-click handler on the overlay finds the top-most control with
  // a uicontextmenu under the cursor (by box), so every widget style is covered
  // without threading a handler through each element. When none matches, the
  // event bubbles to the canvas for the figure-level context menu.
  const onOverlayContextMenu = (e: React.MouseEvent) => {
    const rect = (e.currentTarget as HTMLElement).getBoundingClientRect();
    const lx = e.clientX - rect.left;
    const ly = e.clientY - rect.top;
    for (let i = controls.length - 1; i >= 0; i--) {
      const c = controls[i];
      if (
        c.contextMenu &&
        c.contextMenu.length > 0 &&
        lx >= c.x &&
        lx <= c.x + c.w &&
        ly >= c.y &&
        ly <= c.y + c.h
      ) {
        e.preventDefault();
        // Don't also let the canvas open the figure-level context menu.
        e.stopPropagation();
        setCtx({ items: c.contextMenu, x: e.clientX, y: e.clientY });
        return;
      }
    }
  };
  return (
    <div
      style={{ position: 'absolute', inset: 0, pointerEvents: 'none' }}
      onContextMenu={onOverlayContextMenu}
    >
      {controls.map((c) => {
        const box: CSSProperties = {
          position: 'absolute',
          left: c.x,
          top: c.y,
          width: c.w,
          height: c.h,
          fontSize: c.fontSize ? `${c.fontSize}px` : undefined,
          background: c.background || undefined,
          color: c.foreground || undefined,
          boxSizing: 'border-box',
          // Enable === 'inactive': keep the normal (non-greyed) look but ignore
          // pointer input, so the control is visible yet not clickable/editable.
          pointerEvents: c.inactive ? 'none' : 'auto',
        };
        const common = {
          key: c.handle,
          title: c.tooltip || undefined,
          disabled: !c.enabled,
          style: box,
          className: 'figure-uicontrol',
        };
        switch (c.style) {
          case 'text':
            return (
              <span
                key={c.handle}
                title={c.tooltip || undefined}
                style={{ ...box, display: 'flex', alignItems: 'center' }}
                className="figure-uicontrol figure-uicontrol--label"
              >
                {c.text}
              </span>
            );
          case 'edit':
            return (
              <input
                {...common}
                type="text"
                defaultValue={c.text}
                onBlur={(e) => onAction(c, c.value, e.target.value)}
                onKeyDown={(e) => {
                  if (e.key === 'Enter') onAction(c, c.value, e.currentTarget.value);
                }}
              />
            );
          case 'spinner':
            return (
              <input
                {...common}
                type="number"
                defaultValue={c.text}
                min={Number.isFinite(c.min) ? c.min : undefined}
                max={Number.isFinite(c.max) ? c.max : undefined}
                onBlur={(e) => onAction(c, 0, e.target.value)}
                onKeyDown={(e) => {
                  if (e.key === 'Enter') onAction(c, 0, e.currentTarget.value);
                }}
              />
            );
          case 'checkbox':
          case 'radiobutton':
            return (
              <label
                key={c.handle}
                title={c.tooltip || undefined}
                style={{ ...box, display: 'flex', alignItems: 'center', gap: 4 }}
                className="figure-uicontrol"
              >
                <input
                  type={c.style === 'checkbox' ? 'checkbox' : 'radio'}
                  checked={c.value >= c.max}
                  disabled={!c.enabled}
                  onChange={(e) => onAction(c, e.target.checked ? c.max : c.min, c.text)}
                />
                {c.text}
              </label>
            );
          case 'slider':
            if (c.data?.major && !c.data.range) {
              return (
                <FigureSliderControl key={c.handle} control={c} box={box} onAction={onAction} />
              );
            }
            return (
              <input
                {...common}
                type="range"
                min={c.min}
                max={c.max}
                step={(c.max - c.min) / 100 || 1}
                defaultValue={c.value}
                onChange={(e) => onAction(c, Number(e.target.value), c.text)}
              />
            );
          case 'popupmenu':
            return (
              <select
                {...common}
                value={String(Math.max(1, Math.round(c.value)))}
                onChange={(e) => onAction(c, Number(e.target.value), c.text)}
              >
                {(c.items ?? [c.text]).map((item, i) => (
                  <option key={item + i} value={i + 1}>
                    {item}
                  </option>
                ))}
              </select>
            );
          case 'listbox': {
            const multi = c.data?.multiselect ?? false;
            const selected = c.data?.selected ?? [Math.round(c.value)];
            // Send the selected 1-based indices as JSON; the engine rebuilds
            // Value/ValueIndex (a cell array for multi-select).
            const sendSelection = (e: { target: HTMLSelectElement }) =>
              onAction(
                c,
                0,
                JSON.stringify(Array.from(e.target.selectedOptions).map((o) => Number(o.value)))
              );
            return (
              <select
                {...common}
                multiple={multi}
                size={Math.max(2, Math.floor(c.h / 18))}
                value={multi ? selected.map(String) : String(selected[0] ?? 1)}
                onChange={sendSelection}
              >
                {(c.items ?? []).map((item, i) => (
                  <option key={item + i} value={i + 1}>
                    {item}
                  </option>
                ))}
              </select>
            );
          }
          case 'date':
            return (
              <input
                {...common}
                type="date"
                defaultValue={c.text}
                onChange={(e) => onAction(c, 0, e.target.value)}
              />
            );
          case 'lamp': {
            // Only the disc is painted: the corners keep the parent colour.
            const d = Math.max(0, Math.min(c.w, c.h) - 2);
            return (
              <span
                key={c.handle}
                title={c.tooltip || undefined}
                style={{ ...box, background: undefined }}
              >
                <span
                  style={{
                    position: 'absolute',
                    left: 1,
                    top: 1,
                    width: d,
                    height: d,
                    boxSizing: 'border-box',
                    borderRadius: '50%',
                    border: '1px solid rgb(120,120,120)',
                    background: c.background || undefined,
                  }}
                />
              </span>
            );
          }
          case 'knob':
            return <KnobControl key={c.handle} c={c} box={box} onAction={onAction} />;
          case 'gauge':
            return <GaugeControl key={c.handle} c={c} box={box} />;
          case 'table':
            return <TableControl key={c.handle} c={c} box={box} onAction={onAction} />;
          case 'tree':
            return <TreeControl key={c.handle} c={c} box={box} onAction={onAction} />;
          case 'tabgroup': {
            const tabs = c.items ?? [];
            const sel = Math.round(c.value);
            const tabLocation = (c.data?.tabLocation ?? 'top').toLowerCase();
            const vertical = tabLocation === 'left' || tabLocation === 'right';
            return (
              <div
                key={c.handle}
                className="figure-uicontrol"
                style={{
                  ...box,
                  border: '1px solid #b0b0b0',
                  overflow: 'visible',
                  pointerEvents: 'none',
                }}
              >
                <div
                  style={{
                    position: 'absolute',
                    zIndex: 2,
                    ...(tabLocation === 'bottom'
                      ? { bottom: 0, left: 0 }
                      : tabLocation === 'left'
                      ? { left: 0, top: 0 }
                      : tabLocation === 'right'
                      ? { right: 0, top: 0 }
                      : { left: 0, top: 0 }),
                    display: 'flex',
                    flexDirection: vertical ? 'column' : 'row',
                    background: '#ececec',
                    pointerEvents: 'auto',
                  }}
                >
                  {tabs.map((t, i) => (
                    <button
                      key={i}
                      onClick={() => onAction(c, i + 1, t)}
                      style={{
                        padding: '3px 12px',
                        border: '1px solid #b0b0b0',
                        ...(vertical
                          ? { borderRight: 'none', marginBottom: 2 }
                          : tabLocation === 'bottom'
                          ? { borderTop: 'none', marginRight: 2 }
                          : { borderBottom: 'none', marginRight: 2 }),
                        background: i + 1 === sel ? '#fff' : '#ececec',
                        fontWeight: i + 1 === sel ? 600 : 400,
                        cursor: 'pointer',
                      }}
                    >
                      {t}
                    </button>
                  ))}
                </div>
              </div>
            );
          }
          case 'frame':
            // A classic uicontrol frame is a decorative backdrop. Controls sit on top of
            // it, so it must not intercept their pointer events.
            return (
              <div
                key={c.handle}
                title={c.tooltip || undefined}
                className="figure-uicontrol"
                style={{
                  ...box,
                  border: `1px solid ${c.foreground || '#7d7d7d'}`,
                  background: c.background || undefined,
                  pointerEvents: 'none',
                }}
              />
            );
          case 'panel':
            // The canvas paints the container background, border and title.
            // Repainting an HTML box here hides the title and contained axes.
            return null;
          case 'switch':
            return <SwitchControl key={c.handle} c={c} box={box} onAction={onAction} />;
          case 'togglebutton':
            const togglePending = pendingActionHandle === c.handle;
            return (
              <button
                {...common}
                style={{
                  ...box,
                  ...buttonLabelPlacement(c),
                  cursor: togglePending ? 'progress' : undefined,
                }}
                aria-pressed={c.value >= c.max}
                className={`figure-uicontrol${
                  c.value >= c.max ? ' figure-uicontrol--pressed' : ''
                }`}
                onPointerDown={(e) => {
                  e.preventDefault();
                  sendButtonAction(c, c.value >= c.max ? c.min : c.max, c.text);
                }}
                onKeyDown={(e) => {
                  if (e.key === 'Enter' || e.key === ' ') {
                    e.preventDefault();
                    sendButtonAction(c, c.value >= c.max ? c.min : c.max, c.text);
                  }
                }}
              >
                {togglePending ? `${c.text}…` : c.text}
              </button>
            );
          case 'pushbutton':
            const pushPending = pendingActionHandle === c.handle;
            return (
              <button
                {...common}
                style={{
                  ...box,
                  ...buttonLabelPlacement(c),
                  cursor: pushPending ? 'progress' : undefined,
                }}
                onPointerDown={(e) => {
                  e.preventDefault();
                  sendButtonAction(c, c.value, c.text);
                }}
                onKeyDown={(e) => {
                  if (e.key === 'Enter' || e.key === ' ') {
                    e.preventDefault();
                    sendButtonAction(c, c.value, c.text);
                  }
                }}
              >
                {pushPending ? `${c.text}…` : c.text}
              </button>
            );
          case 'html':
            // uihtml is drawn by an iframe owned by uihtmlHost, outside React: moving an
            // element in the DOM reloads an iframe, and this list reorders with the
            // figure's children. The default case would also cover it with a button
            // carrying the label meant for the export raster.
            return null;
          default:
            return (
              <button {...common} onClick={() => onAction(c, c.value, c.text)}>
                {c.text}
              </button>
            );
        }
      })}
      {ctx ? (
        <ContextMenuPopup
          items={ctx.items}
          x={ctx.x}
          y={ctx.y}
          onSelect={onMenuSelect}
          onClose={() => setCtx(null)}
        />
      ) : null}
    </div>
  );
}
//=============================================================================
export const FigureControls = memo(FigureControlsImpl);
//=============================================================================
