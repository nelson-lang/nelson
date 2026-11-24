//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { afterEach, beforeEach, expect, it, vi } from 'vitest';
import { FigureInspection } from '../../src/panels/FigureInspection';
import { figureHistoryGroup } from '../../src/panels/figureHistory';
import { FigureLegend } from '../../src/panels/FigureLegend';
import { FigureInspector } from '../../src/panels/FigureInspector';
import { FigureToolbar } from '../../src/panels/FigureToolbar';
import { figureShortcut } from '../../src/panels/figureShortcuts';
import {
  figurePreferencesKey,
  readFigurePreferences,
  useFigurePreferences,
} from '../../src/panels/figurePreferences';
import { mockFigureAction } from '../../src/rpc/mockFigureActions';
import type {
  FigureAxesState,
  FigureInspectorState,
  FigurePlotObject,
  InspectorKind,
  InspectorModel,
  InspectorProperty,
  InspectorTreeNode,
} from '../../src/rpc/figureTypes';
//=============================================================================
let host: HTMLDivElement, root: Root;
beforeEach(() => {
  vi.stubGlobal('IS_REACT_ACT_ENVIRONMENT', true);
  localStorage.clear();
  host = document.createElement('div');
  document.body.append(host);
  root = createRoot(host);
});
//=============================================================================
afterEach(() => {
  act(() => root.unmount());
  host.remove();
  vi.useRealTimers();
  vi.unstubAllGlobals();
  localStorage.clear();
});
//=============================================================================
const objects: FigurePlotObject[] = [
  { handle: 10, type: 'line', visible: 'on', DisplayName: 'Signal', Color: [1, 0, 0] },
  { handle: 11, type: 'line', visible: 'off', DisplayName: 'Other' },
];
//=============================================================================
it('keeps gesture identifiers unique without secure-context APIs', () => {
  vi.stubGlobal('crypto', {});
  const first = figureHistoryGroup();
  expect(first).toMatch(/^figure-gesture-/);
  expect(figureHistoryGroup()).not.toBe(first);
});
//=============================================================================
it('places an accessible data tip beside the selected sample without adding it to the canvas', () => {
  act(() =>
    root.render(
      <FigureInspection
        objects={objects}
        width={400}
        height={300}
        point={{ found: true, objectHandle: 10, index: 2, position: [5, 6, 7], pixel: [390, 295] }}
      />
    )
  );
  const tip = host.querySelector<HTMLElement>('[role=status]')!;
  expect(tip.textContent).toContain('Signal · Point 2');
  expect(tip.textContent).toContain('X 5 · Y 6 · Z 7');
  expect(tip.style.top).toBe('0px');
  expect(tip.style.left).toBe('170px');
  expect(host.querySelector('canvas')).toBeNull();
  act(() =>
    root.render(<FigureInspection objects={objects} width={400} height={300} point={null} />)
  );
  expect(host.childElementCount).toBe(0);
});
//=============================================================================
it('distinguishes a single legend click from isolation and cancels pending clicks on unmount', () => {
  vi.useFakeTimers();
  const action = vi.fn();
  act(() => root.render(<FigureLegend objects={objects} action={action} />));
  const buttons = host.querySelectorAll('button');
  const click = (button: Element, detail: number) =>
    act(() => button.dispatchEvent(new MouseEvent('click', { bubbles: true, detail })));
  click(buttons[0], 1);
  click(buttons[1], 1);
  act(() => vi.advanceTimersByTime(500));
  expect(action).toHaveBeenCalledWith({
    action: 'properties',
    objectHandle: 10,
    properties: { Visible: 'off' },
  });
  expect(action).toHaveBeenCalledWith({
    action: 'properties',
    objectHandle: 11,
    properties: { Visible: 'on' },
  });
  action.mockClear();
  click(buttons[0], 1);
  click(buttons[0], 2);
  act(() => buttons[0].dispatchEvent(new MouseEvent('dblclick', { bubbles: true })));
  act(() => vi.advanceTimersByTime(600));
  expect(action).toHaveBeenCalledExactlyOnceWith({ action: 'isolate', objectHandle: 10 });
  click(buttons[1], 1);
  act(() => root.render(<div />));
  act(() => vi.advanceTimersByTime(600));
  expect(action).toHaveBeenCalledTimes(1);
});
//=============================================================================
it('starts each new figure with its inspector closed', () => {
  localStorage.setItem(
    figurePreferencesKey,
    JSON.stringify({ properties: true, navigation: true, details: true, width: 410 })
  );
  function Preferences() {
    const { preferences } = useFigurePreferences();
    return <span>{JSON.stringify(preferences)}</span>;
  }
  act(() => root.render(<Preferences />));
  expect(host.textContent).toContain('"properties":false');
  expect(host.textContent).toContain('"navigation":true');
  expect(host.textContent).toContain('"width":410');
});
//=============================================================================
it('remembers panels and inspector width, validates stored preferences and tolerates storage errors', () => {
  function Preferences() {
    const { preferences, change } = useFigurePreferences();
    return (
      <button
        onClick={() => {
          change('properties', true);
          change('navigation', true);
          change('details', true);
          change('width', 410);
        }}
      >
        {JSON.stringify(preferences)}
      </button>
    );
  }
  act(() => root.render(<Preferences />));
  act(() => host.querySelector('button')!.click());
  expect(readFigurePreferences()).toEqual({
    properties: true,
    navigation: true,
    details: true,
    width: 410,
  });
  act(() => root.render(<div />));
  act(() => root.render(<Preferences />));
  expect(host.textContent).toContain('410');
  localStorage.setItem(figurePreferencesKey, '{bad');
  expect(readFigurePreferences().properties).toBe(false);
  localStorage.setItem(figurePreferencesKey, JSON.stringify({ width: 99999 }));
  expect(readFigurePreferences().width).toBe(600);
  const get = vi.spyOn(Storage.prototype, 'getItem').mockImplementation(() => {
    throw new Error('denied');
  });
  expect(readFigurePreferences().width).toBe(260);
  get.mockRestore();
});
//=============================================================================
it('provides color, line, marker and numeric editors and keyboard resizing', () => {
  const action = vi.fn(),
    resize = vi.fn();
  const state = mockFigureAction(
    { handle: 0, title: '', kind: 'line' },
    { action: 'state' },
    { handle: 0, width: 400, height: 300, primitives: [] }
  ).state as FigureInspectorState;
  const axes = {
    ...state.axes[0],
    objects: [{ ...objects[0], LineStyle: '-', Marker: 'none', MarkerSize: 6, LineWidth: 0.5 }],
  } as FigureAxesState;
  // the editors come from the engine inspector model of the selected object
  const property = (name: string, kind: InspectorKind, value: unknown, choices: string[] = []) =>
    ({ name, kind, writable: true, display: String(value), value, choices } as InspectorProperty);
  const model: InspectorModel = {
    handle: 10,
    type: 'line',
    label: 'Signal',
    properties: [
      property('LineStyle', 'enum', '-', ['-', '--', ':', '-.', 'none']),
      property('MarkerSize', 'number', 6),
      property('Color', 'color', [1, 0, 0]),
    ],
  };
  const tree: InspectorTreeNode = {
    handle: 0,
    type: 'figure',
    label: '',
    children: [
      {
        handle: axes.handle,
        type: 'axes',
        label: '',
        children: [{ handle: 10, type: 'line', label: 'Signal', children: [] }],
      },
    ],
  };
  act(() =>
    root.render(
      <FigureInspector
        state={state}
        axes={axes}
        model={model}
        tree={tree}
        selectedObject={10}
        selectObject={vi.fn()}
        action={action}
        width={350}
        onWidthChange={resize}
      />
    )
  );
  const select = host.querySelector<HTMLSelectElement>('[aria-label="LineStyle"]')!;
  act(() => {
    select.value = '--';
    select.dispatchEvent(new Event('change', { bubbles: true }));
  });
  expect(action).toHaveBeenLastCalledWith({
    action: 'properties',
    objectHandle: 10,
    properties: { LineStyle: '--' },
  });
  expect(host.querySelector('[aria-label="MarkerSize"]')?.getAttribute('type')).toBe('number');
  const picker = host.querySelector<HTMLInputElement>('[aria-label="Color picker"]')!;
  act(() => {
    Object.getOwnPropertyDescriptor(HTMLInputElement.prototype, 'value')!.set!.call(
      picker,
      '#00ff00'
    );
    picker.dispatchEvent(new Event('change', { bubbles: true }));
  });
  expect(action).toHaveBeenLastCalledWith({
    action: 'properties',
    objectHandle: 10,
    properties: { Color: [0, 1, 0] },
  });
  act(() =>
    host
      .querySelector('[role=separator]')!
      .dispatchEvent(new KeyboardEvent('keydown', { key: 'ArrowLeft', bubbles: true }))
  );
  expect(resize).toHaveBeenLastCalledWith(360);
});
//=============================================================================
it('exposes engine-confirmed undo/redo buttons and inspection/history shortcuts', () => {
  const action = vi.fn(),
    noop = vi.fn();
  const props = {
    title: 'Signal',
    is3d: false,
    tool: 'inspect' as const,
    setTool: noop,
    navigationVisible: false,
    toggleNavigation: noop,
    detailsVisible: false,
    toggleDetails: noop,
    inspectorOpen: false,
    toggleInspector: noop,
    maximized: false,
    onAction: action,
    exportFigure: noop,
    exporting: false,
    saveAs: noop,
    print: noop,
    refresh: noop,
    detach: noop,
    detachLabel: 'Undock',
  };
  act(() => root.render(<FigureToolbar {...props} canUndo canRedo={false} />));
  act(() => host.querySelector<HTMLButtonElement>('[aria-label="Undo figure edit"]')!.click());
  expect(action).toHaveBeenLastCalledWith({ action: 'undo' });
  expect(host.querySelector<HTMLButtonElement>('[aria-label="Redo figure edit"]')!.disabled).toBe(
    true
  );
  const key = {
    key: 'z',
    ctrlKey: true,
    altKey: false,
    metaKey: false,
    repeat: false,
    isComposing: false,
  };
  expect(figureShortcut(key, false)).toBe('undo');
  expect(figureShortcut({ ...key, shiftKey: true }, false)).toBe('redo');
  expect(figureShortcut({ ...key, key: 'ArrowRight', ctrlKey: false }, false)).toBe('arrowRight');
  expect(figureShortcut({ ...key, key: 'ArrowLeft', ctrlKey: false }, false)).toBe('arrowLeft');
});
//=============================================================================
