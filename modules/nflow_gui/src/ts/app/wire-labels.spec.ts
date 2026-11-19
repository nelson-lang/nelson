//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { refreshWireLabels, initWireLabels } from './wire-labels.js';
import { state, signalDiagramChanged } from './state.js';
import { serializeDiagram } from './file-operations.js';
//=============================================================================
function makeSvgDom(): SVGSVGElement {
  document.body.innerHTML = `
    <svg id="svgCanvas">
      <g id="wireLayer"></g>
      <g id="blockLayer"></g>
      <g id="overlayLayer"></g>
    </svg>`;
  return document.getElementById('svgCanvas') as unknown as SVGSVGElement;
}
//=============================================================================
beforeEach(() => {
  state.connections.length = 0;
  state.blocks.clear();
  state.locked = false;
});
//=============================================================================
test('refreshWireLabels draws a text at the wire midpoint', () => {
  makeSvgDom();
  (state.connections as unknown as any[]).push({
    from: 'a',
    to: 'b',
    label: 'speed',
    points: [
      { x: 0, y: 0 },
      { x: 100, y: 0 },
      { x: 100, y: 50 },
    ],
    path: null,
  });
  refreshWireLabels();
  const labels = document.querySelectorAll('#overlayLayer .wire-label');
  expect(labels.length).toBe(1);
  expect(labels[0].textContent).toBe('speed');
});
//=============================================================================
test('unlabeled wires draw nothing and stale labels are removed', () => {
  makeSvgDom();
  (state.connections as unknown as any[]).push({
    from: 'a',
    to: 'b',
    points: [
      { x: 0, y: 0 },
      { x: 10, y: 0 },
    ],
  });
  refreshWireLabels();
  expect(document.querySelectorAll('.wire-label').length).toBe(0);
});
//=============================================================================
// The gesture is detected from two rapid clicks (not the native `dblclick`),
// so it survives the second click landing on a grip the selected wire draws
// over itself rather than on the wire path.
function clickAt(el: Element, x: number, y: number): void {
  el.dispatchEvent(new MouseEvent('click', { bubbles: true, clientX: x, clientY: y }));
}
//=============================================================================
test('double-click on a wire opens the editor; Enter saves the label', () => {
  const svg = makeSvgDom();
  initWireLabels(svg);
  const path = document.createElementNS('http://www.w3.org/2000/svg', 'path');
  document.getElementById('wireLayer')!.appendChild(path);
  const conn: any = {
    from: 'a',
    to: 'b',
    points: [
      { x: 0, y: 0 },
      { x: 10, y: 0 },
    ],
    path,
  };
  (state.connections as unknown as any[]).push(conn);

  clickAt(path, 30, 40);
  clickAt(path, 30, 40);
  const input = document.getElementById('wireLabelInput') as HTMLInputElement;
  expect(input).not.toBeNull();
  input.value = 'torque';
  input.dispatchEvent(new KeyboardEvent('keydown', { key: 'Enter', bubbles: true }));
  expect(conn.label).toBe('torque');
  expect(document.querySelectorAll('.wire-label').length).toBe(1);
  const data = serializeDiagram(state) as { connections: Array<{ label?: string }> };
  expect(data.connections[0].label).toBe('torque');
});
//=============================================================================
test('a second click on a selected wire grip still opens the editor', () => {
  const svg = makeSvgDom();
  initWireLabels(svg);
  const path = document.createElementNS('http://www.w3.org/2000/svg', 'path');
  document.getElementById('wireLayer')!.appendChild(path);
  const conn: any = {
    from: 'a',
    to: 'b',
    points: [
      { x: 0, y: 0 },
      { x: 10, y: 0 },
    ],
    path,
  };
  (state.connections as unknown as any[]).push(conn);
  // A selected wire exposes a mid branch handle; the second click lands on it.
  (state as unknown as { selectedConnection?: unknown }).selectedConnection = conn;
  const grip = document.createElementNS('http://www.w3.org/2000/svg', 'g');
  grip.setAttribute('class', 'wire-branch-handle');
  const dot = document.createElementNS('http://www.w3.org/2000/svg', 'circle');
  grip.appendChild(dot);
  document.getElementById('wireLayer')!.appendChild(grip);

  clickAt(path, 30, 40); // first click: the wire itself
  clickAt(dot, 30, 40); // second click: the branch-dot grip drawn over it
  expect(document.getElementById('wireLabelInput')).not.toBeNull();
});
//=============================================================================
test('labels refresh on diagramChanged', () => {
  const svg = makeSvgDom();
  initWireLabels(svg);
  (state.connections as unknown as any[]).push({
    from: 'a',
    to: 'b',
    label: 'v',
    points: [
      { x: 0, y: 0 },
      { x: 10, y: 0 },
    ],
  });
  signalDiagramChanged();
  expect(document.querySelectorAll('.wire-label').length).toBe(1);
});
//=============================================================================
