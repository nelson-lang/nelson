//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import { vi } from 'vitest';
import { renderBlockPanel } from './inspector-block';
import { parseFileSourceCsvText } from './renderers/generators';
import * as FieldNames from '../FieldNames.js';
//=============================================================================
function makeContainer() {
  const el = document.createElement('div');
  return el;
}
//=============================================================================
function makeDeps(inspectorBody: HTMLElement | null) {
  return {
    inspectorBody,
    callUpdate: vi.fn(),
    callResize: vi.fn(),
    renderInspector: vi.fn(),
    signalDiagramChanged: vi.fn(),
  } as any;
}
//=============================================================================
test('sum: editing inputs updates params and calls update', () => {
  const body = makeContainer();
  const deps = makeDeps(body);
  const block: any = { type: 'sum', params: { inputs: '1,2,3' } };
  renderBlockPanel(block, deps);

  const inp = body.querySelector(
    `input[data-edit='${FieldNames.kSumInputs}']`
  ) as HTMLInputElement | null;
  expect(inp).not.toBeNull();
  inp!.value = '4,5';
  inp!.dispatchEvent(new Event('input', { bubbles: true }));

  expect(block.params.Inputs).toBe('4,5');
  expect((deps.callUpdate as vi.Mock).mock.calls.length).toBeGreaterThanOrEqual(1);
});
//=============================================================================
test('generic inspector fields render through React without injecting markup', () => {
  const body = makeContainer();
  const deps = makeDeps(body);
  const block: any = { type: 'constant', params: { Value: '<b>42</b>' } };
  renderBlockPanel(block, deps);

  expect(body.querySelector('[data-react-root="inspector-panel"]')).toBeTruthy();
  expect(body.querySelector('b')).toBeNull();

  const input = body.querySelector(
    `input[data-edit='${FieldNames.kValue}']`
  ) as HTMLInputElement | null;
  expect(input).not.toBeNull();
  expect(input!.value).toBe('<b>42</b>');
});
//=============================================================================
test('clock: React generic fields preserve defaults and update params', () => {
  const body = makeContainer();
  const deps = makeDeps(body);
  const block: any = { type: 'clock', params: {} };
  renderBlockPanel(block, deps);

  expect(body.querySelector('[data-react-root="inspector-panel"]')).toBeTruthy();
  expect(body.textContent).toContain('Outputs simulation time in seconds.');

  const displayTime = body.querySelector(
    "input[data-edit='displayTime']"
  ) as HTMLInputElement | null;
  expect(displayTime).not.toBeNull();
  expect(displayTime!.checked).toBe(false);
  displayTime!.checked = true;
  displayTime!.dispatchEvent(new Event('change', { bubbles: true }));
  expect(block.params.DisplayTime).toBe(true);

  const decimation = body.querySelector(
    `input[data-edit='${FieldNames.kDecimation}']`
  ) as HTMLInputElement | null;
  expect(decimation).not.toBeNull();
  expect(decimation!.value).toBe('10');
  decimation!.value = '4.7';
  decimation!.dispatchEvent(new Event('input', { bubbles: true }));
  expect(block.params.Decimation).toBe(4);
  expect((deps.callUpdate as vi.Mock).mock.calls.length).toBeGreaterThanOrEqual(2);
});
//=============================================================================
test('scope inspector renders through React and escapes signal labels', () => {
  const body = makeContainer();
  const deps = { ...makeDeps(body), getRuntimeSeconds: () => 5 };
  const block: any = {
    type: 'scope',
    inputs: 1,
    params: { signals: [{ label: '<b>y</b>' }] },
    width: 200,
    height: 150,
    computedLimits: {},
  };
  renderBlockPanel(block, deps);

  expect(body.querySelector('[data-react-root="inspector-panel"]')).toBeTruthy();
  expect(body.querySelector('b')).toBeNull();

  const tMax = body.querySelector(
    `input[data-edit='${FieldNames.kTMax}']`
  ) as HTMLInputElement | null;
  expect(tMax).not.toBeNull();
  expect(tMax!.value).toBe('5');

  const signalLabel = body.querySelector(
    "input[data-signal-edit='label']"
  ) as HTMLInputElement | null;
  expect(signalLabel).not.toBeNull();
  expect(signalLabel!.value).toBe('<b>y</b>');
});
//=============================================================================
test('xyScope inspector renders through React and preserves axis defaults', () => {
  const body = makeContainer();
  const renderScope = vi.fn();
  const deps = { ...makeDeps(body), renderScope };
  const block: any = {
    type: 'xyScope',
    inputs: 1,
    params: { signals: [{ label: '<i>xy</i>' }] },
    width: 220,
    height: 160,
    computedLimits: {},
  };
  renderBlockPanel(block, deps);

  expect(body.querySelector('[data-react-root="inspector-panel"]')).toBeTruthy();
  expect(body.querySelector('i')).toBeNull();

  const xMin = body.querySelector(
    `input[data-edit='${FieldNames.kXMin}']`
  ) as HTMLInputElement | null;
  const yMax = body.querySelector(
    `input[data-edit='${FieldNames.kYMax}']`
  ) as HTMLInputElement | null;
  expect(xMin).not.toBeNull();
  expect(yMax).not.toBeNull();
  expect(xMin!.value).toBe('-1.2');
  expect(yMax!.value).toBe('1.2');

  const signalLabel = body.querySelector(
    "input[data-signal-edit='label']"
  ) as HTMLInputElement | null;
  expect(signalLabel).not.toBeNull();
  expect(signalLabel!.value).toBe('<i>xy</i>');
  signalLabel!.value = 'xy';
  signalLabel!.dispatchEvent(new Event('input', { bubbles: true }));
  expect(block.params.signals[0].label).toBe('xy');
  expect(renderScope).toHaveBeenCalledWith(block);
});
//=============================================================================
test('xyzScope inspector renders through React and preserves 3D controls', () => {
  const body = makeContainer();
  const renderScope = vi.fn();
  const deps = { ...makeDeps(body), renderScope };
  const block: any = {
    type: 'xyzScope',
    inputs: 1,
    params: { signals: [{ label: '<u>xyz</u>' }] },
    width: 240,
    height: 180,
    computedLimits: {},
  };
  renderBlockPanel(block, deps);

  expect(body.querySelector('[data-react-root="inspector-panel"]')).toBeTruthy();
  expect(body.querySelector('u')).toBeNull();

  const zMin = body.querySelector("input[data-edit='ZMin']") as HTMLInputElement | null;
  const rotationX = body.querySelector("input[data-edit='RotationX']") as HTMLInputElement | null;
  const zLabel = body.querySelector("input[data-edit='zLabel']") as HTMLInputElement | null;
  expect(zMin).not.toBeNull();
  expect(rotationX).not.toBeNull();
  expect(zLabel).not.toBeNull();
  expect(zMin!.value).toBe('-1.2');
  expect(rotationX!.value).toBe('30');

  rotationX!.value = '60';
  rotationX!.dispatchEvent(new Event('input', { bubbles: true }));
  expect(block.params.RotationX).toBe(60);
  expect(renderScope).toHaveBeenCalledWith(block);

  const signalLabel = body.querySelector(
    "input[data-signal-edit='label']"
  ) as HTMLInputElement | null;
  expect(signalLabel).not.toBeNull();
  expect(signalLabel!.value).toBe('<u>xyz</u>');
});
//=============================================================================
test('simple math panels render static content through React', () => {
  const body = makeContainer();
  const deps = makeDeps(body);

  renderBlockPanel({ type: 'mult', params: {} } as any, deps);
  expect(body.querySelector('[data-react-root="inspector-panel"]')).toBeTruthy();
  expect(body.textContent).toContain('Multiply inputs');

  renderBlockPanel({ type: 'terminator', params: {} } as any, deps);
  expect(body.querySelector('[data-react-root="inspector-panel"]')).toBeTruthy();
  expect(body.textContent).toContain('Terminate signal');

  renderBlockPanel({ type: 'derivative', params: {} } as any, deps);
  expect(body.querySelector('[data-react-root="inspector-panel"]')).toBeTruthy();
  expect(body.textContent).toContain('d/dt');
});
//=============================================================================
test('simple math and sink fields render through React and update params', () => {
  const body = makeContainer();
  const deps = makeDeps(body);
  const biasBlock: any = { type: 'bias', params: {} };
  renderBlockPanel(biasBlock, deps);

  expect(body.querySelector('[data-react-root="inspector-panel"]')).toBeTruthy();
  const bias = body.querySelector(
    `input[data-edit='${FieldNames.kBias}']`
  ) as HTMLInputElement | null;
  expect(bias).not.toBeNull();
  expect(bias!.value).toBe('1');
  bias!.value = '2.5';
  bias!.dispatchEvent(new Event('input', { bubbles: true }));
  expect(biasBlock.params.Bias).toBe('2.5');

  const sinkBlock: any = { type: 'fileSink', params: { FileName: 'out.csv' } };
  renderBlockPanel(sinkBlock, deps);

  expect(body.querySelector('[data-react-root="inspector-panel"]')).toBeTruthy();
  const path = body.querySelector(
    `input[data-edit='${FieldNames.kPath}']`
  ) as HTMLInputElement | null;
  expect(path).not.toBeNull();
  expect(path!.value).toBe('out.csv');
  path!.value = 'next.csv';
  path!.dispatchEvent(new Event('input', { bubbles: true }));
  expect(sinkBlock.params.FileName).toBe('next.csv');
  expect((deps.callUpdate as vi.Mock).mock.calls.length).toBeGreaterThanOrEqual(2);
});
//=============================================================================
test('display: React fields update format, decimation, and inspector title', () => {
  const body = makeContainer();
  const deps = makeDeps(body);
  const block: any = {
    type: FieldNames.kDisplay,
    params: { Label: 'Readout', Format: 'short', Decimation: undefined },
  };
  renderBlockPanel(block, deps);

  expect(body.querySelector('[data-react-root="inspector-panel"]')).toBeTruthy();

  const titleInput = body.querySelector<HTMLInputElement>("input[data-edit='Label']");
  const headerName = body.querySelector<HTMLInputElement>('.inspector-name-input');
  expect(titleInput).not.toBeNull();
  expect(headerName).not.toBeNull();
  expect(titleInput!.value).toBe('Readout');
  expect(headerName!.value).toBe('Readout');

  titleInput!.value = 'Output';
  titleInput!.dispatchEvent(new Event('input', { bubbles: true }));
  expect(block.params.Label).toBe('Output');
  expect(headerName!.value).toBe('Output');

  const format = body.querySelector(
    `select[data-edit='${FieldNames.kFormat}']`
  ) as HTMLSelectElement | null;
  expect(format).not.toBeNull();
  format!.value = 'long';
  format!.dispatchEvent(new Event('change', { bubbles: true }));
  expect(block.params.Format).toBe('long');

  const decimation = body.querySelector(
    `input[data-edit='${FieldNames.kDecimation}']`
  ) as HTMLInputElement | null;
  expect(decimation).not.toBeNull();
  expect(decimation!.value).toBe('1');
  decimation!.value = '3.9';
  decimation!.dispatchEvent(new Event('input', { bubbles: true }));
  expect(block.params.Decimation).toBe(3);
  expect((deps.callUpdate as vi.Mock).mock.calls.length).toBeGreaterThanOrEqual(3);
});
//=============================================================================
test('logic compare panels render through React and preserve defaults', () => {
  const body = makeContainer();
  const deps = makeDeps(body);
  const block: any = { type: 'compareToConstant', params: {} };
  renderBlockPanel(block, deps);

  expect(body.querySelector('[data-react-root="inspector-panel"]')).toBeTruthy();
  const operator = body.querySelector("select[data-edit='relop']") as HTMLSelectElement | null;
  expect(operator).not.toBeNull();
  expect(operator!.value).toBe(FieldNames.kGe);
  operator!.value = FieldNames.kNe;
  operator!.dispatchEvent(new Event('change', { bubbles: true }));
  expect(block.params.relop).toBe(FieldNames.kNe);

  const threshold = body.querySelector(
    `input[data-edit='${FieldNames.kConst}']`
  ) as HTMLInputElement | null;
  expect(threshold).not.toBeNull();
  expect(threshold!.value).toBe('0');
  threshold!.value = '7';
  threshold!.dispatchEvent(new Event('input', { bubbles: true }));
  expect(block.params.const).toBe('7');

  const zeroBlock: any = { type: 'compareToZero', params: {} };
  renderBlockPanel(zeroBlock, deps);
  const zeroOperator = body.querySelector("select[data-edit='relop']") as HTMLSelectElement | null;
  expect(zeroOperator).not.toBeNull();
  expect(zeroOperator!.value).toBe(FieldNames.kNe);
});
//=============================================================================
test('mux and demux panels render through React and coerce counts', () => {
  const body = makeContainer();
  const deps = makeDeps(body);
  const muxBlock: any = { type: 'mux', params: {} };
  renderBlockPanel(muxBlock, deps);

  expect(body.querySelector('[data-react-root="inspector-panel"]')).toBeTruthy();
  const inputs = body.querySelector(
    `input[data-edit='${FieldNames.kSumInputs}']`
  ) as HTMLInputElement | null;
  expect(inputs).not.toBeNull();
  expect(inputs!.value).toBe('2');
  inputs!.value = '5';
  inputs!.dispatchEvent(new Event('input', { bubbles: true }));
  expect(muxBlock.params.Inputs).toBe(5);

  const demuxBlock: any = { type: 'demux', params: {} };
  renderBlockPanel(demuxBlock, deps);
  const outputs = body.querySelector(
    `input[data-edit='${FieldNames.kMuxOutputs}']`
  ) as HTMLInputElement | null;
  expect(outputs).not.toBeNull();
  expect(outputs!.value).toBe('2');
  outputs!.value = '1';
  outputs!.dispatchEvent(new Event('input', { bubbles: true }));
  expect(demuxBlock.params.Outputs).toBe(2);
});
//=============================================================================
test('deadZone and quantizer panels render through React and update params', () => {
  const body = makeContainer();
  const deps = makeDeps(body);
  const deadZoneBlock: any = { type: 'deadZone', params: {} };
  renderBlockPanel(deadZoneBlock, deps);

  expect(body.querySelector('[data-react-root="inspector-panel"]')).toBeTruthy();
  const lower = body.querySelector(
    `input[data-edit='${FieldNames.kLowerValue}']`
  ) as HTMLInputElement | null;
  const upper = body.querySelector(
    `input[data-edit='${FieldNames.kUpperValue}']`
  ) as HTMLInputElement | null;
  expect(lower).not.toBeNull();
  expect(upper).not.toBeNull();
  expect(lower!.value).toBe('-1');
  expect(upper!.value).toBe('1');
  lower!.value = '-2';
  lower!.dispatchEvent(new Event('input', { bubbles: true }));
  expect(deadZoneBlock.params.LowerValue).toBe('-2');

  const quantizerBlock: any = { type: 'quantizer', params: {} };
  renderBlockPanel(quantizerBlock, deps);
  const interval = body.querySelector(
    "input[data-edit='QuantizationInterval']"
  ) as HTMLInputElement | null;
  expect(interval).not.toBeNull();
  expect(interval!.value).toBe('1');
  interval!.value = '0.5';
  interval!.dispatchEvent(new Event('input', { bubbles: true }));
  expect(quantizerBlock.params.QuantizationInterval).toBe('0.5');
  expect((deps.callUpdate as vi.Mock).mock.calls.length).toBeGreaterThanOrEqual(2);
});
//=============================================================================
test('userFunc panel renders through React and preserves the default expression', () => {
  const body = makeContainer();
  const deps = makeDeps(body);
  const block: any = { type: 'expression', params: {} };
  renderBlockPanel(block, deps);

  expect(body.querySelector('[data-react-root="inspector-panel"]')).toBeTruthy();
  const expr = body.querySelector(
    `input[data-edit='${FieldNames.kExpr}']`
  ) as HTMLInputElement | null;
  expect(expr).not.toBeNull();
  expect(expr!.value).toBe('u');
  expr!.value = 'sin(u)';
  expr!.dispatchEvent(new Event('input', { bubbles: true }));
  expect(block.params.Expr).toBe('sin(u)');
  expect((deps.callUpdate as vi.Mock).mock.calls.length).toBeGreaterThanOrEqual(1);
});
//=============================================================================
test('label source and sink panels render through React and update label flags', () => {
  const body = makeContainer();
  const deps = makeDeps(body);
  const sourceBlock: any = { type: FieldNames.kLabelSource, params: { GotoTag: 'in' } };
  renderBlockPanel(sourceBlock, deps);

  expect(body.querySelector('[data-react-root="inspector-panel"]')).toBeTruthy();
  const sourceName = body.querySelector(
    `input[data-edit='${FieldNames.kGotoTag}']`
  ) as HTMLInputElement | null;
  expect(sourceName).not.toBeNull();
  sourceName!.value = '  plant_in  ';
  sourceName!.dispatchEvent(new Event('input', { bubbles: true }));
  expect(sourceBlock.params.GotoTag).toBe('plant_in');

  const sourceExternal = body.querySelector(
    `input[data-edit='${FieldNames.kIsExternalPort}']`
  ) as HTMLInputElement | null;
  expect(sourceExternal).not.toBeNull();
  sourceExternal!.checked = true;
  sourceExternal!.dispatchEvent(new Event('change', { bubbles: true }));
  expect(sourceBlock.params.isExternalPort).toBe(true);

  const sinkBlock: any = { type: FieldNames.kLabelSink, params: { showNode: true } };
  renderBlockPanel(sinkBlock, deps);
  const showNode = body.querySelector(
    `input[data-edit='${FieldNames.kShowNode}']`
  ) as HTMLInputElement | null;
  expect(showNode).not.toBeNull();
  showNode!.checked = false;
  showNode!.dispatchEvent(new Event('change', { bubbles: true }));
  expect(sinkBlock.params.ShowNode).toBe(false);
});
//=============================================================================
test('fileSource panel renders through React and keeps CSV parsing behavior', () => {
  const body = makeContainer();
  const deps = makeDeps(body);
  const block: any = { type: 'fileSource', params: { FileName: 'input.csv', loaded: false } };
  renderBlockPanel(block, deps);

  expect(body.querySelector('[data-react-root="inspector-panel"]')).toBeTruthy();
  expect(body.textContent).toContain('No CSV loaded');

  const path = body.querySelector(
    `input[data-edit='${FieldNames.kPath}']`
  ) as HTMLInputElement | null;
  expect(path).not.toBeNull();
  expect(path!.value).toBe('input.csv');
  path!.value = 'next.csv';
  path!.dispatchEvent(new Event('input', { bubbles: true }));
  expect(block.params.FileName).toBe('next.csv');

  const fileInput = body.querySelector("input[data-edit='file']") as HTMLInputElement | null;
  expect(fileInput).not.toBeNull();
  expect(fileInput!.accept).toBe('.csv,text/csv');

  expect(parseFileSourceCsvText('2,20\n0,1\n7\nbad')).toEqual({
    times: [0, 2, 2],
    values: [1, 20, 7],
  });
});
//=============================================================================
test('switch: changing condition and threshold updates params and calls update', () => {
  const body = makeContainer();
  const deps = makeDeps(body);
  const block: any = { type: 'switch', params: { Criteria: FieldNames.kGe, Threshold: 0 } };
  renderBlockPanel(block, deps);

  const cond = body.querySelector(
    `select[data-edit='${FieldNames.kCondition}']`
  ) as HTMLSelectElement | null;
  expect(cond).not.toBeNull();
  cond!.value = FieldNames.kNe;
  cond!.dispatchEvent(new Event('change', { bubbles: true }));
  expect(block.params.Criteria).toBe(FieldNames.kNe);
  expect((deps.callUpdate as vi.Mock).mock.calls.length).toBeGreaterThanOrEqual(1);

  const thr = body.querySelector(
    `input[data-edit='${FieldNames.kThreshold}']`
  ) as HTMLInputElement | null;
  expect(thr).not.toBeNull();
  thr!.value = '5';
  thr!.dispatchEvent(new Event('input', { bubbles: true }));
  expect(block.params.Threshold).toBe('5');
  expect((deps.callUpdate as vi.Mock).mock.calls.length).toBeGreaterThanOrEqual(2);
});
//=============================================================================
test('toggleSwitch: toggling state and editing labels/values updates params and calls update', () => {
  const body = makeContainer();
  const deps = makeDeps(body);
  const block: any = {
    type: 'toggleSwitch',
    params: { State: 0, OnLabel: 'ON', OffLabel: 'OFF', OnValue: 1, OffValue: 0 },
  };
  renderBlockPanel(block, deps);

  const state = body.querySelector(
    `input[data-edit='${FieldNames.kState}']`
  ) as HTMLInputElement | null;
  expect(state).not.toBeNull();
  state!.checked = true;
  state!.dispatchEvent(new Event('change', { bubbles: true }));
  expect(block.params.State).toBe(1);
  expect((deps.callUpdate as vi.Mock).mock.calls.length).toBeGreaterThanOrEqual(1);

  const onLabel = body.querySelector(
    `input[data-edit='${FieldNames.kOnLabel}']`
  ) as HTMLInputElement | null;
  expect(onLabel).not.toBeNull();
  onLabel!.value = 'YES';
  onLabel!.dispatchEvent(new Event('input', { bubbles: true }));
  expect(block.params.OnLabel).toBe('YES');
});
//=============================================================================
test('pid: editing gains and limits updates params and calls update', () => {
  const body = makeContainer();
  const deps = makeDeps(body);
  const block: any = {
    type: 'pid',
    params: { P: '1', I: '0', D: '0', LowerSaturationLimit: null, UpperSaturationLimit: null },
  };
  renderBlockPanel(block, deps);

  const kp = body.querySelector(`input[data-edit='${FieldNames.kKp}']`) as HTMLInputElement | null;
  expect(kp).not.toBeNull();
  kp!.value = '2';
  kp!.dispatchEvent(new Event('input', { bubbles: true }));
  expect(block.params.P).toBe('2');
  expect((deps.callUpdate as vi.Mock).mock.calls.length).toBeGreaterThanOrEqual(1);
});
//=============================================================================
test('comment: editing textarea and checkbox updates params and calls update', () => {
  const body = makeContainer();
  const deps = makeDeps(body);
  const block: any = { type: 'comment', params: { CommentText: 'hi', ShowBorder: true } };
  renderBlockPanel(block, deps);

  const ta = body.querySelector("textarea[data-edit='CommentText']") as HTMLTextAreaElement | null;
  expect(ta).not.toBeNull();
  ta!.value = 'hello';
  ta!.dispatchEvent(new Event('input', { bubbles: true }));
  expect(block.params.CommentText).toBe('hello');

  const chk = body.querySelector("input[data-edit='ShowBorder']") as HTMLInputElement | null;
  expect(chk).not.toBeNull();
  chk!.checked = false;
  chk!.dispatchEvent(new Event('change', { bubbles: true }));
  expect(block.params.ShowBorder).toBe(false);
});
//=============================================================================
test('subsystem: editing name/description/author updates params and calls update', () => {
  const body = makeContainer();
  const deps = makeDeps(body);
  const block: any = {
    type: FieldNames.kSubsystem,
    params: {
      name: 'Old',
      description: 'desc',
      author: 'me',
      externalInputs: [],
      externalOutputs: [],
    },
  };
  renderBlockPanel(block, deps);

  const name = body.querySelector(
    `input[data-edit='${FieldNames.kName}']`
  ) as HTMLInputElement | null;
  expect(name).not.toBeNull();
  name!.value = 'NewName';
  name!.dispatchEvent(new Event('input', { bubbles: true }));
  expect(block.params[FieldNames.kName]).toBe('NewName');
  expect((deps.callUpdate as vi.Mock).mock.calls.length).toBeGreaterThanOrEqual(1);

  const desc = body.querySelector(
    "textarea[data-edit='description']"
  ) as HTMLTextAreaElement | null;
  expect(desc).not.toBeNull();
  desc!.value = 'newdesc';
  desc!.dispatchEvent(new Event('input', { bubbles: true }));
  expect(block.params.description).toBe('newdesc');
});
//=============================================================================
