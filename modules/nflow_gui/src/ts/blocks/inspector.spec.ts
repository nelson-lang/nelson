//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { createInspector } from './inspector';
import { _resetUiPrefsForTests } from '../app/ui-prefs.js';
//=============================================================================
describe('inspector', () => {
  beforeEach(() => {
    _resetUiPrefsForTests();
    localStorage.clear();
  });
  test('renders placeholder when block is null', () => {
    const body = document.createElement('div');
    const btn = document.createElement('button');
    btn.disabled = false;
    const insp = createInspector({ inspectorBody: body, rotateSelectionBtn: btn });
    insp.renderInspector(null);
    const canvasInput = body.querySelector(
      'input[data-edit="canvasBackground"]'
    ) as HTMLInputElement | null;
    expect(canvasInput).toBeTruthy();
    expect(btn.disabled).toBe(true);
  });

  test('constant input updates value and calls renderer.updateBlockLabel', () => {
    const body = document.createElement('div');
    const updateCalls: any[] = [];
    const renderer = { updateBlockLabel: (b: unknown) => updateCalls.push(b), current: undefined };
    const insp = createInspector({ inspectorBody: body, renderer });
    const block: any = { type: 'constant', params: { Value: '123' } };
    insp.renderInspector(block);
    const input = body.querySelector(
      `input[data-edit="${FieldNames.kValue}"]`
    ) as HTMLInputElement | null;
    expect(input).toBeTruthy();
    if (!input) return;
    input.value = '456';
    input.dispatchEvent(new Event('input', { bubbles: true }));
    expect(block.params.Value).toBe('456');
    expect(updateCalls.length).toBeGreaterThanOrEqual(1);
    expect(updateCalls).toEqual(expect.arrayContaining([block]));
  });

  test('shows code generation and Modelica capabilities', () => {
    const body = document.createElement('div');
    const inspector = createInspector({ inspectorBody: body });
    inspector.renderInspector({ type: 'sine', params: {} });

    expect(body.querySelector('[data-capability="c"]')?.getAttribute('data-status')).toBe(
      'supported'
    );
    expect(body.querySelector('[data-capability="rust"]')?.getAttribute('data-status')).toBe(
      'supported'
    );
    expect(body.querySelector('[data-capability="modelica"]')).toBeNull();
    expect(body.querySelectorAll<HTMLButtonElement>('.inspector-capability-value')).toHaveLength(2);
    expect(
      body.querySelector<HTMLButtonElement>('[data-capability="c"] .inspector-capability-value')
        ?.title
    ).toBe('Open related help');
  });

  test('renders collapsible capabilities and parameters sections', () => {
    const body = document.createElement('div');
    const inspector = createInspector({ inspectorBody: body });
    inspector.renderInspector({ type: 'sine', params: {} });

    const capabilities = body.querySelector<HTMLDetailsElement>(
      '[data-inspector-section="capabilities"]'
    );
    const parameters = body.querySelector<HTMLDetailsElement>(
      '[data-inspector-section="parameters"]'
    );

    expect(capabilities?.open).toBe(true);
    expect(parameters?.open).toBe(true);
    expect(capabilities?.querySelector(':scope > summary')?.textContent).toBe('Capabilities');
    expect(parameters?.querySelector(':scope > summary')?.textContent).toBe('Parameters');

    capabilities?.querySelector<HTMLElement>(':scope > summary')?.click();
    parameters?.querySelector<HTMLElement>(':scope > summary')?.click();

    expect(capabilities?.open).toBe(false);
    expect(parameters?.open).toBe(false);
  });

  test('does not repeat the selected type for a block without parameters', () => {
    const body = document.createElement('div');
    const inspector = createInspector({ inspectorBody: body });
    inspector.renderInspector({
      type: 'RotSpeedSensor',
      acausal: true,
      physicalPins: [{}],
      params: {},
    });

    expect(body.textContent).not.toContain('Selected RotSpeedSensor');
    expect(body.querySelector('[data-capability="c"]')?.getAttribute('data-status')).toBe(
      'model-dependent'
    );
  });

  test('restores collapsed capabilities and parameters sections', () => {
    localStorage.setItem('nf-inspector-section-capabilities-open', '0');
    localStorage.setItem('nf-inspector-section-parameters-open', '0');
    const body = document.createElement('div');
    const inspector = createInspector({ inspectorBody: body });
    inspector.renderInspector({ type: 'gain', params: {} });
    expect(
      body.querySelector<HTMLDetailsElement>('[data-inspector-section="capabilities"]')?.open
    ).toBe(false);
    expect(
      body.querySelector<HTMLDetailsElement>('[data-inspector-section="parameters"]')?.open
    ).toBe(false);
  });

  test('shows OpenModelica only when the selected block requires it', () => {
    const body = document.createElement('div');
    const inspector = createInspector({ inspectorBody: body });
    inspector.renderInspector({ type: 'modelica', params: {} });

    expect(body.querySelector('[data-capability="modelica"]')?.getAttribute('data-status')).toBe(
      'required'
    );
    expect(
      body
        .querySelector<HTMLButtonElement>(
          '[data-capability="modelica"] .inspector-capability-value'
        )
        ?.getAttribute('aria-label')
    ).toContain('Open related help');
  });

  test('tf inputs parse lists and call update and signalDiagramChanged', () => {
    const body = document.createElement('div');
    let updateCalled = false;
    const renderer = { updateBlockLabel: () => (updateCalled = true), current: undefined };
    let signalCalled = false;
    const insp = createInspector({
      inspectorBody: body,
      renderer,
      signalDiagramChanged: () => (signalCalled = true),
    });
    const block: any = { type: 'tf', params: { Numerator: [], Denominator: [] } };
    insp.renderInspector(block);
    const numInput = body.querySelector(
      `input[data-edit="${FieldNames.kNum}"]`
    ) as HTMLInputElement | null;
    const denInput = body.querySelector(
      `input[data-edit="${FieldNames.kDen}"]`
    ) as HTMLInputElement | null;
    expect(numInput).toBeTruthy();
    expect(denInput).toBeTruthy();
    if (!numInput || !denInput) return;
    numInput.value = '3, 4';
    numInput.dispatchEvent(new Event('input', { bubbles: true }));
    denInput.value = '5,6';
    denInput.dispatchEvent(new Event('input', { bubbles: true }));
    expect(Array.isArray(block.params.Numerator)).toBe(true);
    expect(block.params.Numerator).toEqual([3, 4]);
    expect(block.params.Denominator).toEqual([5, 6]);
    expect(updateCalled).toBe(true);
    expect(signalCalled).toBe(true);
  });
});
//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { vi } from 'vitest';
//=============================================================================
describe('inspector', () => {
  test('renderInspector shows placeholder when no block selected', async () => {
    const mod = await import('./inspector.tsx');
    const { createInspector } = mod;
    const div = document.createElement('div');
    const inspector = createInspector({ inspectorBody: div });
    inspector.renderInspector(null);
    expect(div.querySelector('input[data-edit="canvasBackground"]')).toBeTruthy();
  });

  test('constant block input updates params and calls renderer.updateBlockLabel', async () => {
    const mod = await import('./inspector.tsx');
    const { createInspector } = mod;
    const div = document.createElement('div');
    const mockRenderer = {
      updateBlockLabel: vi.fn(),
      current: null,
      resizeBlock: vi.fn(),
    } as any;
    const inspector = createInspector({ inspectorBody: div, renderer: mockRenderer });
    const block: any = { type: 'constant', params: { Value: '1' } };
    inspector.renderInspector(block);
    const input = div.querySelector(
      `input[data-edit="${FieldNames.kValue}"]`
    ) as HTMLInputElement | null;
    expect(input).toBeTruthy();
    if (!input) return;
    input.value = '2';
    input.dispatchEvent(new Event('input'));
    expect(block.params.Value).toBe('2');
    expect(mockRenderer.updateBlockLabel).toHaveBeenCalled();
  });

  test('inspector shows title and updates when label edited', async () => {
    const mod = await import('./inspector.tsx');
    const { createInspector } = mod;
    const div = document.createElement('div');
    const inspector = createInspector({ inspectorBody: div });
    const block: any = { type: 'display', params: { Label: 'My Display' } };
    inspector.renderInspector(block);
    const titleEl = div.querySelector('.inspector-title') as HTMLElement | null;
    expect(titleEl).toBeTruthy();

    const typeEl = div.querySelector('.inspector-type-text') as HTMLElement | null;
    expect(typeEl).toBeTruthy();
    expect(typeEl?.textContent).toBe('display');

    const nameInput = div.querySelector('.inspector-name-input') as HTMLInputElement | null;
    expect(nameInput).toBeTruthy();
    expect(nameInput?.value).toBe('My Display');

    const labelInput = div.querySelector('input[data-edit="Label"]') as HTMLInputElement | null;
    expect(labelInput).toBeTruthy();
    if (!labelInput) return;
    labelInput.value = 'New Title';
    labelInput.dispatchEvent(new Event('input', { bubbles: true }));
    expect(nameInput?.value).toBe('New Title');
  });
});
//=============================================================================
