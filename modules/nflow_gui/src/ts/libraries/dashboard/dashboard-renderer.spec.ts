//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import { nflowRpc } from '../../app/native-rpc.js';
import { state } from '../../app/state.js';
import { installDashboardBindingEvents } from '../../app/dashboard-bindings.js';
import type { Block } from '../types.js';
import { dashboardManifest } from './blocks.js';
import { dashboardRenderer } from './dashboard.js';
//=============================================================================
vi.mock('../../app/native-rpc.js', () => ({ nflowRpc: vi.fn(() => Promise.resolve({})) }));
const SVG_NS = 'http://www.w3.org/2000/svg';
installDashboardBindingEvents();
function createBlock(type: string, id = type): Block {
  const definition = dashboardManifest.blocks.find((block) => block.type === type);
  if (!definition) throw new Error(`Missing dashboard definition: ${type}`);
  const group = document.createElementNS(SVG_NS, 'g');
  document.body.appendChild(document.createElementNS(SVG_NS, 'svg')).appendChild(group);
  const block = {
    id,
    type,
    group,
    width: definition.width,
    height: definition.height,
    params: structuredClone(definition.defaultParams),
  } as unknown as Block;
  return block;
}
//=============================================================================
function bindToGain(block: Block, targetId: string): void {
  block.params.Binding = {
    kind: 'parameter',
    blockPath: [targetId],
    paramName: 'Gain',
    element: '',
  };
  state.blocks.set(`runtime-key-${targetId}`, {
    id: targetId,
    params: { Gain: 0 },
  });
}
//=============================================================================
function render(block: Block): void {
  dashboardRenderer(block, {} as never);
}
//=============================================================================
function press(group: Element): void {
  group.dispatchEvent(new MouseEvent('pointerdown', { button: 0, bubbles: true }));
  window.dispatchEvent(new MouseEvent('pointerup', { button: 0, bubbles: true }));
}
//=============================================================================
describe('dashboard renderer inventory', () => {
  beforeEach(() => {
    document.body.replaceChildren();
    state.blocks.clear();
    state.variables = {};
    vi.mocked(nflowRpc).mockClear();
  });

  afterEach(() => vi.useRealTimers());

  it('drops the runtime update listener once the block left the canvas', () => {
    const block = createBlock('dashboardGauge', 'detached-gauge');
    render(block);
    block.group.ownerSVGElement?.remove();
    const removeSpy = vi.spyOn(document, 'removeEventListener');

    document.dispatchEvent(
      new CustomEvent('nf-dashboard-update', { detail: { id: block.id, value: 1 } })
    );

    expect(removeSpy.mock.calls.some(([type]) => type === 'nf-dashboard-update')).toBe(true);
    removeSpy.mockRestore();
  });

  it('renders every contracted type and accepts typed runtime updates', () => {
    for (const definition of dashboardManifest.blocks) {
      const block = createBlock(definition.type);
      render(block);
      expect(block.group.classList.contains('dashboard-block'), definition.type).toBe(true);
      document.dispatchEvent(
        new CustomEvent('nf-dashboard-update', {
          detail: {
            id: block.id,
            connected: true,
            valid: true,
            value: definition.type === 'dashboardScope' ? [2] : 2,
            series: [[1, 2]],
            times: [0, 1],
          },
        })
      );
      expect(block.group.classList.contains('dashboard-invalid'), definition.type).toBe(false);
    }
    expect(document.querySelectorAll('.dashboard-block')).toHaveLength(20);
  });

  it('keeps dashboard scope samples and markers inside the plot area', () => {
    const block = createBlock('dashboardScope', 'bounded-scope');
    block.params.YLimits = [-3, 3];
    block.params.Markers = 'on';
    render(block);
    document.dispatchEvent(
      new CustomEvent('nf-dashboard-update', {
        detail: {
          id: block.id,
          connected: true,
          valid: true,
          value: 0,
          series: [[10, 0, -10]],
          times: [0, 1, 2],
        },
      })
    );

    const path = block.group.querySelector('.dashboard-scope-path') as SVGPathElement;
    const markers = Array.from(block.group.querySelectorAll('.dashboard-scope-marker'));
    const yValues = Array.from(path.getAttribute('d')?.matchAll(/[ML] [\d.]+ ([\d.]+)/g) ?? []).map(
      (match) => Number(match[1])
    );
    expect(path.getAttribute('clip-path')).toMatch(/^url\(#nflow-dashboard-scope-clip-/);
    expect(yValues).toEqual([18, 80.5, 143]);
    expect(markers).toHaveLength(3);
    expect(markers.every((marker) => Number(marker.getAttribute('cy')) >= 18)).toBe(true);
    expect(markers.every((marker) => Number(marker.getAttribute('cy')) <= 143)).toBe(true);
    expect(block.group.querySelector('.dashboard-scope-markers')?.getAttribute('clip-path')).toBe(
      path.getAttribute('clip-path')
    );
  });

  it.each(['dashboardSlider', 'dashboardKnob'])('%s sends keyboard changes', (type) => {
    const block = createBlock(type);
    bindToGain(block, `${type}-target`);
    render(block);
    block.group.dispatchEvent(new KeyboardEvent('keydown', { key: 'ArrowRight', bubbles: true }));
    expect(nflowRpc).toHaveBeenCalledWith(
      'nflow.sim.event',
      expect.objectContaining({ name: 'DashboardControl' })
    );
    expect((state.blocks.get(`runtime-key-${type}-target`) as any).params.Gain).not.toBe(0);
  });

  it('provides an enlarged slider pointer target without changing its artwork', () => {
    const block = createBlock('dashboardSlider');
    bindToGain(block, 'slider-pointer-target');
    render(block);
    const hitTrack = block.group.querySelector('.dashboard-control-hit') as SVGLineElement;
    const track = block.group.querySelector('.dashboard-control-track') as SVGLineElement;
    track.getBoundingClientRect = () => ({ left: 100, top: 200, width: 164, height: 8 } as DOMRect);
    hitTrack.dispatchEvent(
      new PointerEvent('pointerdown', {
        button: 0,
        clientX: 100 + 164 * 0.8,
        clientY: 212,
        pointerId: 31,
        bubbles: true,
      })
    );
    window.dispatchEvent(new PointerEvent('pointerup', { pointerId: 31 }));
    expect((state.blocks.get('runtime-key-slider-pointer-target') as any).params.Gain).toBeCloseTo(
      80
    );
  });

  it('maps knob pointers in screen coordinates and supports dragging', () => {
    const block = createBlock('dashboardKnob');
    bindToGain(block, 'knob-pointer-target');
    render(block);
    const group = block.group as SVGGraphicsElement;
    group.getScreenCTM = () => null;
    group.getBoundingClientRect = () =>
      ({ left: 100, top: 200, width: 55, height: 57.5 } as DOMRect);
    group.dispatchEvent(
      new PointerEvent('pointerdown', {
        button: 0,
        clientX: 127.5,
        clientY: 200,
        pointerId: 32,
        bubbles: true,
      })
    );
    window.dispatchEvent(
      new PointerEvent('pointermove', {
        clientX: 155,
        clientY: 228.75,
        pointerId: 32,
      })
    );
    window.dispatchEvent(new PointerEvent('pointerup', { pointerId: 32 }));
    const value = (state.blocks.get('runtime-key-knob-pointer-target') as any).params.Gain;
    expect(value).toBeGreaterThan(80);
  });

  it('validates edit input before sending a value', () => {
    const block = createBlock('dashboardEdit');
    bindToGain(block, 'edit-target');
    render(block);
    const input = block.group.querySelector('input') as HTMLInputElement;
    input.value = 'invalid';
    input.dispatchEvent(new Event('change'));
    expect(nflowRpc).not.toHaveBeenCalled();
    input.value = '12.5';
    input.dispatchEvent(new Event('change'));
    expect((state.blocks.get('runtime-key-edit-target') as any).params.Gain).toBe(12.5);
    expect(nflowRpc).toHaveBeenCalledTimes(1);
  });

  it.each([
    'dashboardRotarySwitch',
    'dashboardRadioButton',
    'dashboardCheckBox',
    'dashboardRockerSwitch',
    'dashboardSliderSwitch',
    'dashboardToggleSwitch',
  ])('%s cycles through states from the keyboard', (type) => {
    const block = createBlock(type);
    bindToGain(block, `${type}-target`);
    render(block);
    block.group.dispatchEvent(new KeyboardEvent('keydown', { key: 'ArrowRight', bubbles: true }));
    expect(nflowRpc).toHaveBeenCalledTimes(1);
    expect((state.blocks.get(`runtime-key-${type}-target`) as any).params.Gain).not.toBe(0);
  });

  it('sends combo box selections', () => {
    const block = createBlock('dashboardComboBox');
    bindToGain(block, 'combo-target');
    render(block);
    const select = block.group.querySelector('select') as HTMLSelectElement;
    const foreign = block.group.querySelector('foreignObject');
    expect(foreign?.getAttribute('y')).toBe('0');
    expect(foreign?.getAttribute('height')).toBe(String(block.height));
    expect(foreign?.classList.contains('dashboard-combo-foreign')).toBe(true);
    select.selectedIndex = Math.min(1, select.options.length - 1);
    select.dispatchEvent(new Event('change'));
    expect(nflowRpc).toHaveBeenCalledTimes(1);
  });

  it('keeps combo box clicks inside the control and releases focus with Escape', () => {
    const block = createBlock('dashboardComboBox');
    render(block);
    const select = block.group.querySelector('select') as HTMLSelectElement;
    const outsideClick = vi.fn();
    block.group.parentElement?.addEventListener('click', outsideClick);
    select.focus();
    select.dispatchEvent(new MouseEvent('click', { bubbles: true }));
    expect(outsideClick).not.toHaveBeenCalled();
    select.dispatchEvent(new KeyboardEvent('keydown', { key: 'Escape', bubbles: true }));
    expect(document.activeElement).not.toBe(select);
  });

  it('restores a momentary push button target on release', () => {
    const block = createBlock('dashboardPushButton');
    bindToGain(block, 'push-target');
    (state.blocks.get('runtime-key-push-target') as any).params.Gain = 4;
    block.params.DashboardValue = 4;
    block.params.ButtonType = 'Momentary';
    block.params.OnValue = 9;
    render(block);
    press(block.group);
    expect((state.blocks.get('runtime-key-push-target') as any).params.Gain).toBe(4);
    expect(nflowRpc).toHaveBeenCalledTimes(2);
  });

  it('runs press and click callbacks with the configured delay', async () => {
    vi.useFakeTimers();
    const block = createBlock('dashboardCallbackButton');
    block.params.ButtonType = 'Momentary';
    block.params.PressFcn = 'pressed = true;';
    block.params.ClickFcn = 'clicked = true;';
    block.params.PressDelay = 500;
    render(block);
    block.group.dispatchEvent(new MouseEvent('pointerdown', { button: 0, bubbles: true }));
    await vi.advanceTimersByTimeAsync(500);
    window.dispatchEvent(new MouseEvent('pointerup', { button: 0, bubbles: true }));
    expect(nflowRpc).toHaveBeenCalledWith(
      'nflow.callback.invoke',
      expect.objectContaining({ code: 'pressed = true;' })
    );
    expect(nflowRpc).toHaveBeenCalledWith(
      'nflow.callback.invoke',
      expect.objectContaining({ code: 'clicked = true;' })
    );
  });
});
//=============================================================================
