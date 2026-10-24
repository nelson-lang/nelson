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
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import { createRoot, type Root } from 'react-dom/client';
import { FigureControls } from '../../src/panels/FigureControls';
import type { UIControl } from '../../src/rpc/types';
//=============================================================================
const mounted: Array<{ root: Root; host: HTMLDivElement }> = [];
function control(overrides: Partial<UIControl> = {}): UIControl {
  return {
    handle: 1,
    style: 'pushbutton',
    text: 'Run calculation',
    x: 20,
    y: 30,
    w: 160,
    h: 28,
    value: 0.25,
    min: 0,
    max: 1,
    fontSize: 12,
    enabled: true,
    ...overrides,
  };
}
//=============================================================================
function renderControls(controls: UIControl[], onAction = vi.fn()) {
  const host = document.createElement('div');
  document.body.appendChild(host);
  const root = createRoot(host);
  mounted.push({ root, host });
  const update = (next: UIControl[]) => {
    act(() => {
      root.render(<FigureControls controls={next} onAction={onAction} onMenuSelect={vi.fn()} />);
    });
  };
  update(controls);
  return { host, update, onAction };
}
//=============================================================================
beforeEach(() => vi.stubGlobal('IS_REACT_ACT_ENVIRONMENT', true));
afterEach(() => {
  for (const { root, host } of mounted.splice(0)) {
    act(() => root.unmount());
    host.remove();
  }
  vi.unstubAllGlobals();
});
//=============================================================================
describe('FigureControls modern dials', () => {
  it('leaves container chrome on the canvas so its title and axes remain visible', () => {
    const { host } = renderControls([
      control({ style: 'panel', text: 'Line style', background: '#f5f5f5', foreground: '#fff' }),
      control({ handle: 2, style: 'radiobutton', text: 'Solid' }),
    ]);
    expect(host.querySelectorAll('.figure-uicontrol')).toHaveLength(1);
    expect(host.textContent).toBe('Solid');
  });

  it('anchors switch labels outside the track without moving the track', () => {
    const { host } = renderControls([
      control({ style: 'switch', w: 46, h: 22, items: ['Hidden', 'Visible'] }),
    ]);
    expect(host.querySelector('button')?.style.width).toBe('46px');
    expect(host.querySelector('span')?.style.right).toBe('calc(100% + 4px)');
  });

  it('renders vertical rocker switches with the shared SVG geometry', () => {
    const { host, onAction } = renderControls([
      control({
        style: 'switch',
        text: 'Power',
        w: 24,
        h: 46,
        items: ['Off', 'On'],
        data: { vertical: true },
      }),
    ]);
    const rocker = host.querySelector('svg[role="button"]') as SVGElement | null;
    expect(rocker).not.toBeNull();
    expect(rocker?.getAttribute('aria-label')).toBe('Off/On');
    expect(rocker?.querySelector('rect')).not.toBeNull();
    act(() => rocker?.dispatchEvent(new MouseEvent('click', { bubbles: true })));
    expect(onAction).toHaveBeenCalledWith(expect.objectContaining({ style: 'switch' }), 1, 'Power');
  });

  it('paints slider tick labels outside its thin track and retains keyboard actions', () => {
    const slider = control({
      style: 'slider',
      w: 225,
      h: 3,
      value: 1,
      min: 0.5,
      max: 6,
      data: { major: [1, 2, 3], labels: ['1', '2', '3'] },
    });
    const { host, onAction } = renderControls([slider]);
    expect(host.querySelector('svg')?.style.overflow).toBe('visible');
    expect(host.querySelectorAll('svg text')).toHaveLength(3);
    const input = host.querySelector('input')!;
    expect(input.getAttribute('aria-orientation')).toBe('horizontal');
    act(() => {
      Object.getOwnPropertyDescriptor(HTMLInputElement.prototype, 'value')!.set!.call(input, '2');
      input.dispatchEvent(new Event('input', { bubbles: true }));
      input.dispatchEvent(new Event('change', { bubbles: true }));
    });
    expect(onAction).toHaveBeenCalledWith(slider, 2, slider.text);
  });

  it('allows knob ticks to extend outside the authored face', () => {
    const { host } = renderControls([control({ style: 'knob', w: 70, h: 70 })]);
    const svg = host.querySelector('svg')!;
    expect(svg.style.overflow).toBe('visible');
    expect(svg.querySelector('circle')?.getAttribute('r')).toBe('29');
  });
});
//=============================================================================
describe('FigureControls pushbuttons', () => {
  it('renders the caption and authored control geometry', () => {
    const { host } = renderControls([control({ tooltip: 'Start the calculation' })]);
    const button = host.querySelector('button');
    expect(button).not.toBeNull();
    expect(button?.textContent).toBe('Run calculation');
    expect(button?.title).toBe('Start the calculation');
    expect(button?.style.left).toBe('20px');
    expect(button?.style.top).toBe('30px');
    expect(button?.style.width).toBe('160px');
    expect(button?.style.height).toBe('28px');
    expect(button?.disabled).toBe(false);
  });

  it('dispatches one click with the existing value and caption', () => {
    const push = control();
    const { host, onAction } = renderControls([push]);
    act(() => {
      host
        .querySelector('button')
        ?.dispatchEvent(new PointerEvent('pointerdown', { bubbles: true }));
    });
    expect(onAction).toHaveBeenCalledExactlyOnceWith(push, 0.25, 'Run calculation');
    expect(push.value).toBe(0.25);
  });

  it('does not dispatch the activation callback on mouse-down', () => {
    const { host, onAction } = renderControls([control()]);
    act(() => {
      host.querySelector('button')?.dispatchEvent(new MouseEvent('mousedown', { bubbles: true }));
    });
    expect(onAction).not.toHaveBeenCalled();
  });

  it('keeps disabled controls visible without activating them', () => {
    const { host, onAction } = renderControls([control({ enabled: false })]);
    const button = host.querySelector('button');
    expect(button?.textContent).toBe('Run calculation');
    expect(button?.disabled).toBe(true);
    act(() => button?.click());
    expect(onAction).not.toHaveBeenCalled();
  });

  it('uses refreshed enabled state and caption for the next activation', () => {
    const { host, update, onAction } = renderControls([control({ enabled: false })]);
    const updated = control({ text: 'Recalculate' });
    update([updated]);
    expect(host.querySelector('button')?.disabled).toBe(false);
    act(() => {
      host
        .querySelector('button')
        ?.dispatchEvent(new PointerEvent('pointerdown', { bubbles: true }));
    });
    expect(onAction).toHaveBeenCalledExactlyOnceWith(updated, 0.25, 'Recalculate');
  });

  it('shows immediate pending feedback until the backend refreshes the caption', () => {
    const { host, update } = renderControls([control({ text: 'Start' })]);
    const button = host.querySelector('button');
    act(() => {
      button?.dispatchEvent(new PointerEvent('pointerdown', { bubbles: true }));
    });
    expect(button?.textContent).toBe('Start…');
    update([control({ text: 'Reset' })]);
    expect(host.querySelector('button')?.textContent).toBe('Reset');
  });

  it('leaves html controls to their iframe host while rendering adjacent pushbuttons', () => {
    const html = control({ handle: 2, style: 'html', text: 'HTML export label' });
    const { host, update } = renderControls([html]);
    expect(host.querySelector('.figure-uicontrol')).toBeNull();
    update([html, control()]);
    expect(host.querySelectorAll('.figure-uicontrol')).toHaveLength(1);
    expect(host.querySelector('button')?.textContent).toBe('Run calculation');
    expect(host.textContent).not.toContain('HTML export label');
    expect(host.querySelector('iframe')).toBeNull();
  });
});
//=============================================================================
describe('FigureControls button label alignment', () => {
  it('places the label with HorizontalAlignment and VerticalAlignment', () => {
    const { host } = renderControls([
      control({ handle: 1, hAlign: 'right', vAlign: 'bottom', text: 'top\nbottom' }),
      control({ handle: 2, style: 'togglebutton', hAlign: 'left', vAlign: 'top' }),
    ]);
    const [push, toggle] = Array.from(host.querySelectorAll('button'));
    expect(push.style.display).toBe('flex');
    expect(push.style.alignItems).toBe('flex-end');
    expect(push.style.justifyContent).toBe('flex-end');
    expect(push.style.whiteSpace).toBe('pre-line');
    expect(push.style.top).toBe('30px');
    expect(toggle.style.alignItems).toBe('flex-start');
    expect(toggle.style.justifyContent).toBe('flex-start');
  });

  it('centers the label when no alignment is sent', () => {
    const { host } = renderControls([control()]);
    const button = host.querySelector('button');
    expect(button?.style.alignItems).toBe('center');
    expect(button?.style.justifyContent).toBe('center');
  });
});
//=============================================================================
describe('FigureControls tab groups', () => {
  it('keeps top tabs inside the control so a group at the canvas edge stays visible', () => {
    const { host } = renderControls([
      control({
        style: 'tabgroup',
        items: ['Component gallery', 'Grid layout'],
        data: { tabLocation: 'top' },
      }),
    ]);
    const header = host.querySelector('.figure-uicontrol > div');
    expect(header).not.toBeNull();
    expect(header?.style.top).toBe('0px');
    expect(header?.style.bottom).toBe('');
    expect(Array.from(host.querySelectorAll('button')).map((button) => button.textContent)).toEqual(
      ['Component gallery', 'Grid layout']
    );
  });

  it('honours a vertical tab location', () => {
    const { host } = renderControls([
      control({ style: 'tabgroup', items: ['One', 'Two'], data: { tabLocation: 'left' } }),
    ]);
    const header = host.querySelector('.figure-uicontrol > div');
    expect(header?.style.flexDirection).toBe('column');
    expect(header?.style.left).toBe('0px');
    expect(header?.style.top).toBe('0px');
  });
});
//=============================================================================
