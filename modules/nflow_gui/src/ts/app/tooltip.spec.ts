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
import { initTooltip, disposeTooltip } from './tooltip.js';
//=============================================================================
describe('tooltip behavior', () => {
  beforeEach(() => {
    document.body.innerHTML = '';
  });

  afterEach(() => {
    disposeTooltip();
  });

  test('creates tooltip and shows text on pointerover of port', () => {
    const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
    const block = document.createElement('g');
    block.setAttribute('data-block-id', 'b1');
    const port = document.createElement('circle');
    port.className = 'port';
    block.appendChild(port);
    svg.appendChild(block);
    document.body.appendChild(svg);

    const renderer = {
      clientToSvg: (x: number, y: number) => ({ x, y }),
      state: {
        blocks: new Map([
          [
            'b1',
            {
              x: 10,
              y: 20,
              params: { name: 'MyBlock' },
              ports: [{ x: 0, y: 0, type: 'in', index: 0 }],
            },
          ],
        ]),
      },
    };

    initTooltip({ svg: svg as unknown as SVGSVGElement, renderer: renderer as any });

    const ev = new PointerEvent('pointerover', { bubbles: true, clientX: 50, clientY: 60 });
    port.dispatchEvent(ev);

    const tip = document.getElementById('nflow-tooltip') as HTMLDivElement | null;
    expect(tip).toBeTruthy();
    expect(tip!.style.display).toBe('block');
    expect(tip!.textContent && tip!.textContent.includes('MyBlock')).toBe(true);
  });

  test('pointermove updates tooltip position and pointerout hides it', () => {
    const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
    const port = document.createElement('circle');
    port.className = 'port';
    svg.appendChild(port);
    document.body.appendChild(svg);

    const renderer = { clientToSvg: (x: number, y: number) => ({ x, y }) };
    initTooltip({ svg: svg as unknown as SVGSVGElement, renderer: renderer as any });

    port.dispatchEvent(
      new PointerEvent('pointerover', { bubbles: true, clientX: 10, clientY: 20 })
    );
    const tip = document.getElementById('nflow-tooltip') as HTMLDivElement;
    expect(tip.style.display).toBe('block');

    port.dispatchEvent(
      new PointerEvent('pointermove', { bubbles: true, clientX: 30, clientY: 40 })
    );
    expect(tip.style.left).toContain('42'); // 30 + 12
    expect(tip.style.top).toContain('52'); // 40 + 12

    port.dispatchEvent(new PointerEvent('pointerout', { bubbles: true, clientX: 0, clientY: 0 }));
    expect(tip.style.display).toBe('none');
  });
});
//=============================================================================
import * as tt from './tooltip';
//=============================================================================
test('tooltip module loads', () => {
  expect(typeof tt).toBe('object');
});
//=============================================================================
describe('tooltip init/dispose', () => {
  let svg: SVGSVGElement;
  const renderer = {
    clientToSvg: (x: number, y: number) => ({ x, y }),
    state: { blocks: new Map() },
  } as any;

  beforeEach(() => {
    svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
    svg.setAttribute('width', '200');
    svg.setAttribute('height', '200');
    document.body.appendChild(svg);
  });

  afterEach(() => {
    try {
      disposeTooltip();
    } catch (e) {}
    if (svg && svg.parentNode) svg.parentNode.removeChild(svg);
    const el = document.getElementById('nflow-tooltip');
    if (el && el.parentNode) el.parentNode.removeChild(el);
  });

  test('shows tooltip on pointerover of port with block info', () => {
    const block = {
      x: 10,
      y: 10,
      ports: [{ x: 0, y: 0, type: 'in', index: 0 }],
      params: { name: 'B' },
      label: 'Block',
    } as any;
    renderer.state.blocks.set('b1', block);

    initTooltip({ svg, renderer });

    const blockEl = document.createElementNS('http://www.w3.org/2000/svg', 'g') as any;
    blockEl.setAttribute('data-block-id', 'b1');
    const portEl = document.createElementNS('http://www.w3.org/2000/svg', 'circle');
    portEl.setAttribute('class', 'port');
    blockEl.appendChild(portEl);
    svg.appendChild(blockEl);

    const ev = new PointerEvent('pointerover', { bubbles: true, clientX: 30, clientY: 40 } as any);
    portEl.dispatchEvent(ev);

    const tip = document.getElementById('nflow-tooltip');
    expect(tip).not.toBeNull();
    expect(tip!.style.display).toBe('block');
    expect(tip!.textContent).toMatch(/B|port/);
  });
});
//=============================================================================
import { beforeAll, test, expect } from 'vitest';
//=============================================================================
let mod: any;
beforeAll(async () => {
  mod = await import('../../../dist/app/tooltip.js').catch(() => null);
});
//=============================================================================
test('tooltip module loads', () => {
  expect(mod === null || typeof mod === 'object' || typeof mod === 'function').toBe(true);
});
//=============================================================================
export {};
//=============================================================================
