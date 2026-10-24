//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import * as se from './svg-elements.js';
//=============================================================================
describe('svg-elements', () => {
  beforeEach(() => {
    document.body.innerHTML = '';
    (globalThis as any).katex = undefined;
    se.katexQueue.clear && se.katexQueue.clear();
  });

  test('createSvgElement creates namespaced SVG element and sets attrs/text', () => {
    const el = se.createSvgElement('text', { x: 1, y: 2 }, 'hello');

    expect(el.namespaceURI).toBe('http://www.w3.org/2000/svg');
    expect(el.getAttribute('x')).toBe('1');
    expect(el.getAttribute('y')).toBe('2');
    expect(el.textContent).toBe('hello');
  });

  test('renderSvgMath injects foreignObject with math html', () => {
    const group = document.createElement('div');
    se.renderSvgMath(group, '<span>m</span>', 50, 20);
    const fo = group.querySelector('foreignObject');
    expect(fo).toBeTruthy();
    const div = fo?.querySelector('.math-foreign');
    expect(div).toBeTruthy();
    expect(div?.innerHTML).toContain('<span>m</span>');
  });

  test('renderTeXMath uses katex.render when available', () => {
    const group = document.createElement('div');
    (globalThis as any).katex = {
      render: (tex: string, node: Element) => {
        (node as HTMLElement).textContent = `OK:${tex}`;
      },
    };

    se.renderTeXMath(group, '\\alpha', 10, 10);
    const span = group.querySelector('span');
    expect(span).toBeTruthy();
    expect(span?.textContent).toBe('OK:\\alpha');
    expect(span?.classList.contains('katex-target')).toBe(false);
  });

  test('uses native SVG text for macOS WebKit rendering', () => {
    expect(
      se.shouldUseSvgMathFallback(
        'Mozilla/5.0 (Macintosh) AppleWebKit/605.1.15 Version/18.6 Safari/605.1.15'
      )
    ).toBe(true);
  });

  test('keeps KaTeX HTML rendering in Chromium browsers', () => {
    expect(
      se.shouldUseSvgMathFallback(
        'Mozilla/5.0 (Macintosh) AppleWebKit/537.36 Chrome/140.0.0.0 Safari/537.36'
      )
    ).toBe(false);
  });

  test('uses native SVG text for Linux WebKit (WebKitGTK) rendering', () => {
    expect(
      se.shouldUseSvgMathFallback('Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/605.1 Safari/605.1')
    ).toBe(true);
  });

  test('keeps KaTeX HTML rendering in Linux Chromium', () => {
    expect(
      se.shouldUseSvgMathFallback(
        'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 Chrome/140.0.0.0 Safari/537.36'
      )
    ).toBe(false);
  });

  test('linearizes common TeX constructs for SVG fallback text', () => {
    expect(se.texToSvgText('\\frac{1}{s^2 + 0.5s + 1}')).toBe('(1) / (s² + 0.5s + 1)');
    expect(se.texToSvgText('\\mathsf{PID}')).toBe('PID');
    expect(se.texToSvgText('z^{-1}')).toBe('z⁻¹');
  });

  test('splits a top-level fraction while preserving nested groups', () => {
    expect(se.splitTexFraction('\\frac{1}{s^{2} + 1}')).toEqual({
      numerator: '1',
      denominator: 's^{2} + 1',
    });
    expect(se.splitTexFraction('x + \\frac{1}{s}')).toBeNull();
  });

  test('renders the macOS WebKit fallback without foreignObject', () => {
    const originalUserAgent = window.navigator.userAgent;
    Object.defineProperty(window.navigator, 'userAgent', {
      value: 'Mozilla/5.0 (Macintosh) AppleWebKit/605.1.15 Safari/605.1.15',
      configurable: true,
    });
    try {
      const group = document.createElementNS('http://www.w3.org/2000/svg', 'g');
      group.setAttribute('class', 'nf-tf-formula');
      se.renderTeXMath(group, '\\frac{1}{s^2 + 1}', 80, 80);
      expect(group.querySelector('foreignObject')).toBeNull();
      expect(group.querySelectorAll('.svg-math-text')).toHaveLength(2);
      expect(group.querySelector('.svg-math-fraction-line')).not.toBeNull();
    } finally {
      Object.defineProperty(window.navigator, 'userAgent', {
        value: originalUserAgent,
        configurable: true,
      });
    }
  });

  test('uses the configured block font size in the macOS WebKit fallback', () => {
    const originalUserAgent = window.navigator.userAgent;
    Object.defineProperty(window.navigator, 'userAgent', {
      value: 'Mozilla/5.0 (Macintosh) AppleWebKit/605.1.15 Safari/605.1.15',
      configurable: true,
    });
    try {
      const group = document.createElementNS('http://www.w3.org/2000/svg', 'g');
      group.setAttribute('class', 'custom-math');
      group.setAttribute('data-text-size', '16px');
      se.renderTeXMath(group, '\\text{AND}', 80, 80);

      const text = group.querySelector('.svg-math-text');
      expect(text?.textContent).toBe('AND');
      expect(text?.getAttribute('font-size')).toBe('16');
    } finally {
      Object.defineProperty(window.navigator, 'userAgent', {
        value: originalUserAgent,
        configurable: true,
      });
    }
  });

  test('uses the compact logical block font size in the macOS WebKit fallback', () => {
    const originalUserAgent = window.navigator.userAgent;
    Object.defineProperty(window.navigator, 'userAgent', {
      value: 'Mozilla/5.0 (Macintosh) AppleWebKit/605.1.15 Safari/605.1.15',
      configurable: true,
    });
    try {
      for (const name of ['and', 'or', 'xor', 'not']) {
        const group = document.createElementNS('http://www.w3.org/2000/svg', 'g');
        group.setAttribute('class', `nf-${name}-formula`);
        se.renderTeXMath(group, `\\text{${name.toUpperCase()}}`, 80, 80);

        expect(group.querySelector('.svg-math-text')?.getAttribute('font-size')).toBe('16');
      }
    } finally {
      Object.defineProperty(window.navigator, 'userAgent', {
        value: originalUserAgent,
        configurable: true,
      });
    }
  });

  test('renderTeXMath enqueues when katex missing and queue processes later', async () => {
    const group = document.createElement('div');
    (globalThis as any).katex = undefined;

    se.renderTeXMath(group, 'X', 10, 10);
    expect(se.katexQueue.has(group)).toBe(true);

    (globalThis as any).katex = {
      render: (tex: string, node: Element) => {
        node.textContent = `R:${tex}`;
      },
    };
    // wait for the queueKatexRender timeout (100ms)
    await new Promise((r) => setTimeout(r, 220));
    const span = group.querySelector('span');
    expect(span?.textContent).toBe('R:X');
  });

  test('scheduleUserFuncResize notifies resize functions for svg-block', () => {
    const group = document.createElement('g');
    const block = document.createElement('div');
    block.className = 'svg-block';
    block.dataset.blockId = 'B2';
    block.appendChild(group);
    document.body.appendChild(block);
    const calls: any[] = [];
    const fn = (...args: any[]) => calls.push(args);
    // @ts-ignore
    (globalThis as any).nflowResizeUserFunc = fn;
    se.scheduleUserFuncResize(group);
    expect(calls.length).toBeGreaterThan(0);
    expect(calls[0][0]).toBe('B2');
  });

  test('ensureUserFuncMeasureRoot and measureUserFuncTex produce dimensions when katex available', () => {
    (globalThis as any).katex = {
      render: (tex: string, node: Element) => {
        (node as HTMLElement).className = 'katex';
        Object.defineProperty(node, 'getBoundingClientRect', {
          value: () => ({ width: 123, height: 45, left: 0, top: 0, right: 0, bottom: 0 }),
        });
        Object.defineProperty(node as HTMLElement, 'scrollWidth', {
          value: 123,
          configurable: true,
        });
        Object.defineProperty(node as HTMLElement, 'scrollHeight', {
          value: 45,
          configurable: true,
        });
      },
    };

    const root = se.ensureUserFuncMeasureRoot();
    expect(root).toBeTruthy();
    const res = se.measureUserFuncTex('x');
    expect(res).toBeTruthy();
    expect(res?.w).toBeGreaterThan(0);
    expect(res?.h).toBeGreaterThan(0);
  });
});
//=============================================================================
