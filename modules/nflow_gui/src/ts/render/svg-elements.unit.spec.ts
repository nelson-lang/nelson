//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
let mod: typeof import('./svg-elements');
beforeAll(async () => {
  mod = (await import('./svg-elements.ts')) as any;
});
//=============================================================================
function makeSpy() {
  const calls: any[] = [];
  const fn: any = (...args: any[]) => {
    calls.push(args);
  };
  fn.getCalls = () => calls;
  return fn;
}
//=============================================================================
describe('createSvgElement', () => {
  test('creates SVG element with attributes and text', () => {
    const el = mod.createSvgElement('text', { x: 1, y: 2 }, 'hi');
    expect(el.namespaceURI).toBe('http://www.w3.org/2000/svg');
    expect(el.tagName.toLowerCase()).toBe('text');
    expect(el.getAttribute('x')).toBe('1');
    expect(el.textContent).toBe('hi');
  });
});
//=============================================================================
describe('renderSvgMath', () => {
  test('renders foreignObject with mathML content', () => {
    const group = document.createElement('g');
    mod.renderSvgMath(group, '<mi>x</mi>', 100, 20);
    const fo = group.querySelector('foreignObject');
    expect(fo).not.toBeNull();
    const div = fo!.querySelector('.math-foreign');
    expect(div).not.toBeNull();
    expect(div!.innerHTML).toContain('<mi>x</mi>');
  });
});
//=============================================================================
describe('renderTeXMath', () => {
  afterEach(() => {
    // @ts-ignore
    delete (global as any).katex;
    // @ts-ignore
    delete (global as any).nflowResizeUserFunc;
    mod.katexQueue.clear();
    document.body.innerHTML = '';
  });

  test('calls katex.render when available', () => {
    const group = document.createElement('g');
    const block = document.createElement('div');
    block.className = 'svg-block';
    block.dataset.blockId = 'b1';
    block.appendChild(group);
    document.body.appendChild(block);

    const renderSpy = makeSpy();
    const renderWrapper = (tex: string, node: Element) => {
      renderSpy(tex, node);
      (node as HTMLElement).textContent = 'rendered';
    };
    // @ts-ignore
    (global as any).katex = { render: renderWrapper };
    const nflowSpy = makeSpy();
    // @ts-ignore
    (global as any).nflowResizeUserFunc = nflowSpy;

    mod.renderTeXMath(group, '\\alpha', 10, 10);
    expect(renderSpy.getCalls().length).toBeGreaterThan(0);
    expect(group.querySelectorAll('.katex-target').length).toBe(0);
  });

  test('queues katex render when katex missing', () => {
    // @ts-ignore
    delete (global as any).katex;
    mod.katexQueue.clear();
    const group = document.createElement('g');
    mod.renderTeXMath(group, '\\beta', 10, 10);
    expect(mod.katexQueue.has(group)).toBe(true);
  });
});
//=============================================================================
describe('scheduleUserFuncResize and measurement helpers', () => {
  test('scheduleUserFuncResize notifies resize functions for svg-block', () => {
    const group = document.createElement('g');
    const block = document.createElement('div');
    block.className = 'svg-block';
    block.dataset.blockId = 'B2';
    block.appendChild(group);
    document.body.appendChild(block);
    const nflowSpy = makeSpy();
    // @ts-ignore
    (global as any).nflowResizeUserFunc = nflowSpy;
    mod.scheduleUserFuncResize(group);
    expect(nflowSpy.getCalls().length).toBeGreaterThan(0);
    expect(nflowSpy.getCalls()[0][0]).toBe('B2');
  });

  test('ensureUserFuncMeasureRoot creates a hidden root and measure works', () => {
    const root = mod.ensureUserFuncMeasureRoot();
    expect(root).toBeTruthy();
    expect(root.wrapper.parentElement).toBe(document.body);
    const root2 = mod.ensureUserFuncMeasureRoot();
    expect(root2).toBe(root);
  });

  test('measureUserFuncTex returns dimensions when katex present', () => {
    // @ts-ignore
    (global as any).katex = {
      render: (tex: string, node: Element) => {
        (node as HTMLElement).className = 'katex';
      },
    };
    const orig = Element.prototype.getBoundingClientRect;
    // @ts-ignore
    Element.prototype.getBoundingClientRect = function () {
      return { width: 20, height: 10, top: 0, left: 0, bottom: 0, right: 0 } as DOMRect;
    };
    try {
      const res = mod.measureUserFuncTex('x');
      expect(res).toBeTruthy();
      expect(res!.w).toBeGreaterThan(0);
      expect(res!.h).toBeGreaterThan(0);
    } finally {
      // @ts-ignore
      Element.prototype.getBoundingClientRect = orig;
    }
  });
});
//=============================================================================
