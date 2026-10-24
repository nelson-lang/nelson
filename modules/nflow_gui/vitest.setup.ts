//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Vitest setup: stub browser APIs not implemented by jsdom + test helpers.
//=============================================================================
import path from 'node:path';
import * as FieldNames from './src/ts/FieldNames';
//=============================================================================
const g = globalThis as any;
function createMemoryStorage() {
  const entries = new Map<string, string>();
  return {
    get length() {
      return entries.size;
    },
    clear: () => entries.clear(),
    getItem: (key: string) => entries.get(String(key)) ?? null,
    key: (index: number) => Array.from(entries.keys())[index] ?? null,
    removeItem: (key: string) => entries.delete(String(key)),
    setItem: (key: string, value: string) => entries.set(String(key), String(value)),
  };
}
//=============================================================================
if (typeof g.alert === 'undefined') {
  g.alert = () => {};
}
//=============================================================================
if (typeof g.confirm === 'undefined') {
  g.confirm = () => true;
}
//=============================================================================
// Some specs resolve paths as path.resolve(process.cwd(), 'modules/nflow_gui/...')
// and expect the repository root as cwd: shim it when run from modules/nflow_gui.
try {
  const cwd = process.cwd();
  const marker = path.join('modules', 'nflow_gui');
  if (cwd.endsWith(marker) || cwd.includes(marker + path.sep)) {
    const repoRoot = path.resolve(cwd.slice(0, cwd.lastIndexOf(marker)));
    process.cwd = () => repoRoot;
  }
} catch {
  // ignore if cwd cannot be overridden
}
//=============================================================================
// Stand-in for the native ExprEngine binding (`evalExpressionNative`), so unit
// tests run without Nelson. It is not a production evaluator: its fidelity is
// enforced by the shared parity battery (src/ts/utils/expr-parity-cases.ts;
// the unit side checks this mock, the b2b side checks the real engine).
if (typeof g.evalExpressionNative !== 'function') {
  g.evalExpressionNative = async (payload: unknown): Promise<string> => {
    try {
      const data = typeof payload === 'string' ? JSON.parse(payload) : (payload as any);
      const expr = String(data.expr ?? '');
      const vars = data.variables || {};

      const helpers: Record<string, unknown> = {
        abs: Math.abs,
        fabs: Math.abs,
        ceil: Math.ceil,
        floor: Math.floor,
        round: Math.round,
        sqrt: Math.sqrt,
        sign: Math.sign,
        exp: Math.exp,
        log: Math.log,
        log10: Math.log10,
        log2: Math.log2,
        sin: Math.sin,
        cos: Math.cos,
        tan: Math.tan,
        asin: Math.asin,
        acos: Math.acos,
        atan: Math.atan,
        sinh: Math.sinh,
        cosh: Math.cosh,
        tanh: Math.tanh,
        pow: Math.pow,
        min: Math.min,
        fmin: Math.min,
        max: Math.max,
        fmax: Math.max,
        atan2: Math.atan2,
        clamp: (v: number, a: number, b: number) => Math.min(Math.max(v, a), b),
        sinc: (x: number) => (x === 0 ? 1 : Math.sin(x) / x),
      };
      Object.assign(g, helpers);
      g.pi = Math.PI;
      g.e = Math.E;
      g.inf = Infinity;

      let exprJs = expr.replace(/\^/g, '**');
      exprJs = exprJs.replace(/([+-]{2,})(?=\d|\()/g, (m: string) => {
        const minusCount = (m.match(/-/g) || []).length;
        return minusCount % 2 === 0 ? '' : '-';
      });

      const fn = new Function('vars', 'with (vars || {}) { return (' + exprJs + '); }');
      let value = fn(vars);
      if (typeof value === 'number' && !Number.isFinite(value)) value = null;
      return JSON.stringify({ ok: true, value });
    } catch (err: any) {
      return JSON.stringify({ ok: false, error: String(err?.message ?? err) });
    }
  };
}
//=============================================================================
if (typeof g.window !== 'undefined') {
  if (typeof g.window.focus !== 'function') g.window.focus = () => {};
  if (!g.window.navigator) g.window.navigator = {};
  if (!g.window.navigator.clipboard) {
    g.window.navigator.clipboard = { writeText: async () => {} };
  }
  if (typeof g.window.alert !== 'function') g.window.alert = () => {};
  for (const key of ['localStorage', 'sessionStorage']) {
    try {
      const storage = g.window[key]?.getItem ? g.window[key] : createMemoryStorage();
      Object.defineProperty(g.window, key, {
        value: storage,
        configurable: true,
        writable: true,
      });
      Object.defineProperty(g, key, {
        value: storage,
        configurable: true,
        writable: true,
      });
    } catch {
      // ignore
    }
  }
}
//=============================================================================
if (typeof g.navigator === 'undefined') g.navigator = {};
// jsdom's default userAgent has an AppleWebKit token without a Chrome token,
// which shouldUseSvgMathFallback() classifies as non-Chromium WebKit. Most
// render specs assume the Chromium/KaTeX foreignObject path, so pin a Chromium
// UA. Specs of the WebKit SVG fallback set and restore their own UA.
try {
  Object.defineProperty(g.navigator, 'userAgent', {
    value:
      'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/140.0.0.0 Safari/537.36',
    configurable: true,
  });
} catch {
  // ignore if userAgent cannot be overridden
}
//=============================================================================
if (typeof g.navigator.clipboard === 'undefined') {
  try {
    Object.defineProperty(g.navigator, 'clipboard', {
      value: { writeText: async () => {} },
      configurable: true,
    });
  } catch {
    // ignore
  }
}
//=============================================================================
try {
  if (typeof document !== 'undefined' && document?.body) {
    if (typeof (document.body as any).focus !== 'function') {
      (document.body as any).focus = () => {};
    }
  }
} catch {
  // ignore
}
//=============================================================================
if (typeof g.PointerEvent === 'undefined') {
  class PointerEventPolyfill extends Event {
    constructor(type: string, props: any = {}) {
      const { bubbles = false, cancelable = false } = props || {};
      super(type, { bubbles, cancelable });
      for (const key of [
        'clientX',
        'clientY',
        'buttons',
        'button',
        'pointerId',
        'pointerType',
        'ctrlKey',
        'metaKey',
        'shiftKey',
        'altKey',
      ]) {
        if (props && props[key] !== undefined) {
          try {
            Object.defineProperty(this, key, {
              value: props[key],
              enumerable: true,
              configurable: true,
            });
          } catch {
            // ignore
          }
        }
      }
    }
  }
  g.PointerEvent = PointerEventPolyfill;
}
//=============================================================================
{
  const origError = console.error;
  const FILTER_PATTERNS = [
    'Not implemented: window.alert',
    'Not implemented: window.focus',
    '[initSidebarUI] giving up after retries',
    'Error adding block: Error: oops',
    '[library-loader] Failed to load library',
  ];
  console.error = function (...args: unknown[]) {
    try {
      const joined = args
        .map((a: any) => {
          if (typeof a === 'string') return a;
          if (a && a.stack) return a.stack;
          try {
            return JSON.stringify(a);
          } catch {
            return String(a);
          }
        })
        .join(' ');
      for (const p of FILTER_PATTERNS) {
        if (joined.includes(p)) return;
      }
    } catch {
      // fall through
    }
    return origError.apply(this, args as any);
  };
}
//=============================================================================
{
  const origWarn = console.warn;
  console.warn = function (...args: unknown[]) {
    try {
      const joined = args.map((a) => (typeof a === 'string' ? a : String(a))).join(' ');
      if (joined.includes('skipping missing file')) return;
    } catch {
      // ignore
    }
    return origWarn.apply(this, args as any);
  };
}
//=============================================================================
// Legacy specs reference a global FieldNames object without importing it.
g.FieldNames = FieldNames;
if (typeof g.window !== 'undefined') {
  g.window.FieldNames = FieldNames;
}
//=============================================================================
