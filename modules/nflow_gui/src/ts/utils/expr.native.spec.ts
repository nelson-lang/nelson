//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/** @jest-environment node */
type NativeEvalResult = { ok: boolean; value?: number; error?: string };
type WebUiBinding = {
  call(name: string, payload: string): Promise<string>;
};
//=============================================================================
type NativeHost = typeof globalThis & {
  webui?: WebUiBinding;
};
//=============================================================================
function getHost(): NativeHost {
  return globalThis as NativeHost;
}
//=============================================================================
const hasNativeBinding = typeof getHost().webui?.call === 'function';
const describeNative = hasNativeBinding ? describe : describe.skip;
async function evalNative(
  expr: string,
  variables?: Record<string, number>
): Promise<{ ok: boolean; value?: number; error?: string }> {
  const w = getHost();
  if (typeof w.webui?.call !== 'function') {
    throw new Error('native RPC binding not available');
  }
  const payload = JSON.stringify({ expr, variables: variables || {} });
  const raw = await w.webui.call(
    'rpc',
    JSON.stringify({ id: 1, method: 'nflow.expr.eval', params: { payload } })
  );
  const envelope = JSON.parse(String(raw));
  if (envelope.error) throw new Error(envelope.error.message || 'native RPC error');
  return envelope.result as NativeEvalResult;
}
//=============================================================================
describeNative('ExprEngine (native RPC binding)', () => {
  describe('Basic arithmetic', () => {
    it('evaluates addition', async () => {
      const result = await evalNative('2 + 3');
      expect(result.ok).toBe(true);
      expect(result.value).toBe(5);
    });

    it('evaluates subtraction', async () => {
      const result = await evalNative('10 - 3');
      expect(result.ok).toBe(true);
      expect(result.value).toBe(7);
    });

    it('evaluates multiplication', async () => {
      const result = await evalNative('4 * 5');
      expect(result.ok).toBe(true);
      expect(result.value).toBe(20);
    });

    it('evaluates division', async () => {
      const result = await evalNative('20 / 4');
      expect(result.ok).toBe(true);
      expect(result.value).toBe(5);
    });

    it('evaluates modulo', async () => {
      const result = await evalNative('17 % 5');
      expect(result.ok).toBe(true);
      expect(result.value).toBe(2);
    });

    it('evaluates power', async () => {
      const result = await evalNative('2 ^ 3');
      expect(result.ok).toBe(true);
      expect(result.value).toBe(8);
    });

    it('respects operator precedence', async () => {
      const result = await evalNative('2 + 3 * 4');
      expect(result.ok).toBe(true);
      expect(result.value).toBe(14);
    });

    it('handles parentheses', async () => {
      const result = await evalNative('(2 + 3) * 4');
      expect(result.ok).toBe(true);
      expect(result.value).toBe(20);
    });
  });

  describe('Variables', () => {
    it('substitutes single variable', async () => {
      const result = await evalNative('x * 2', { x: 5 });
      expect(result.ok).toBe(true);
      expect(result.value).toBe(10);
    });

    it('substitutes multiple variables', async () => {
      const result = await evalNative('x * 2 + y', { x: 5, y: 3 });
      expect(result.ok).toBe(true);
      expect(result.value).toBe(13);
    });

    it('handles complex variable expressions', async () => {
      const result = await evalNative('(a + b) * (c - d)', {
        a: 2,
        b: 3,
        c: 10,
        d: 5,
      });
      expect(result.ok).toBe(true);
      expect(result.value).toBe(25);
    });
  });

  describe('Math functions - single argument', () => {
    it('evaluates abs()', async () => {
      const result = await evalNative('abs(-5)');
      expect(result.ok).toBe(true);
      expect(result.value).toBe(5);
    });

    it('evaluates ceil()', async () => {
      const result = await evalNative('ceil(3.2)');
      expect(result.ok).toBe(true);
      expect(result.value).toBe(4);
    });

    it('evaluates floor()', async () => {
      const result = await evalNative('floor(3.8)');
      expect(result.ok).toBe(true);
      expect(result.value).toBe(3);
    });

    it('evaluates round()', async () => {
      const result = await evalNative('round(3.5)');
      expect(result.ok).toBe(true);
      expect(result.value).toBe(4);
    });

    it('evaluates sqrt()', async () => {
      const result = await evalNative('sqrt(16)');
      expect(result.ok).toBe(true);
      expect(result.value).toBe(4);
    });

    it('evaluates sign()', async () => {
      const posResult = await evalNative('sign(5)');
      expect(posResult.ok).toBe(true);
      expect(posResult.value).toBe(1);

      const negResult = await evalNative('sign(-5)');
      expect(negResult.ok).toBe(true);
      expect(negResult.value).toBe(-1);

      const zeroResult = await evalNative('sign(0)');
      expect(zeroResult.ok).toBe(true);
      expect(zeroResult.value).toBe(0);
    });

    it('evaluates exp()', async () => {
      const result = await evalNative('exp(1)');
      expect(result.ok).toBe(true);
      expect(Math.abs((result.value || 0) - Math.E)).toBeLessThan(0.01);
    });

    it('evaluates log()', async () => {
      const result = await evalNative('log(e)');
      expect(result.ok).toBe(true);
      expect(Math.abs((result.value || 0) - 1)).toBeLessThan(0.001);
    });

    it('evaluates log10()', async () => {
      const result = await evalNative('log10(100)');
      expect(result.ok).toBe(true);
      expect(result.value).toBe(2);
    });

    it('evaluates log2()', async () => {
      const result = await evalNative('log2(8)');
      expect(result.ok).toBe(true);
      expect(result.value).toBe(3);
    });

    it('evaluates sin()', async () => {
      const result = await evalNative('sin(0)');
      expect(result.ok).toBe(true);
      expect(Math.abs(result.value || 0)).toBeLessThan(0.001);
    });

    it('evaluates cos()', async () => {
      const result = await evalNative('cos(0)');
      expect(result.ok).toBe(true);
      expect(Math.abs((result.value || 0) - 1)).toBeLessThan(0.001);
    });

    it('evaluates tan()', async () => {
      const result = await evalNative('tan(0)');
      expect(result.ok).toBe(true);
      expect(Math.abs(result.value || 0)).toBeLessThan(0.001);
    });

    it('evaluates asin()', async () => {
      const result = await evalNative('asin(0)');
      expect(result.ok).toBe(true);
      expect(Math.abs(result.value || 0)).toBeLessThan(0.001);
    });

    it('evaluates acos()', async () => {
      const result = await evalNative('acos(1)');
      expect(result.ok).toBe(true);
      expect(Math.abs(result.value || 0)).toBeLessThan(0.001);
    });

    it('evaluates atan()', async () => {
      const result = await evalNative('atan(0)');
      expect(result.ok).toBe(true);
      expect(Math.abs(result.value || 0)).toBeLessThan(0.001);
    });

    it('evaluates sinh()', async () => {
      const result = await evalNative('sinh(0)');
      expect(result.ok).toBe(true);
      expect(Math.abs(result.value || 0)).toBeLessThan(0.001);
    });

    it('evaluates cosh()', async () => {
      const result = await evalNative('cosh(0)');
      expect(result.ok).toBe(true);
      expect(Math.abs((result.value || 0) - 1)).toBeLessThan(0.001);
    });

    it('evaluates tanh()', async () => {
      const result = await evalNative('tanh(0)');
      expect(result.ok).toBe(true);
      expect(Math.abs(result.value || 0)).toBeLessThan(0.001);
    });

    it('evaluates sinc()', async () => {
      const result = await evalNative('sinc(0)');
      expect(result.ok).toBe(true);
      expect(Math.abs((result.value || 0) - 1)).toBeLessThan(0.001);
    });
  });

  describe('Math functions - multiple arguments', () => {
    it('evaluates pow()', async () => {
      const result = await evalNative('pow(2, 3)');
      expect(result.ok).toBe(true);
      expect(result.value).toBe(8);
    });

    it('evaluates min()', async () => {
      const result = await evalNative('min(5, 3)');
      expect(result.ok).toBe(true);
      expect(result.value).toBe(3);
    });

    it('evaluates max()', async () => {
      const result = await evalNative('max(5, 3)');
      expect(result.ok).toBe(true);
      expect(result.value).toBe(5);
    });

    it('evaluates atan2()', async () => {
      const result = await evalNative('atan2(0, 1)');
      expect(result.ok).toBe(true);
      expect(Math.abs(result.value || 0)).toBeLessThan(0.001);
    });

    it('evaluates clamp()', async () => {
      const result = await evalNative('clamp(15, 0, 10)');
      expect(result.ok).toBe(true);
      expect(result.value).toBe(10);
    });

    it('clamps lower bound', async () => {
      const result = await evalNative('clamp(-5, 0, 10)');
      expect(result.ok).toBe(true);
      expect(result.value).toBe(0);
    });

    it('clamps within bounds', async () => {
      const result = await evalNative('clamp(5, 0, 10)');
      expect(result.ok).toBe(true);
      expect(result.value).toBe(5);
    });
  });

  describe('Constants', () => {
    it('provides pi', async () => {
      const result = await evalNative('pi');
      expect(result.ok).toBe(true);
      expect(Math.abs((result.value || 0) - Math.PI)).toBeLessThan(0.001);
    });

    it('provides e', async () => {
      const result = await evalNative('e');
      expect(result.ok).toBe(true);
      expect(Math.abs((result.value || 0) - Math.E)).toBeLessThan(0.001);
    });

    it('provides inf', async () => {
      const result = await evalNative('inf');
      expect(result.ok).toBe(true);
      // JSON cannot represent Infinity; nlohmann::json serializes it as null.
      expect(result.value ?? null).toBeNull();
    });
  });

  describe('Complex expressions', () => {
    it('evaluates sin(pi/2) + cos(0)', async () => {
      const result = await evalNative('sin(pi/2) + cos(0)');
      expect(result.ok).toBe(true);
      expect(Math.abs((result.value || 0) - 2)).toBeLessThan(0.001);
    });

    it('evaluates nested functions', async () => {
      const result = await evalNative('sqrt(pow(3, 2) + pow(4, 2))');
      expect(result.ok).toBe(true);
      expect(result.value).toBe(5);
    });

    it('evaluates mixed operations with variables', async () => {
      const result = await evalNative('sqrt(x*x + y*y)', { x: 3, y: 4 });
      expect(result.ok).toBe(true);
      expect(result.value).toBe(5);
    });
  });

  describe('Unary minus', () => {
    it('handles unary minus at start', async () => {
      const result = await evalNative('-5');
      expect(result.ok).toBe(true);
      expect(result.value).toBe(-5);
    });

    it('handles unary minus in expression', async () => {
      const result = await evalNative('10 + -5');
      expect(result.ok).toBe(true);
      expect(result.value).toBe(5);
    });

    it('handles double negative', async () => {
      const result = await evalNative('--5');
      expect(result.ok).toBe(true);
      expect(result.value).toBe(5);
    });
  });

  describe('Error handling', () => {
    it('returns error for undefined variable', async () => {
      const result = await evalNative('undefined_var + 1');
      expect(result.ok).toBe(false);
      expect(result.error).toBeDefined();
    });

    it('returns error for invalid syntax', async () => {
      const result = await evalNative('2 +* 3');
      expect(result.ok).toBe(false);
    });

    it('returns error for unknown function', async () => {
      const result = await evalNative('unknown_func(5)');
      expect(result.ok).toBe(false);
      expect(result.error).toBeDefined();
    });
  });
});
//=============================================================================
