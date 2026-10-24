//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
export function buildTransferMathML(
  num: Array<string | number> = [],
  den: Array<string | number> = []
) {
  const numRow = buildPolyMathML(num);
  const denRow = buildPolyMathML(den);
  return `<math xmlns="http://www.w3.org/1998/Math/MathML"><mfrac>${numRow}${denRow}</mfrac></math>`;
}
//=============================================================================
export function buildPolyMathML(coeffs: Array<string | number> = []) {
  const items = Array.isArray(coeffs) ? coeffs : [];
  if (items.length === 0) return '<mrow><mn>0</mn></mrow>';

  const degree = items.length - 1;
  const parts: string[] = [];

  const parse = (raw: string | number) => {
    const txt = typeof raw === 'string' ? raw.trim() : String(raw);
    const num = Number(txt);
    const isNumeric = Number.isFinite(num);
    return { txt, num, isNumeric };
  };

  let hadAny = false;
  for (const [idx, it] of items.entries()) {
    const power = degree - idx;
    const { txt, num, isNumeric } = parse(it as string | number);
    if (isNumeric && num === 0) continue;

    let sign = '+';
    let absVal = num;
    let sym: string | null = null;

    if (!isNumeric) {
      if (!txt) continue;
      if (txt.startsWith('-')) {
        sign = '-';
        sym = txt.slice(1).trim() || '0';
      } else if (txt.startsWith('+')) {
        sym = txt.slice(1).trim() || '0';
      } else {
        sym = txt;
      }
    } else {
      sign = num < 0 ? '-' : '+';
      absVal = Math.abs(num);
    }

    if (!hadAny) {
      if (sign === '-') parts.push('<mo>-</mo>');
    } else {
      parts.push(`<mo>${sign}</mo>`);
    }
    hadAny = true;

    if (power === 0) {
      if (sym) parts.push(`<mi>${sym}</mi>`);
      else parts.push(`<mn>${absVal}</mn>`);
      continue;
    }

    if (sym) {
      parts.push(`<mi>${sym}</mi>`);
      parts.push('<mo>&#x2062;</mo>');
    } else if (absVal !== 1) {
      parts.push(`<mn>${absVal}</mn>`);
      parts.push('<mo>&#x2062;</mo>');
    }

    if (power === 1) parts.push('<mi>s</mi>');
    else parts.push(`<msup><mi>s</mi><mn>${power}</mn></msup>`);
  }

  if (!hadAny) return '<mrow><mn>0</mn></mrow>';
  return `<mrow>${parts.join('')}</mrow>`;
}
//=============================================================================
export function buildTransferTeX(
  num: Array<string | number> = [],
  den: Array<string | number> = [],
  variable = 's'
) {
  const numRow = buildPolyTeX(num, variable);
  const denRow = buildPolyTeX(den, variable);
  return `\\frac{${numRow}}{${denRow}}`;
}
//=============================================================================
export function buildPolyTeX(coeffs: Array<string | number> = [], variable = 's') {
  const items = Array.isArray(coeffs) ? coeffs : [];
  if (items.length === 0) return '0';

  const deg = items.length - 1;
  const out: string[] = [];

  const norm = (raw: string | number) => {
    const s = typeof raw === 'string' ? raw.trim() : String(raw);
    const n = Number(s);
    const isN = Number.isFinite(n);
    return { s, n, isN };
  };

  let seen = false;
  for (const [i, it] of items.entries()) {
    const power = deg - i;
    const { s, n, isN } = norm(it as string | number);
    if (isN && n === 0) continue;

    let sign = '+';
    let absVal = n;
    let sym: string | null = null;

    if (!isN) {
      if (!s) continue;
      if (s.startsWith('-')) {
        sign = '-';
        sym = s.slice(1).trim() || '0';
      } else if (s.startsWith('+')) {
        sym = s.slice(1).trim() || '0';
      } else {
        sym = s;
      }
    } else {
      sign = n < 0 ? '-' : '+';
      absVal = Math.abs(n);
    }

    if (!seen) {
      if (sign === '-') out.push('-');
    } else {
      out.push(sign);
    }
    seen = true;

    if (power === 0) {
      out.push(sym ? sym : `${absVal}`);
      continue;
    }

    if (sym) out.push(sym);
    else if (absVal !== 1) out.push(`${absVal}`);

    if (sym) out.push('\\,');

    if (power === 1) out.push(variable);
    else out.push(`${variable}^{${power}}`);
  }

  if (!seen) return '0';
  return out.join('');
}
//=============================================================================
