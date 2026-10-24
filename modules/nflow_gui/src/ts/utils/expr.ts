//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import * as FieldNames from '../FieldNames.js';
//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/**
 * Remove leading LaTeX backslashes from identifiers inside a string.
 * Example: "\\alpha + x" -> "alpha + x".
 */
export const replaceLatexVars = (expr: unknown): string =>
  String(expr || '').replace(/\\[A-Za-z]+/g, (match) => match.slice(1));
type Token =
  | { type: 'number'; value: number }
  | { type: 'ident'; value: string }
  | { type: 'op'; value: string }
  | { type: 'lparen' }
  | { type: 'rparen' }
  | { type: 'comma' };
const lexExpr = (expr: unknown): Token[] => {
  const tokens: Token[] = [];
  const input = String(expr);
  let i = 0;
  while (i < input.length) {
    const ch = input[i];
    if (/\s/.test(ch)) {
      i += 1;
      continue;
    }
    if (/[0-9.]/.test(ch)) {
      const start = i;
      i += 1;
      while (i < input.length && /[0-9.]/.test(input[i])) i += 1;
      if (i < input.length && /[eE]/.test(input[i])) {
        i += 1;
        if (/[+-]/.test(input[i])) i += 1;
        while (i < input.length && /[0-9]/.test(input[i])) i += 1;
      }
      tokens.push({ type: 'number', value: Number(input.slice(start, i)) });
      continue;
    }
    if (/[A-Za-z_]/.test(ch)) {
      const start = i;
      i += 1;
      while (i < input.length && /[A-Za-z0-9_]/.test(input[i])) i += 1;
      const name = input.slice(start, i);
      tokens.push({ type: 'ident', value: name });
      continue;
    }
    if ('+-*/%^(),'.includes(ch)) {
      if (ch === '(') tokens.push({ type: 'lparen' });
      else if (ch === ')') tokens.push({ type: 'rparen' });
      else if (ch === ',') tokens.push({ type: 'comma' });
      else tokens.push({ type: 'op', value: ch });
      i += 1;
      continue;
    }
    return [];
  }
  return tokens;
};
//=============================================================================
const opInfo: Record<string, { prec: number; assoc: 'left' | 'right'; arity: number }> = {
  '^': { prec: 4, assoc: 'right', arity: 2 },
  neg: { prec: 3, assoc: 'right', arity: 1 },
  '*': { prec: 2, assoc: 'left', arity: 2 },
  '/': { prec: 2, assoc: 'left', arity: 2 },
  '%': { prec: 2, assoc: 'left', arity: 2 },
  '+': { prec: 1, assoc: 'left', arity: 2 },
  '-': { prec: 1, assoc: 'left', arity: 2 },
};
//=============================================================================
type RpnToken =
  | { type: 'number'; value: number }
  | { type: 'ident'; value: string }
  | { type: 'op'; value: string }
  | { type: 'func'; name: string; argc?: number };
const shuntingYard = (tokens: Token[]): RpnToken[] => {
  const output: RpnToken[] = [];
  type OpStackItem = Token | { type: 'func'; name: string } | { type: 'op'; value: string };
  const opStack: OpStackItem[] = [];
  const parenStack: Array<{ isFunc: boolean; argc: number }> = [];
  let prevType: string = FieldNames.kStart;
  const tokenCount = tokens.length;
  for (let idx = 0; idx < tokenCount; idx += 1) {
    const token = tokens[idx];
    if (token.type === 'number') {
      output.push(token);
      if (
        parenStack.length &&
        parenStack[parenStack.length - 1].isFunc &&
        (prevType === 'lparen' || prevType === 'comma')
      ) {
        parenStack[parenStack.length - 1].argc += 1;
      }
      prevType = FieldNames.kValue;
      continue;
    }
    if (token.type === 'ident') {
      const next = tokens[idx + 1];
      if (next && next.type === 'lparen') {
        if (
          parenStack.length &&
          parenStack[parenStack.length - 1].isFunc &&
          (prevType === 'lparen' || prevType === 'comma')
        ) {
          parenStack[parenStack.length - 1].argc += 1;
        }
        opStack.push({ type: 'func', name: token.value.toLowerCase() });
        prevType = 'func';
      } else {
        output.push({ type: 'ident', value: token.value });
        if (
          parenStack.length &&
          parenStack[parenStack.length - 1].isFunc &&
          (prevType === 'lparen' || prevType === 'comma')
        ) {
          parenStack[parenStack.length - 1].argc += 1;
        }
        prevType = FieldNames.kValue;
      }
      continue;
    }
    if (token.type === 'op') {
      let op: string = token.value;
      if (
        op === '-' &&
        (prevType === FieldNames.kStart ||
          prevType === 'op' ||
          prevType === 'lparen' ||
          prevType === 'comma')
      ) {
        op = 'neg';
        if (
          parenStack.length &&
          parenStack[parenStack.length - 1].isFunc &&
          (prevType === 'lparen' || prevType === 'comma')
        ) {
          parenStack[parenStack.length - 1].argc += 1;
        }
      }
      const info = opInfo[op];
      if (!info) return [];
      while (opStack.length) {
        const top = opStack[opStack.length - 1] as OpStackItem | undefined;
        if (!top || (top as OpStackItem).type !== 'op') break;
        const topInfo = opInfo[(top as { type: string; value?: string }).value as string];
        if (
          (info.assoc === 'left' && info.prec <= topInfo.prec) ||
          (info.assoc === 'right' && info.prec < topInfo.prec)
        ) {
          output.push(opStack.pop() as RpnToken | { type: 'op'; value: string });
          continue;
        }
        break;
      }
      opStack.push({ type: 'op', value: op });
      prevType = 'op';
      continue;
    }
    if (token.type === 'lparen') {
      if (
        parenStack.length &&
        parenStack[parenStack.length - 1].isFunc &&
        (prevType === 'lparen' || prevType === 'comma')
      ) {
        parenStack[parenStack.length - 1].argc += 1;
      }
      opStack.push(token);
      const isFunc = opStack.length > 1 && opStack[opStack.length - 2].type === 'func';
      parenStack.push({ isFunc, argc: 0 });
      prevType = 'lparen';
      continue;
    }
    if (token.type === 'comma') {
      while (opStack.length && opStack[opStack.length - 1].type !== 'lparen') {
        output.push(opStack.pop()! as RpnToken);
      }
      if (!opStack.length) return [];
      prevType = 'comma';
      continue;
    }
    if (token.type === 'rparen') {
      while (opStack.length && opStack[opStack.length - 1].type !== 'lparen') {
        output.push(opStack.pop()! as RpnToken);
      }
      if (!opStack.length) return [];
      opStack.pop();
      const paren = parenStack.pop();
      if (paren && paren.isFunc) {
        const funcToken = opStack.pop();
        if (!funcToken || funcToken.type !== 'func') return [];
        if (paren.argc <= 0) return [];
        output.push({ type: 'func', name: funcToken.name, argc: paren.argc });
      }
      prevType = FieldNames.kValue;
      continue;
    }
  }
  while (opStack.length) {
    const top = opStack.pop() as OpStackItem;
    if (top.type === 'lparen') return [];
    output.push(top as RpnToken | { type: 'op'; value: string });
  }
  return output;
};
/**
 * Evaluate a numeric expression string with optional variables.
 * Returns NaN when unavailable or invalid.
 */
export const evalExpression = (expr: unknown, variables?: Record<string, unknown>): number => {
  if (typeof expr === 'number') return expr;
  if (expr == null) return NaN;
  const trimmed = replaceLatexVars(expr).trim();
  if (!trimmed) return NaN;
  const direct = Number(trimmed);
  if (!Number.isNaN(direct)) return direct;

  const tokens = lexExpr(trimmed);
  if (!tokens.length) return NaN;
  const rpn = shuntingYard(tokens);
  if (!rpn.length) return NaN;
  const ast = astFromRpn(rpn);
  const scope = {
    pi: Math.PI,
    e: Math.E,
    inf: Infinity,
    infinity: Infinity,
    ...(variables || {}),
  } as Record<string, unknown>;
  const value = evalAst(ast, scope);
  return Number.isFinite(value) ? value : NaN;
};
//=============================================================================
type AstNumber = { type: 'number'; value: number };
type AstIdent = { type: 'ident'; name: string };
type AstOp = { type: 'op'; op: string; arg?: AstNode; left?: AstNode; right?: AstNode };
type AstFunc = { type: 'func'; name: string; args: AstNode[] };
type AstNode = AstNumber | AstIdent | AstOp | AstFunc | null;
const astFromRpn = (rpn: RpnToken[]): AstNode => {
  const stack: AstNode[] = [];
  for (const token of rpn) {
    if (token.type === 'number') {
      stack.push({ type: 'number', value: token.value });
      continue;
    }
    if (token.type === 'ident') {
      stack.push({ type: 'ident', name: token.value });
      continue;
    }
    if (token.type === 'op') {
      const info = opInfo[token.value];
      if (!info || stack.length < info.arity) return null;
      if (info.arity === 1) {
        const arg = stack.pop() || null;
        stack.push({ type: 'op', op: token.value, arg });
      } else {
        const right = stack.pop() || null;
        const left = stack.pop() || null;
        stack.push({ type: 'op', op: token.value, left, right });
      }
      continue;
    }
    if (token.type === 'func') {
      if (stack.length < (token.argc || 0)) return null;
      const args = stack.splice(stack.length - (token.argc || 0), token.argc || 0) as AstNode[];
      stack.push({ type: 'func', name: token.name, args });
    }
  }
  if (stack.length !== 1) return null;
  return stack[0];
};
//=============================================================================
const sinc = (value: number) => {
  if (value === 0) return 1;
  const x = Math.PI * value;
  return Math.sin(x) / x;
};
//=============================================================================
const evalFunction: Record<string, (...args: number[]) => number> = {
  abs: Math.abs,
  ceil: Math.ceil,
  floor: Math.floor,
  round: Math.round,
  sign: Math.sign,
  min: Math.min,
  max: Math.max,
  pow: Math.pow,
  sqrt: Math.sqrt,
  acos: Math.acos,
  asin: Math.asin,
  atan: Math.atan,
  atan2: Math.atan2,
  cos: Math.cos,
  cosh: Math.cosh,
  sin: Math.sin,
  sinh: Math.sinh,
  tan: Math.tan,
  tanh: Math.tanh,
  exp: Math.exp,
  log: Math.log,
  log10: Math.log10,
  log2: Math.log2,
  sinc,
  clamp: (value: number, min: number, max: number) => Math.min(Math.max(value, min), max),
};
//=============================================================================
const evalAst = (node: AstNode, variables: Record<string, unknown>): number => {
  if (!node) return NaN;
  if (node.type === 'number') return node.value;
  if (node.type === 'ident') {
    const value = variables[node.name];
    return typeof value === 'number' ? value : Number(value);
  }
  if (node.type === 'op') {
    if (node.op === 'neg') return -evalAst(node.arg || null, variables);
    const left = evalAst(node.left || null, variables);
    const right = evalAst(node.right || null, variables);
    if (!Number.isFinite(left) || !Number.isFinite(right)) return NaN;
    switch (node.op) {
      case '+':
        return left + right;
      case '-':
        return left - right;
      case '*':
        return left * right;
      case '/':
        return left / right;
      case '%':
        return left % right;
      case '^':
        return Math.pow(left, right);
    }
  }
  if (node.type === 'func') {
    const fn = evalFunction[node.name];
    if (!fn) return NaN;
    const args = node.args.map((arg) => evalAst(arg, variables));
    if (args.some((value) => !Number.isFinite(value))) return NaN;
    return fn(...args);
  }
  return NaN;
};
//=============================================================================
const latexFunctions: Record<string, string> = {
  sin: '\\sin',
  cos: '\\cos',
  tan: '\\tan',
  asin: '\\arcsin',
  acos: '\\arccos',
  atan: '\\arctan',
  sinh: '\\sinh',
  cosh: '\\cosh',
  tanh: '\\tanh',
  exp: '\\exp',
  log: '\\log',
};
//=============================================================================
const FORMATTERS: Record<string, (a: string[]) => string> = {
  sqrt: (a) => (a[0] ? `\\sqrt{${a[0]}}` : ''),
  abs: (a) => (a[0] ? `\\left|${a[0]}\\right|` : ''),
  min: (a) => `\\min${parenthesize(a.join(', '), true)}`,
  max: (a) => `\\max${parenthesize(a.join(', '), true)}`,
  sinc: (a) => `\\operatorname{sinc}${parenthesize(a.join(', '), true)}`,
  clamp: (a) => `\\operatorname{clamp}${parenthesize(a.join(', '), true)}`,
  log10: (a) => `\\log_{10}${parenthesize(a.join(', '), true)}`,
  log2: (a) => `\\log_{2}${parenthesize(a.join(', '), true)}`,
  pow: (a) =>
    a.length === 2 ? `${a[0]}^{${a[1]}}` : `\\operatorname{pow}${parenthesize(a.join(', '), true)}`,
};
//=============================================================================
const escapeLatexText = (text: unknown) => String(text).replace(/_/g, '\\_');
const precedenceOf = (node: AstNode) => {
  if (!node) return 0;
  if (node.type === 'number' || node.type === 'ident') return 5;
  if (node.type === 'func') return 4;
  if (node.type === 'op') {
    if (node.op === 'neg') return 3;
    if (node.op === '^') return 3;
    if (node.op === '*' || node.op === '/') return 2;
    return 1;
  }
  return 0;
};
//=============================================================================
const parenthesize = (content: string, shouldWrap: boolean) => {
  if (!shouldWrap) return content;
  const needsLarge = /\\left\\(|\\right\\)|\(/.test(content);
  return needsLarge ? `\\left(${content}\\right)` : `(${content})`;
};
//=============================================================================
const formatLatex = (node: AstNode, parentPrec = 0, parentOp: string | null = null): string => {
  if (!node) return '';
  if (node.type === 'number') return Number.isFinite(node.value) ? String(node.value) : '0';
  if (node.type === 'ident') {
    if (node.name === FieldNames.kPi) return '\\pi';
    if (node.name === 'e') return 'e';
    if (/^[A-Za-z]$/.test(node.name)) return node.name;
    return `\\mathrm{${escapeLatexText(node.name)}}`;
  }
  if (node.type === 'func') {
    const name = node.name;
    const args = node.args.map((a) => formatLatex(a, 0));
    if (Object.prototype.hasOwnProperty.call(FORMATTERS, name)) return FORMATTERS[name](args);
    const fn = latexFunctions[name] || `\operatorname{${escapeLatexText(name)}}`;
    return `${fn}${parenthesize(args.join(', '), true)}`;
  }
  if (node.type === 'op') {
    const prec = precedenceOf(node);
    if (node.op === 'neg') {
      const inner = formatLatex(node.arg || null, prec, 'neg');
      return parenthesize(`-${inner}`, prec < parentPrec);
    }
    const left = formatLatex(node.left || null, prec, node.op);
    const right = formatLatex(node.right || null, prec, node.op);
    let expr = '';
    switch (node.op) {
      case '+':
        expr = `${left} + ${right}`;
        break;
      case '-':
        expr = `${left} - ${right}`;
        break;
      case '*':
        expr = `${left} ${right}`;
        break;
      case '/':
        expr = `\\frac{${left}}{${right}}`;
        break;
      case '^':
        expr = `${left}^{${right}}`;
        break;
    }
    const needsWrap = prec < parentPrec || (parentOp === '^' && node.op === '^');
    return parenthesize(expr, needsWrap);
  }
  return '';
};
//=============================================================================
export const exprToLatex = (expr: unknown): string => {
  if (expr == null) return '';
  const trimmed = replaceLatexVars(expr).trim();
  if (!trimmed) return '';
  const tokens = lexExpr(trimmed);
  if (!tokens.length) return escapeLatexText(trimmed);
  const rpn = shuntingYard(tokens);
  if (!rpn.length) return escapeLatexText(trimmed);
  const ast = astFromRpn(rpn);
  if (!ast) return escapeLatexText(trimmed);
  return formatLatex(ast);
};
//=============================================================================
const stripLatexCommands = (latex: unknown): string =>
  String(latex || '')
    .replace(/\\left|\\right/g, '')
    .replace(/\\operatorname\{([^}]*)\}/g, '$1')
    .replace(/\\([a-zA-Z]+)/g, '$1')
    .replace(/[{}]/g, '')
    .replace(/\s+/g, ' ')
    .trim();
export const estimateLatexWidth = (latex: unknown): number => {
  const visible = stripLatexCommands(latex);
  const length = Math.max(visible.length, 1);
  const charWidth = 12;
  const padding = 28;
  const minWidth = 80;
  return Math.max(minWidth, Math.ceil(length * charWidth + padding));
};
//=============================================================================
export const resolveNumeric = (
  value: unknown,
  variables?: Record<string, unknown>,
  { allowExpressions = true }: { allowExpressions?: boolean } = {}
): number => {
  if (value == null) return 0;
  if (typeof value === 'number' && Number.isFinite(value)) return value;
  const text = String(value).trim();
  if (!text) return 0;
  const direct = Number(text);
  if (Number.isFinite(direct)) return direct;
  const merged = { pi: Math.PI, e: Math.E, ...(variables || {}) } as Record<string, unknown>;
  if (Object.prototype.hasOwnProperty.call(merged, text)) {
    return Number(merged[text] as unknown) || 0;
  }
  const stripped = text.startsWith('\\') ? text.slice(1) : text;
  if (Object.prototype.hasOwnProperty.call(merged, stripped)) {
    return Number(merged[stripped] as unknown) || 0;
  }
  if (!allowExpressions) return 0;
  const evaluated = evalExpression(text, merged);
  return Number.isFinite(evaluated) ? evaluated : 0;
};
//=============================================================================
export const resolveArray = (
  value: unknown,
  variables?: Record<string, unknown>,
  options?: Record<string, unknown>
): number[] => {
  if (Array.isArray(value)) {
    return value.map((v) => resolveNumeric(v, variables, options));
  }
  if (typeof value === 'string') {
    return value
      .split(',')
      .map((v) => v.trim())
      .filter(Boolean)
      .map((v) => resolveNumeric(v, variables, options));
  }
  return [];
};
//=============================================================================
export const normalizeVarName = (name: unknown): string => {
  if (!name) return '';
  const trimmed = String(name).trim();
  if (trimmed.startsWith('\\')) return trimmed.slice(1);
  return trimmed;
};
//=============================================================================
export const parseVariables = (
  text: unknown
): { vars: Record<string, number>; display: string[] } => {
  const vars: Record<string, number> = { pi: Math.PI, e: Math.E };
  const display: string[] = [];
  const lines = String(text || '').split(/\r?\n/);
  lines.forEach((line) => {
    const trimmed = line.trim();
    if (!trimmed || trimmed.startsWith('#')) return;
    const idx = trimmed.indexOf('=');
    if (idx < 0) return;
    const name = trimmed.slice(0, idx).trim();
    const expr = trimmed.slice(idx + 1).trim();
    if (!name) return;
    const key = normalizeVarName(name);
    if (!key) return;
    const value = evalExpression(expr, vars as Record<string, unknown>);
    if (Number.isFinite(value)) {
      vars[key] = value;
      display.push(`${name}=${value}`);
    }
  });
  return { vars, display };
};
//=============================================================================
class LatexRenderer {
  render(node: AstNode, parentPrec = 0, parentOp: string | null = null): string {
    // Delegates to the shared formatter so token sequences are not duplicated.
    return formatLatex(node, parentPrec, parentOp);
  }
}
//=============================================================================
export class ExpressionParser {
  expr: unknown;
  constructor(expr: unknown) {
    this.expr = expr;
  }
  //=============================================================================
  static replaceLatexVars(expr: unknown) {
    return replaceLatexVars(expr);
  }
  //=============================================================================
  static tokenize(expr: unknown) {
    return lexExpr(expr);
  }
  //=============================================================================
  static toRpn(tokens: Token[]) {
    return shuntingYard(tokens);
  }
  //=============================================================================
  static buildAst(rpn: RpnToken[]) {
    return astFromRpn(rpn);
  }
  //=============================================================================
  parse() {
    const txt = replaceLatexVars(this.expr).trim();
    if (!txt) return null;
    const tokens = lexExpr(txt);
    if (!tokens.length) return null;
    const rpn = shuntingYard(tokens);
    if (!rpn.length) return null;
    return astFromRpn(rpn) as AstNode | null;
  }
  //=============================================================================
  evaluate(variables?: Record<string, unknown>): number {
    return evalExpression(this.expr, variables);
  }
  //=============================================================================
  toLatex(): string {
    const ast = this.parse();
    if (!ast) return escapeLatexText(String(this.expr || ''));
    const renderer = new LatexRenderer();
    return renderer.render(ast);
  }
}
//=============================================================================
