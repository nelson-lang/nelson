//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useEffect, useState } from 'react';
import { rpc } from '../../rpc/client';
import type { WorkspaceListResult } from '../../rpc/types';
import { runCell } from '../kernel';
//=============================================================================
interface LiveTaskProps {
  block: any;
  editor: any;
}
//=============================================================================
// A minimal Live Editor-style task: an interactive component that GENERATES
// Nelson code from its parameters and runs it, showing the generated code and
// the result inline. Operations map to engine builtins.
interface TaskOp {
  value: string;
  label: string;
  cat: string;
  gen: (v: string, out: string) => string;
}
//=============================================================================
// A small task gallery, grouped by category. Each task generates Nelson code
// from the chosen workspace variable.
const OPS: TaskOp[] = [
  // Visualization (no output variable)
  { value: 'plot', label: 'Line plot', cat: 'Visualization', gen: (v) => `plot(${v})` },
  { value: 'bar', label: 'Bar chart', cat: 'Visualization', gen: (v) => `bar(${v})` },
  { value: 'hist', label: 'Histogram', cat: 'Visualization', gen: (v) => `hist(${v})` },
  { value: 'stairs', label: 'Stairs', cat: 'Visualization', gen: (v) => `stairs(${v})` },
  { value: 'area', label: 'Area', cat: 'Visualization', gen: (v) => `area(${v})` },
  { value: 'mean', label: 'Mean', cat: 'Statistics', gen: (v, o) => `${o} = mean(${v})` },
  { value: 'median', label: 'Median', cat: 'Statistics', gen: (v, o) => `${o} = median(${v})` },
  { value: 'std', label: 'Std deviation', cat: 'Statistics', gen: (v, o) => `${o} = std(${v})` },
  { value: 'var', label: 'Variance', cat: 'Statistics', gen: (v, o) => `${o} = var(${v})` },
  { value: 'max', label: 'Maximum', cat: 'Statistics', gen: (v, o) => `${o} = max(${v})` },
  { value: 'min', label: 'Minimum', cat: 'Statistics', gen: (v, o) => `${o} = min(${v})` },
  { value: 'sum', label: 'Sum', cat: 'Statistics', gen: (v, o) => `${o} = sum(${v})` },
  { value: 'sort', label: 'Sort', cat: 'Transform', gen: (v, o) => `${o} = sort(${v})` },
  { value: 'unique', label: 'Unique', cat: 'Transform', gen: (v, o) => `${o} = unique(${v})` },
  {
    value: 'cumsum',
    label: 'Cumulative sum',
    cat: 'Transform',
    gen: (v, o) => `${o} = cumsum(${v})`,
  },
  { value: 'diff', label: 'Difference', cat: 'Transform', gen: (v, o) => `${o} = diff(${v})` },
  {
    value: 'fft',
    label: 'FFT magnitude',
    cat: 'Transform',
    gen: (v, o) => `${o} = abs(fft(${v}))`,
  },
];
//=============================================================================
const OP_CATEGORIES = [...new Set(OPS.map((o) => o.cat))];
export function LiveTaskCell({ block, editor }: LiveTaskProps) {
  const { op, inputVar } = block.props;
  const output: string = block.props.output ?? '';
  const errorMsg: string = block.props.errorMsg ?? '';
  let figures: string[] = [];
  try {
    figures = JSON.parse(block.props.figures || '[]');
  } catch {
    figures = [];
  }
  const [vars, setVars] = useState<string[]>([]);
  useEffect(() => {
    let alive = true;
    rpc<WorkspaceListResult>('workspace.list')
      .then((r) => alive && setVars((r.variables ?? []).map((v) => v.name)))
      .catch(() => {});
    return () => {
      alive = false;
    };
  }, []);

  const genCode = (p: Record<string, any>): string => {
    if (p.op === 'custom') {
      return String(p.template || 'result = f($var)')
        .replace(/\$var|\$x/g, p.inputVar || 'x')
        .replace(/\$out/g, p.outVar || 'result');
    }
    const s = OPS.find((o) => o.value === p.op) ?? OPS[0];
    return s.gen(p.inputVar || 'x', p.outVar || 'result');
  };
  const code = genCode(block.props);

  const run = async (nextProps: Record<string, unknown>) => {
    const merged = { ...block.props, ...nextProps };
    const generated = genCode(merged);
    const res = await runCell(generated);
    editor.updateBlock(block, {
      props: {
        ...nextProps,
        output: res.errorMessage ? '' : res.output,
        figures: JSON.stringify(res.figures.map((f) => f.image)),
        errorMsg: res.errorMessage,
      },
    });
  };

  const options = vars.length ? vars : inputVar ? [inputVar] : [];

  return (
    <div className="nls-task" contentEditable={false}>
      <div className="nls-task-head">
        <span className="nls-task-badge">TASK</span>
        <select
          className="nls-select"
          value={op}
          onChange={(e) => void run({ op: e.target.value })}
        >
          {OP_CATEGORIES.map((cat) => (
            <optgroup key={cat} label={cat}>
              {OPS.filter((o) => o.cat === cat).map((o) => (
                <option key={o.value} value={o.value}>
                  {o.label}
                </option>
              ))}
            </optgroup>
          ))}
          <optgroup label="Custom">
            <option value="custom">Custom template…</option>
          </optgroup>
        </select>
        <span className="nls-task-of">of</span>
        <select
          className="nls-select"
          value={inputVar}
          onChange={(e) => void run({ inputVar: e.target.value })}
        >
          <option value="">(variable)</option>
          {options.map((v) => (
            <option key={v} value={v}>
              {v}
            </option>
          ))}
        </select>
        <button className="nls-btn nls-btn-primary" onClick={() => void run({})}>
          Run
        </button>
      </div>
      {op === 'custom' && (
        <input
          className="nls-task-template"
          defaultValue={block.props.template}
          placeholder="e.g. result = smooth($var)  (use $var and $out)"
          onKeyDown={(e) => e.stopPropagation()}
          onBlur={(e) => void run({ template: e.target.value })}
        />
      )}
      <pre className="nls-task-code">{code}</pre>
      {(output || figures.length > 0 || errorMsg) && (
        <div className="nls-result">
          {errorMsg ? (
            <pre className="nls-output nls-error">Error: {errorMsg}</pre>
          ) : (
            output && <pre className="nls-output">{output}</pre>
          )}
          {figures.map((src, i) => (
            <div className="nls-figure-wrap" key={i}>
              <img className="nls-figure" src={src} alt="figure" />
            </div>
          ))}
        </div>
      )}
    </div>
  );
}
//=============================================================================
