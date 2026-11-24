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
import { t, tf } from '../../i18n';
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
function taskOps(): TaskOp[] {
  const vis = t('Visualization');
  const stats = t('Statistics');
  const tr = t('Transform');
  return [
    { value: 'plot', label: t('Line plot'), cat: vis, gen: (v) => `plot(${v})` },
    { value: 'bar', label: t('Bar chart'), cat: vis, gen: (v) => `bar(${v})` },
    { value: 'hist', label: t('Histogram'), cat: vis, gen: (v) => `hist(${v})` },
    { value: 'stairs', label: t('Stairs'), cat: vis, gen: (v) => `stairs(${v})` },
    { value: 'area', label: t('Area'), cat: vis, gen: (v) => `area(${v})` },
    { value: 'mean', label: t('Mean'), cat: stats, gen: (v, o) => `${o} = mean(${v})` },
    { value: 'median', label: t('Median'), cat: stats, gen: (v, o) => `${o} = median(${v})` },
    { value: 'std', label: t('Std deviation'), cat: stats, gen: (v, o) => `${o} = std(${v})` },
    { value: 'var', label: t('Variance'), cat: stats, gen: (v, o) => `${o} = var(${v})` },
    { value: 'max', label: t('Maximum'), cat: stats, gen: (v, o) => `${o} = max(${v})` },
    { value: 'min', label: t('Minimum'), cat: stats, gen: (v, o) => `${o} = min(${v})` },
    { value: 'sum', label: t('Sum'), cat: stats, gen: (v, o) => `${o} = sum(${v})` },
    { value: 'sort', label: t('Sort'), cat: tr, gen: (v, o) => `${o} = sort(${v})` },
    { value: 'unique', label: t('Unique'), cat: tr, gen: (v, o) => `${o} = unique(${v})` },
    { value: 'cumsum', label: t('Cumulative sum'), cat: tr, gen: (v, o) => `${o} = cumsum(${v})` },
    { value: 'diff', label: t('Difference'), cat: tr, gen: (v, o) => `${o} = diff(${v})` },
    { value: 'fft', label: t('FFT magnitude'), cat: tr, gen: (v, o) => `${o} = abs(fft(${v}))` },
  ];
}
//=============================================================================
export function LiveTaskCell({ block, editor }: LiveTaskProps) {
  const { op, inputVar } = block.props;
  const ops = taskOps();
  const categories = [...new Set(ops.map((o) => o.cat))];
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
    const s = ops.find((o) => o.value === p.op) ?? ops[0];
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
        <span className="nls-task-badge">{t('TASK')}</span>
        <select
          className="nls-select"
          value={op}
          onChange={(e) => void run({ op: e.target.value })}
        >
          {categories.map((cat) => (
            <optgroup key={cat} label={cat}>
              {ops
                .filter((o) => o.cat === cat)
                .map((o) => (
                  <option key={o.value} value={o.value}>
                    {o.label}
                  </option>
                ))}
            </optgroup>
          ))}
          <optgroup label={t('Custom')}>
            <option value="custom">{t('Custom template…')}</option>
          </optgroup>
        </select>
        <span className="nls-task-of">{t('of')}</span>
        <select
          className="nls-select"
          value={inputVar}
          onChange={(e) => void run({ inputVar: e.target.value })}
        >
          <option value="">{t('(variable)')}</option>
          {options.map((v) => (
            <option key={v} value={v}>
              {v}
            </option>
          ))}
        </select>
        <button className="nls-btn nls-btn-primary" onClick={() => void run({})}>
          {t('Run')}
        </button>
      </div>
      {op === 'custom' && (
        <input
          className="nls-task-template"
          defaultValue={block.props.template}
          placeholder={t('e.g. result = smooth($var)  (use $var and $out)')}
          onKeyDown={(e) => e.stopPropagation()}
          onBlur={(e) => void run({ template: e.target.value })}
        />
      )}
      <pre className="nls-task-code">{code}</pre>
      {(output || figures.length > 0 || errorMsg) && (
        <div className="nls-result">
          {errorMsg ? (
            <pre className="nls-output nls-error">{tf('Error: %1', errorMsg)}</pre>
          ) : (
            output && <pre className="nls-output">{output}</pre>
          )}
          {figures.map((src, i) => (
            <div className="nls-figure-wrap" key={i}>
              <img className="nls-figure" src={src} alt={t('figure')} />
            </div>
          ))}
        </div>
      )}
    </div>
  );
}
//=============================================================================
