//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useNotebook } from '../context';
import { runWithMode } from '../runMode';
import { ControlConfig } from './ControlConfig';
//=============================================================================
interface SliderCellProps {
  block: any;
  editor: any;
}
//=============================================================================
export function SliderCell({ block, editor }: SliderCellProps) {
  const ctx = useNotebook();
  const { label, varName, value, min, max, step, runMode, runOn } = block.props;

  const commit = (next: number, released: boolean) => {
    editor.updateBlock(block, { props: { value: next } });
    const onRelease = (runOn ?? 'change') === 'release';
    if (onRelease ? released : !released) runWithMode(ctx, runMode ?? 'all', block.id);
  };

  return (
    <div className="nls-slider" contentEditable={false}>
      <div className="nls-slider-head">
        <span className="nls-control-label">{label || varName}</span>
        <span className="nls-var-badge">{varName}</span>
        <span className="nls-slider-value">{formatNum(value)}</span>
        <ControlConfig block={block} editor={editor} slider />
      </div>
      <input
        type="range"
        min={min}
        max={max}
        step={step}
        value={value}
        onInput={(e) => commit(parseFloat((e.target as HTMLInputElement).value), false)}
        onPointerUp={() => commit(value, true)}
        onKeyUp={() => commit(value, true)}
      />
      <div className="nls-slider-scale">
        <span>{formatNum(min)}</span>
        <span>{formatNum(max)}</span>
      </div>
    </div>
  );
}
//=============================================================================
function formatNum(n: number): string {
  return Number.isInteger(n) ? String(n) : n.toFixed(2);
}
//=============================================================================
