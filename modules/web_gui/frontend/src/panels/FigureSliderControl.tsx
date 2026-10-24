//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useEffect, useState, type CSSProperties } from 'react';
import type { UIControl } from '../rpc/types';
import { controlToSvg } from './figureSvg';
//=============================================================================
export function FigureSliderControl({
  control: c,
  box,
  onAction,
}: {
  control: UIControl;
  box: CSSProperties;
  onAction: (control: UIControl, value: number, text: string) => void;
}): JSX.Element {
  const vertical = c.data?.vertical ?? false;
  const [value, setValue] = useState(c.value);
  useEffect(() => setValue(c.value), [c.value]);
  return (
    <div className="figure-uicontrol" style={{ ...box, background: undefined }}>
      <svg
        width={c.w}
        height={c.h}
        viewBox={`0 0 ${c.w} ${c.h}`}
        style={{ position: 'absolute', overflow: 'visible', pointerEvents: 'none' }}
        dangerouslySetInnerHTML={{ __html: controlToSvg({ ...c, x: 0, y: 0 }) }}
      />
      <input
        type="range"
        title={c.tooltip || undefined}
        aria-label={c.tooltip || 'Slider'}
        aria-orientation={vertical ? 'vertical' : 'horizontal'}
        disabled={!c.enabled}
        min={c.min}
        max={c.max}
        step="any"
        value={value}
        style={{
          position: 'absolute',
          margin: 0,
          opacity: 0,
          left: 0,
          top: vertical ? 0 : -20,
          width: vertical ? 32 : c.w,
          height: vertical ? c.h : 24,
          writingMode: vertical ? 'vertical-lr' : undefined,
          direction: vertical ? 'rtl' : undefined,
          cursor: c.enabled ? 'pointer' : 'default',
        }}
        onChange={(event) => {
          const next = Number(event.target.value);
          setValue(next);
          onAction(c, next, c.text);
        }}
      />
    </div>
  );
}
//=============================================================================
