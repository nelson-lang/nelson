//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useEffect, useRef } from 'react';
//=============================================================================
export function FigureInspectorResize({
  width,
  onChange,
}: {
  width: number;
  onChange: (width: number) => void;
}) {
  const cleanup = useRef<() => void>();
  useEffect(() => () => cleanup.current?.(), []);
  const update = (value: number) => onChange(Math.max(220, Math.min(600, value)));
  return (
    <div
      className="figure-inspector-resize"
      role="separator"
      tabIndex={0}
      aria-label="Properties width"
      aria-orientation="vertical"
      aria-valuemin={220}
      aria-valuemax={600}
      aria-valuenow={width}
      onKeyDown={(event) => {
        if (event.key !== 'ArrowLeft' && event.key !== 'ArrowRight') return;
        event.preventDefault();
        update(width + (event.key === 'ArrowLeft' ? 10 : -10));
      }}
      onPointerDown={(event) => {
        if (event.button !== 0) return;
        event.preventDefault();
        cleanup.current?.();
        const start = event.clientX;
        const move = (next: PointerEvent) => update(width + start - next.clientX);
        const end = () => {
          window.removeEventListener('pointermove', move);
          window.removeEventListener('pointerup', end);
          window.removeEventListener('pointercancel', end);
          window.removeEventListener('blur', end);
          cleanup.current = undefined;
        };
        cleanup.current = end;
        window.addEventListener('pointermove', move);
        window.addEventListener('pointerup', end);
        window.addEventListener('pointercancel', end);
        window.addEventListener('blur', end);
      }}
    />
  );
}
//=============================================================================
