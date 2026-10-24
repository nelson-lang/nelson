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
import type { FigurePlotObject } from '../rpc/figureTypes';
//=============================================================================
export function FigureLegend({
  objects,
  action,
}: {
  objects: FigurePlotObject[];
  action: (request: Record<string, unknown>) => void;
}) {
  const timers = useRef(new Map<number, ReturnType<typeof setTimeout>>());
  useEffect(() => {
    const pending = timers.current;
    return () => pending.forEach(clearTimeout);
  }, []);
  return (
    <div className="figure-workspace-legend" aria-label="Plotted objects">
      {objects.map((object, index) => (
        <button
          key={object.handle}
          title="Click to show/hide; double-click to isolate/restore"
          aria-pressed={object.visible === 'on'}
          onClick={(event) => {
            clearTimeout(timers.current.get(object.handle));
            if (event.detail > 1) return;
            const toggle = () =>
              action({
                action: 'properties',
                objectHandle: object.handle,
                properties: { Visible: object.visible === 'on' ? 'off' : 'on' },
              });
            if (event.detail === 0) toggle();
            else timers.current.set(object.handle, setTimeout(toggle, 500));
          }}
          onDoubleClick={() => {
            clearTimeout(timers.current.get(object.handle));
            action({ action: 'isolate', objectHandle: object.handle });
          }}
        >
          <span
            className="figure-legend-line"
            style={{
              background: Array.isArray(object.Color)
                ? `rgb(${object.Color.map((value) => Math.round(value * 255)).join(',')})`
                : undefined,
            }}
          />
          {object.DisplayName || `${object.type} ${index + 1}`}
        </button>
      ))}
    </div>
  );
}
//=============================================================================
