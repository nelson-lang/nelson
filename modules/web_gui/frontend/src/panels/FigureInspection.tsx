//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { FigureDataPoint, FigurePlotObject } from '../rpc/figureTypes';
//=============================================================================
export function FigureInspection({
  point,
  objects,
  width,
  height,
}: {
  point: FigureDataPoint | null;
  objects: FigurePlotObject[];
  width: number;
  height: number;
}) {
  if (!point?.found || !point.pixel || !point.position) return null;
  const object = objects.find((item) => item.handle === point.objectHandle);
  const name = object?.DisplayName || object?.type || 'Data';
  const left = point.pixel[0],
    top = height - point.pixel[1];
  return (
    <>
      <span className="figure-inspected-point" aria-hidden="true" style={{ left, top }} />
      <div
        className="figure-point-tip"
        role="status"
        style={{
          left: Math.max(0, Math.min(left + 14, width - 230)),
          top: Math.max(0, Math.min(top - 65, height - 60)),
        }}
      >
        <strong>
          {name} · Point {point.index}
        </strong>
        <div>
          {point.position
            .map((value, index) => `${'XYZ'[index]} ${Number(value.toPrecision(7))}`)
            .join(' · ')}
        </div>
      </div>
    </>
  );
}
//=============================================================================
