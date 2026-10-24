//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { NelsonWorkerClientApi } from '@nelson/wasm-client';
import type { FigureDisplayListResult } from '../rpc/types';
//=============================================================================
interface FigureViewHost {
  client: NelsonWorkerClientApi;
  handle: number;
  busy: boolean;
  current: () => FigureDisplayListResult;
  accept: (frame: FigureDisplayListResult) => FigureDisplayListResult;
  text: (value: string) => string;
  finite: (value: unknown, name: string) => number;
}
//=============================================================================
export async function setWasmFigureView(
  params: Record<string, unknown>,
  host: FigureViewHost
): Promise<FigureDisplayListResult> {
  const { client, handle } = host;
  const axesHandleValue = Number(params.axesHandle ?? 0);
  if (!Number.isFinite(axesHandleValue) || axesHandleValue < 0)
    throw new Error('Invalid WebAssembly axes handle');
  const axesHandle = Math.trunc(axesHandleValue);
  const azimuth = host.finite(params.azimuth, 'azimuth');
  const elevation = host.finite(params.elevation, 'elevation');
  const compactView = Boolean(params.compactView);
  if (typeof params.historyGroup === 'string') {
    const request = {
      action: 'view',
      axesHandle,
      azimuth,
      elevation,
      compactView,
      historyGroup: params.historyGroup,
      softwareFallback: params.softwareFallback !== false,
    };
    if (host.busy) {
      client.queueCommand(
        `__web_figure_deferred_action__(${handle}, ${host.text(JSON.stringify(request))});`
      );
      return host.current();
    }
    const result = (await client.figureAction(handle, request)) as {
      displayList: FigureDisplayListResult;
    };
    return host.accept(result.displayList);
  }
  if (host.busy) {
    const request = {
      action: 'view',
      axesHandle,
      azimuth,
      elevation,
      compactView,
      softwareFallback: params.softwareFallback !== false,
    };
    client.queueCommand(
      `__web_figure_deferred_action__(${handle}, ${host.text(JSON.stringify(request))});`
    );
    return host.accept({ ...host.current(), view: [azimuth, elevation] });
  }
  return host.accept(
    (await client.setFigureView(
      handle,
      azimuth,
      elevation,
      compactView,
      axesHandle
    )) as FigureDisplayListResult
  );
}
//=============================================================================
