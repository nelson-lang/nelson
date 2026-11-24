//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { FigureActionResult, FigureInspectorState, FigureAxesState } from './figureTypes';
import type { FigureDisplayListResult } from './types';
import { displayListToSvg } from '../panels/figureSvg';
//=============================================================================
const states = new WeakMap<object, FigureInspectorState>();
const references = new WeakMap<object, Partial<FigureAxesState>>();
type MockFigure = { handle: number; title: string; kind: string };
function mockFigureState(figure: MockFigure, displayList: FigureDisplayListResult) {
  let state = states.get(figure);
  if (!state) {
    const numbered = `Figure ${figure.handle + 1}`;
    const named = figure.title.startsWith(numbered + ': ');
    state = {
      handle: figure.handle,
      name:
        figure.title === numbered
          ? ''
          : named
          ? figure.title.slice(numbered.length + 2)
          : figure.title,
      numberTitle: figure.title === numbered || named ? 'on' : 'off',
      color: [245 / 255, 245 / 255, 245 / 255],
      axes: [
        {
          handle: displayList.rotatableAxes?.[0]?.handle ?? 10000 + figure.handle,
          title: '',
          View: displayList.view ?? [0, 90],
          XLim: [0, 10],
          YLim: [0, 10],
          ZLim: [0, 1],
          XLimMode: 'auto',
          YLimMode: 'auto',
          ZLimMode: 'auto',
          XGrid: 'off',
          YGrid: 'off',
          ZGrid: 'off',
          XScale: 'linear',
          YScale: 'linear',
          ZScale: 'linear',
          CameraPosition: [0, -30, 20],
          CameraTarget: [0, 0, 0],
          CameraUpVector: [0, 0, 1],
          CameraViewAngle: 10,
          CameraPositionMode: 'auto',
          CameraTargetMode: 'auto',
          CameraUpVectorMode: 'auto',
          CameraViewAngleMode: 'auto',
          Projection: 'orthographic',
          projectionFloor: false,
          objects: [
            {
              handle: 20000 + figure.handle,
              type: figure.kind === 'surf' ? 'surface' : 'line',
              visible: 'on',
              DisplayName: '',
              LineWidth: 1,
              Color: [0, 0.447, 0.741],
            },
          ],
        },
      ],
    };
    states.set(figure, state);
  }
  return state;
}
//=============================================================================
export function mockFigureAction(
  figure: MockFigure,
  request: Record<string, unknown>,
  displayList: FigureDisplayListResult
): FigureActionResult | { bytes: number[] } {
  const state = mockFigureState(figure, displayList);
  const ax = state.axes[0];
  if (request.action === 'state' && displayList.view) ax.View = displayList.view;
  const axisEdit = request.action === 'properties' && !request.target && !request.objectHandle;
  if (
    request.action === 'reference' ||
    (!references.has(ax) &&
      (axisEdit ||
        ['capture', 'view', 'zoom', 'pan', 'fit', 'restore'].includes(String(request.action))))
  ) {
    references.set(
      ax,
      structuredClone(
        Object.fromEntries(
          Object.entries(ax).filter(
            ([key]) =>
              !['handle', 'title', 'objects', 'projectionFloor'].includes(key) &&
              !key.endsWith('Grid')
          )
        )
      )
    );
  }
  switch (request.action) {
    case 'restore':
      Object.assign(ax, structuredClone(references.get(ax)));
      break;
    case 'properties': {
      const props = request.properties as Record<string, unknown>;
      if (request.target === 'figure') {
        if (props.Name !== undefined) state.name = String(props.Name);
        if (props.NumberTitle !== undefined) state.numberTitle = String(props.NumberTitle);
        if (props.Color) state.color = props.Color as number[];
      } else if (request.target === 'title') ax.title = String(props.String);
      else if (request.objectHandle) {
        Object.assign(ax.objects[0], props);
        if (props.Visible) ax.objects[0].visible = String(props.Visible);
      } else Object.assign(ax, props);
      break;
    }
    case 'view':
      ax.View = [Number(request.azimuth), Number(request.elevation)];
      break;
    case 'zoom':
      ax.CameraViewAngle /= Number(request.factor);
      break;
    case 'pan': {
      (window as unknown as { __mockLastPan?: unknown }).__mockLastPan = request;
      const dx = Number(request.dxFrac) * (ax.XLim[1] - ax.XLim[0]);
      const dy = Number(request.dyFrac) * (ax.YLim[1] - ax.YLim[0]);
      ax.XLim = ax.XLim.map((v) => v - dx);
      ax.YLim = ax.YLim.map((v) => v + dy);
      break;
    }
    case 'floor':
      ax.projectionFloor = !!request.enabled;
      break;
    case 'fit':
      ax.XLim = [0, 10];
      ax.YLim = [0, 10];
      ax.CameraViewAngle = 10;
      break;
    case 'pick':
      return {
        state,
        point: {
          found: true,
          axesHandle: ax.handle,
          objectHandle: ax.objects[0].handle,
          index: 1,
          position: [0, 0, 0],
          pixel: [Number(request.x), Number(request.y)],
        },
      };
    case 'export': {
      if (request.format === 'svg')
        return {
          bytes: [
            ...new TextEncoder().encode(
              displayListToSvg(
                displayList.primitives,
                displayList.width,
                displayList.height,
                displayList.controls ?? []
              )
            ),
          ],
        };
      if (request.format === 'pdf')
        return { bytes: [...new TextEncoder().encode('%PDF-1.4\n%%EOF')] };
      return {
        bytes: [
          ...Uint8Array.from(
            atob(
              'iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVQIHWP4z8DwHwAFgAI/ScLbtAAAAABJRU5ErkJggg=='
            ),
            (c) => c.charCodeAt(0)
          ),
        ],
      };
    }
  }
  if (request.action === 'properties' && request.target === 'figure') {
    figure.title =
      state.numberTitle === 'on'
        ? `Figure ${figure.handle + 1}${state.name ? ': ' + state.name : ''}`
        : state.name;
  }
  return {
    state,
    ...(request.action === 'state' ? {} : { displayList: { ...displayList, view: ax.View } }),
  };
}
//=============================================================================
