//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { FigureDisplayListResult } from './types';
//=============================================================================
export interface FigurePlotObject {
  handle: number;
  type: string;
  visible: string;
  DisplayName?: string;
  LineWidth?: number;
  Color?: number[] | string;
  FaceColor?: number[] | string;
  EdgeColor?: number[] | string;
  LineStyle?: string;
  Marker?: string;
  MarkerSize?: number;
  MarkerFaceColor?: number[] | string;
  MarkerEdgeColor?: number[] | string;
}
//=============================================================================
export interface FigureAxesState {
  handle: number;
  title: string;
  objects: FigurePlotObject[];
  projectionFloor: boolean;
  View: [number, number];
  XLim: number[];
  YLim: number[];
  ZLim: number[];
  XLimMode: string;
  YLimMode: string;
  ZLimMode: string;
  XGrid: string;
  YGrid: string;
  ZGrid: string;
  XScale: string;
  YScale: string;
  ZScale: string;
  CameraPosition: number[];
  CameraTarget: number[];
  CameraUpVector: number[];
  CameraViewAngle: number;
  CameraPositionMode: string;
  CameraTargetMode: string;
  CameraUpVectorMode: string;
  CameraViewAngleMode: string;
  Projection: string;
}
//=============================================================================
export interface FigureInspectorState {
  handle: number;
  name: string;
  numberTitle: string;
  color: number[];
  axes: FigureAxesState[];
  canUndo?: boolean;
  canRedo?: boolean;
  inspector?: { target: number; close: boolean };
  inspectorValues?: Record<string, string>;
}
//=============================================================================
export type InspectorKind = 'enum' | 'color' | 'number' | 'vector' | 'bool' | 'text' | 'info';
export interface InspectorProperty {
  name: string;
  kind: InspectorKind;
  writable: boolean;
  display: string;
  value: unknown;
  choices: string[];
}
//=============================================================================
export interface InspectorTreeNode {
  handle: number;
  type: string;
  label: string;
  children: InspectorTreeNode[] | InspectorTreeNode;
}
//=============================================================================
export interface InspectorModel {
  handle: number;
  type: string;
  label: string;
  properties: InspectorProperty[] | InspectorProperty;
}
//=============================================================================
export interface InspectorPayload {
  model: InspectorModel;
  tree: InspectorTreeNode;
}
//=============================================================================
export interface FigureDataPoint {
  found: boolean;
  objectHandle?: number;
  axesHandle?: number;
  index?: number;
  count?: number;
  position?: [number, number, number];
  pixel?: [number, number];
}
//=============================================================================
export interface FigureActionResult {
  queued?: boolean;
  state?: FigureInspectorState;
  displayList?: FigureDisplayListResult;
  point?: FigureDataPoint;
  model?: InspectorModel;
  tree?: InspectorTreeNode;
}
//=============================================================================
// jsonencode emits a single-element cell/array as the bare object, so inspector
// lists arrive as either an array or a lone object; normalise to an array.
export function inspectorList<T>(value: T[] | T | undefined): T[] {
  if (value === undefined || value === null) return [];
  return Array.isArray(value) ? value : [value];
}
//=============================================================================
export type FigureTool = 'inspect' | 'zoom' | 'pan' | 'rotate';
export function figureStateArrays(state: FigureInspectorState): FigureInspectorState {
  const list = <T>(value: T | T[] | undefined): T[] =>
    value === undefined ? [] : Array.isArray(value) ? value : [value];
  return { ...state, axes: list(state.axes).map((ax) => ({ ...ax, objects: list(ax.objects) })) };
}
//=============================================================================
