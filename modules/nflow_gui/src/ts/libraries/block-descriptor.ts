//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
export type MathRenderDescriptor = {
  type: 'math';
  bodyClass?: string;
  /** CSS font-size of the math group: a number is pixels (e.g. 14), or a string like '14px' */
  textSize?: string | number;
  /** Variable name passed to buildTransferTeX for blocks with num/den params (e.g. "z" for discrete TF) */
  transferVariable?: string;
  useRectElement?: boolean;
  mathGroupClass?: string;
  formula: string;
  width?: number;
  height?: number;
  defaultParams?: Record<string, unknown> | null;
} & RenderColorOverrides;
//=============================================================================
export interface RenderColorOverrides {
  backgroundColor?: string;
  borderColor?: string;
  textColor?: string;
}
//=============================================================================
export type PlotRenderDescriptor = {
  type: 'plot';
  path: string;
} & RenderColorOverrides;
//=============================================================================
export type ImageRenderDescriptor = {
  type: 'image';
  src: string;
  svgMode?: 'element' | 'inline';
  bodyClass?: string;
  preserveAspectRatio?: string;
  x?: number;
  y?: number;
  width?: number;
  height?: number;
  defaultParams?: Record<string, unknown> | null;
  /** Resolved absolute URL (populated by the manifest loader when possible) */
  _src?: string;
} & RenderColorOverrides;
//=============================================================================
export type ToggleRenderDescriptor = {
  type: 'toggle';
  trackWidth?: number;
  trackHeight?: number;
  knobRadius?: number;
  bodyClass?: string;
  trackClass?: string;
  knobClass?: string;
  labelClass?: string;
  defaultParams?: Record<string, unknown> | null;
} & RenderColorOverrides;
//=============================================================================
export type RenderDescriptor =
  | MathRenderDescriptor
  | PlotRenderDescriptor
  | ImageRenderDescriptor
  | ToggleRenderDescriptor;
export interface ManifestBlock {
  type: string;
  label?: string;
  icon?: string;
  phases?: string[];
  width?: number;
  height?: number;
  inputs?: IOPoint[];
  outputs?: IOPoint[];
  defaultParams?: Record<string, unknown> | null;
  render?: RenderDescriptor;
  // Acausal (physical) blocks: undirected pins + domain (from the library
  // manifest). Absent for causal blocks.
  physicalPins?: Array<{ name: string; x?: number; y?: number; side?: string; domain?: string }>;
  acausal?: boolean;
  domain?: string;
}
//=============================================================================
export interface IOPoint {
  x?: number;
  y?: number;
  side?: 'left' | 'right' | 'top' | 'bottom';
  wireX?: number;
  wireY?: number;
  [key: string]: unknown;
}
//=============================================================================
export interface LibraryManifest {
  id: string;
  title?: string;
  version?: string;
  format?: string;
  blocks: ManifestBlock[];
}
//=============================================================================
