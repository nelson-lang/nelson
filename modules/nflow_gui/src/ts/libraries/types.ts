//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
export type CreateSvgElement = (
  tagName: string,
  attrs?: Record<string, string | number | boolean | null>,
  content?: string | null
) => Element & { dataset?: Record<string, string> };
//=============================================================================
export type RenderTeXMath = (group: Element, tex: string, width: number, height: number) => void;
export type Block = {
  id?: string | number;
  group: Element & { appendChild: (child: Node) => void };
  width: number;
  height: number;
  params: Record<string, unknown>;
  dynamicInputs?: Array<{ y: number }>;
  dynamicOutputs?: Array<{ y: number }>;
};
//=============================================================================
export type ScopeAxes = {
  xAxis?: Element;
  yAxis?: Element;
  zAxis?: Element;
  xTicks?: Element[];
  yTicks?: Element[];
  zTicks?: Element[];
  xTickLabels?: Element[];
  yTickLabels?: Element[];
  zTickLabels?: Element[];
};
//=============================================================================
export type UiBlock = Block & {
  bodyRect?: Element;
  scopeTitle?: Element;
  scopePaths?: Element[];
  scopePlot?: Element;
  scopeAxes?: ScopeAxes;
  scopeClipRect?: Element;
  scopeInputHints?: Element[];
  commentBody?: Element;
  commentForeign?: Element;
  commentTextEl?: HTMLElement;
  commentTextFallback?: Element;
  displayValueEl?: Element;
  displayTitleEl?: Element;
  scopeGridLines?: { xGridLines: Element[]; yGridLines: Element[] };
  scopeAxisLabels?: { xLabel: Element; yLabel: Element; zLabel?: Element };
  scopeLegendItems?: Element[];
  scopeBlockTitle?: Element;
};
//=============================================================================
export type Helpers = {
  createSvgElement: CreateSvgElement;
  renderTeXMath?: RenderTeXMath;
  svgRect: (x: number, y: number, w: number, h: number, className?: string) => Element;
  renderSourcePlot: (group: Element, w: number, h: number, d: string | null) => void;
  renderCenteredAxesPlot: (group: Element, w: number, h: number, d: string | null) => void;
  svgText: (x: number, y: number, text: string, className?: string) => Element;
  formatLabelTeX?: (s: string) => string;
  renderLabelNode?: (block: Block, ...args: unknown[]) => Element | void;
  buildTransferTeX?: (...args: unknown[]) => string;
};
//=============================================================================
