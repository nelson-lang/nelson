//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { Point, Block as GeoBlock } from '../geometry.js';
//=============================================================================
export type Port = { type: 'in' | 'out'; index: number; x: number; y: number };
export type Block = GeoBlock & {
  id?: string;
  type?: string;
  ports?: Port[];
  group?: Element;
};
//=============================================================================
export type Conn = {
  from: string;
  to: string;
  fromIndex?: number;
  toIndex?: number;
  points?: Point[];
  path: Element;
  hitPath?: Element;
  routeFailed?: boolean;
  debugRenderPoints?: Point[];
  debugRenderSegments?: number;
  debugSegmentPreview?: string[];
  debugPath?: string;
  debugIssues?: string[];
};
//=============================================================================
export type RendererState = {
  blocks: Map<string, Block>;
  connections: Conn[];
  fastRouting?: boolean;
  autoRoute?: boolean | null;
  debugOverlapCount?: number;
  nextId?: number;
  dirtyBlocks?: Set<unknown>;
  dirtyConnections?: Set<unknown>;
  selectedIds?: Set<string>;
  selectedConnections?: Set<unknown>;
  pendingPort?: unknown;
};
//=============================================================================
export type RendererCtx = {
  svg: SVGSVGElement | null;
  blockLayer?: Element | null;
  wireLayer?: Element | null;
  overlayLayer?: Element | null;
  state: RendererState;
  isValidStub?: (portPos: Point, next: Point | undefined, side: string) => boolean;
  simplifyOrthogonalPath?: (p: Point[]) => Point[];
  dedupePoints?: (p: Point[]) => Point[];
  tryShorterOrthogonalPath?: (conn: Conn, p: Point[]) => Point[];
  applyWireOffsets?: (conn: Conn, p: Point[]) => Point[];
  buildFallbackPathFromPorts?: (conn: Conn) => Point[];
  buildDragRenderPoints?: (conn: Conn, p: Point[]) => Point[];
  renderSharedSourceJunctionDots?: () => void;
  checkWireIssues?: (conn: Conn, log?: Element | null) => boolean;
  debugLog?: Element | null;
  selectionRect?: Element | null;
  marqueeRect?: Element | null;
  wireJunctionLayer?: Element | null;
  wireCornerLayer?: Element | null;
};
//=============================================================================
