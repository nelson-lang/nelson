//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
export * from './geometry.js';
export * from './svg-elements.js';
//=============================================================================
function mk<T>(v: T): T {
  return v;
}
//=============================================================================
const _r_g1 = {
  FORCE_FULL_ROUTE_TIME_LIMIT_MS: mk(4000),
  DEBUG_WIRE_CHECKS: mk(false),
  SELECTION_PAD: mk(10),
  HOP_RADIUS: mk(4),
  WIRE_CORNER_HANDLE_RADIUS: mk(6),
  USERFUNC_MIN_WIDTH: mk(80),
  USERFUNC_FIXED_HEIGHT: mk(80),
  USERFUNC_PADDING_X: mk(12),
  USERFUNC_PADDING_Y: mk(16),
  USERFUNC_SETTLE_RETRIES: mk(4),
  USERFUNC_SETTLE_DELAY_MS: mk(60),
  TF_MIN_WIDTH: mk(80),
} as const;
//=============================================================================
export const FORCE_FULL_ROUTE_TIME_LIMIT_MS = _r_g1.FORCE_FULL_ROUTE_TIME_LIMIT_MS;
export const DEBUG_WIRE_CHECKS = _r_g1.DEBUG_WIRE_CHECKS;
export const SELECTION_PAD = _r_g1.SELECTION_PAD;
export const HOP_RADIUS = _r_g1.HOP_RADIUS;
export const WIRE_CORNER_HANDLE_RADIUS = _r_g1.WIRE_CORNER_HANDLE_RADIUS;
export const USERFUNC_MIN_WIDTH = _r_g1.USERFUNC_MIN_WIDTH;
export const USERFUNC_FIXED_HEIGHT = _r_g1.USERFUNC_FIXED_HEIGHT;
export const USERFUNC_PADDING_X = _r_g1.USERFUNC_PADDING_X;
export const USERFUNC_PADDING_Y = _r_g1.USERFUNC_PADDING_Y;
export const USERFUNC_SETTLE_RETRIES = _r_g1.USERFUNC_SETTLE_RETRIES;
export const USERFUNC_SETTLE_DELAY_MS = _r_g1.USERFUNC_SETTLE_DELAY_MS;
export const TF_MIN_WIDTH = _r_g1.TF_MIN_WIDTH;
const _r_g2 = {
  DTF_MIN_WIDTH: mk(80),
  DIRTY_ROUTE_SCORE_WORSE_MARGIN: mk(8),
  DIRTY_ROUTE_SCORE_WORSE_RATIO: mk(1.2),
  HOP_SIZE: mk(5),
  HOP_SNAP: mk(2),
  MAX_HOP_SEGMENTS: mk(100),
  WIRE_NEAR_GAP: mk(8),
  MIN_HOP_LENGTH: mk(10),
  ROUTE_WORKER_TIMEOUT: mk(5000),
  HANDLE_RADIUS: mk(4),
  CORNER_SNAP: mk(1),
  WIRES_OVERLAP_GAP: mk(1),
} as const;
//=============================================================================
export const DTF_MIN_WIDTH = _r_g2.DTF_MIN_WIDTH;
export const DIRTY_ROUTE_SCORE_WORSE_MARGIN = _r_g2.DIRTY_ROUTE_SCORE_WORSE_MARGIN;
export const DIRTY_ROUTE_SCORE_WORSE_RATIO = _r_g2.DIRTY_ROUTE_SCORE_WORSE_RATIO;
export const HOP_SIZE = _r_g2.HOP_SIZE;
export const HOP_SNAP = _r_g2.HOP_SNAP;
export const MAX_HOP_SEGMENTS = _r_g2.MAX_HOP_SEGMENTS;
export const WIRE_NEAR_GAP = _r_g2.WIRE_NEAR_GAP;
export const MIN_HOP_LENGTH = _r_g2.MIN_HOP_LENGTH;
export const ROUTE_WORKER_TIMEOUT = _r_g2.ROUTE_WORKER_TIMEOUT;
export const HANDLE_RADIUS = _r_g2.HANDLE_RADIUS;
export const CORNER_SNAP = _r_g2.CORNER_SNAP;
export const WIRES_OVERLAP_GAP = _r_g2.WIRES_OVERLAP_GAP;
const _r_g3 = {
  BLOCK_MARGIN: mk(5),
  SPLIT_GAP: mk(10),
  SPLIT_SPACING: mk(3),
  MIN_BEND_LENGTH: mk(5),
  LABEL_WIRE_GAP: mk(8),
  LABEL_PAD: mk(5),
  MIN_LABEL_DISTANCE: mk(20),
  INITIAL_VIEWBOX_SIZE: mk(1000),
  INITIAL_ZOOM: mk(1),
  MAX_ZOOM: mk(5),
  MIN_ZOOM: mk(0.1),
  GRID_SIZE: mk(20),
} as const;
//=============================================================================
export const BLOCK_MARGIN = _r_g3.BLOCK_MARGIN;
export const SPLIT_GAP = _r_g3.SPLIT_GAP;
export const SPLIT_SPACING = _r_g3.SPLIT_SPACING;
export const MIN_BEND_LENGTH = _r_g3.MIN_BEND_LENGTH;
export const LABEL_WIRE_GAP = _r_g3.LABEL_WIRE_GAP;
export const LABEL_PAD = _r_g3.LABEL_PAD;
export const MIN_LABEL_DISTANCE = _r_g3.MIN_LABEL_DISTANCE;
export const INITIAL_VIEWBOX_SIZE = _r_g3.INITIAL_VIEWBOX_SIZE;
export const INITIAL_ZOOM = _r_g3.INITIAL_ZOOM;
export const MAX_ZOOM = _r_g3.MAX_ZOOM;
export const MIN_ZOOM = _r_g3.MIN_ZOOM;
export const GRID_SIZE = _r_g3.GRID_SIZE;
const _r_g4 = {
  WIREFRAME_STROKE_WIDTH: mk(1),
  WIREFRAME_DASH_ARRAY: mk('4,4'),
  SELECTION_STROKE_WIDTH: mk(1),
  SELECTION_DASH_ARRAY: mk('4,4'),
  HIGHLIGHT_STROKE_WIDTH: mk(2),
  HIGHLIGHT_COLOR: mk('#0078d4'),
  ERROR_COLOR: mk('#ff0000'),
  WARNING_COLOR: mk('#ff8c00'),
  INFO_COLOR: mk('#00bfff'),
  SUCCESS_COLOR: mk('#00ff00'),
  DEFAULT_BLOCK_WIDTH: mk(80),
  DEFAULT_BLOCK_HEIGHT: mk(40),
} as const;
//=============================================================================
export const WIREFRAME_STROKE_WIDTH = _r_g4.WIREFRAME_STROKE_WIDTH;
export const WIREFRAME_DASH_ARRAY = _r_g4.WIREFRAME_DASH_ARRAY;
export const SELECTION_STROKE_WIDTH = _r_g4.SELECTION_STROKE_WIDTH;
export const SELECTION_DASH_ARRAY = _r_g4.SELECTION_DASH_ARRAY;
export const HIGHLIGHT_STROKE_WIDTH = _r_g4.HIGHLIGHT_STROKE_WIDTH;
export const HIGHLIGHT_COLOR = _r_g4.HIGHLIGHT_COLOR;
export const ERROR_COLOR = _r_g4.ERROR_COLOR;
export const WARNING_COLOR = _r_g4.WARNING_COLOR;
export const INFO_COLOR = _r_g4.INFO_COLOR;
export const SUCCESS_COLOR = _r_g4.SUCCESS_COLOR;
export const DEFAULT_BLOCK_WIDTH = _r_g4.DEFAULT_BLOCK_WIDTH;
export const DEFAULT_BLOCK_HEIGHT = _r_g4.DEFAULT_BLOCK_HEIGHT;
const _r_g5 = {
  MIN_BLOCK_WIDTH: mk(40),
  MIN_BLOCK_HEIGHT: mk(20),
  PORT_RADIUS: mk(4),
  PORT_SPACING: mk(10),
  PORT_MARGIN: mk(5),
  WIRE_WIDTH: mk(1.5),
  WIRE_SELECTED_WIDTH: mk(2.5),
  WIRE_HOVER_WIDTH: mk(2),
  WIRE_COLOR: mk('#000000'),
  WIRE_SELECTED_COLOR: mk('#0078d4'),
  WIRE_HOVER_COLOR: mk('#00bfff'),
  GRID_COLOR: mk('#cccccc'),
} as const;
//=============================================================================
export const MIN_BLOCK_WIDTH = _r_g5.MIN_BLOCK_WIDTH;
export const MIN_BLOCK_HEIGHT = _r_g5.MIN_BLOCK_HEIGHT;
export const PORT_RADIUS = _r_g5.PORT_RADIUS;
export const PORT_SPACING = _r_g5.PORT_SPACING;
export const PORT_MARGIN = _r_g5.PORT_MARGIN;
export const WIRE_WIDTH = _r_g5.WIRE_WIDTH;
export const WIRE_SELECTED_WIDTH = _r_g5.WIRE_SELECTED_WIDTH;
export const WIRE_HOVER_WIDTH = _r_g5.WIRE_HOVER_WIDTH;
export const WIRE_COLOR = _r_g5.WIRE_COLOR;
export const WIRE_SELECTED_COLOR = _r_g5.WIRE_SELECTED_COLOR;
export const WIRE_HOVER_COLOR = _r_g5.WIRE_HOVER_COLOR;
export const GRID_COLOR = _r_g5.GRID_COLOR;
const _r_g6 = {
  GRID_MAJOR_COLOR: mk('#999999'),
  GRID_MAJOR_SIZE: mk(100),
  BACKGROUND_COLOR: mk('#ffffff'),
  CANVAS_BACKGROUND_COLOR: mk('#f8f8f8'),
  STATUS_BAR_HEIGHT: mk(24),
  STATUS_BAR_COLOR: mk('#f0f0f0'),
  STATUS_BAR_TEXT_COLOR: mk('#333333'),
  MODAL_BACKGROUND_COLOR: mk('rgba(0, 0, 0, 0.5)'),
  MODAL_CONTENT_COLOR: mk('#ffffff'),
  MODAL_BORDER_COLOR: mk('#cccccc'),
  MODAL_SHADOW: mk('0 4px 12px rgba(0, 0, 0, 0.15)'),
  TOOLTIP_BACKGROUND_COLOR: mk('#ffffff'),
} as const;
//=============================================================================
export const GRID_MAJOR_COLOR = _r_g6.GRID_MAJOR_COLOR;
export const GRID_MAJOR_SIZE = _r_g6.GRID_MAJOR_SIZE;
export const BACKGROUND_COLOR = _r_g6.BACKGROUND_COLOR;
export const CANVAS_BACKGROUND_COLOR = _r_g6.CANVAS_BACKGROUND_COLOR;
export const STATUS_BAR_HEIGHT = _r_g6.STATUS_BAR_HEIGHT;
export const STATUS_BAR_COLOR = _r_g6.STATUS_BAR_COLOR;
export const STATUS_BAR_TEXT_COLOR = _r_g6.STATUS_BAR_TEXT_COLOR;
export const MODAL_BACKGROUND_COLOR = _r_g6.MODAL_BACKGROUND_COLOR;
export const MODAL_CONTENT_COLOR = _r_g6.MODAL_CONTENT_COLOR;
export const MODAL_BORDER_COLOR = _r_g6.MODAL_BORDER_COLOR;
export const MODAL_SHADOW = _r_g6.MODAL_SHADOW;
export const TOOLTIP_BACKGROUND_COLOR = _r_g6.TOOLTIP_BACKGROUND_COLOR;
const _r_g7 = {
  TOOLTIP_BORDER_COLOR: mk('#cccccc'),
  TOOLTIP_TEXT_COLOR: mk('#333333'),
  TOOLTIP_SHADOW: mk('0 2px 8px rgba(0, 0, 0, 0.1)'),
  CONTEXT_MENU_BACKGROUND_COLOR: mk('#ffffff'),
  CONTEXT_MENU_BORDER_COLOR: mk('#cccccc'),
  CONTEXT_MENU_TEXT_COLOR: mk('#333333'),
  CONTEXT_MENU_SHADOW: mk('0 2px 8px rgba(0, 0, 0, 0.1)'),
  SCROLLBAR_WIDTH: mk(12),
  SCROLLBAR_TRACK_COLOR: mk('#f0f0f0'),
  SCROLLBAR_THUMB_COLOR: mk('#cccccc'),
  SCROLLBAR_THUMB_HOVER_COLOR: mk('#999999'),
  ZOOM_CONTROLS_WIDTH: mk(32),
} as const;
//=============================================================================
export const TOOLTIP_BORDER_COLOR = _r_g7.TOOLTIP_BORDER_COLOR;
export const TOOLTIP_TEXT_COLOR = _r_g7.TOOLTIP_TEXT_COLOR;
export const TOOLTIP_SHADOW = _r_g7.TOOLTIP_SHADOW;
export const CONTEXT_MENU_BACKGROUND_COLOR = _r_g7.CONTEXT_MENU_BACKGROUND_COLOR;
export const CONTEXT_MENU_BORDER_COLOR = _r_g7.CONTEXT_MENU_BORDER_COLOR;
export const CONTEXT_MENU_TEXT_COLOR = _r_g7.CONTEXT_MENU_TEXT_COLOR;
export const CONTEXT_MENU_SHADOW = _r_g7.CONTEXT_MENU_SHADOW;
export const SCROLLBAR_WIDTH = _r_g7.SCROLLBAR_WIDTH;
export const SCROLLBAR_TRACK_COLOR = _r_g7.SCROLLBAR_TRACK_COLOR;
export const SCROLLBAR_THUMB_COLOR = _r_g7.SCROLLBAR_THUMB_COLOR;
export const SCROLLBAR_THUMB_HOVER_COLOR = _r_g7.SCROLLBAR_THUMB_HOVER_COLOR;
export const ZOOM_CONTROLS_WIDTH = _r_g7.ZOOM_CONTROLS_WIDTH;
const _r_g8 = {
  ZOOM_CONTROLS_HEIGHT: mk(128),
  ZOOM_CONTROLS_BACKGROUND_COLOR: mk('#ffffff'),
  ZOOM_CONTROLS_BORDER_COLOR: mk('#cccccc'),
  ZOOM_CONTROLS_SHADOW: mk('0 2px 8px rgba(0, 0, 0, 0.1)'),
  DEBUG_PANEL_WIDTH: mk(300),
  DEBUG_PANEL_HEIGHT: mk(400),
  DEBUG_PANEL_BACKGROUND_COLOR: mk('#ffffff'),
  DEBUG_PANEL_BORDER_COLOR: mk('#cccccc'),
  DEBUG_PANEL_TEXT_COLOR: mk('#333333'),
  DEBUG_PANEL_SHADOW: mk('0 2px 8px rgba(0, 0, 0, 0.1)'),
} as const;
//=============================================================================
export const ZOOM_CONTROLS_HEIGHT = _r_g8.ZOOM_CONTROLS_HEIGHT;
export const ZOOM_CONTROLS_BACKGROUND_COLOR = _r_g8.ZOOM_CONTROLS_BACKGROUND_COLOR;
export const ZOOM_CONTROLS_BORDER_COLOR = _r_g8.ZOOM_CONTROLS_BORDER_COLOR;
export const ZOOM_CONTROLS_SHADOW = _r_g8.ZOOM_CONTROLS_SHADOW;
export const DEBUG_PANEL_WIDTH = _r_g8.DEBUG_PANEL_WIDTH;
export const DEBUG_PANEL_HEIGHT = _r_g8.DEBUG_PANEL_HEIGHT;
export const DEBUG_PANEL_BACKGROUND_COLOR = _r_g8.DEBUG_PANEL_BACKGROUND_COLOR;
export const DEBUG_PANEL_BORDER_COLOR = _r_g8.DEBUG_PANEL_BORDER_COLOR;
export const DEBUG_PANEL_TEXT_COLOR = _r_g8.DEBUG_PANEL_TEXT_COLOR;
export const DEBUG_PANEL_SHADOW = _r_g8.DEBUG_PANEL_SHADOW;
export type RendererOptions = {
  svg?: SVGSVGElement | null;
  blockLayer?: Element | null;
  wireLayer?: Element | null;
  overlayLayer?: Element | null;
  gridSize?: number;
  blockMargin?: number;
  portRadius?: number;
};
//=============================================================================
export function createRenderer(options: RendererOptions = {}) {
  const {
    svg,
    blockLayer,
    wireLayer,
    overlayLayer,
    gridSize = GRID_SIZE,
    blockMargin = BLOCK_MARGIN,
    portRadius = PORT_RADIUS,
  } = options;

  return {
    svg,
    blockLayer,
    wireLayer,
    overlayLayer,
    gridSize,
    blockMargin,
    portRadius,
  };
}
//=============================================================================
