//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Shared RPC payload types for the nelson-webview protocol.
// The same method names are implemented by the C++ backend (webui_bind 'rpc')
// and by the standalone mock engine.
export interface PingResult {
  pong: boolean;
  backend: string;
  version: string;
}
//=============================================================================
export interface AppCapabilitiesResult {
  nflow: boolean;
  // nmm_gui module configured: exposes the Window > Package Manager panel.
  packageManager?: boolean;
  // Installed packages with a newer registry version, as last computed by the
  // Package Manager window in this session (0 until then): shown as a badge.
  packageUpdates?: number;
  launchMode?: 'webview' | 'browser' | 'server' | 'none';
}
//=============================================================================
export interface DiagnosticsInfoResult {
  backend: {
    name: string;
    version?: string;
    launchMode?: 'webview' | 'browser' | 'server' | 'none';
  };
  runtime?: {
    name: string;
    profile?: string;
    buildId?: string;
  };
  delivery?: {
    files?: number;
    totalBytes?: number;
    startupBytes?: number;
    onDemandBytes?: number;
  };
  capabilities?: {
    modules: string[];
    capabilities: string[];
    features: Record<string, boolean>;
    limits: Record<string, number>;
    unavailable: string[];
  };
  persistence?: {
    commandHistory: boolean;
    preferencesPath?: string;
    workspacePath?: string;
  };
  browser?: {
    userAgent?: string;
    platform?: string;
    features: Record<string, boolean>;
    unsupported: string[];
    notes: string[];
  };
}
//=============================================================================
export interface ProgressDialogPush {
  id: number;
  visible: boolean;
  title: string;
  message: string;
  value: number;
  minimum: number;
  maximum: number;
  indeterminate: boolean;
}
//=============================================================================
export interface TermInputParams {
  line: string;
}
//=============================================================================
export interface CompletionResult {
  prefix: string;
  builtin: string[];
  macros: string[];
  variables: string[];
  files: string[];
}
//=============================================================================
export interface WorkspaceVarSummary {
  name: string;
  className: string;
  size: [number, number];
  bytes: number;
  value: string; // short preview
}
//=============================================================================
export interface WorkspaceListResult {
  scope: string;
  variables: WorkspaceVarSummary[];
}
//=============================================================================
export interface HistoryListResult {
  entries: string[];
}
//=============================================================================
export interface NewsfeedItem {
  title: string;
  link: string;
  date: string;
  summary: string;
  image: string;
}
//=============================================================================
export interface NewsfeedFeed {
  title?: string;
  link?: string;
  url?: string;
  items?: NewsfeedItem[];
}
//=============================================================================
export interface NewsfeedListResult {
  feed: NewsfeedFeed;
}
//=============================================================================
export interface FsEntry {
  name: string;
  path: string;
  isDir: boolean;
  size: number;
}
//=============================================================================
export interface FsListResult {
  cwd: string;
  entries: FsEntry[];
}
//=============================================================================
export interface FsCwdResult {
  cwd: string;
  separator: string;
  recentDirectories: string[];
}
//=============================================================================
export type FsDialogMode = 'open' | 'save' | 'dir';
export interface FsDialogStartResult {
  directory: string;
}
//=============================================================================
export type CellValue = number | string | boolean | null;
export interface VarOpenResult {
  name: string;
  className: string;
  rows: number;
  cols: number;
  columns: string[]; // column headers (for tables/structs), else 1..cols
  editable: boolean;
  kind: 'matrix' | 'char' | 'string' | 'cell' | 'struct' | 'table' | 'text';
  // For kind 'text' (sparse / N-D / function handle / classdef): the full
  // display() dump, shown read-only instead of a grid.
  text?: string;
}
//=============================================================================
export interface VarBlockParams {
  name: string;
  row0: number;
  col0: number;
  rows: number;
  cols: number;
}
//=============================================================================
export interface VarBlockResult {
  row0: number;
  col0: number;
  cells: CellValue[][];
}
//=============================================================================
export interface VarSetCellParams {
  name: string;
  row: number;
  col: number;
  value: CellValue;
}
//=============================================================================
// figure WindowStyle. 'normal'/'modal'/'alwaysontop' live in a
// separate native window; 'docked' is a panel embedded in the desktop.
export type FigureWindowStyle = 'normal' | 'docked' | 'modal' | 'alwaysontop';
export interface FigureSummary {
  handle: number;
  title: string;
  windowStyle?: FigureWindowStyle;
}
//=============================================================================
export interface FigureListResult {
  figures: FigureSummary[];
}
//=============================================================================
// client.role tells a freshly loaded page whether it is the desktop or a
// standalone figure window (and, if so, which figure it hosts).
export interface ClientRoleResult {
  role: 'desktop' | 'figure';
  handle?: number;
  // Launch mode of the backend. client.role resolves reliably at startup, unlike the
  // polled app.capabilities. Drives whether a new figure opens in its own native window.
  launchMode?: 'webview' | 'browser' | 'server' | 'none';
}
//=============================================================================
export interface FigureImageResult {
  handle: number;
  // data URL (PNG in the real backend, SVG data URL in the mock)
  image: string;
  width: number;
  height: number;
  // true when the engine kept this figure alive so it can be rendered inline
  // with the interactive figure panel (the image is then only a fallback).
  interactive?: boolean;
}
//=============================================================================
export interface FigureEventParams {
  handle: number;
  type: 'mouse' | 'wheel' | 'resize';
  dx?: number;
  dy?: number;
  delta?: number;
  width?: number;
  height?: number;
}
//=============================================================================
// Native rendering: the backend serializes the figure as draw primitives (RenderWeb)
// instead of a raster image; the frontend replays them on a canvas.
export type DrawPrimitive =
  | { kind: 'rect'; x: number; y: number; w: number; h: number; fill: string }
  | {
      kind: 'line';
      x1: number;
      y1: number;
      x2: number;
      y2: number;
      stroke: string;
      width: number;
      dash?: number[];
    }
  // polyline / polygon / segments carry their vertices as a base64 little-endian
  // float32 blob (x0,y0,x1,y1,...) with `count` points, decoded by decodePointPairs.
  // Vertex-heavy plots (contourf, high-resolution curves) are dominated by these,
  // so the binary form is far smaller and cheaper than a JSON [[x,y],...] array.
  | {
      kind: 'segments';
      count: number;
      pointsB64: string;
      stroke: string;
      width: number;
      dash?: number[];
    }
  | {
      kind: 'polyline';
      count: number;
      pointsB64: string;
      stroke: string;
      width: number;
      dash?: number[];
    }
  | {
      kind: 'polygon';
      count: number;
      pointsB64: string;
      fill: string;
      stroke?: string;
      width?: number;
    }
  | {
      kind: 'image';
      x: number;
      y: number;
      w: number;
      h: number;
      image?: string;
      imageId?: string;
      rgba?: string;
      rgbaAttachment?: string;
      rgbaBytes?: ArrayBuffer;
      bitmap?: ImageBitmap;
      imageWidth?: number;
      imageHeight?: number;
    }
  | { kind: 'clip'; x: number; y: number; w: number; h: number }
  | { kind: 'clipSave' }
  | { kind: 'clipRestore' }
  // Marker batch: N markers of one shape as a base64 binary blob (20 bytes per
  // point: x, y, radius as little-endian float32, then fill RGBA and outline RGBA
  // as four bytes each), so a several-thousand-point scatter ships as one compact
  // primitive instead of one polygon/stroke set per marker. `shape` is a
  // SymbolType; hasFace/hasEdge say which colour block is meaningful.
  | {
      kind: 'points';
      shape: number;
      hasFace: boolean;
      hasEdge: boolean;
      edgeWidth: number;
      count: number;
      data: string;
    }
  // Rectangle batch (bar/histogram): N axis-aligned rects as a base64 binary blob
  // (16 bytes per rect: x, y, w, h little-endian float32) sharing one fill and one
  // outline, instead of a polygon + polyline per bar.
  | { kind: 'rects'; count: number; data: string; fill?: string; stroke?: string; width?: number }
  | {
      kind: 'text';
      x: number;
      y: number;
      text: string;
      align: 'left' | 'center' | 'right';
      color: string;
      size: number;
      rotation?: number;
      font?: string;
      bold?: boolean;
      italic?: boolean;
    };
//=============================================================================
export type FigureWebGlFloatArray = number[] | Float32Array;
export type FigureWebGlIndexArray = number[] | Uint16Array | Uint32Array;
export type FigureWebGlIndexType = 'uint16' | 'uint32';
export interface FigureWebGlMesh {
  id: number;
  revision: string;
  positions?: FigureWebGlFloatArray;
  vertexColors?: FigureWebGlFloatArray;
  triangleIndices?: FigureWebGlIndexArray;
  lineIndices?: FigureWebGlIndexArray;
  positionsBase64?: string;
  vertexColorsBase64?: string;
  // Compact wire form: positions as normalized base64 float16 with a per-axis
  // bounding box to restore them, vertex colours as base64 uint8. Decoded back to
  // float32 for the GPU pipeline. See figureWebGlGeometry.
  positionsF16?: string;
  positionBBox?: number[]; // [minx, miny, minz, maxx, maxy, maxz]
  vertexColorsU8?: string;
  triangleIndicesBase64?: string;
  lineIndicesBase64?: string;
  triangleIndexType?: FigureWebGlIndexType;
  lineIndexType?: FigureWebGlIndexType;
  positionsLength?: number;
  vertexColorsLength?: number;
  triangleIndicesLength?: number;
  lineIndicesLength?: number;
  model: number[];
  projection: number[];
  viewport: [number, number, number, number];
  lineColor: [number, number, number, number];
  lineWidth: number;
  drawFaces: boolean;
}
//=============================================================================
// A uicontrol described by the engine. The desktop builds a real HTML control from
// it so it stays interactive.
export interface UIControl {
  handle: number;
  style:
    | 'pushbutton'
    | 'togglebutton'
    | 'checkbox'
    | 'radiobutton'
    | 'edit'
    | 'text'
    | 'slider'
    | 'popupmenu'
    | 'listbox'
    | 'frame'
    | string;
  text: string;
  items?: string[];
  tooltip?: string;
  background?: string;
  foreground?: string;
  units?: string;
  x: number;
  y: number;
  w: number;
  h: number;
  value: number;
  min: number;
  max: number;
  fontSize: number;
  enabled: boolean;
  // Enable === 'inactive': the control looks normal (enabled) but must not
  // react to user input. ButtonDownFcn still fires. Present only when true.
  inactive?: boolean;
  // Label placement of pushbutton / togglebutton (absent means centered).
  hAlign?: 'left' | 'center' | 'right' | string;
  vAlign?: 'top' | 'center' | 'bottom' | string;
  // Right-click context menu (uicontextmenu) attached to this control.
  contextMenu?: MenuNode[];
  // Rich payload for widgets that don't fit the flat descriptor (uitable,
  // uitree, uigauge/uiknob/uislider scale). Present only for those styles.
  data?: {
    columns?: string[];
    columnWidths?: Array<number | null>;
    rows?: string[][];
    editable?: boolean[];
    nodes?: TreeNode[];
    multiselect?: boolean;
    selected?: number[];
    // uigauge / uiknob / uislider scale, aligned on the Qt widgets.
    shape?: 'circular' | 'linear' | 'semicircular' | 'ninetydegree' | string;
    vertical?: boolean;
    discrete?: boolean;
    major?: number[];
    minor?: number[];
    labels?: string[];
    range?: number[];
    // uitabgroup header placement, matching the native TabLocation property.
    tabLocation?: 'top' | 'bottom' | 'left' | 'right' | string;
  };
}
//=============================================================================
export interface TreeNode {
  text: string;
  handle?: number;
  children?: TreeNode[];
}
//=============================================================================
export interface FigureDisplayListResult {
  handle: number;
  width: number;
  height: number;
  controlLayoutWidth?: number;
  controlLayoutHeight?: number;
  displaySignature?: string;
  layoutSignature?: string;
  primitives: DrawPrimitive[];
  webglMeshes?: FigureWebGlMesh[];
  softwareFallbackImage?: string;
  controls?: UIControl[];
  matrixOnly?: boolean;
  // Figure menu bar: the figure's uimenu subtrees.
  menus?: MenuNode[];
  // Figure-level context menu (right-click the canvas).
  contextMenu?: MenuNode[];
  // 3D axes: the surface is baked into `primitives` (Nelson's own projection).
  // `is3d` flags the figure as rotatable; `view` is the current [az, el] (deg).
  is3d?: boolean;
  view?: [number, number];
  // One entry per axes so a drag rotates only the subplot under the cursor.
  // `rect` is [left, top, width, height] in normalized top-origin figure
  // fractions; `view` is that axes' current [az, el] in degrees.
  rotatableAxes?: RotatableAxes[];
  // Set when the figure defines WindowScrollWheelFcn: the script owns the
  // wheel, so the panel leaves its built-in wheel zoom off.
  hasScrollWheelCallback?: boolean;
  // Set when the figure ToolBar is 'none': the panel hides its navigation
  // tools and details bar, as the native figure window does.
  toolbarHidden?: boolean;
}
//=============================================================================
export interface RotatableAxes {
  handle: number;
  rect: [number, number, number, number];
  view: [number, number];
  highlight?: boolean;
  // Set to false by disableDefaultInteractivity: no modeless drag-pan/rotate or wheel zoom.
  interactive?: boolean;
}
//=============================================================================
export interface MenuNode {
  handle: number;
  text: string;
  separator?: boolean;
  checked?: boolean;
  disabled?: boolean;
  hasCallback?: boolean;
  children?: MenuNode[];
}
//=============================================================================
export interface EditorOpenResult {
  path: string;
  content: string;
  language: string;
}
//=============================================================================
export interface DebuggerState {
  running: boolean;
  currentLine: number | null;
  currentFile: string | null;
  stack: { name: string; line: number }[];
}
//=============================================================================
export interface BreakpointsResult {
  file: string;
  lines: number[];
}
//=============================================================================
