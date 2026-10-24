//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { CSSProperties } from 'react';
//=============================================================================
const iconUrls = {
  editor: new URL('../../../../../resources/action-editor.svg', import.meta.url).href,
  examples: new URL('../../../../../resources/action-examples.svg', import.meta.url).href,
  nflow: new URL('../../../../../resources/action-nflow.svg', import.meta.url).href,
  package: new URL('../../../../../resources/action-package.svg', import.meta.url).href,
  execute: new URL('../../../../../resources/action-run.svg', import.meta.url).href,
  stop: new URL('../../../../../resources/action-stop.svg', import.meta.url).href,
  command: new URL('../../../../../resources/panel-command.svg', import.meta.url).href,
  files: new URL('../../../../../resources/panel-files.svg', import.meta.url).href,
  workspace: new URL('../../../../../resources/panel-workspace.svg', import.meta.url).href,
  history: new URL('../../../../../resources/panel-history.svg', import.meta.url).href,
  variables: new URL('../../../../../resources/panel-variables.svg', import.meta.url).href,
  profiler: new URL('../../../../../resources/panel-profiler.svg', import.meta.url).href,
  newsfeed: new URL('../../../../../resources/panel-newsfeed.svg', import.meta.url).href,
  terminal: new URL('../../../../../resources/panel-terminal.svg', import.meta.url).href,
  folder: new URL('../../../../../resources/action-folder.svg', import.meta.url).href,
  help: new URL('../../../../../resources/action-help.svg', import.meta.url).href,
  load: new URL('../../../../../resources/action-load.svg', import.meta.url).href,
  save: new URL('../../../../../resources/action-save.svg', import.meta.url).href,
  layout: new URL('../../../../../resources/action-layout.svg', import.meta.url).href,
  more: new URL('../../../../../resources/action-more.svg', import.meta.url).href,
  collapse: new URL('../../../../../resources/action-collapse.svg', import.meta.url).href,
  expand: new URL('../../../../../resources/action-expand.svg', import.meta.url).href,
  up: new URL('../../../../../resources/action-up.svg', import.meta.url).href,
  upload: new URL('../../../../../resources/action-upload.svg', import.meta.url).href,
  file: new URL('../../../../../resources/action-file.svg', import.meta.url).href,
  maximize: new URL('../../../../../resources/action-maximize.svg', import.meta.url).href,
  restore: new URL('../../../../../resources/action-restore.svg', import.meta.url).href,
  close: new URL('../../../../../resources/action-close.svg', import.meta.url).href,
  check: new URL('../../../../../resources/action-check.svg', import.meta.url).href,
  search: new URL('../../../../../resources/action-search.svg', import.meta.url).href,
  warning: new URL('../../../../../resources/action-warning.svg', import.meta.url).href,
  question: new URL('../../../../../resources/action-question.svg', import.meta.url).href,
  info: new URL('../../../../../resources/action-info.svg', import.meta.url).href,
  error: new URL('../../../../../resources/action-error.svg', import.meta.url).href,
  plot: new URL('../../../../../resources/action-plot.svg', import.meta.url).href,
  print: new URL('../../../../../resources/action-print.svg', import.meta.url).href,
  refresh: new URL('../../../../../resources/action-refresh.svg', import.meta.url).href,
  pan: new URL('../../../../../resources/action-pan.svg', import.meta.url).href,
  crosshair: new URL('../../../../../resources/crosshair.svg', import.meta.url).href,
  rotate: new URL('../../../../../resources/rotate.svg', import.meta.url).href,
  zoom: new URL('../../../../../resources/zoom-in.svg', import.meta.url).href,
  zoomfit: new URL('../../../../../resources/view-zoom-0.svg', import.meta.url).href,
  back: new URL('../../../../../resources/action-back.svg', import.meta.url).href,
  right: new URL('../../../../../resources/action-right.svg', import.meta.url).href,
  home: new URL('../../../../../resources/action-home.svg', import.meta.url).href,
  copy: new URL('../../../../../resources/action-copy.svg', import.meta.url).href,
  paste: new URL('../../../../../resources/action-paste.svg', import.meta.url).href,
  add: new URL('../../../../../resources/action-add.svg', import.meta.url).href,
  undo: new URL('../../../../../resources/action-undo.svg', import.meta.url).href,
  redo: new URL('../../../../../resources/action-redo.svg', import.meta.url).href,
  down: new URL('../../../../../resources/action-down.svg', import.meta.url).href,
  dock: new URL('../../../../../resources/dock-window.svg', import.meta.url).href,
  undock: new URL('../../../../../resources/undock-window.svg', import.meta.url).href,
} as const;
//=============================================================================
export type DesktopIconName = keyof typeof iconUrls;
export function DesktopIcon({
  name,
  size = 20,
}: {
  name: DesktopIconName;
  size?: number;
}): JSX.Element {
  const style = {
    '--desktop-icon-url': `url("${iconUrls[name]}")`,
    width: `${size}px`,
    height: `${size}px`,
  } as CSSProperties;
  return <span className="desktop-icon" style={style} aria-hidden="true" />;
}
//=============================================================================
export function desktopIconUrl(name: DesktopIconName): string {
  return iconUrls[name];
}
//=============================================================================
