//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Types of the shared examples gallery (example-gallery.js). The catalog
// schema is produced by the C++ side (ExamplesCatalog.hpp).
//=============================================================================
export type ExampleKind = 'script' | 'command' | 'model';
export type ExampleSource = 'nelson' | 'nflow' | 'external';

export interface ExampleEntry {
  title: string;
  description: string;
  kind: ExampleKind;
  file?: string;
  relativeFile?: string;
  command?: string;
  tags: string[];
  requirements: string[];
  blockCount?: number;
}

export interface ExampleGroup {
  name: string;
  title: string;
  source: ExampleSource;
  examples: ExampleEntry[];
}

export interface ExampleDiagnostic {
  module: string;
  message: string;
}

export interface ExampleCatalog {
  groups: ExampleGroup[];
  diagnostics: ExampleDiagnostic[];
  locale: string;
}

export interface ExampleGalleryOptions {
  /** Fetches the catalog; `refresh` asks the host to rescan the modules. */
  load: (options: { refresh: boolean }) => Promise<unknown>;
  /** Runs a script or a command entry. */
  onRun: (entry: ExampleEntry, group: ExampleGroup) => void | Promise<void>;
  /** Opens a model, or a script in the editor when `hasEditor` is set. */
  onOpen?: (entry: ExampleEntry, group: ExampleGroup) => void | Promise<void>;
  /** Source listed first in the rail; the others follow. */
  primarySource?: ExampleSource;
  /** Shows "Open in editor" on script entries. */
  hasEditor?: boolean;
  /** `auto` switches to the single-column layout below 640px. */
  compact?: boolean | 'auto';
  /** Locale used until the catalog reports its own. */
  locale?: string;
  /** Host translation hook; falls back to the built-in tables. */
  translate?: (key: string) => string;
  /** SVG markup for a model card, or null for the fallback glyph. */
  thumbnail?: (entry: ExampleEntry) => Promise<string | null> | string | null;
  /** Adds a close button; the host owns the surrounding dialog. */
  onClose?: () => void;
  footerAction?: { label: string; onClick: () => void };
}

export interface ExampleGalleryHandle {
  reload: (refresh?: boolean) => Promise<void>;
  focusSearch: () => void;
  destroy: () => void;
}

export function mountExampleGallery(
  root: HTMLElement,
  options: ExampleGalleryOptions
): ExampleGalleryHandle;
