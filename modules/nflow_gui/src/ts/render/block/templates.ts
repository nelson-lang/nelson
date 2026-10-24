//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { GRID_SIZE } from '../geometry.js';
import { createSvgElement, renderTeXMath } from '../svg-elements.js';
import { svgRect, svgText, renderSourcePlot, renderCenteredAxesPlot } from './block-icons.js';
import { formatLabelTeX, renderLabelNode } from './label.js';
import { buildTransferTeX } from '../transfer-fn.js';
import { buildAllBlockTemplates, getDiscoveredManifests } from '../../app/library-loader.js';
import { buildRegistryFromManifest } from '../../libraries/generic-block-renderer.js';
import type { LibraryManifest } from '../../libraries/block-descriptor.js';
import type { BlockDefinition } from '../../app/block-registry.js';
//=============================================================================
const _templateHelpers: Record<string, unknown> = {
  GRID_SIZE,
  svgRect,
  svgText,
  createSvgElement,
  renderTeXMath,
  formatLabelTeX,
  renderSourcePlot,
  renderCenteredAxesPlot,
  buildTransferTeX,
  renderLabelNode,
};
//=============================================================================
export let blockTemplates: Record<string, unknown> = {};
export function refreshBlockTemplates(): void {
  const merged = (buildAllBlockTemplates(_templateHelpers) as Record<string, unknown>) || {};

  // Tests can mutate the loaded libraries, so also build templates from the
  // discovered manifests. This works when some compiled `blocks.js` are missing.
  try {
    const discovered = getDiscoveredManifests();
    for (const [, manifestRec] of discovered) {
      try {
        const manifest = manifestRec as unknown as LibraryManifest;
        const registry = buildRegistryFromManifest(manifest);
        const defs: Record<string, BlockDefinition> = registry.getDefinitions();

        type Template = {
          width: number;
          height: number;
          inputs: unknown[];
          outputs: unknown[];
          defaultParams?: Record<string, unknown> | null;
          render: (block: unknown) => unknown;
          resize?: (...args: unknown[]) => unknown;
          physicalPins?: unknown[];
          acausal?: boolean;
          domain?: string;
        };

        for (const [type, def] of Object.entries(defs || {}) as [string, BlockDefinition][]) {
          if (!type) continue;
          if (Object.prototype.hasOwnProperty.call(merged, type)) continue;
          const width = def.width ?? 80;
          const height = def.height ?? 60;
          const inputs = def.inputs ?? [];
          const outputs = def.outputs ?? [{ x: width, y: height / 2, side: 'right' }];
          const defaultParams = def.defaultParams ?? {};
          const renderFn = def.render ?? null;
          if (!renderFn) continue;
          const tmpl: Template = {
            width,
            height,
            inputs,
            outputs,
            defaultParams,
            render: (block: unknown) => renderFn(block, _templateHelpers),
          };
          if (def.resize) tmpl.resize = def.resize;
          // Acausal blocks: forward the physical pins so createBlock renders the
          // undirected 'phys' ports (and the load path can reconstruct wires).
          const dphys = (def as { physicalPins?: unknown[] }).physicalPins;
          if (Array.isArray(dphys) && dphys.length > 0) {
            tmpl.physicalPins = dphys;
            tmpl.acausal = (def as { acausal?: boolean }).acausal ?? true;
            const dd = (def as { domain?: string }).domain;
            if (typeof dd === 'string') tmpl.domain = dd;
          }
          (merged as Record<string, unknown>)[type] = tmpl;
          try {
            const ns = `nflow.${manifest.id}.${type}`;
            (merged as Record<string, unknown>)[ns] = tmpl;
          } catch {}
        }
      } catch {
        // ignore problems building from a single manifest
      }
    }
  } catch {
    // ignore discovery failures
  }

  blockTemplates = merged as Record<string, unknown>;
  // Keep window.blockTemplates in sync so other modules (e.g. pasteSelection)
  // can check template availability without importing from this module.
  try {
    (window as unknown as { blockTemplates?: Record<string, unknown> }).blockTemplates =
      blockTemplates;
  } catch (_e) {}
}
//=============================================================================
export {};
//=============================================================================
