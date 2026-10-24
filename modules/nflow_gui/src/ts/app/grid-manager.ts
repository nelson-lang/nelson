//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import { getPreferencesPath } from './host-utils.js';
import { GRID_SETTINGS_HIDE_EVENT, GRID_SETTINGS_SHOW_EVENT } from './components/modal-events.js';
import { canUseNFlowRpc, nflowRpc } from './native-rpc.js';
//=============================================================================
let gridVisible = false;
let gridSize = 5;
let canvas: HTMLElement | null = null;
let gridToggleBtn: HTMLElement | null = null;
let gridSettingsBtn: HTMLElement | null = null;
let gridSizeInput: HTMLInputElement | null = null;
let gridSizeSlider: HTMLInputElement | null = null;
let gridSettingsApplyBtn: HTMLElement | null = null;
export async function loadGridSettings(): Promise<void> {
  try {
    try {
      const root = await getPreferencesPath();
      if (root) {
        const sep = root.includes('\\') ? '\\' : '/';
        const path = root + sep + 'nflow' + sep + 'ui-settings.json';
        if (canUseNFlowRpc('nflow.file.readText')) {
          try {
            const respText = await nflowRpc<string>('nflow.file.readText', { path });
            let parsed: any = null;
            try {
              parsed = JSON.parse(String(respText || '{}'));
            } catch (_e) {
              // The raw text may itself be the config; tried below.
            }

            // Support two host return formats:
            // 1) { success: true, content: "{ ... }" }
            // 2) direct config JSON: { gridVisible: true, gridSize: 5 }
            let cfg: any = null;
            if (parsed && parsed.success === true && typeof parsed.content === 'string') {
              try {
                cfg = JSON.parse(parsed.content);
              } catch (_e) {
                cfg = null;
              }
            } else if (
              parsed &&
              (typeof parsed.gridVisible === 'boolean' || parsed.gridSize !== undefined)
            ) {
              cfg = parsed;
            } else {
              try {
                cfg = JSON.parse(String(respText));
              } catch (_e) {
                cfg = null;
              }
            }

            if (cfg) {
              if (typeof cfg.gridVisible === 'boolean') gridVisible = cfg.gridVisible;
              if (cfg && Number.isFinite(Number(cfg.gridSize))) gridSize = Number(cfg.gridSize);
              updateGridVisibility();
              updateGridSize();
              updateUI();
              return;
            }
          } catch (_e) {
            // ignore and fallthrough to fetch
          }
        }
      }
    } catch (_e) {
      // ignore and fallthrough to fetch
    }

    // Fallback: try loading from the UI root (served config)
    const url = 'nflow/ui-settings.json';
    try {
      const res = await fetch(url, { cache: 'no-store' });
      const cfg = await res.json();
      if (cfg && typeof cfg.gridVisible === 'boolean') gridVisible = cfg.gridVisible;
      if (cfg && Number.isFinite(Number(cfg.gridSize))) gridSize = Number(cfg.gridSize);
      updateGridVisibility();
      updateGridSize();
      updateUI();
      return;
    } catch (_e) {
      // ignore and fallthrough to apply defaults
    }
  } catch (_e) {
    // ignore
  }
  updateGridVisibility();
  updateGridSize();
  updateUI();
}
//=============================================================================
function saveGridSettings(): void {
  (async () => {
    try {
      const root = await getPreferencesPath();
      if (!root) return;
      const sep = root.includes('\\') ? '\\' : '/';
      const path = root + sep + 'nflow' + sep + 'ui-settings.json';
      const data = JSON.stringify({ gridVisible: gridVisible, gridSize: gridSize }, null, 2);
      await nflowRpc('nflow.file.writeText', { content: data, path });
    } catch (_e) {
      // ignore
    }
  })();
}
//=============================================================================
export function updateGridVisibility(): void {
  if (canvas) {
    if (gridVisible) {
      canvas.style.backgroundImage = `
        linear-gradient(to right, var(--canvas-grid-color) 1px, transparent 1px),
        linear-gradient(to bottom, var(--canvas-grid-color) 1px, transparent 1px)
      `;
    } else {
      canvas.style.backgroundImage = 'none';
    }
  }
}
//=============================================================================
export function updateGridSize(): void {
  if (canvas) {
    canvas.style.setProperty('--grid-size', `${gridSize}px`);
  }
}
//=============================================================================
function updateUI(): void {
  if (gridToggleBtn) {
    if (gridVisible) gridToggleBtn.classList.add('active');
    else gridToggleBtn.classList.remove('active');
  }

  if (gridSizeInput) gridSizeInput.value = String(gridSize);
  if (gridSizeSlider) gridSizeSlider.value = String(gridSize);
}
//=============================================================================
export function toggleGridVisibility(): void {
  gridVisible = !gridVisible;
  updateGridVisibility();
  updateUI();
  saveGridSettings();
}
//=============================================================================
export function setGridSize(size: string | number): boolean {
  const parsedSize = parseInt(String(size), 10);
  if (isNaN(parsedSize) || parsedSize < 5 || parsedSize > 100) {
    // eslint-disable-next-line no-alert
    alert('Grid size must be between 5 and 100 pixels.');
    return false;
  }
  gridSize = parsedSize;
  updateGridSize();
  if (!gridVisible) {
    gridVisible = true;
    updateGridVisibility();
  }
  updateUI();
  saveGridSettings();
  return true;
}
//=============================================================================
function showGridSettingsModal(): void {
  const event = new CustomEvent(GRID_SETTINGS_SHOW_EVENT, { cancelable: true });
  document.dispatchEvent(event);
}
//=============================================================================
function hideGridSettingsModal(): void {
  const event = new CustomEvent(GRID_SETTINGS_HIDE_EVENT, { cancelable: true });
  document.dispatchEvent(event);
}
//=============================================================================
export async function initGridManager(): Promise<void> {
  canvas = document.getElementById('svgCanvas')?.parentElement as HTMLElement | null;
  gridToggleBtn = document.getElementById('gridToggleBtn');
  gridSettingsBtn = document.getElementById('gridSettingsBtn');
  gridSizeInput = document.getElementById('gridSizeInput') as HTMLInputElement | null;
  gridSizeSlider = document.getElementById('gridSizeSlider') as HTMLInputElement | null;
  gridSettingsApplyBtn = document.getElementById('gridSettingsApplyBtn');

  await loadGridSettings();

  if (gridToggleBtn) gridToggleBtn.addEventListener('click', toggleGridVisibility);
  if (gridSettingsBtn) gridSettingsBtn.addEventListener('click', showGridSettingsModal);
  if (gridSettingsApplyBtn) {
    gridSettingsApplyBtn.addEventListener('click', () => {
      const success = setGridSize(gridSizeInput?.value ?? gridSize);
      if (success) hideGridSettingsModal();
    });
  }

  if (gridSizeInput && gridSizeSlider) {
    gridSizeInput.addEventListener('input', (e: Event) => {
      const t = e.target as HTMLInputElement;
      gridSizeSlider!.value = t.value;
    });
    gridSizeSlider.addEventListener('input', (e: Event) => {
      const t = e.target as HTMLInputElement;
      gridSizeInput!.value = t.value;
    });
  }
}
//=============================================================================
export function getGridState(): { visible: boolean; size: number } {
  return { visible: gridVisible, size: gridSize };
}
//=============================================================================
export function getGridSize(): number {
  return gridSize;
}
//=============================================================================
