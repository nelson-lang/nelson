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
import { state } from './state.js';
import { t } from '../utils/i18n.js';
import { openBlockHelp } from './block-help.js';
import { showShortcutsHelp } from './shortcuts-help.js';
import { handleAlignAction } from './context-menu-manager.js';
import {
  _setStatus,
  moveSelectedBlocksByDelta,
  shouldUseNativeKeyboardBehavior,
} from './event-context.js';
import type { EventRefs } from './event-context.js';
import type { SimulationActions } from './sim-controls.js';
//=============================================================================
export function initKeyboardShortcuts(refs: EventRefs, sim: SimulationActions) {
  const { handleRun, handleReset } = sim;
  const { rendererRef, statusEl, GRID_SIZE, renderInspector } = refs;
  const handleDeleteSelection = () => {
    if (state.selectedId) {
      rendererRef?.current?.deleteBlock?.(state.selectedId as any);
      rendererRef?.current?.selectBlock?.(null);
      renderInspector(null);
      _setStatus(statusEl, t('Block deleted'), 'info', 2000);
    } else if (state.selectedConnection) {
      rendererRef?.current?.deleteConnection?.(state.selectedConnection as any);
      rendererRef?.current?.selectConnection?.(null);
      renderInspector(null);
      _setStatus(statusEl, t('Wire deleted'), 'info', 2000);
    }
  };

  window.addEventListener(
    'keydown',
    (event) => {
      if (event.key === 'F1') {
        let selectedBlock: unknown =
          state.selectedId == null ? null : state.blocks.get(state.selectedId);
        if (!selectedBlock && state.selectedId != null) {
          selectedBlock = Array.from(state.blocks.entries()).find(
            ([id]) => String(id) === String(state.selectedId)
          )?.[1];
        }
        if (selectedBlock) void openBlockHelp(selectedBlock as { type?: unknown });
        else showShortcutsHelp();
        event.preventDefault();
        return;
      }
      const isMod = event.ctrlKey || event.metaKey;
      const isSelectAllShortcut = isMod && (event.key === 'a' || event.key === 'A');
      if (shouldUseNativeKeyboardBehavior(event.target, isSelectAllShortcut)) return;
      if (event.key === '?') {
        try {
          showShortcutsHelp();
          event.preventDefault();
          return;
        } catch (e) {}
      }
      if (event.key === 'Escape') {
        try {
          rendererRef?.current?.clearPending?.();
          rendererRef?.current?.selectBlock?.(null);
          rendererRef?.current?.selectConnection?.(null);
        } catch (e) {}
        try {
          renderInspector(null);
        } catch (e) {}
        state.pendingPort = null;
        event.preventDefault();
        return;
      }
      if (event.key === 'Delete') {
        handleDeleteSelection();
        event.preventDefault();
        return;
      }
      if (isSelectAllShortcut) {
        try {
          const allIds = new Set(Array.from(state.blocks.keys()));
          const allConns = new Set(state.connections || []);
          if (rendererRef?.current && typeof rendererRef.current.setMultiSelection === 'function') {
            rendererRef.current.setMultiSelection(allIds as any, allConns as any);
          } else {
            if (state.selectedIds) {
              state.selectedIds.clear();
              allIds.forEach((id) => state.selectedIds.add(id as any));
            }
            if (state.selectedConnections) {
              state.selectedConnections.clear();
              allConns.forEach((c) => state.selectedConnections.add(c as any));
            }
            state.selectedId = null;
            if (
              rendererRef?.current &&
              typeof rendererRef.current.updateSelectionBox === 'function'
            )
              rendererRef.current.updateSelectionBox();
          }
        } catch (e) {}
        event.preventDefault();
        return;
      }
      if (isMod && (event.key === 'z' || event.key === 'Z')) {
        try {
          void import('./history.js')
            .then((m: any) => {
              const p = m.undo && m.undo();
              if (p && p.then) p.catch((err: any) => console.error('[history.undo error]', err));
            })
            .catch((err: any) => console.error('[history import error]', err));
        } catch (e) {}
        event.preventDefault();
        return;
      }
      if (
        isMod &&
        (event.key === 'y' || (event.shiftKey && (event.key === 'Z' || event.key === 'z')))
      ) {
        try {
          void import('./history.js')
            .then((m: any) => {
              const p = m.redo && m.redo();
              if (p && p.then) p.catch((err: any) => console.error('[history.redo error]', err));
            })
            .catch((err: any) => console.error('[history import error]', err));
        } catch (e) {}
        event.preventDefault();
        return;
      }
      if (isMod && (event.key === 'd' || event.key === 'D')) {
        try {
          void import('./history.js')
            .then((m: any) => {
              try {
                const res: any = m.duplicateSelection && m.duplicateSelection();
                if (res && res.then)
                  res.catch((err: any) => console.error('[history.duplicate error]', err));
              } catch (err) {
                console.error('[history.duplicate error]', err);
              }
            })
            .catch((err: any) => console.error('[history import error]', err));
        } catch (e) {}
        event.preventDefault();
        return;
      }
      if (isMod && (event.key === 'c' || event.key === 'C')) {
        try {
          void import('./history.js')
            .then((m: any) => {
              try {
                const res: any = m.copySelection && m.copySelection();
                if (res && res.then)
                  res.catch((err: any) => console.error('[history.copy error]', err));
              } catch (err) {
                console.error('[history.copy error]', err);
              }
            })
            .catch((err: any) => console.error('[history import error]', err));
        } catch (e) {}
        event.preventDefault();
        return;
      }
      if (isMod && (event.key === 'x' || event.key === 'X')) {
        try {
          void import('./history.js')
            .then((m: any) => {
              try {
                const res: any = m.cutSelection && m.cutSelection();
                if (res && res.then)
                  res.catch((err: any) => console.error('[history.cut error]', err));
              } catch (err) {
                console.error('[history.cut error]', err);
              }
            })
            .catch((err: any) => console.error('[history import error]', err));
        } catch (e) {}
        event.preventDefault();
        return;
      }
      if (isMod && (event.key === 'v' || event.key === 'V')) {
        try {
          void import('./history.js')
            .then((m: any) => {
              try {
                const res: any = m.pasteSelection && m.pasteSelection();
                if (res && res.then)
                  res.catch((err: any) => console.error('[history.paste error]', err));
              } catch (err) {
                console.error('[history.paste error]', err);
              }
            })
            .catch((err: any) => console.error('[history import error]', err));
        } catch (e) {}
        event.preventDefault();
        return;
      }
      try {
        const key = event.key || '';
        const keyLower = String(key).toLowerCase();
        if ((isMod && !event.shiftKey && keyLower === 't') || key === 'F5') {
          event.preventDefault();
          handleRun();
          return;
        }
        // Support common application shortcuts (New/Open) in browser-only test
        // environments where native menu accelerators are not present.
        if (isMod && !event.shiftKey && keyLower === 'n') {
          try {
            event.preventDefault();
            const el = document.querySelector('[data-action="new"]') as HTMLElement | null;
            if (el) el.click();
          } catch (_e) {}
          return;
        }
        if (isMod && !event.shiftKey && keyLower === 'o') {
          try {
            event.preventDefault();
            const el = document.querySelector('[data-action="open"]') as HTMLElement | null;
            if (el) el.click();
          } catch (_e) {}
          return;
        }
        // View menu shortcuts (Reset View / Zoom). Native menu accelerators do not
        // fire in the browser/webview build, so click the toolbar buttons, which
        // own the fit / zoom logic.
        if (isMod && !event.shiftKey && key === '0') {
          try {
            event.preventDefault();
            (document.getElementById('homeBtn') as HTMLElement | null)?.click();
          } catch (_e) {}
          return;
        }
        if (isMod && (key === '=' || key === '+')) {
          try {
            event.preventDefault();
            (document.getElementById('zoomInBtn') as HTMLElement | null)?.click();
          } catch (_e) {}
          return;
        }
        if (isMod && key === '-') {
          try {
            event.preventDefault();
            (document.getElementById('zoomOutBtn') as HTMLElement | null)?.click();
          } catch (_e) {}
          return;
        }
        if (isMod && event.shiftKey && keyLower === 't') {
          event.preventDefault();
          handleReset();
          return;
        }
      } catch (e) {}
      // Ctrl/Cmd+Shift+Arrow aligns blocks (2 or more selected)
      if (isMod && event.shiftKey) {
        const alignMap: Record<string, string> = {
          ArrowLeft: 'alignLeft',
          ArrowRight: 'alignRight',
          ArrowUp: 'alignTop',
          ArrowDown: 'alignBottom',
        };
        const alignAction = alignMap[event.key];
        if (alignAction) {
          try {
            handleAlignAction(alignAction);
          } catch (_e) {}
          event.preventDefault();
          return;
        }
      }
      let dx = 0;
      let dy = 0;
      if (event.key === 'ArrowLeft') dx = -GRID_SIZE;
      if (event.key === 'ArrowRight') dx = GRID_SIZE;
      if (event.key === 'ArrowUp') dy = -GRID_SIZE;
      if (event.key === 'ArrowDown') dy = GRID_SIZE;
      if (!dx && !dy) return;
      if (moveSelectedBlocksByDelta(dx, dy)) {
        event.preventDefault();
      }
    },
    true
  );
}
//=============================================================================
