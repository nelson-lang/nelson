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
import {
  getViewBox,
  setViewBox,
  getZoomScale,
  setZoomScale,
  clampZoomScale,
} from './workspace-handlers.js';
import { t } from '../utils/i18n.js';
import { initTooltip } from './tooltip.js';
import { updateViewBoxWithAnchor, updateGrid } from './canvas-interaction.js';
import * as FieldNames from '../FieldNames.js';
import { _setStatus, deepClone, getViewportSize } from './event-context.js';
import type { EventRefs } from './event-context.js';
//=============================================================================
export function initCanvasEvents(refs: EventRefs) {
  const { rendererRef, statusEl, _rotateSelectionBtn, svg, updateStatusBar } = refs;
  const printBtn = document.getElementById('printBtn');
  if (printBtn)
    printBtn.addEventListener('click', () => {
      window.print();
    });

  if (_rotateSelectionBtn) {
    _rotateSelectionBtn.addEventListener('click', () => {
      try {
        const selected: any = state.selectedId ? state.blocks.get(state.selectedId) : null;
        if (!selected) return;
        selected.rotation = ((selected.rotation || 0) + 90) % 360;
        if (rendererRef && typeof rendererRef.current?.updateBlockTransform === 'function')
          rendererRef.current.updateBlockTransform(selected);
        if (rendererRef && typeof rendererRef.current?.updateConnections === 'function')
          rendererRef.current.updateConnections(true);
      } catch (e) {}
    });
  }

  if (svg) {
    const svgEl = svg as SVGSVGElement;
    try {
      initTooltip({ svg: svgEl, renderer: rendererRef as any });
    } catch (e) {}

    svgEl.addEventListener(
      'wheel',
      (event) => {
        event.preventDefault();
        const baseViewBox = { ...getViewBox() };
        const delta = Math.sign((event as WheelEvent).deltaY);
        const factor = delta > 0 ? 0.9 : 1.1;
        const newZoomScale = clampZoomScale(getZoomScale() * factor);
        setZoomScale(newZoomScale);
        const anchor = rendererRef?.current?.clientToSvg
          ? rendererRef.current.clientToSvg(
              (event as WheelEvent).clientX,
              (event as WheelEvent).clientY
            )
          : undefined;
        updateViewBoxWithAnchor(newZoomScale, anchor || { x: 0, y: 0 }, baseViewBox);
        updateStatusBar(null, null, newZoomScale);
      },
      { passive: false }
    );

    svgEl.addEventListener(
      'pointerdown',
      (event) => {
        if ((event as PointerEvent).button !== 0 && (event as PointerEvent).button !== 1) return;
        if (
          (event.target as Element).closest('.drag-handle') ||
          (event.target as Element).closest('.port')
        )
          return;
        if ((event as PointerEvent).ctrlKey && event.target === svgEl) {
          rendererRef?.current?.startMarqueeSelection?.(event as PointerEvent);
          return;
        }
        if ((event.target as Element) !== svgEl && (event as PointerEvent).button === 0) return;
        event.preventDefault();
        const panStart = {
          clientX: (event as PointerEvent).clientX,
          clientY: (event as PointerEvent).clientY,
          viewBox: { ...getViewBox() },
        };
        svgEl.setPointerCapture((event as PointerEvent).pointerId);
        state.isPanning = true;
        svgEl.classList.add('panning');

        const handlePointerMove = (event: PointerEvent) => {
          if (!state.isPanning) return;
          event.preventDefault();
          const dxClient = panStart.clientX - event.clientX;
          const dyClient = panStart.clientY - event.clientY;
          const currentViewBox = getViewBox();
          const currentZoomScale = getZoomScale();
          const scaleX = currentViewBox.w / (svgEl.clientWidth || 1);
          const scaleY = currentViewBox.h / (svgEl.clientHeight || 1);
          const dx = dxClient * scaleX;
          const dy = dyClient * scaleY;
          const newViewBox = {
            x: panStart.viewBox.x + dx,
            y: panStart.viewBox.y + dy,
            w: panStart.viewBox.w,
            h: panStart.viewBox.h,
          };
          setViewBox(newViewBox);
          const canvas = document.getElementById('canvas');
          updateGrid(canvas, currentZoomScale, newViewBox);
        };

        const handlePointerUp = () => {
          state.isPanning = false;
          svgEl.classList.remove('panning');
          svgEl.releasePointerCapture((event as PointerEvent).pointerId);
          svgEl.removeEventListener('pointermove', handlePointerMove as any);
          svgEl.removeEventListener('pointerup', handlePointerUp as any);
          svgEl.removeEventListener('pointercancel', handlePointerUp as any);
          if (state.routingDirty) rendererRef?.current?.updateConnections?.(true);
        };
        svgEl.addEventListener('pointermove', handlePointerMove as any);
        svgEl.addEventListener('pointerup', handlePointerUp as any);
        svgEl.addEventListener('pointercancel', handlePointerUp as any);
      },
      { passive: false }
    );

    svgEl.addEventListener('pointerdown', (event) => {
      const active = document.activeElement;
      if (
        active instanceof HTMLElement &&
        active.matches('.dashboard-combo-select, .dashboard-edit-input') &&
        event.target !== active
      ) {
        active.blur();
      }
    });

    svgEl.addEventListener('click', () => {
      if (state.suppressNextCanvasClick) {
        state.suppressNextCanvasClick = false;
        return;
      }
      rendererRef?.current?.clearPending?.();
      rendererRef?.current?.selectBlock?.(null);
      rendererRef?.current?.selectConnection?.(null);
    });

    svgEl.addEventListener('dragover', (event) => {
      event.preventDefault();
      (event.dataTransfer as DataTransfer).dropEffect = 'copy';
    });

    svgEl.addEventListener('drop', (event) => {
      event.preventDefault();
      const data = (event.dataTransfer as DataTransfer).getData('text/plain');
      if (!data) return;
      let draggedBlockType: string | undefined, draggedSubsystemKey: string | undefined;
      try {
        const parsed = JSON.parse(data);
        draggedBlockType = parsed.type;
        draggedSubsystemKey = parsed.subsystemKey || '';
      } catch (e) {
        return;
      }
      const svgPoint = rendererRef?.current?.clientToSvg
        ? rendererRef.current.clientToSvg(
            (event as DragEvent).clientX,
            (event as DragEvent).clientY
          )
        : undefined;
      if (!svgPoint) return;
      const templates = (window as any).blockTemplates as Record<string, any> | undefined;
      const template = templates ? templates[draggedBlockType as any] : undefined;
      const blockWidth = template?.width || 80;
      const blockHeight = template?.height || 80;
      const blockX = svgPoint.x - blockWidth / 2;
      const blockY = svgPoint.y - blockHeight / 2;
      try {
        const options: any = {};
        if (draggedBlockType === FieldNames.kSubsystem && draggedSubsystemKey) {
          const spec: any = state.loadedSubsystems.get(draggedSubsystemKey);
          if (!spec) throw new Error('Subsystem spec not found');
          options.params = {
            name: spec.name,
            externalInputs: deepClone(spec.externalInputs),
            externalOutputs: deepClone(spec.externalOutputs),
            subsystem: deepClone(spec),
          };
        }
        const block = rendererRef?.current?.createBlock?.(
          draggedBlockType as string,
          blockX,
          blockY,
          options
        );
        if (block) {
          state.routingDirty = true;
          if (state.dirtyBlocks) state.dirtyBlocks.add(block.id);
          rendererRef?.current?.updateConnections?.(true);
          _setStatus(
            statusEl,
            t('Added {type}', { type: draggedBlockType ?? '' }),
            'success',
            2000
          );
        }
      } catch (error) {
        _setStatus(statusEl, t('Error adding {type}', { type: draggedBlockType ?? '' }), 'error');
      }
    });
  }

  window.addEventListener('resize', () => {
    const canvas = document.getElementById('canvas');
    if (canvas && svg) {
      const currentViewBox = getViewBox();
      const currentZoomScale = getZoomScale();
      const { w, h } = getViewportSize();
      const center = {
        x: currentViewBox.x + currentViewBox.w / 2,
        y: currentViewBox.y + currentViewBox.h / 2,
      };
      const newW = w / currentZoomScale;
      const newH = h / currentZoomScale;
      const newViewBox = { x: center.x - newW / 2, y: center.y - newH / 2, w: newW, h: newH };
      setViewBox(newViewBox);
      updateGrid(canvas, currentZoomScale, newViewBox);
      rendererRef?.current?.updateConnections?.(true);
    }
  });

  const canvasEl = document.getElementById('canvas');
  if (canvasEl && 'ResizeObserver' in window) {
    let resizeRaf = 0;
    let lastSize = { w: 0, h: 0 };
    const ro = new ResizeObserver((entries) => {
      const entry = entries[0];
      const rect = entry?.contentRect;
      const w = rect?.width || canvasEl.clientWidth || 0;
      const h = rect?.height || canvasEl.clientHeight || 0;
      if (!w || !h) return;
      if (Math.abs(w - lastSize.w) < 0.5 && Math.abs(h - lastSize.h) < 0.5) return;
      lastSize = { w, h };
      if (resizeRaf) cancelAnimationFrame(resizeRaf);
      resizeRaf = requestAnimationFrame(() => {
        if (!svg) return;
        const currentViewBox = getViewBox();
        const currentZoomScale = getZoomScale();
        const { w, h } = getViewportSize();
        const center = {
          x: currentViewBox.x + currentViewBox.w / 2,
          y: currentViewBox.y + currentViewBox.h / 2,
        };
        const newW = w / currentZoomScale;
        const newH = h / currentZoomScale;
        const newViewBox = { x: center.x - newW / 2, y: center.y - newH / 2, w: newW, h: newH };
        setViewBox(newViewBox);
        updateGrid(canvasEl, currentZoomScale, newViewBox);
        rendererRef?.current?.updateConnections?.(true);
      });
    });
    ro.observe(canvasEl as Element);
  }

  const collapseLibraryOnMobile = () => {
    if (!window.matchMedia('(max-width: 900px)').matches) return;
    document.querySelectorAll('.toolbox details').forEach((group) => {
      (group as HTMLDetailsElement).open = false;
    });
  };
  collapseLibraryOnMobile();
}
//=============================================================================
