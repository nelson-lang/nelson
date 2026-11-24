//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { DrawPrimitive } from '../rpc/types';
import { rgbaImageData } from './figureCanvasPainter';
//=============================================================================
export type FigureCanvasTransferMode = 'structured' | 'clone' | 'bitmap';
export type FigureCanvasTransferFrame = {
  primitives: DrawPrimitive[];
  overlayPrimitives: DrawPrimitive[];
  transferList: Transferable[];
};
//=============================================================================
export function clonedTransferFrame(
  primitives: DrawPrimitive[],
  overlayPrimitives: DrawPrimitive[]
): FigureCanvasTransferFrame {
  const transferList: Transferable[] = [];
  const clonePrimitives = (items: DrawPrimitive[]): DrawPrimitive[] => {
    let changed = false;
    const next = items.map((primitive) => {
      if (primitive.kind !== 'image' || !(primitive.rgbaBytes instanceof ArrayBuffer)) {
        return primitive;
      }
      const rgbaBytes = primitive.rgbaBytes.slice(0);
      transferList.push(rgbaBytes);
      changed = true;
      return { ...primitive, rgbaBytes };
    });
    return changed ? next : items;
  };
  return {
    primitives: clonePrimitives(primitives),
    overlayPrimitives: clonePrimitives(overlayPrimitives),
    transferList,
  };
}
//=============================================================================
export async function bitmapTransferFrame(
  primitives: DrawPrimitive[],
  overlayPrimitives: DrawPrimitive[]
): Promise<FigureCanvasTransferFrame | null> {
  if (typeof createImageBitmap !== 'function') return null;
  const transferList: Transferable[] = [];
  const convertPrimitives = async (items: DrawPrimitive[]): Promise<DrawPrimitive[]> => {
    let changed = false;
    const next: DrawPrimitive[] = [];
    for (const primitive of items) {
      if (primitive.kind !== 'image' || (!primitive.rgba && !primitive.rgbaBytes)) {
        next.push(primitive);
        continue;
      }
      const imageData = rgbaImageData(primitive);
      if (!imageData) {
        next.push(primitive);
        continue;
      }
      const bitmap = await createImageBitmap(imageData);
      transferList.push(bitmap);
      changed = true;
      next.push({ ...primitive, bitmap, rgba: undefined, rgbaBytes: undefined });
    }
    return changed ? next : items;
  };
  return {
    primitives: await convertPrimitives(primitives),
    overlayPrimitives: await convertPrimitives(overlayPrimitives),
    transferList,
  };
}
//=============================================================================
export function closeTransferFrame(frame: FigureCanvasTransferFrame): void {
  for (const item of frame.transferList) {
    const close = (item as { close?: () => void }).close;
    if (typeof close === 'function') close.call(item);
  }
}
//=============================================================================
