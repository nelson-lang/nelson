//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { DrawPrimitive, FigureDisplayListResult } from '../rpc/types';
//=============================================================================
type ImagePrimitive = Extract<DrawPrimitive, { kind: 'image' }>;
type Payload = Pick<ImagePrimitive, 'image' | 'rgba' | 'rgbaBytes' | 'imageWidth' | 'imageHeight'>;
// Streaming frames may refer to pixels sent by an earlier frame. Hydrate before
// frame coalescing or renderer selection, including frames never painted by React.
export class FigureImagePayloadCache {
  private readonly entries = new Map<string, { payload: Payload; bytes: number }>();
  private bytes = 0;
  retain(primitives: DrawPrimitive[]): void {
    for (const primitive of primitives) {
      if (primitive.kind !== 'image' || !primitive.imageId) continue;
      if (!primitive.image && !primitive.rgba && !primitive.rgbaBytes) continue;
      const { image, rgba, rgbaBytes, imageWidth, imageHeight } = primitive;
      const bytes = rgbaBytes?.byteLength ?? ((rgba?.length ?? image?.length ?? 0) * 3) / 4;
      const previous = this.entries.get(primitive.imageId);
      this.bytes -= previous?.bytes ?? 0;
      this.entries.delete(primitive.imageId);
      this.entries.set(primitive.imageId, {
        payload: { image, rgba, rgbaBytes, imageWidth, imageHeight },
        bytes,
      });
      this.bytes += bytes;
    }
    while (this.entries.size > 64 || this.bytes > 32 * 1024 * 1024) {
      const oldest = this.entries.keys().next().value;
      if (oldest === undefined) break;
      this.bytes -= this.entries.get(oldest)!.bytes;
      this.entries.delete(oldest);
    }
  }
  //=============================================================================
  hydrate(figure: FigureDisplayListResult): FigureDisplayListResult {
    this.retain(figure.primitives);
    const primitives = figure.primitives.map((primitive) => {
      if (primitive.kind !== 'image' || !primitive.imageId) return primitive;
      if (primitive.image || primitive.rgba || primitive.rgbaBytes) return primitive;
      const cached = this.entries.get(primitive.imageId);
      if (!cached) return primitive;
      // Referenced sprites stay recent even when only the background changes.
      this.entries.delete(primitive.imageId);
      this.entries.set(primitive.imageId, cached);
      return { ...primitive, ...cached.payload };
    });
    return { ...figure, primitives };
  }
  //=============================================================================
  clear(): void {
    this.entries.clear();
    this.bytes = 0;
  }
}
//=============================================================================
