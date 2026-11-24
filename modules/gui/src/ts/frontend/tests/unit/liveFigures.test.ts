//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { describe, it, expect, vi, beforeEach } from 'vitest';
//=============================================================================
const rpc = vi.fn(async () => ({}));
vi.mock('../../src/rpc/client', () => ({ rpc: (...a: unknown[]) => rpc(...a) }));
import {
  markFiguresLive,
  isFigureLive,
  parseFigureHandles,
  closeLiveFigures,
  releaseFigure,
} from '../../src/notebook/liveFigures';
//=============================================================================
beforeEach(() => rpc.mockClear());
describe('Notebook live-figure registry', () => {
  it('parseFigureHandles tolerates missing or malformed input', () => {
    expect(parseFigureHandles(undefined)).toEqual([]);
    expect(parseFigureHandles('not json')).toEqual([]);
    expect(parseFigureHandles('[3, -1, 8]')).toEqual([3, -1, 8]);
  });

  it('marks handles live and ignores -1 / invalid placeholders', () => {
    markFiguresLive([10, -1, 11]);
    expect(isFigureLive(10)).toBe(true);
    expect(isFigureLive(11)).toBe(true);
    expect(isFigureLive(-1)).toBe(false);
    expect(isFigureLive(999)).toBe(false);
  });

  it('closeLiveFigures forgets live handles and asks the engine to close them', async () => {
    markFiguresLive([20, 21]);
    await closeLiveFigures([20, 21, -1, 999]);
    expect(rpc).toHaveBeenCalledWith('notebook.closeFigures', { handles: [20, 21] });
    expect(isFigureLive(20)).toBe(false);
    expect(isFigureLive(21)).toBe(false);
  });

  it('closeLiveFigures makes no engine call when nothing is live', async () => {
    await closeLiveFigures([-1, 12345]);
    expect(rpc).not.toHaveBeenCalled();
  });

  it('releaseFigure forgets the handle and hands it back to the desktop', async () => {
    markFiguresLive([30]);
    await releaseFigure(30);
    expect(rpc).toHaveBeenCalledWith('notebook.releaseFigure', { handle: 30 });
    expect(isFigureLive(30)).toBe(false);
  });
});
//=============================================================================
