//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { afterEach, describe, expect, it } from 'vitest';
import {
  addFigureCanvasValue,
  getFigureCanvasProfile,
  incrementFigureCanvasCounter,
  measureFigureCanvas,
  recordFigureCanvasMeasurement,
  resetFigureCanvasProfile,
} from '../../src/panels/figureCanvasProfile';
//=============================================================================
describe('figure canvas profile', () => {
  afterEach(() => {
    window.__nelsonFigureCanvasProfileEnabled = false;
    resetFigureCanvasProfile();
  });

  it('keeps counters disabled until explicitly enabled', () => {
    window.__nelsonFigureCanvasProfileEnabled = false;
    resetFigureCanvasProfile();

    incrementFigureCanvasCounter('workerFramePosts');
    measureFigureCanvas('paintCalls', 'paintMs', () => undefined);
    recordFigureCanvasMeasurement('effectRuns', 'effectMs', 12);

    expect(getFigureCanvasProfile().paintCalls).toBe(0);
    expect(getFigureCanvasProfile().workerFramePosts).toBe(0);
    expect(getFigureCanvasProfile().workerPostCalls).toBe(0);
    expect(getFigureCanvasProfile().workerRefreshCalls).toBe(0);
    expect(getFigureCanvasProfile().workerPaintCalls).toBe(0);
    expect(getFigureCanvasProfile().effectRuns).toBe(0);
  });

  it('records measured counters and elapsed durations when enabled', () => {
    window.__nelsonFigureCanvasProfileEnabled = true;
    resetFigureCanvasProfile();

    measureFigureCanvas('paintCalls', 'paintMs', () => undefined);
    incrementFigureCanvasCounter('workerFramePosts');
    addFigureCanvasValue('primitiveImageBytes', 1024);
    measureFigureCanvas('workerPostCalls', 'workerPostMs', () => undefined);
    recordFigureCanvasMeasurement('workerRefreshCalls', 'workerRefreshMs', 3.5);
    recordFigureCanvasMeasurement('workerPaintCalls', 'workerPaintMs', 4.25);
    recordFigureCanvasMeasurement('effectRuns', 'effectMs', 12.5);

    const profile = getFigureCanvasProfile();
    expect(profile.paintCalls).toBe(1);
    expect(profile.paintMs).toBeGreaterThanOrEqual(0);
    expect(profile.workerFramePosts).toBe(1);
    expect(profile.primitiveImageBytes).toBe(1024);
    expect(profile.workerPostCalls).toBe(1);
    expect(profile.workerPostMs).toBeGreaterThanOrEqual(0);
    expect(profile.workerRefreshCalls).toBe(1);
    expect(profile.workerRefreshMs).toBeCloseTo(3.5);
    expect(profile.workerPaintCalls).toBe(1);
    expect(profile.workerPaintMs).toBeCloseTo(4.25);
    expect(profile.effectRuns).toBe(1);
    expect(profile.effectMs).toBeCloseTo(12.5);
  });
});
//=============================================================================
