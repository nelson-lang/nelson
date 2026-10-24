//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { createRoot } from 'react-dom/client';
import { FigureCanvas } from '../../src/panels/FigureCanvas';
import {
  getFigureCanvasProfile,
  resetFigureCanvasProfile,
} from '../../src/panels/figureCanvasProfile';
import type { FigureDisplayListResult } from '../../src/rpc/types';
//=============================================================================
declare global {
  interface Window {
    renderComparisonFrame: (frame: FigureDisplayListResult) => void;
    renderComparisonReady: () => boolean;
  }
}
//=============================================================================
const root = createRoot(document.getElementById('root')!);
let hasMeshes = false;
window.__nelsonFigureCanvasProfileEnabled = true;
window.renderComparisonFrame = (frame) => {
  hasMeshes = (frame.webglMeshes?.length ?? 0) > 0;
  resetFigureCanvasProfile();
  root.render(<FigureCanvas {...frame} key={frame.handle} />);
};
//=============================================================================
window.renderComparisonReady = () => {
  const profile = getFigureCanvasProfile();
  return (
    profile.paintCalls + profile.workerPaintCalls > 0 &&
    (!hasMeshes || profile.webGlRenderCalls > 0)
  );
};
//=============================================================================
