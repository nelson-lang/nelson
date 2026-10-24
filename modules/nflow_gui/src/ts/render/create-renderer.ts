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
import { makeMathHelpers } from './math-helpers.js';
import { makeRouteWorker } from './wire/route-worker.js';
import { makeWireHelpers } from './wire/wire-helpers.js';
import { setupRendererDom } from './core/renderer-setup.js';
import { setupSvgListeners } from './interaction/renderer-events.js';
import { makeWireCornerHandlers } from './wire/renderer-wire-corner.js';
import { makeWireSegmentHandlers } from './interaction/renderer-wire-segment.js';
import { makePortHandlers } from './interaction/renderer-port.js';
import { makeScopeCommentLayout } from './core/renderer-scope-layout.js';
import { makeDragHandlers } from './interaction/renderer-drag.js';
import { makeInteractionHandlers } from './interaction/renderer-interaction.js';
import { makeSelectionHandlers } from './interaction/renderer-selection.js';
import { makeConnectionHandlers } from './interaction/renderer-connection.js';
import { makeRoutingHandlers } from './wire/renderer-routing.js';
import { makeRoutingQualityHandlers } from './wire/renderer-routing-quality.js';
import { makeWireApplyHandlers } from './wire/renderer-wire-apply.js';
import { makeWireGeometryHandlers } from './wire/renderer-wire-geometry.js';
import { makeWireCheckHandlers } from './wire/renderer-wire-checks.js';
import { makeDebugHandlers } from './core/renderer-debug.js';
import { makeDebugSnapshotHandlers } from './core/renderer-debug-snapshot.js';
import { makeBlockTransformHandlers } from './block/renderer-block-transform.js';
import { makeUtilHandlers } from './core/renderer-util.js';
import { makeParamDisplayHandlers } from './block/renderer-param-display.js';
import { makeBlockLabelHandlers } from './block/renderer-block-label.js';
import { makeBlockCreation } from './block/renderer-block-create.js';
//=============================================================================
const userFuncResizeAttempts = new Map();
export function createRenderer({
  svg,
  blockLayer,
  wireLayer,
  overlayLayer,
  state,
  onSelectBlock,
  onSelectConnection,
  onOpenSubsystem,
  onConnectionError,
}: any) {
  const math = makeMathHelpers({ svg });
  const wire = makeWireHelpers({ state });
  const {
    scaleMathToFit,
    computeUserFuncSize,
    getAutoMathConfig,
    computeRenderedAutoMathSize,
    positionGainMath,
    toLeadingMinusTex,
  } = math;
  const {
    dedupePoints,
    removeColinearPoints,
    samePoint,
    segmentOrientation,
    getTurnIndices,
    buildOrthogonalHead,
    buildOrthogonalTail,
    buildFallbackPathFromPorts,
    buildFastDragPathFromPorts,
    buildBlockAwareFallbackFromPorts,
    wireCrossesForeignBlock,
    getPortPosition,
    getPortPositionRaw,
    getWirePosition,
    getWirePositionRaw,
    applyWireOffsets,
  } = wire;
  const routeHelpers = makeRouteWorker({ svg, state });

  const ctx: any = {
    svg,
    blockLayer,
    wireLayer,
    overlayLayer,
    state,
    onSelectBlock: onSelectBlock || (() => {}),
    onSelectConnection: onSelectConnection || (() => {}),
    onOpenSubsystem: onOpenSubsystem || (() => {}),
    onConnectionError: onConnectionError || (() => {}),
    wireCornerDrag: { active: false, conn: null, index: -1, pointerId: null },
    marqueeState: { active: false, pointerId: null, start: null },
    routeAfterPreviewScheduled: false,
    lastConnectionError: null,
    userFuncResizeAttempts,
    routeHelpers,
    scaleMathToFit,
    computeUserFuncSize,
    getAutoMathConfig,
    computeRenderedAutoMathSize,
    positionGainMath,
    toLeadingMinusTex,
    dedupePoints,
    removeColinearPoints,
    samePoint,
    segmentOrientation,
    getTurnIndices,
    buildOrthogonalHead,
    buildOrthogonalTail,
    buildFallbackPathFromPorts,
    buildFastDragPathFromPorts,
    buildBlockAwareFallbackFromPorts,
    wireCrossesForeignBlock,
    getPortPosition,
    getPortPositionRaw,
    getWirePosition,
    getWirePositionRaw,
    applyWireOffsets,
  };

  setupRendererDom(ctx);

  const _handlerFactories = [
    makeWireCornerHandlers,
    makeWireSegmentHandlers,
    makePortHandlers,
    makeWireGeometryHandlers,
    makeWireCheckHandlers,
    makeDebugSnapshotHandlers,
    makeDebugHandlers,
    makeWireApplyHandlers,
    makeRoutingQualityHandlers,
    makeRoutingHandlers,
    makeUtilHandlers,
    makeScopeCommentLayout,
    makeBlockTransformHandlers,
    makeParamDisplayHandlers,
    makeBlockLabelHandlers,
    makeDragHandlers,
    makeInteractionHandlers,
    makeSelectionHandlers,
    makeConnectionHandlers,
    makeBlockCreation,
  ];

  for (const factory of _handlerFactories) {
    Object.assign(ctx, factory(ctx));
  }

  setupSvgListeners(ctx);

  return {
    createBlock: ctx.createBlock,
    createConnection: ctx.createConnection,
    updateConnections: ctx.updateConnections,
    updateBlockTransform: ctx.updateBlockTransform,
    selectConnection: ctx.selectConnection,
    clearPending: ctx.clearPending,
    selectBlock: ctx.selectBlock,
    setMultiSelection: ctx.setMultiSelection,
    startMarqueeSelection: ctx.startMarqueeSelection,
    clearWorkspace: ctx.clearWorkspace,
    findNearestConnection: ctx.findNearestConnection,
    deleteConnection: ctx.deleteConnection,
    deleteBlock: ctx.deleteBlock,
    updateBlockLabel: ctx.updateBlockLabel,
    updateSelectionBox: ctx.updateSelectionBox,
    clientToSvg: ctx.clientToSvg,
    forceFullRoute: ctx.forceFullRoute,
    renderCurrentWirePaths: (preservePathShape = false) => {
      ctx.applyWirePaths(new Map(), { preservePathShape });
      ctx.updateSelectionBox();
      ctx.refreshDebugLog();
    },
    resizeBlock: ctx.resizeBlock,
    setLoopHighlight: ctx.setLoopHighlight,
    releaseWirePins: ctx.releaseWirePins,
    getLastConnectionError: () => ctx.lastConnectionError,
  };
}
//=============================================================================
