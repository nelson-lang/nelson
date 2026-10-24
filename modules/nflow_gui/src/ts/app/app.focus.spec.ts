//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { vi } from 'vitest';
//=============================================================================
test('app.init exercises renderer branch and UI setup', async () => {
  vi.resetModules();

  // Mock many internal deps to keep init lightweight but exercise branches
  await vi.doMock('./state.js', () => ({
    state: { blocks: new Map(), connections: [], diagramName: 'test' },
    signalDiagramChanged: () => {},
    setCurrentFilePath: () => {},
    setFitToDiagram: () => {},
    setUpdateStabilityPanel: () => {},
    getCurrentFilePath: () => '',
  }));

  await vi.doMock('./file-operations.js', () => ({
    toJSON: (o) => JSON.stringify(o),
    serializeDiagram: (s) => ({ d: true }),
    parseDiagram: (t) => ({ parsed: true }),
    sanitizeFilename: (n) => n.replace(/[^a-z0-9]/gi, '_'),
  }));

  await vi.doMock('./modal-handlers.js', () => ({
    setStatusEl: () => {},
    handleSaveAsSubsystem: () => {},
  }));

  await vi.doMock('./menu-handlers.js', () => ({
    handleMenuAction: () => {},
    initMenuHandlers: () => {},
    setHomeBtn: () => {},
    setZoomInBtn: () => {},
    setZoomOutBtn: () => {},
    performOpen: () => {},
    newDiagram: () => {},
    setDiagramNameInput: () => {},
    setRuntimeInput: () => {},
    setSimDt: () => {},
    setRealTimeModeInput: () => {},
    setAutoRouteInput: () => {},
    setVariablesInput: () => {},
    setVariablesPreview: () => {},
  }));

  await vi.doMock('./diagram-handlers.js', () => ({
    loadDiagram: async () => {},
    setSubsystemUpBtn: () => {},
    updateSubsystemNavUi: () => {},
    setRendererRef: () => {},
    setLockedInput: () => {},
    setRealTimeModeInput: () => {},
    syncLockedUi: () => {},
  }));

  await vi.doMock('./subsystem-handlers.js', () => ({
    openSubsystemFromBlock: () => {},
    closeSubsystemView: () => {},
  }));

  await vi.doMock('./workspace-handlers.js', () => ({
    setRendererRef: () => {},
    setViewBox: () => {},
    setZoomScale: () => {},
    initViewBox: () => {},
    setSvg: () => {},
    setUpdateStatusBar: () => {},
  }));

  await vi.doMock('../render/render.js', () => ({
    createRenderer: (opts) => ({
      some: 'renderer',
    }),
    refreshBlockTemplates: () => {},
  }));

  await vi.doMock('./library-loader.js', () => ({
    initLibraries: () => Promise.resolve(),
    buildBlockLibrary: () => [],
  }));

  await vi.doMock('./library-manager.js', () => ({
    mountLibraryManager: () => {},
  }));

  await vi.doMock('../utils/expr.js', () => ({
    parseVariables: () => ({ vars: {}, display: [] }),
    estimateLatexWidth: () => 100,
  }));

  await vi.doMock('../blocks/inspector.js', () => ({
    createInspector: () => ({ renderInspector: () => {} }),
  }));

  await vi.doMock('../sim/sim.js', () => ({ renderScope: () => {} }));

  await vi.doMock('./block-library-handlers.js', () => ({
    renderBlockLibrary: () => {},
    setBlockLibraryGroups: () => {},
    setBlockLibrary: () => {},
    setGridSize: () => {},
    setRendererRef: () => {},
    setStatusEl: () => {},
  }));

  await vi.doMock('./ui-handlers.js', () => ({
    initSidebarUI: () => {},
    initZoomButtons: () => {},
    setStatusElRef: () => {},
    setHomeBtnRef: () => {},
    setZoomInBtnRef: () => {},
    setZoomOutBtnRef: () => {},
    initModals: () => {},
  }));

  await vi.doMock('./event-handlers.js', () => ({
    initEventListeners: () => {},
    setRendererRef: () => {},
    setStatusEl: () => {},
    setRuntimeInput: () => {},
    setSimSolverInput: () => {},
    setSimToleranceInputs: () => {},
    setSimStopFcnInput: () => {},
    setRealTimeModeInput: () => {},
    setInspectorBody: () => {},
    setRotateSelectionBtn: () => {},
    setMarginLoopSelect: () => {},
    setMarginOutputText: () => {},
    setRenderInspector: () => {},
    setLockedInput: () => {},
    syncLockedUi: () => {},
  }));

  await vi.doMock('./theme-manager.js', () => ({
    loadThemes: async () => {},
    applyTheme: () => {},
    getThemes: () => [{ id: 'light', name: 'Light' }],
  }));

  await vi.doMock('./grid-manager.js', () => ({ initGridManager: async () => {} }));

  await vi.doMock('./context-menu-manager.js', () => ({
    initContextMenu: () => {},
    setContextMenus: () => {},
    setStateRef: () => {},
    setRendererRef: () => {},
    setFitToDiagram: () => {},
    setRenderInspector: () => {},
    setFocusPropertiesPanel: () => {},
    setSvg: () => {},
    setOnGenerateBlockCode: () => {},
  }));

  await vi.doMock('./components/sidebar-panels.js', () => ({
    mountSidebarPanels: () => {},
  }));
  await vi.doMock('./components/status-bar.js', () => ({
    mountStatusBar: () => {},
  }));
  await vi.doMock('./components/modals.js', () => ({ mountModals: () => {} }));
  await vi.doMock('./components/context-menus.js', () => ({
    mountContextMenus: () => {},
  }));

  document.body.innerHTML = `
    <div id="svgCanvas"></div>
    <div id="blockLayer"></div>
    <div id="wireLayer"></div>
    <div id="overlayLayer"></div>
    <div id="blockLibraryGroups"></div>
    <div id="statusBarInfo"></div>
    <div id="statusBarTime"></div>
    <div id="statusBarZoom"></div>
    <div id="statusBarBlocks"></div>
    <div id="statusBarConnections"></div>
    <button id="subsystemUpBtn"></button>
    <select id="marginLoopSelect"></select>
    <div id="marginOutputText"></div>
    <input id="fileOpenInput" />
    <button id="fileSaveAs"></button>
    <input id="loadSubsystemInput" />
    <input id="loadInput" />
    <button id="deleteSelection"></button>
    <button id="rotateSelection"></button>
    <button id="homeBtn"></button>
    <button id="zoomInBtn"></button>
    <button id="zoomOutBtn"></button>
    <button id="print"></button>
    <div id="status"></div>
    <input id="diagramName" />
    <input id="runtimeInput" />
    <input id="simDt" />
    <select id="realTimeMode"></select>
    <input id="autoRouteInput" />
    <textarea id="variablesInput"></textarea>
    <div id="variablesPreview"></div>
    <input id="authorInput" />
    <input id="licenseInput" />
    <textarea id="commentsInput"></textarea>
    <div id="inspectorBody"></div>
    <button id="applyVariables"></button>
    <div id="libraryManager"></div>
  `;

  if (typeof window.matchMedia !== 'function') {
    global.window.matchMedia = (query) => ({
      matches: false,
      media: query,
      addListener: () => {},
      removeListener: () => {},
      addEventListener: () => {},
      removeEventListener: () => {},
      onchange: null,
      dispatchEvent: () => false,
    });
  }

  // prevent library startup work if present
  global.window.__nf_init_libraries_promise = Promise.resolve();

  const mod = await import('./app.js');
  await expect(mod.init()).resolves.not.toThrow();

  const statusBarInfo = document.getElementById('statusBarInfo')!;
  expect(statusBarInfo.textContent).toBe('Ready');
});
//=============================================================================
