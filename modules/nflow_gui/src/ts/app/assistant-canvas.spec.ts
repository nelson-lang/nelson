//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Unit tests for the in-place canvas build the assistant triggers. The native
// bridge and the canvas loader are mocked, so these run with NO AI model and NO
// engine (what CI has): they pin that the scripted model is run, then pulled
// from the registry and drawn on the current canvas. The regression they guard
// is the getDiagram contract: the host answers over the string-result channel,
// so the envelope arrives as JSON TEXT (not a ready object) and its `content`
// is itself the model serialized as a string; both must be parsed or the canvas
// comes up empty.
import { afterEach, beforeEach, expect, test, vi } from 'vitest';
//=============================================================================
const nflowRpc = vi.fn(async (_method: string, _params?: unknown): Promise<unknown> => ({}));
const loadDiagram = vi.fn(async () => undefined);
const setBoundModel = vi.fn();
const handleMenuAction = vi.fn();
// The confirm modal: by default the canvas is clean, so it is never consulted.
// A dirty-canvas test sets `confirmChoice` to drive the Save/Don't/Cancel reply.
// sessionState is hoisted so the state.js mock factory (hoisted above imports)
// can return the same object the tests mutate.
const { sessionState } = vi.hoisted(() => ({ sessionState: { dirty: false } }));
// The model the window is bound to (empty = none), mutated per test. Hoisted so
// the model-session mock factory (hoisted above imports) returns the live object.
const { boundModelRef } = vi.hoisted(() => ({ boundModelRef: { name: '' } }));
let confirmChoice: boolean | null = false;
const showConfirmSaveModal = vi.fn((cb: (r: boolean | null) => void) => cb(confirmChoice));
//=============================================================================
vi.mock('./native-rpc.js', () => ({
  nflowRpc: (method: string, params?: unknown) => nflowRpc(method, params),
}));
vi.mock('./diagram-handlers.js', () => ({
  loadDiagram: (data: unknown) => loadDiagram(data),
}));
vi.mock('./model-session.js', () => ({
  setBoundModel: (name: string, revision?: number) => setBoundModel(name, revision),
  getBoundModelName: () => boundModelRef.name,
}));
vi.mock('./state.js', () => ({ state: sessionState }));
vi.mock('./modal-handlers.js', () => ({
  showConfirmSaveModal: (cb: (r: boolean | null) => void) => showConfirmSaveModal(cb),
}));
vi.mock('./menu-handlers.js', () => ({
  handleMenuAction: (action: string) => handleMenuAction(action),
}));
//=============================================================================
// Imported after the mocks are registered.
import { buildModelOnCanvas } from './assistant-canvas.js';
//=============================================================================
function diagramEnvelope(name: string): string {
  // Exactly the shape nflow.model.getDiagram returns: a JSON string whose
  // `content` field is the model document, itself serialized as a JSON string.
  const model = {
    format: 'nflow-2',
    name,
    blocks: [{ id: 'b1', type: 'nflow/source/constant', name: 'K', x: 0, y: 0 }],
    connections: [],
  };
  return JSON.stringify({ success: true, name, revision: 7, content: JSON.stringify(model) });
}
//=============================================================================
beforeEach(() => {
  nflowRpc.mockReset();
  loadDiagram.mockReset();
  setBoundModel.mockReset();
  handleMenuAction.mockReset();
  showConfirmSaveModal.mockClear();
  nflowRpc.mockResolvedValue({});
  loadDiagram.mockResolvedValue(undefined);
  sessionState.dirty = false;
  confirmChoice = false;
  boundModelRef.name = '';
});
afterEach(() => {
  vi.restoreAllMocks();
});
//=============================================================================
test('runs the scripted model then draws the registry diagram on the canvas', async () => {
  const script = "new_system('demo');\nadd_block('nflow/source/constant', 'demo/K', 'Value', 1);";
  nflowRpc.mockImplementation(async (method: string) => {
    if (method === 'nflow.model.getDiagram') {
      return diagramEnvelope('demo');
    }
    return {};
  });

  await buildModelOnCanvas(script);

  // The body ran through the live callback bridge, open_system/save_system aside.
  expect(nflowRpc).toHaveBeenCalledWith('nflow.callback.invoke', {
    code: expect.stringContaining("new_system('demo')"),
    blockId: '',
  });
  // The JSON-string envelope was parsed, and the model (with its block) reached
  // the loader - the empty-canvas bug was reading .success off the raw string.
  expect(loadDiagram).toHaveBeenCalledTimes(1);
  const drawn = loadDiagram.mock.calls[0][0] as { blocks?: unknown[] };
  expect(Array.isArray(drawn.blocks)).toBe(true);
  expect(drawn.blocks).toHaveLength(1);
  // The window is rebound to the built model (with its revision) so later edits
  // commit to it and Save targets it, not a stale binding.
  expect(setBoundModel).toHaveBeenCalledWith('demo', 7);
});
//=============================================================================
test('strips open_system/save_system so no extra window or file write happens', async () => {
  const script =
    "new_system('demo');\nadd_block('nflow/source/constant', 'demo/K', 'Value', 1);\n" +
    "save_system('demo');\nopen_system('demo');";
  nflowRpc.mockImplementation(async (method: string) =>
    method === 'nflow.model.getDiagram' ? diagramEnvelope('demo') : {}
  );

  await buildModelOnCanvas(script);

  const invoked = nflowRpc.mock.calls
    .filter(([method]) => method === 'nflow.callback.invoke')
    .map(([, params]) => (params as { code: string }).code)
    .join('\n');
  expect(invoked).not.toContain('open_system');
  expect(invoked).not.toContain('save_system');
  expect(loadDiagram).toHaveBeenCalledTimes(1);
});
//=============================================================================
test('a non-model snippet just runs, with no diagram fetch or canvas load', async () => {
  await buildModelOnCanvas('disp(42)');

  expect(nflowRpc).toHaveBeenCalledWith('nflow.callback.invoke', {
    code: 'disp(42)',
    blockId: '',
  });
  expect(nflowRpc).not.toHaveBeenCalledWith('nflow.model.getDiagram', expect.anything());
  expect(loadDiagram).not.toHaveBeenCalled();
});
//=============================================================================
test('rebuilds once after an already-loaded error so re-running stays safe', async () => {
  let firstBuild = true;
  nflowRpc.mockImplementation(async (method: string, params?: unknown) => {
    if (method === 'nflow.model.getDiagram') {
      return diagramEnvelope('demo');
    }
    const code = (params as { code?: string } | undefined)?.code ?? '';
    if (code.includes('new_system') && firstBuild) {
      firstBuild = false;
      throw new Error("A model named 'demo' is already loaded.");
    }
    return {};
  });

  await buildModelOnCanvas("new_system('demo');");

  const codes = nflowRpc.mock.calls
    .filter(([method]) => method === 'nflow.callback.invoke')
    .map(([, params]) => (params as { code: string }).code);
  expect(codes.some((c) => c.includes("bdclose('demo')"))).toBe(true);
  expect(loadDiagram).toHaveBeenCalledTimes(1);
});
//=============================================================================
test('a dirty canvas asks before building, and Cancel skips the build', async () => {
  sessionState.dirty = true;
  confirmChoice = null; // Cancel
  nflowRpc.mockImplementation(async (method: string) =>
    method === 'nflow.model.getDiagram' ? diagramEnvelope('demo') : {}
  );

  await buildModelOnCanvas("new_system('demo');");

  expect(showConfirmSaveModal).toHaveBeenCalledTimes(1);
  // Cancel keeps the current canvas: nothing runs, nothing is drawn.
  expect(nflowRpc).not.toHaveBeenCalled();
  expect(loadDiagram).not.toHaveBeenCalled();
});
//=============================================================================
test('a dirty canvas with Save persists first, then builds', async () => {
  sessionState.dirty = true;
  confirmChoice = true; // Save
  nflowRpc.mockImplementation(async (method: string) =>
    method === 'nflow.model.getDiagram' ? diagramEnvelope('demo') : {}
  );

  await buildModelOnCanvas("new_system('demo');");

  expect(showConfirmSaveModal).toHaveBeenCalledTimes(1);
  expect(handleMenuAction).toHaveBeenCalledWith('save');
  // Having saved, the build proceeds and the model reaches the canvas.
  expect(loadDiagram).toHaveBeenCalledTimes(1);
});
//=============================================================================
test('a build error is handed back to the assistant to self-correct', async () => {
  const requestFix = vi.fn();
  nflowRpc.mockImplementation(async (method: string, params?: unknown) => {
    const code = (params as { code?: string } | undefined)?.code ?? '';
    if (method === 'nflow.callback.invoke' && code.includes('new_system')) {
      throw new Error("Block type 'FixedHeatFlow' does not declare a parameter 'Q_flow'");
    }
    return {};
  });

  await buildModelOnCanvas("new_system('demo');", requestFix);

  // The failing build never reaches the canvas, and the engine error is handed
  // to the agent as a correction prompt that quotes it.
  expect(loadDiagram).not.toHaveBeenCalled();
  expect(requestFix).toHaveBeenCalledTimes(1);
  expect(requestFix.mock.calls[0][0]).toContain('Q_flow');
});
//=============================================================================
test('an edit without new_system updates the bound model in place, not a rebuild', async () => {
  // A model is already on the canvas; the assistant sends only the change.
  boundModelRef.name = 'demo';
  sessionState.dirty = true; // an edit must not trigger the replace confirm
  nflowRpc.mockImplementation(async (method: string) =>
    method === 'nflow.model.getDiagram' ? diagramEnvelope('demo') : {}
  );

  await buildModelOnCanvas("set_param('demo/K', 'Gain', 5);");

  const codes = nflowRpc.mock.calls
    .filter(([method]) => method === 'nflow.callback.invoke')
    .map(([, params]) => (params as { code: string }).code);
  // The change ran on the live model; nothing wiped or recreated it.
  expect(codes.some((c) => c.includes("set_param('demo/K'"))).toBe(true);
  expect(codes.some((c) => c.includes('bdclose'))).toBe(false);
  expect(codes.some((c) => c.includes('new_system'))).toBe(false);
  expect(showConfirmSaveModal).not.toHaveBeenCalled();
  // The canvas refreshed from the updated model.
  expect(nflowRpc).toHaveBeenCalledWith('nflow.model.getDiagram', { name: 'demo' });
  expect(loadDiagram).toHaveBeenCalledTimes(1);
});
//=============================================================================
