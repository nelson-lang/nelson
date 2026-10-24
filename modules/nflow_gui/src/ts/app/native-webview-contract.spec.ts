//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { existsSync, readFileSync } from 'node:fs';
import path from 'node:path';
//=============================================================================
const cwd = path.resolve(process.cwd());
const moduleDir = path.basename(cwd) === 'nflow_gui' ? cwd : path.join(cwd, 'modules', 'nflow_gui');
const repoDir = path.resolve(moduleDir, '..', '..');
const webviewProjectGuid = '{585FA4C3-C0BE-4FE9-B4D3-476FE810CEB8}';
const nflowGuiProjectGuid = '{F8F4AB1A-3E4B-4207-911C-1304618410AA}';
test('nflow_gui depends on modules/webview for WebUI', () => {
  const cmake = readFileSync(path.join(moduleDir, 'CMakeLists.txt'), 'utf8');
  const helpers = readFileSync(path.join(moduleDir, 'src/cpp/NFlowHelpers.cpp'), 'utf8');
  const bootstrapHtml = readFileSync(
    path.join(moduleDir, 'src/cpp/NFlowBootstrapHtml.cpp'),
    'utf8'
  );
  const nflowGuiBuiltin = readFileSync(
    path.join(moduleDir, 'builtin/cpp/nflow_guiBuiltin.cpp'),
    'utf8'
  );
  const simEngineRuntime = readFileSync(
    path.join(repoDir, 'modules/nflow_engine/src/cpp/SimEngineRuntime.cpp'),
    'utf8'
  );
  const rpcBindings = readFileSync(path.join(moduleDir, 'src/cpp/nflow_rpc_bindings.cpp'), 'utf8');
  const nflowGuiProject = readFileSync(path.join(moduleDir, 'src/c/nlsNflow_gui.vcxproj'), 'utf8');
  const nflowGuiBuiltinProject = readFileSync(
    path.join(moduleDir, 'builtin/c/nlsNflow_gui_builtin.vcxproj'),
    'utf8'
  );
  const webviewProject = readFileSync(
    path.join(repoDir, 'modules/webview/src/c/nlsWebview.vcxproj'),
    'utf8'
  );
  const solution = readFileSync(path.join(repoDir, 'NelSon.sln'), 'utf8');
  const nflowGuiSolutionBlock = solution.slice(
    solution.indexOf('= "nlsNflow_gui"'),
    solution.indexOf('= "nlsNflow_gui_builtin"')
  );
  const nflowGuiBuiltinSolutionBlock = solution.slice(
    solution.indexOf('= "nlsNflow_gui_builtin"'),
    solution.indexOf('Global')
  );

  expect(cmake).toContain('nlsWebview');
  expect(cmake).toContain('../webview/src/include');
  expect(cmake).toContain('nflow_gui must use modules/webview for WebUI');
  expect(helpers).not.toContain('escape_json_string');
  expect(bootstrapHtml).toContain('#include "WebUiHelpers.hpp"');
  expect(bootstrapHtml).toContain('escapeWebUiJsonString');
  expect(bootstrapHtml).toContain('webUiSetSessionStorageItemScript');
  expect(bootstrapHtml).toContain('webUiSetLocalStorageItemScript');
  expect(nflowGuiBuiltin).toContain('WEB_SIM_PROGRESS_CHUNK_SIZE = 250');
  expect(nflowGuiBuiltin).toContain('resolvedJson, WEB_SIM_PROGRESS_CHUNK_SIZE');
  // Poll delivery accumulates delta frames in a BOUNDED queue: replacing a
  // pending frame (the old clear()+push_back pattern) would lose samples. The
  // remaining clear() is the run-start reset in startSimulationForEntry.
  expect(nflowGuiBuiltin).toContain('MAX_PENDING_PARTIALS = 64');
  expect(nflowGuiBuiltin).toContain('simPartialResults.clear()');
  expect(nflowGuiBuiltin).toContain('findNelsonApplicationIconPath');
  expect(nflowGuiBuiltin).toContain('set_icon_file(windowIconFile)');
  expect(nflowGuiBuiltin).toContain('nflow.png');
  expect(nflowGuiBuiltin).toContain('nflow.ico');
  // Progress cadence is wall-clock scheduled, no step-count chunking.
  expect(simEngineRuntime).toContain('publishSimProgress');
  expect(simEngineRuntime).toContain('progressEnabled = chunkSize > 0');
  expect(simEngineRuntime).not.toContain('realTimeChunkSize');
  expect(rpcBindings).toContain('#include "WebUiHelpers.hpp"');
  expect(rpcBindings).toContain('escapeWebUiJsonString');
  expect(rpcBindings).toContain('POLL_RESULT_INLINE_THRESHOLD = 32 * 1024');
  expect(rpcBindings).toContain('BLOB_CHUNK_SIZE = 32 * 1024');
  expect(rpcBindings).toContain('"application/json", POLL_RESULT_INLINE_THRESHOLD');
  expect(rpcBindings).not.toContain('escape_json_string');
  expect(nflowGuiProject).toContain('modules/webview/src/include');
  expect(nflowGuiProject).toContain('nlsWebview.vcxproj');
  expect(nflowGuiProject).toContain('..\\include\\webui_bindings.hpp');
  expect(nflowGuiProject).toContain('..\\include\\nflow_rpc_codegen.hpp');
  expect(nflowGuiBuiltinProject).toContain('modules/webview/src/include');
  expect(nflowGuiBuiltinProject).toContain('nlsWebview.vcxproj');
  expect(nflowGuiBuiltinProject).toContain('..\\..\\src\\cpp\\nflow_rpc_bindings.cpp');
  expect(nflowGuiBuiltinProject).toContain('..\\..\\src\\cpp\\nflow_rpc_codegen.cpp');
  expect(webviewProject).toContain('..\\cpp\\WebUiHelpers.cpp');
  expect(webviewProject).toContain('..\\include\\WebUiHelpers.hpp');
  expect(nflowGuiSolutionBlock).toContain(webviewProjectGuid);
  expect(nflowGuiBuiltinSolutionBlock).toContain(webviewProjectGuid);
  expect(nflowGuiBuiltinSolutionBlock).toContain(nflowGuiProjectGuid);
  expect(existsSync(path.join(moduleDir, 'src/cpp/webui'))).toBe(false);
});
//=============================================================================
test('async simulation publishes its final result before running StopFcn', () => {
  const source = readFileSync(path.join(moduleDir, 'builtin/cpp/nflow_guiBuiltin.cpp'), 'utf8');
  const asyncStart = source.indexOf('simulationWorkers.start');
  const pollFinal = source.indexOf('entry->simResultReady = true;', asyncStart);
  const pushFinal = source.indexOf("window.__nelsonPush('nflow.sim.final'", asyncStart);
  const stopFcn = source.indexOf('diagramJson, result, stopFcnError);', asyncStart);

  expect(asyncStart).toBeGreaterThanOrEqual(0);
  expect(pollFinal).toBeGreaterThan(asyncStart);
  expect(pushFinal).toBeGreaterThan(asyncStart);
  expect(stopFcn).toBeGreaterThan(pollFinal);
  expect(stopFcn).toBeGreaterThan(pushFinal);
});
//=============================================================================
test('Qt prompt exposes interpreter idle state to the NFlow bridge', () => {
  const terminal = readFileSync(path.join(repoDir, 'modules/gui/src/cpp/QtTerminal.cpp'), 'utf8');
  const bridge = readFileSync(
    path.join(repoDir, 'modules/nflow_engine/builtin/cpp/NFlowNelsonBridge.cpp'),
    'utf8'
  );
  const taskQueue = readFileSync(
    path.join(repoDir, 'modules/interpreter/src/cpp/MainThreadTaskQueue.cpp'),
    'utf8'
  );

  expect(terminal).toContain('taskQueue->setInterpreterIdle(true);');
  expect(terminal).toContain('~IdleReset() { queue->setInterpreterIdle(false); }');
  expect(terminal).toContain('taskQueue->processTasks(eval);');
  expect(bridge).toContain('runNowIfIdleOrQueue(');
  expect(taskQueue).toContain('MainThreadTaskQueue::getInstance()');
});
//=============================================================================
