//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Inspector panel for the FMU block: a "Browse FMU..." button that picks a
// .fmu, inspects it through the nflow.fmi3.inspect RPC, and sets one output
// port per FMU output. The heavy lifting lives in libraries/fmi/fmu-inspect.
//=============================================================================
import type { Block } from '../inspector.js';
import type { BlockPanelDeps } from '../inspector-block.js';
import { registerRenderer } from '../inspector-registry.js';
import { browseAndInspectFmu, recomputeFmuPortWidths } from '../../libraries/fmi/fmu-inspect.js';
import { openFilePicker } from '../../app/file-dialogs.js';
import { canUseNFlowRpc, nflowRpc } from '../../app/native-rpc.js';
//=============================================================================
export function renderFmi(block: Block, deps: BlockPanelDeps): boolean {
  const blockType = String(block.type);
  // Both FMU block flavours share the Browse/inspect inspector: "fmu" is the
  // Co-Simulation block (owns its solver), "fmuMe" the Model Exchange block
  // (its states join the diagram's global solver).
  const isMe = blockType === 'fmuMe';
  if (blockType !== 'fmu' && !isMe) {
    return false;
  }
  const body = deps.inspectorBody;
  if (!body) {
    return true;
  }
  body.innerHTML = '';
  const params = block.params;

  const pathRow = document.createElement('div');
  pathRow.className = 'inspector-row';
  const pathLabel = document.createElement('label');
  pathLabel.textContent = 'FMU file';
  const pathInput = document.createElement('input');
  pathInput.type = 'text';
  pathInput.value = String(params.path ?? '');
  pathInput.placeholder = 'path to a .fmu archive';
  pathInput.addEventListener('input', () => {
    params.path = pathInput.value;
    deps.signalDiagramChanged?.();
  });
  pathRow.appendChild(pathLabel);
  pathRow.appendChild(pathInput);
  body.appendChild(pathRow);

  const status = document.createElement('div');
  status.className = 'inspector-status';

  const ports = document.createElement('div');
  ports.className = 'inspector-note';
  const renderPorts = () => {
    const inNames = Array.isArray(params.inputNames) ? (params.inputNames as string[]) : [];
    const outNames = Array.isArray(params.outputNames) ? (params.outputNames as string[]) : [];
    const lines: string[] = [];
    if (inNames.length) lines.push(`Inputs: ${inNames.join(', ')}`);
    if (outNames.length) lines.push(`Outputs: ${outNames.join(', ')}`);
    ports.textContent = lines.join('  |  ');
  };
  renderPorts();

  const btn = document.createElement('button');
  btn.type = 'button';
  btn.className = 'inspector-button';
  btn.textContent = 'Browse FMU…';
  const canInspect = canUseNFlowRpc('nflow.fmi3.inspect');
  if (!canInspect) {
    status.textContent =
      'This FMU interface is unavailable in this session. Browser support requires an FMI 3.0 Wasm component. Native binaries require desktop Nelson.';
    btn.disabled = true;
    btn.title = status.textContent;
  }
  btn.addEventListener('click', async () => {
    if (!canInspect) return;
    btn.disabled = true;
    status.textContent = 'Inspecting FMU…';
    try {
      const result = await browseAndInspectFmu(block, {
        pickFmuFile: () =>
          openFilePicker({
            mode: 'open',
            title: 'Open FMU',
            defaultPath: String(params.path || '.'),
            filters: ['fmu'],
          }),
        rpc: (method, p) => nflowRpc<string>(method, p),
      });
      if (result.canceled) {
        status.textContent = '';
      } else if (result.info?.ok) {
        // Ports were written into params; rebuild the block geometry.
        deps.callUpdate(block);
        deps.signalDiagramChanged?.();
        pathInput.value = String(params.path ?? '');
        renderPorts();
        const hasInterface = isMe ? result.info.modelExchange : result.info.coSimulation;
        if (!hasInterface) {
          status.textContent = isMe
            ? 'Warning: this FMU does not provide the Model Exchange interface and cannot be simulated.'
            : 'Warning: this FMU does not provide the Co-Simulation interface and cannot be simulated.';
        } else {
          status.textContent = `Loaded ${result.info.modelIdentifier ?? 'FMU'} (FMI ${
            result.info.fmiVersion ?? '?'
          }): ${result.info.inputs.length} input(s), ${result.info.outputs.length} output(s)`;
        }
      } else {
        status.textContent = result.info?.error ?? 'FMU inspection failed.';
      }
    } catch (err) {
      status.textContent = (err as Error)?.message ?? String(err);
    } finally {
      btn.disabled = false;
    }
  });

  body.appendChild(btn);
  body.appendChild(status);
  body.appendChild(ports);

  renderFmiParameters(block, deps);
  return true;
}
//=============================================================================
function renderFmiParameters(block: Block, deps: BlockPanelDeps): void {
  const body = deps.inspectorBody;
  if (!body) return;
  const params = block.params;
  // Editable tunable parameters (params.parameterValues maps name -> value).
  const paramNames: string[] = Array.isArray(params.parameterNames)
    ? (params.parameterNames as string[])
    : [];
  const paramDescs: string[] = Array.isArray(params.parameterDescriptions)
    ? (params.parameterDescriptions as string[])
    : [];
  if (paramNames.length) {
    if (typeof params.parameterValues !== 'object' || params.parameterValues === null) {
      params.parameterValues = {};
    }
    const values = params.parameterValues as Record<string, unknown>;
    const structural: boolean[] = Array.isArray(params.parameterStructural)
      ? (params.parameterStructural as boolean[])
      : [];
    const heading = document.createElement('div');
    heading.className = 'inspector-subheading';
    heading.textContent = 'Parameters';
    body.appendChild(heading);
    paramNames.forEach((name, i) => {
      const row = document.createElement('div');
      row.className = 'inspector-row';
      const label = document.createElement('label');
      label.textContent = structural[i] === true ? name + ' (structural)' : name;
      if (paramDescs[i]) {
        label.title = paramDescs[i];
      }
      const input = document.createElement('input');
      input.type = 'number';
      input.step = 'any';
      input.value = String(values[name] ?? '');
      input.addEventListener('input', () => {
        const num = Number(input.value);
        if (Number.isFinite(num)) {
          values[name] = num;
          // A structural-parameter override can resize array ports whose
          // dimension is bound to it.
          if (structural[i] === true && recomputeFmuPortWidths(block)) {
            deps.callUpdate?.(block);
          }
          deps.signalDiagramChanged?.();
        }
      });
      row.appendChild(label);
      row.appendChild(input);
      body.appendChild(row);
    });
  }
}
//=============================================================================
try {
  registerRenderer(renderFmi);
} catch (e) {
  void e;
}
//=============================================================================
