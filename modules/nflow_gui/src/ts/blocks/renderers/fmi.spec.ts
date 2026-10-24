//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { describe, it, expect, vi, beforeEach } from 'vitest';
//=============================================================================
const { openFilePicker, canUseNFlowRpc, nflowRpc } = vi.hoisted(() => ({
  openFilePicker: vi.fn(),
  canUseNFlowRpc: vi.fn(),
  nflowRpc: vi.fn(),
}));
//=============================================================================
vi.mock('../../app/file-dialogs.js', () => ({ openFilePicker }));
vi.mock('../../app/native-rpc.js', () => ({ canUseNFlowRpc, nflowRpc }));
import { renderFmi } from './fmi.js';
//=============================================================================
const VANDERPOL = JSON.stringify({
  success: true,
  modelIdentifier: 'VanDerPol',
  fmiVersion: '3.0',
  coSimulation: true,
  variables: [
    { name: 'x0', valueReference: 1, causality: 'output', dataType: 'Float64' },
    { name: 'x1', valueReference: 3, causality: 'output', dataType: 'Float64' },
  ],
});
//=============================================================================
function makeDeps() {
  const inspectorBody = document.createElement('div');
  return {
    deps: {
      inspectorBody,
      callUpdate: vi.fn(),
      callResize: vi.fn(),
      renderInspector: vi.fn(),
      signalDiagramChanged: vi.fn(),
    } as any,
    inspectorBody,
  };
}
//=============================================================================
describe('renderFmi', () => {
  beforeEach(() => {
    openFilePicker.mockReset();
    canUseNFlowRpc.mockReset().mockReturnValue(true);
    nflowRpc.mockReset();
  });

  it('ignores non-FMU blocks', () => {
    const { deps } = makeDeps();
    expect(renderFmi({ type: 'constant', params: {} } as any, deps)).toBe(false);
  });

  it('renders a Browse FMU button for the fmu block', () => {
    const { deps, inspectorBody } = makeDeps();
    const handled = renderFmi({ type: 'fmu', params: {} } as any, deps);
    expect(handled).toBe(true);
    const button = inspectorBody.querySelector('button');
    expect(button?.textContent).toContain('Browse FMU');
    expect(button?.disabled).toBe(false);
  });

  it.each(['fmu', 'fmuMe'])('explains why %s cannot browse FMUs in Wasm', async (type) => {
    const rpc = await vi.importActual<typeof import('../../app/native-rpc.js')>(
      '../../app/native-rpc.js'
    );
    const restore = rpc.installWasmWorkerHost({ simulateNFlow: vi.fn(), interrupt: vi.fn() });
    canUseNFlowRpc.mockImplementation(rpc.canUseNFlowRpc);
    try {
      const { deps, inspectorBody } = makeDeps();
      const block = { type, params: { path: '/models/existing.fmu' } };
      renderFmi(block as any, deps);
      const button = inspectorBody.querySelector('button') as HTMLButtonElement;
      expect(button.disabled).toBe(true);
      expect(inspectorBody.textContent).toContain('desktop Nelson');
      expect(inspectorBody.textContent).toContain('Wasm component');
      button.click();
      expect(openFilePicker).not.toHaveBeenCalled();
      expect(nflowRpc).not.toHaveBeenCalled();
      expect(block.params).toEqual({ path: '/models/existing.fmu' });
    } finally {
      restore();
    }
  });

  it('inspects the picked FMU and rebuilds the block ports', async () => {
    openFilePicker.mockResolvedValue('D:/fmus/VanDerPol.fmu');
    nflowRpc.mockResolvedValue(VANDERPOL);
    const { deps, inspectorBody } = makeDeps();
    const block: any = { type: 'fmu', params: {} };
    renderFmi(block, deps);
    const button = inspectorBody.querySelector('button') as HTMLButtonElement;
    button.click();
    await vi.waitFor(() => expect(block.params.numOutputs).toBe(2));
    expect(block.params.path).toBe('D:/fmus/VanDerPol.fmu');
    expect(block.params.outputNames).toEqual(['x0', 'x1']);
    expect(deps.callUpdate).toHaveBeenCalledWith(block);
  });

  it('warns when the FMU does not support Co-Simulation', async () => {
    openFilePicker.mockResolvedValue('D:/fmus/MeOnly.fmu');
    nflowRpc.mockResolvedValue(
      JSON.stringify({ success: true, coSimulation: false, fmiVersion: '3.0', variables: [] })
    );
    const { deps, inspectorBody } = makeDeps();
    renderFmi({ type: 'fmu', params: {} } as any, deps);
    (inspectorBody.querySelector('button') as HTMLButtonElement).click();
    await vi.waitFor(() =>
      expect(inspectorBody.textContent).toContain('does not provide the Co-Simulation')
    );
  });

  it('does nothing when the picker is canceled', async () => {
    openFilePicker.mockResolvedValue(null);
    const { deps, inspectorBody } = makeDeps();
    const block: any = { type: 'fmu', params: {} };
    renderFmi(block, deps);
    (inspectorBody.querySelector('button') as HTMLButtonElement).click();
    await Promise.resolve();
    expect(nflowRpc).not.toHaveBeenCalled();
    expect(block.params.numOutputs).toBeUndefined();
  });
});
//=============================================================================
