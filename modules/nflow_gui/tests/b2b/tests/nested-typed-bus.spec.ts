//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { test, expect } from '@playwright/test';

test('nested typed bus survives editor serialization and simulates on the native engine', async ({
  page,
}) => {
  const model = {
    format: 'nflow-2',
    name: 'Nested typed bus',
    sampleTime: 0.01,
    duration: 0.1,
    busTypes: [
      {
        name: 'EnvironmentBus',
        nonvirtual: true,
        members: [
          { name: 'gravity', type: 'double' },
          { name: 'radius', type: 'double' },
        ],
      },
      {
        name: 'VehicleBus',
        nonvirtual: true,
        members: [
          { name: 'environment', bus: 'EnvironmentBus' },
          { name: 'count', type: 'int64' },
        ],
      },
    ],
    blocks: [
      { id: 'gravity', type: 'constant', x: 80, y: 80, params: { Value: 9.80665 } },
      { id: 'radius', type: 'constant', x: 80, y: 220, params: { Value: 6378137 } },
      { id: 'count', type: 'constant', x: 300, y: 350, params: { Value: 7, OutDataType: 'int64' } },
      {
        id: 'environment',
        type: 'busCreator',
        x: 300,
        y: 120,
        params: { BusType: 'EnvironmentBus', NonVirtual: true, MemberNames: ['gravity', 'radius'] },
      },
      {
        id: 'vehicle',
        type: 'busCreator',
        x: 500,
        y: 220,
        params: { BusType: 'VehicleBus', NonVirtual: true, MemberNames: ['environment', 'count'] },
      },
      {
        id: 'select',
        type: 'busSelector',
        x: 700,
        y: 220,
        params: { SelectedSignals: ['environment.gravity', 'count'] },
      },
      { id: 'gravity_scope', type: 'scope', x: 900, y: 80, params: {} },
      { id: 'count_scope', type: 'scope', x: 900, y: 350, params: {} },
    ],
    connections: [
      { from: 'gravity', to: 'environment', toIndex: 0 },
      { from: 'radius', to: 'environment', toIndex: 1 },
      { from: 'environment', to: 'vehicle', toIndex: 0 },
      { from: 'count', to: 'vehicle', toIndex: 1 },
      { from: 'vehicle', to: 'select' },
      { from: 'select', fromIndex: 0, to: 'gravity_scope' },
      { from: 'select', fromIndex: 1, to: 'count_scope' },
    ],
  };
  await page.goto('/nflow.html');
  await page.waitForSelector('#svgCanvas');
  await page.locator('#fileOpenInput').setInputFiles({
    name: 'nested-bus.nflow',
    mimeType: 'application/json',
    buffer: Buffer.from(JSON.stringify(model)),
  });
  await page.waitForSelector('#blockLayer > [data-block-id="select"]');
  const result = await page.evaluate(async () => {
    const host = window as unknown as {
      __nf_serializeDiagram: () => string;
      __nf_runSimulationForTest: () => Promise<string>;
    };
    const saved = JSON.parse(host.__nf_serializeDiagram());
    const simulation = JSON.parse(await host.__nf_runSimulationForTest());
    return { saved, simulation };
  });
  expect(result.saved.busTypes.map((bus: { name: string }) => bus.name)).toEqual([
    'EnvironmentBus',
    'VehicleBus',
  ]);
  expect(result.simulation.status, result.simulation.reason).toBe('done');
  expect(result.simulation.signals.select).toEqual(
    expect.arrayContaining([expect.closeTo(9.80665), 7])
  );
});

test('an external nested-bus example simulates on the native engine', async ({ page }) => {
  const example = process.env.NFLOW_NESTED_BUS_EXAMPLE;
  test.skip(!example, 'Set NFLOW_NESTED_BUS_EXAMPLE to validate a local example');
  await page.goto('/nflow.html');
  await page.waitForSelector('#svgCanvas');
  await page.locator('#fileOpenInput').setInputFiles(example!);
  await page.waitForSelector('#blockLayer > [data-block-id="select"]');
  const result = await page.evaluate(async () => {
    const host = window as unknown as {
      __nf_serializeDiagram: () => string;
      __nf_runSimulationForTest: () => Promise<string>;
    };
    return {
      saved: JSON.parse(host.__nf_serializeDiagram()),
      simulation: JSON.parse(await host.__nf_runSimulationForTest()),
    };
  });
  expect(result.saved.busTypes.map((bus: { name: string }) => bus.name)).toEqual([
    'EnvironmentBus',
    'VehicleBus',
  ]);
  expect(result.simulation.status, result.simulation.reason).toBe('done');
  expect(result.simulation.signals.select).toEqual(
    expect.arrayContaining([expect.closeTo(9.80665), 7])
  );
});
