//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { test, expect } from '@playwright/test';
import { readFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { unzipSync } from 'fflate';
import { generate, $init } from '@bytecodealliance/jco-transpile/component';
//=============================================================================
const fixtures = resolve('../../modules/nflow_fmi/examples/wasm');
async function componentFixture(
  file: string,
  identifier: string,
  info: Record<string, unknown>,
  path: string
) {
  await $init;
  const archive = unzipSync(await readFile(file));
  const xml = new TextDecoder().decode(archive['modelDescription.xml']);
  const generated = generate(archive[`binaries/wasm32-wasip2/${identifier}.wasm`], {
    name: 'fmu',
    noTypescript: true,
    noNodejsCompat: true,
    instantiation: { tag: 'async' },
    base64Cutoff: 0,
  });
  return {
    path,
    token: /instantiationToken="([^"]+)"/.exec(xml)![1],
    info: { success: true, modelIdentifier: identifier, fmiVersion: '3.0', ...info },
    source: new TextDecoder().decode(generated.files.find(([name]) => name === 'fmu.js')![1]),
    files: generated.files
      .filter(([name]) => name.endsWith('.wasm'))
      .map(([name, bytes]) => [name, Array.from(bytes)]),
  };
}
//=============================================================================
test('lists the consolidated FMI diagrams and opens a bundled demo', async ({ page }) => {
  await page.goto('/nflow/index.html');
  await expect(page.locator('#svgCanvas')).toBeVisible({ timeout: 120000 });
  const catalog = await page.evaluate(async () => {
    const response = await fetch('./examples/catalog.json');
    return response.json();
  });
  const fmi = catalog.groups.find((group: { name: string }) => group.name === 'nflow:fmi');
  expect(fmi.examples).toHaveLength(11);
  expect(
    fmi.examples.every((entry: { file: string }) => entry.file.startsWith('nflow-example:/fmi/'))
  ).toBe(true);
  expect(JSON.stringify(fmi)).not.toContain('/modelica/');
  const shared = await (await page.request.get('/examples/catalog.json')).json();
  expect(shared.groups.some((group: { name: string }) => group.name === 'nflow_fmi')).toBe(false);
  expect(shared.diagnostics.some((entry: { module: string }) => entry.module === 'nflow_fmi')).toBe(
    false
  );
  await page.getByRole('button', { name: 'Examples', exact: true }).click();
  const dialog = page.getByRole('dialog', { name: 'Examples', exact: true });
  const card = dialog.locator('.example-gallery-card', {
    has: page.getByText('BouncingBall FMU', { exact: true }),
  });
  await expect(card).toBeVisible({ timeout: 30000 });
  await card.getByRole('button', { name: 'Open', exact: true }).click();
  await expect(dialog).toBeHidden();
  await expect(page.locator('.svg-block[data-block-id="fmu1"]')).toBeVisible();
  const response = await page.request.get('/nflow/examples/files/fmi/BouncingBall.fmu');
  expect(response.ok()).toBe(true);
  expect(await response.body()).toEqual(await readFile(resolve(fixtures, '../BouncingBall.fmu')));
});
//=============================================================================
for (const language of ['c', 'rust', 'wat']) {
  test(`imports and simulates the ${language} FMU in the NFlow Wasm editor`, async ({ page }) => {
    await page.goto('/nflow/index.html');
    await expect(page.locator('#svgCanvas')).toBeVisible({ timeout: 120000 });
    const model = {
      version: '1.0.0',
      format: 'nflow-2',
      name: 'FMU import regression',
      solver: 'discrete',
      runtime: '0.5',
      sampleTime: 0.1,
      variables: '',
      blocks: [
        { id: 'a', type: 'constant', x: 100, y: 160, params: { Value: 2 } },
        { id: 'b', type: 'constant', x: 100, y: 300, params: { Value: 3 } },
        {
          id: 'fmu',
          type: 'fmu',
          x: 360,
          y: 220,
          inputs: 2,
          outputs: 1,
          params: { path: '', numInputs: 2, numOutputs: 1 },
        },
        { id: 'scope', type: 'scope', x: 650, y: 220, inputs: 1, outputs: 0, params: {} },
      ],
      connections: [
        { from: 'a', to: 'fmu', fromIndex: 0, toIndex: 0 },
        { from: 'b', to: 'fmu', fromIndex: 0, toIndex: 1 },
        { from: 'fmu', to: 'scope', fromIndex: 0, toIndex: 0 },
      ],
    };
    await page.locator('#fileOpenInput').setInputFiles({
      name: 'FMU-regression.nflow',
      mimeType: 'application/json',
      buffer: Buffer.from(JSON.stringify(model)),
    });
    await expect(page.locator('.svg-block[data-block-id="fmu"]')).toBeVisible();
    await page.locator('.svg-block[data-block-id="fmu"]').click({ position: { x: 48, y: 30 } });
    const browse = page.getByRole('button', { name: 'Browse FMU…', exact: true });
    await expect(browse).toBeEnabled();
    const chooser = page.waitForEvent('filechooser');
    await browse.click();
    await (await chooser).setFiles(resolve(fixtures, `adder-${language}-fmu.fmu`));
    await expect(page.locator('.inspector-status')).toContainText('2 input(s), 1 output(s)', {
      timeout: 120000,
    });
    for (let repeat = 0; repeat < 2; repeat++) {
      const result = (await page.evaluate(async () => {
        const run = (window as unknown as { __nf_runSimulationForTest: () => Promise<unknown> })
          .__nf_runSimulationForTest;
        return await run();
      })) as { status: string; scopes: Array<{ series: number[][] }> };
      expect(result.status).toBe('done');
      const series = result.scopes[0].series;
      const values = Array.isArray(series[0]) ? series[0] : (series as unknown as number[]);
      expect(values.length).toBeGreaterThan(4);
      expect(values.slice(1).every((value) => Math.abs(value - 5) < 1e-12)).toBe(true);
    }
  });
}
//=============================================================================
test('runs the Nelson FMU bridge regression against a real component in the Worker', async ({
  page,
}) => {
  const model = await componentFixture(
    resolve(fixtures, 'adder-c-fmu.fmu'),
    'adder_c_fmu',
    {
      coSimulation: true,
      modelExchange: false,
      variables: [
        { name: 'input_a', valueReference: 1, causality: 'input', dataType: 'Float64' },
        { name: 'input_b', valueReference: 2, causality: 'input', dataType: 'Float64' },
        { name: 'sum', valueReference: 3, causality: 'output', dataType: 'Float64' },
      ],
    },
    '/workspace/fmus/regression.fmu'
  );
  const reference = await componentFixture(
    resolve(fixtures, '../Dahlquist.fmu'),
    'Dahlquist',
    {
      coSimulation: true,
      modelExchange: true,
      numberOfContinuousStates: 1,
      numberOfEventIndicators: 0,
      variables: [
        { name: 'x', valueReference: 1, causality: 'output', dataType: 'Float64' },
        {
          name: 'k',
          valueReference: 3,
          causality: 'parameter',
          dataType: 'Float64',
          startValue: 1,
        },
      ],
    },
    '/workspace/fmus/dahlquist.fmu'
  );
  const code = (
    await Promise.all(
      [
        '../../modules/nflow_blocks/tests/test_nflow_wasm_fmu_cs.m',
        '../../modules/nflow_fmi/tests/test_nflow_wasm_module.m',
      ].map((path) => readFile(resolve(path), 'utf8'))
    )
  ).join('\n');
  await page.route('**/nelson-worker-client.mjs', (route) =>
    route.fulfill({
      path: resolve('../../tools/wasm/nelson-worker-client.mjs'),
      contentType: 'text/javascript',
    })
  );
  await page.goto('/nflow/index.html');
  await expect(page.locator('#svgCanvas')).toBeVisible({ timeout: 120000 });
  const result = await page.evaluate(
    async ({ model, reference, code }) => {
      const url = location.origin;
      const { createNelsonWorkerClient } = await import(
        /* @vite-ignore */ `${url}/nelson-worker-client.mjs`
      );
      const runtimeCapabilities = await (await fetch(`${url}/capabilities.json`)).json();
      const client = await createNelsonWorkerClient({
        workerUrl: `${url}/nelson-worker.mjs`,
        moduleUrl: `${url}/nelson-portable.mjs`,
        runtimeCapabilities,
      });
      try {
        for (const component of [model, reference]) {
          await client.registerNFlowFmu({
            ...component,
            files: component.files.map(([name, bytes]) => [
              name,
              new Uint8Array(bytes as number[]),
            ]),
            wasiUrl: `${url}/nflow/fmu/wasi.js`,
          });
        }
        let stdout = '';
        const result = await client.evaluate(
          `setenv('NELSON_WASM_FMU_PATH', '${model.path}'); setenv('NELSON_WASM_FMU_ME_PATH', '${reference.path}');\n${code}\ndisp('FMU_BRIDGE_OK');`,
          (output: { stream?: string; text?: string }) => {
            if (output.stream === 'stdout') stdout += output.text || '';
          }
        );
        return { ...result, stdout: stdout + (result.stdout || '') };
      } finally {
        client.terminate();
      }
    },
    { model, reference, code }
  );
  expect(result.stdout).toContain('FMU_BRIDGE_OK');
});
//=============================================================================
test('opens and runs the three packaged FMU demos from Examples', async ({ page }) => {
  await page.goto('/nflow/index.html');
  await expect(page.locator('#svgCanvas')).toBeVisible({ timeout: 120000 });
  for (const language of ['C', 'Rust', 'WAT']) {
    await page.getByRole('button', { name: 'Examples', exact: true }).click();
    const dialog = page.getByRole('dialog', { name: 'Examples', exact: true });
    const card = dialog.locator('.example-gallery-card', {
      hasText: `FMU Wasm - Adder (${language})`,
    });
    await expect(card).toBeVisible({ timeout: 30000 });
    await card.getByRole('button', { name: 'Open', exact: true }).click();
    await expect(dialog).toBeHidden();
    await expect
      .poll(() =>
        page.evaluate(() =>
          (window as unknown as { __nf_serializeDiagram: () => string }).__nf_serializeDiagram()
        )
      )
      .toContain(`FMU Wasm - Adder (${language})`);
    const result = (await page.evaluate(() =>
      (
        window as unknown as { __nf_runSimulationForTest: () => Promise<unknown> }
      ).__nf_runSimulationForTest()
    )) as { status: string; signals: { fmu: number[] } };
    expect(result.status).toBe('done');
    expect(result.signals.fmu).toEqual([5]);
  }
});
//=============================================================================
for (const [filename, title] of [
  ['bouncingball_fmu.nflow', 'BouncingBall FMU'],
  ['dahlquist_fmu.nflow', 'Dahlquist FMU'],
  ['feedthrough_fmu.nflow', 'Feedthrough FMU'],
  ['statespace_fmu.nflow', 'StateSpace FMU'],
  ['vanderpol_fmu.nflow', 'VanDerPol FMU'],
  ['bouncingball_me_fmu.nflow', 'BouncingBall FMU (Model Exchange)'],
  ['vanderpol_me_fmu.nflow', 'VanDerPol FMU (Model Exchange)'],
  ['Acausal_FMU_RLC_Demo.nflow', 'Acausal RLC driven by FMU'],
]) {
  test(`simulates the packaged reference demo ${title}`, async ({ page }) => {
    await page.goto('/nflow/index.html');
    await expect(page.locator('#svgCanvas')).toBeVisible({ timeout: 120000 });
    await page.getByRole('button', { name: 'Examples', exact: true }).click();
    const dialog = page.getByRole('dialog', { name: 'Examples', exact: true });
    const card = dialog.locator('.example-gallery-card', {
      has: page.getByText(title, { exact: true }),
    });
    await expect(card).toBeVisible({ timeout: 30000 });
    await card.getByRole('button', { name: 'Open', exact: true }).click();
    await expect(dialog).toBeHidden();
    const result = await page.evaluate(() =>
      (
        window as unknown as {
          __nf_runSimulationForTest: () => Promise<{
            status: string;
            scopes: Array<{ series: number[][] }>;
            diagnostics?: unknown;
          }>;
        }
      ).__nf_runSimulationForTest()
    );
    expect(result.status, JSON.stringify(result)).toBe('done');
    expect(result.scopes.length).toBeGreaterThan(0);
    // Scope reserves channels for unwired ports, serialized as null samples.
    const channels = result.scopes
      .flatMap((scope) => scope.series ?? [])
      .filter((series) => series.some((value) => value !== null));
    const values = channels.flat();
    expect(values.length).toBeGreaterThan(20);
    expect(values.every(Number.isFinite)).toBe(true);
    expect(Math.max(...values) - Math.min(...values)).toBeGreaterThan(0.01);
    if (filename === 'dahlquist_fmu.nflow') {
      expect(values[0]).toBe(1);
      expect(values.at(-1)).toBeGreaterThan(0);
      expect(values.at(-1)).toBeLessThan(0.001);
    }
    if (filename.startsWith('bouncingball')) {
      const height = result.scopes[0].series[0];
      expect(Math.min(...height)).toBeGreaterThan(-0.05);
      const velocity = result.scopes[1].series[0];
      expect(Math.max(...velocity)).toBeGreaterThan(1);
      expect(Math.min(...velocity)).toBeLessThan(-1);
    }
  });
}
//=============================================================================
