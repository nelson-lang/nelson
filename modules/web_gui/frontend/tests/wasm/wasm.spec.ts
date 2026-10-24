//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { inflateSync } from 'node:zlib';
import { expect, test, type Locator, type Page } from '@playwright/test';
import { showFigureBars } from '../figureBars';
//=============================================================================
type ImageStats = {
  opaque: number;
  bright: number;
  colors: number;
};
//=============================================================================
function paeth(left: number, up: number, upLeft: number): number {
  const estimate = left + up - upLeft;
  const leftDistance = Math.abs(estimate - left);
  const upDistance = Math.abs(estimate - up);
  const upLeftDistance = Math.abs(estimate - upLeft);
  if (leftDistance <= upDistance && leftDistance <= upLeftDistance) return left;
  if (upDistance <= upLeftDistance) return up;
  return upLeft;
}
//=============================================================================
function decodePng(buffer: Buffer): { width: number; height: number; rgba: Uint8Array } {
  const signature = '89504e470d0a1a0a';
  if (buffer.subarray(0, 8).toString('hex') !== signature) {
    throw new Error('Unexpected canvas screenshot format.');
  }

  let offset = 8;
  let width = 0;
  let height = 0;
  let colorType = 0;
  const idat: Buffer[] = [];
  while (offset < buffer.length) {
    const length = buffer.readUInt32BE(offset);
    const type = buffer.subarray(offset + 4, offset + 8).toString('ascii');
    const data = buffer.subarray(offset + 8, offset + 8 + length);
    offset += 12 + length;
    if (type === 'IHDR') {
      width = data.readUInt32BE(0);
      height = data.readUInt32BE(4);
      const bitDepth = data[8];
      colorType = data[9];
      if (bitDepth !== 8 || (colorType !== 2 && colorType !== 6)) {
        throw new Error(`Unsupported PNG format: bitDepth=${bitDepth}, colorType=${colorType}`);
      }
    } else if (type === 'IDAT') {
      idat.push(data);
    } else if (type === 'IEND') {
      break;
    }
  }

  const channels = colorType === 6 ? 4 : 3;
  const rowSize = width * channels;
  const inflated = inflateSync(Buffer.concat(idat));
  const raw = new Uint8Array(height * rowSize);
  let inputOffset = 0;
  for (let row = 0; row < height; row += 1) {
    const filter = inflated[inputOffset++];
    const rowOffset = row * rowSize;
    const previousOffset = rowOffset - rowSize;
    for (let column = 0; column < rowSize; column += 1) {
      const value = inflated[inputOffset++];
      const left = column >= channels ? raw[rowOffset + column - channels] : 0;
      const up = row > 0 ? raw[previousOffset + column] : 0;
      const upLeft = row > 0 && column >= channels ? raw[previousOffset + column - channels] : 0;
      switch (filter) {
        case 0:
          raw[rowOffset + column] = value;
          break;
        case 1:
          raw[rowOffset + column] = (value + left) & 0xff;
          break;
        case 2:
          raw[rowOffset + column] = (value + up) & 0xff;
          break;
        case 3:
          raw[rowOffset + column] = (value + Math.floor((left + up) / 2)) & 0xff;
          break;
        case 4:
          raw[rowOffset + column] = (value + paeth(left, up, upLeft)) & 0xff;
          break;
        default:
          throw new Error(`Unsupported PNG filter: ${filter}`);
      }
    }
  }

  const rgba = new Uint8Array(width * height * 4);
  for (let source = 0, target = 0; source < raw.length; source += channels, target += 4) {
    rgba[target] = raw[source];
    rgba[target + 1] = raw[source + 1];
    rgba[target + 2] = raw[source + 2];
    rgba[target + 3] = channels === 4 ? raw[source + 3] : 255;
  }
  return { width, height, rgba };
}
//=============================================================================
async function canvasFingerprint(canvas: Locator): Promise<string> {
  return (await canvas.screenshot()).toString('base64');
}
//=============================================================================
async function canvasImageStats(canvas: Locator, stride: number): Promise<ImageStats> {
  const { rgba } = decodePng(await canvas.screenshot());
  let opaque = 0;
  let bright = 0;
  const colors = new Set<string>();
  for (let index = 0; index < rgba.length; index += stride * 4) {
    const red = rgba[index];
    const green = rgba[index + 1];
    const blue = rgba[index + 2];
    const alpha = rgba[index + 3];
    if (alpha > 0) opaque += 1;
    if (Math.max(red, green, blue) > 48) bright += 1;
    colors.add(`${red},${green},${blue},${alpha}`);
  }
  return { opaque, bright, colors: colors.size };
}
//=============================================================================
async function expectFigureCountAtLeast(page: Page, minimum: number): Promise<void> {
  await expect
    .poll(async () => page.getByRole('tab', { name: /^Figure \d+$/ }).count(), { timeout: 60_000 })
    .toBeGreaterThanOrEqual(minimum);
}
//=============================================================================
async function openRuntime(page: Page): Promise<void> {
  await page.goto('/');
  await expect(page.getByRole('region', { name: 'Command Window', exact: true })).toBeVisible({
    timeout: 120_000,
  });
  await expect(page.getByRole('region', { name: 'Command Window transcript' })).toContainText(
    'connected: WebAssembly Worker vportable',
    { timeout: 120_000 }
  );
}
//=============================================================================
// A figure tab is titled "Figure N" until its creating command finishes;
// afterwards the figure Name (NumberTitle off) replaces it. Demos that keep
// running are seen under the first form, finished demos under the second.
function figureTab(page: Page, name: string, index = 1): Locator {
  const escaped = name.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
  return page.getByRole('tab', { name: new RegExp(`^(Figure ${index}|${escaped})$`) });
}
//=============================================================================
async function run(page: Page, command: string, timeout = 60_000): Promise<void> {
  await page.getByRole('tab', { name: 'Command Window' }).click();
  const input = page.getByRole('textbox', { name: 'Command input' });
  const readyAfterBusy = page.evaluate(
    (limit) =>
      new Promise<void>((resolve, reject) => {
        const state = document.querySelector('.statusbar__state');
        if (!state) {
          reject(new Error('Status bar state is missing'));
          return;
        }
        let sawBusy = state.textContent?.includes('Busy') ?? false;
        const timeout = window.setTimeout(() => {
          observer.disconnect();
          reject(new Error('Evaluation did not complete'));
        }, limit);
        const observer = new MutationObserver(() => {
          sawBusy ||= state.textContent?.includes('Busy') ?? false;
          if (sawBusy && state.textContent?.includes('Ready')) {
            window.clearTimeout(timeout);
            observer.disconnect();
            resolve();
          }
        });
        observer.observe(state, { childList: true, subtree: true, characterData: true });
      }),
    timeout
  );
  await input.fill(command);
  await input.press('Enter');
  await readyAfterBusy;
  await expect(page.getByRole('status')).toContainText('Ready');
}
//=============================================================================
test('publishes the generated runtime capability contract', async ({ request }) => {
  const deploymentResponse = await request.get('/manifest.json');
  expect(deploymentResponse.ok()).toBe(true);
  const deployment = (await deploymentResponse.json()) as {
    capabilityManifest?: string;
    files?: { file?: string }[];
  };
  expect(deployment.capabilityManifest).toBe('capabilities.json');
  expect(deployment.files?.some(({ file }) => file === 'capabilities.json')).toBe(true);

  const response = await request.get(`/${deployment.capabilityManifest}`);
  expect(response.ok()).toBe(true);
  const capabilities = (await response.json()) as {
    profile?: string;
    modules?: string[];
    capabilities?: string[];
    features?: Record<string, boolean>;
    limits?: Record<string, number>;
  };
  expect(capabilities.profile).toBe('portable-nflow');
  expect(capabilities.modules).toEqual(
    expect.arrayContaining([
      'audio',
      'control_system',
      'graphics',
      'help_tools',
      'nflow_engine',
      'random',
      'slicot',
      'statistics',
    ])
  );
  expect(capabilities.capabilities).toEqual(
    expect.arrayContaining([
      'audio',
      'audio-output',
      'blas',
      'control-system',
      'help',
      'lapack',
      'nflow',
      'ode-solvers',
      'plots',
      'random',
      'slicot',
      'statistics',
      'worker',
    ])
  );
  expect(capabilities.capabilities).not.toEqual(
    expect.arrayContaining(['audio-flac', 'audio-mpeg', 'audio-ogg'])
  );
  expect(capabilities.features).toMatchObject({
    blas: true,
    controlSystem: true,
    lapack: true,
    nflow: true,
    odeSolvers: true,
    packageManager: true,
    plots: true,
    slicot: true,
    virtualFilesystem: true,
    worker: true,
  });
  expect(capabilities.limits).toMatchObject({
    maxSourceBytes: 4 * 1024 * 1024,
    maxVirtualFileBytes: 32 * 1024 * 1024,
  });
});
//=============================================================================
test('writes and reads a WAV file with the portable audio backend', async ({ page }) => {
  await openRuntime(page);
  await run(
    page,
    "audioWasmFile = [tempname(), '.wav']; " +
      "audioWasmFs = 8000; audioWasmY = sin(2 * pi * 440 * (0:99)' / audioWasmFs); " +
      'audiowrite(audioWasmFile, audioWasmY, audioWasmFs); ' +
      '[audioWasmZ, audioWasmReadFs] = audioread(audioWasmFile); ' +
      'audioWasmInfo = audioinfo(audioWasmFile); ' +
      'asserts.isequal(audioWasmReadFs, audioWasmFs); ' +
      'asserts.isequal(audioWasmInfo.SampleRate, audioWasmFs); ' +
      'asserts.isequal(size(audioWasmZ), size(audioWasmY)); ' +
      'asserts.isapprox(audioWasmZ, audioWasmY, 1e-4); ' +
      "rmfile(audioWasmFile); disp('wasm audio ok');"
  );
  await expect(page.getByRole('region', { name: 'Command Window transcript' })).toContainText(
    'wasm audio ok',
    { timeout: 60_000 }
  );
});
//=============================================================================
test('plays sound through the browser Web Audio bridge', async ({ page }) => {
  await page.addInitScript(() => {
    class FakeAudioContext {
      state: AudioContextState = 'suspended';
      destination = {};
      async resume() {
        this.state = 'running';
      }
      createBuffer(channels: number, frames: number, sampleRate: number) {
        const data = Array.from({ length: channels }, () => new Float32Array(frames));
        return {
          sampleRate,
          length: frames,
          numberOfChannels: channels,
          getChannelData: (channel: number) => data[channel],
        };
      }
      createBufferSource() {
        return {
          buffer: null,
          connect: () => undefined,
          start: () => undefined,
        };
      }
    }
    Object.defineProperty(window, 'AudioContext', {
      value: FakeAudioContext,
      configurable: true,
    });
  });
  await openRuntime(page);
  await run(
    page,
    "webAudioFs = 8000; webAudioY = sin(2 * pi * 440 * (0:31)' / webAudioFs); " +
      "sound(webAudioY, webAudioFs); disp('wasm web audio ok');"
  );
  await expect(page.getByRole('region', { name: 'Command Window transcript' })).toContainText(
    'wasm web audio ok',
    { timeout: 60_000 }
  );
  await expect
    .poll(() => page.evaluate(() => window.__nelsonLastPlayedAudio ?? null), {
      timeout: 60_000,
    })
    .toEqual({ sampleRate: 8000, channels: 1, frames: 32 });
});
//=============================================================================
test('controls audioplayer playback through the browser Web Audio bridge', async ({ page }) => {
  await page.addInitScript(() => {
    type AudioEventWindow = Window & {
      __nelsonAudioEvents?: string[];
    };
    const audioWindow = window as AudioEventWindow;
    audioWindow.__nelsonAudioEvents = [];
    class FakeAudioContext {
      state: AudioContextState = 'suspended';
      currentTime = 0;
      destination = {};
      async resume() {
        this.state = 'running';
        audioWindow.__nelsonAudioEvents?.push('context-resume');
      }
      createBuffer(channels: number, frames: number, sampleRate: number) {
        const data = Array.from({ length: channels }, () => new Float32Array(frames));
        audioWindow.__nelsonAudioEvents?.push(`buffer:${channels}:${frames}:${sampleRate}`);
        return {
          sampleRate,
          length: frames,
          numberOfChannels: channels,
          getChannelData: (channel: number) => data[channel],
        };
      }
      createBufferSource() {
        return {
          buffer: null,
          connect: () => undefined,
          start: (_when = 0, offset = 0) => {
            audioWindow.__nelsonAudioEvents?.push(`start:${offset}`);
          },
          stop: () => {
            audioWindow.__nelsonAudioEvents?.push('stop');
          },
          onended: null,
        };
      }
    }
    Object.defineProperty(window, 'AudioContext', {
      value: FakeAudioContext,
      configurable: true,
    });
  });

  await openRuntime(page);
  await run(
    page,
    "audioControlFs = 8000; audioControlY = sin(2 * pi * 440 * (0:255)' / audioControlFs); " +
      'audioControlPlayer = audioplayer(audioControlY, audioControlFs); ' +
      'play(audioControlPlayer); pause(audioControlPlayer); resume(audioControlPlayer); ' +
      "stop(audioControlPlayer); delete(audioControlPlayer); disp('wasm audioplayer controls ok');"
  );
  await expect(page.getByRole('region', { name: 'Command Window transcript' })).toContainText(
    'wasm audioplayer controls ok',
    { timeout: 60_000 }
  );
  const events = await page.evaluate(
    () => (window as Window & { __nelsonAudioEvents?: string[] }).__nelsonAudioEvents ?? []
  );
  expect(events).toEqual(
    expect.arrayContaining(['context-resume', 'buffer:1:256:8000', 'start:0', 'stop'])
  );
  expect(events.filter((event) => event.startsWith('start:')).length).toBeGreaterThanOrEqual(2);
  expect(events.filter((event) => event === 'stop').length).toBeGreaterThanOrEqual(2);
});
//=============================================================================
test('runs the portable generated audio demo through Web Audio', async ({ page, request }) => {
  await page.addInitScript(() => {
    class FakeAudioContext {
      state: AudioContextState = 'suspended';
      destination = {};
      async resume() {
        this.state = 'running';
      }
      createBuffer(channels: number, frames: number, sampleRate: number) {
        const data = Array.from({ length: channels }, () => new Float32Array(frames));
        return {
          sampleRate,
          length: frames,
          numberOfChannels: channels,
          getChannelData: (channel: number) => data[channel],
        };
      }
      createBufferSource() {
        return {
          buffer: null,
          connect: () => undefined,
          start: () => undefined,
        };
      }
    }
    Object.defineProperty(window, 'AudioContext', {
      value: FakeAudioContext,
      configurable: true,
    });
  });

  const response = await request.get('/examples/catalog.json');
  expect(response.ok()).toBe(true);
  const catalog = (await response.json()) as {
    groups: { examples: { title?: string; file?: string }[] }[];
  };
  const examples = catalog.groups.flatMap((group) => group.examples);
  expect(examples).toEqual(
    expect.arrayContaining([
      expect.objectContaining({
        title: 'Play a generated stereo tone',
        file: '/modules/audio/examples/play_generated_tone.m',
      }),
      expect.objectContaining({
        title: 'Play a WAV file',
        file: '/modules/audio/examples/play_wav.m',
      }),
    ])
  );
  expect(examples).not.toEqual(
    expect.arrayContaining([
      expect.objectContaining({ file: '/modules/audio/examples/play_flac.m' }),
      expect.objectContaining({ file: '/modules/audio/examples/play_ogg.m' }),
      expect.objectContaining({ file: '/modules/audio/examples/play_mp3.m' }),
    ])
  );

  await openRuntime(page);
  await run(
    page,
    'audioToneDuration = 0.02; audioToneSampleRate = 8000; ' +
      "run('/modules/audio/examples/play_generated_tone.m'); " +
      "disp('generated audio demo ok');"
  );
  await page.getByRole('tab', { name: 'Command Window' }).click();
  await expect(page.getByRole('region', { name: 'Command Window transcript' })).toContainText(
    'generated audio demo ok',
    { timeout: 60_000 }
  );
  await expect(figureTab(page, 'Generated stereo tone')).toBeVisible({ timeout: 60_000 });
  await expect
    .poll(() => page.evaluate(() => window.__nelsonLastPlayedAudio ?? null), {
      timeout: 60_000,
    })
    .toEqual({ sampleRate: 8000, channels: 2, frames: 160 });

  await run(page, "run('/modules/audio/examples/play_wav.m'); disp('wav audio demo ok');");
  await expect(page.getByRole('region', { name: 'Command Window transcript' })).toContainText(
    'wav audio demo ok',
    { timeout: 60_000 }
  );
  await expect
    .poll(() => page.evaluate(() => window.__nelsonLastPlayedAudio ?? null), {
      timeout: 60_000,
    })
    .toMatchObject({ sampleRate: 8000, channels: 1 });
});
//=============================================================================
test('uses the desktop Command Window to evaluate code', async ({ page }) => {
  await openRuntime(page);
  await run(page, 'missingWasmFunction();');
  await run(page, 'wasmAnswer = 41;');
  await run(page, 'disp(wasmAnswer + 1);');
  await expect(page.getByRole('region', { name: 'Command Window transcript' })).toContainText(
    '42',
    { timeout: 60_000 }
  );
  await expect(page.getByRole('cell', { name: 'wasmAnswer' })).toBeVisible({ timeout: 60_000 });
  await expect(page.getByRole('cell', { name: '1x1 double' })).toBeVisible();
  const input = page.getByRole('textbox', { name: 'Command input' });
  await input.fill('wasmA');
  await input.press('Tab');
  await expect(input).toHaveValue('wasmAnswer', { timeout: 60_000 });

  await input.fill('disp(pl');
  await input.press('Tab');
  const popup = page.locator('.console-popup');
  await expect(popup).toContainText('plot', { timeout: 60_000 });
  await expect(popup).toContainText('plus');
  const placement = await page.evaluate(() => {
    const popupBounds = document.querySelector('.console-popup')!.getBoundingClientRect();
    const inputElement = document.querySelector('.console-input')!;
    const inputBounds = inputElement.getBoundingClientRect();
    const style = getComputedStyle(inputElement);
    const canvas = document.createElement('canvas');
    const context = canvas.getContext('2d')!;
    context.font = style.font;
    const tokenStart =
      inputBounds.left + parseFloat(style.paddingLeft || '0') + context.measureText('disp(').width;
    return {
      above: popupBounds.bottom <= inputBounds.top + 1,
      tokenOffset: Math.abs(popupBounds.left - tokenStart),
    };
  });
  expect(placement.above).toBe(true);
  expect(placement.tokenOffset).toBeLessThanOrEqual(2);
});
//=============================================================================
test('clears the Command Window from clc in the portable evaluator', async ({ page }) => {
  await openRuntime(page);
  const transcript = page.getByRole('region', { name: 'Command Window transcript' });

  await run(page, "disp('before clear'); clc; disp('after clear');");

  await expect(transcript).toContainText('after clear', { timeout: 60_000 });
  await expect(transcript).not.toContainText('before clear');
});
//=============================================================================
test('does not recreate a prompt when clc runs inside a busy loop', async ({ page }) => {
  await openRuntime(page);
  const input = page.getByRole('textbox', { name: 'Command input' });

  await input.fill('while 1, clc, disp(1), end');
  await input.press('Enter');
  await expect(page.getByRole('status')).toContainText('Busy', { timeout: 10_000 });
  await expect(input).toBeHidden();

  const activeLineText = await page.locator('.console-line--active').last().textContent();
  expect(activeLineText).not.toMatch(/(^|\n)>>\s*$/);

  await page.keyboard.down('Control');
  await page.keyboard.press('c');
  await page.keyboard.up('Control');
  await expect(page.getByRole('status')).toContainText('Ready', { timeout: 60_000 });
  await expect(input).toBeEnabled();
});
//=============================================================================
test('term.sync does not recreate a prompt while the interpreter is busy', async ({ page }) => {
  await openRuntime(page);
  const input = page.getByRole('textbox', { name: 'Command input' });
  const transcript = page.getByRole('region', { name: 'Command Window transcript' });

  await input.fill('while 1, pause(0.05), end');
  await input.press('Enter');
  await expect(page.getByRole('status')).toContainText('Busy', { timeout: 10_000 });
  await expect(input).toBeHidden();

  await page.evaluate(() => {
    window.__nelsonPush?.('term.prompt', JSON.stringify({ prompt: '>> ' }));
  });
  await expect(input).toBeHidden();
  await expect
    .poll(async () => (await transcript.textContent().catch(() => '')) ?? '', { timeout: 5_000 })
    .not.toMatch(/(^|\n)>>\s*$/);

  await page.keyboard.down('Control');
  await page.keyboard.press('c');
  await page.keyboard.up('Control');
  await expect(page.getByRole('status')).toContainText('Ready', { timeout: 60_000 });
  await expect(input).toBeEnabled();
});
//=============================================================================
test('runs deterministic random and statistics functions in the browser', async ({ page }) => {
  await openRuntime(page);
  await run(
    page,
    "rng(123, 'twister'); a = rand(1, 3); rng(123, 'twister'); b = rand(1, 3); " +
      "assert_isequal(a, b); disp('RNG_REPRODUCIBLE'); " +
      'disp(mean([1 2 3 4])); disp(normcdf(0));'
  );
  const transcript = page.getByRole('region', { name: 'Command Window transcript' });
  await expect(transcript).toContainText('RNG_REPRODUCIBLE', { timeout: 60_000 });
  await expect(transcript).toContainText('2.5000');
  await expect(transcript).toContainText('0.5000');
});
//=============================================================================
test('runs packaged unit tests through the public test_run entry point', async ({ page }) => {
  await openRuntime(page);
  await run(page, "test_run('string', 'test_strfind');");
  const transcript = page.getByRole('region', { name: 'Command Window transcript' });
  await expect(transcript).toContainText('01/01 - test_strfind', { timeout: 60_000 });
  await expect(transcript).toContainText('Failed: 0');
});
//=============================================================================
test('runs a portable benchmark through the public bench_run entry point', async ({ page }) => {
  test.setTimeout(480_000);
  await openRuntime(page);
  await run(
    page,
    "filewrite('/workspace/bench_wasm_browser_smoke.m', " +
      "{'benchBrowserValue = sum(1:8);', 'asserts.isequal(benchBrowserValue, 36);'}); " +
      "bench_run('/workspace/bench_wasm_browser_smoke.m', 'Workers', 1);"
  );
  const transcript = page.getByRole('region', { name: 'Command Window transcript' });
  await expect(transcript).toContainText('bench_wasm_browser_smoke', { timeout: 60_000 });
  await expect(transcript).toContainText('Failed: 0');

  await run(page, "bench_run([], 'Name', 'bench_loop_00', 'Workers', 1);", 180_000);
  // The runtime-resource count depends on the packaged profile (tests and
  // help preloaded or deferred); only a complete mount matters here.
  await expect(transcript).toContainText(/mounted (\d+)\/\1 runtime resource files/, {
    timeout: 60_000,
  });
  await expect(transcript).toContainText('01/01 - bench_loop_00', { timeout: 60_000 });
  await expect(transcript).toContainText('Failed: 0');

  await run(page, "bench_run('interpreter', 'Workers', 1);", 300_000);
  await expect(transcript).toContainText('bench_loop_00', { timeout: 60_000 });
  await expect(transcript).toContainText('bench_loop_19', { timeout: 60_000 });
  await expect(transcript).toContainText('bench_scalar_expression_loops', { timeout: 60_000 });
  await expect(transcript).toContainText('Failed: 0');

  await run(page, "bench_run('special_functions', 'Workers', 1);", 180_000);
  await expect(transcript).toContainText('bench_interp2', { timeout: 60_000 });
  await expect(transcript).toContainText('Failed: 0');
});
//=============================================================================
test('remounts packaged resources after interrupting test_run before bench_run', async ({
  page,
}) => {
  test.setTimeout(420_000);
  await openRuntime(page);
  const transcript = page.getByRole('region', { name: 'Command Window transcript' });
  const input = page.getByRole('textbox', { name: 'Command input' });

  await input.fill('test_run');
  await input.press('Enter');
  // The module count follows the packaged profile; only the first module matters.
  await expect(transcript).toContainText(/01\/\d+ \[functions_manager\]/, { timeout: 120_000 });
  await expect(transcript).toContainText('test_addpath', { timeout: 60_000 });

  await page.keyboard.down('Control');
  await page.keyboard.press('c');
  await page.keyboard.up('Control');
  await expect(page.getByRole('status')).toContainText('Ready', { timeout: 60_000 });
  await expect(transcript).toContainText('Nelson evaluation interrupted', { timeout: 60_000 });

  await run(page, "bench_run([], 'Name', 'bench_loop_00', 'Workers', 1);", 180_000);
  // The runtime-resource count depends on the packaged profile (tests and
  // help preloaded or deferred); only a complete mount matters here.
  await expect(transcript).toContainText(/mounted (\d+)\/\1 runtime resource files/, {
    timeout: 60_000,
  });
  await expect(transcript).toContainText('01/01 - bench_loop_00', { timeout: 60_000 });
  await expect(transcript).toContainText('Benchs: 1', { timeout: 60_000 });
  await expect(transcript).toContainText('Failed: 0', { timeout: 60_000 });
});
//=============================================================================
test('profiles browser Wasm execution from the shared Profiler panel', async ({ page }) => {
  await openRuntime(page);
  await run(
    page,
    "filewrite('/workspace/wasm_profile_target.m', " +
      "{'profileTargetValue = 0;', 'for k = 1:2000', " +
      "'profileTargetValue = profileTargetValue + sin(k);', 'end'});"
  );

  await page.getByRole('button', { name: /Profiler/ }).click();
  const profiler = page.locator('.profiler');
  await expect(profiler).toBeVisible({ timeout: 60_000 });
  const toggle = profiler.locator('.profiler__btn').first();
  await expect(toggle).toContainText('Start');
  await toggle.click();
  await expect(toggle).toContainText('Stop');
  await expect(profiler.locator('.profiler__on')).toBeVisible();

  await run(page, "run('/workspace/wasm_profile_target.m');");
  await page.getByRole('button', { name: /Profiler/ }).click();
  await expect(toggle).toContainText('Stop');
  await toggle.click();
  await expect(toggle).toContainText('Start', { timeout: 60_000 });
  await expect(profiler.locator('tbody')).toContainText('wasm_profile_target.m', {
    timeout: 60_000,
  });
});
//=============================================================================
test('loads integrated documentation through the Wasm help service', async ({ page }) => {
  await openRuntime(page);
  await page.locator('.menubar').getByText('Help', { exact: true }).click();
  await page.locator('.menu').getByText('Documentation', { exact: true }).click();
  const panel = page.locator('.help-panel');
  await expect(panel).toBeVisible({ timeout: 60_000 });
  const search = panel.locator('.help-panel__search');
  await expect(search).toBeVisible({ timeout: 60_000 });
  await search.click();
  await search.fill('help');
  await expect(search).toHaveValue('help');
  await search.press('Enter');
  await expect(panel.locator('.help-panel__topic')).toHaveText('help', { timeout: 60_000 });
  await expect(panel.locator('.help-panel__text')).toContainText(/help/i, { timeout: 60_000 });
  await expect(panel.locator('.help-panel__text')).not.toContainText('No documentation found');
});
//=============================================================================
test('opens the packaged NMM Package Manager', async ({ page }) => {
  await openRuntime(page);
  await page
    .getByRole('toolbar', { name: 'Main toolbar' })
    .getByRole('button', { name: 'Package Manager', exact: true })
    .click();
  const manager = page.locator('.packages-panel');
  await expect(manager).toBeVisible({ timeout: 60_000 });
  await expect(manager.locator('.toolbar__title')).toHaveText('Package Manager', {
    timeout: 60_000,
  });
});
//=============================================================================
test('streams terminal output while an evaluation is still busy', async ({ page }) => {
  await openRuntime(page);
  const transcript = page.getByRole('region', { name: 'Command Window transcript' });
  const input = page.getByRole('textbox', { name: 'Command input' });
  await input.fill(
    'disp(char([83 84 82 69 65 77 95 66 69 70 79 82 69])); ' +
      'pause(2); disp(char([83 84 82 69 65 77 95 65 70 84 69 82]));'
  );
  await input.press('Enter');

  await expect(page.getByRole('status')).toContainText('Busy', { timeout: 10_000 });
  await expect(input).toBeHidden();
  await expect(transcript).toContainText('STREAM_BEFORE', { timeout: 10_000 });
  await expect(transcript).not.toContainText('STREAM_AFTER');
  await expect(page.getByRole('status')).toContainText('Ready', { timeout: 60_000 });
  await expect(input).toBeEnabled();
  await expect(transcript).toContainText('STREAM_AFTER');
});
//=============================================================================
test('persists command history across a full page reload', async ({ page }) => {
  await openRuntime(page);
  const command = 'persistentHistoryAnswer = 314;';
  await run(page, command);

  await openRuntime(page);
  await page.getByRole('button', { name: /Command History/ }).click();
  await expect(page.getByText(command, { exact: true })).toBeVisible({ timeout: 60_000 });
});
//=============================================================================
test('reports an allocation failure and keeps the Worker usable', async ({ page }) => {
  await openRuntime(page);
  await run(page, 'wasmAllocationProbe = zeros(50000, 50000);');
  await expect(page.getByRole('region', { name: 'Command Window transcript' })).toContainText(
    'Nelson:outOfMemory',
    { timeout: 60_000 }
  );

  await run(page, 'disp(6 * 7);');
  await expect(page.getByRole('region', { name: 'Command Window transcript' })).toContainText(
    '42',
    { timeout: 60_000 }
  );
});
//=============================================================================
test('interrupts a busy Worker and evaluates again in the replacement runtime', async ({
  page,
}) => {
  await openRuntime(page);
  await page.locator('.browser input[type="file"]').setInputFiles({
    name: 'survives-worker-restart.txt',
    mimeType: 'text/plain',
    buffer: Buffer.from('persistent worker file'),
  });
  await expect(page.getByText('survives-worker-restart.txt', { exact: true })).toBeVisible();
  const input = page.getByRole('textbox', { name: 'Command input' });
  await input.fill('while true; end');
  await input.press('Enter');
  await expect(page.getByRole('status')).toContainText('Busy', { timeout: 10_000 });

  await page.keyboard.down('Control');
  await page.keyboard.press('c');
  await page.keyboard.up('Control');
  await expect(page.getByRole('status')).toContainText('Ready', { timeout: 60_000 });

  await run(page, 'disp(6 * 7);');
  await expect(page.getByRole('region', { name: 'Command Window transcript' })).toContainText(
    '42',
    { timeout: 60_000 }
  );
  await run(page, "disp(fileread('/workspace/survives-worker-restart.txt')); ");
  await expect(page.getByRole('region', { name: 'Command Window transcript' })).toContainText(
    'persistent worker file',
    { timeout: 60_000 }
  );
});
//=============================================================================
test('streams and interrupts a noisy Command Window loop with Ctrl+C', async ({ page }) => {
  await openRuntime(page);
  const transcript = page.getByRole('region', { name: 'Command Window transcript' });
  const input = page.getByRole('textbox', { name: 'Command input' });
  await input.fill('while 1, disp(1), end');
  await input.press('Enter');
  await expect(page.getByRole('status')).toContainText('Busy', { timeout: 10_000 });
  await expect(input).toBeHidden();
  await expect(transcript).toContainText('1', { timeout: 10_000 });

  await page.keyboard.down('Control');
  await page.keyboard.press('c');
  await page.keyboard.up('Control');
  await expect(page.getByRole('status')).toContainText('Ready', { timeout: 60_000 });
  await expect(input).toBeEnabled();

  await run(page, 'disp(6 * 7);');
  await expect(transcript).toContainText('42', { timeout: 60_000 });
});
//=============================================================================
test('hides the prompt while a demo runs and stops it with Ctrl+C from Command Window', async ({
  page,
}) => {
  await openRuntime(page);
  await page.getByRole('tab', { name: 'Command Window' }).click();
  const input = page.getByRole('textbox', { name: 'Command input' });
  await input.fill('demo()');
  await input.press('Enter');

  const gallery = page.locator('.examples-panel');
  await expect(gallery).toBeVisible({ timeout: 60_000 });
  const card = gallery.locator('.example-gallery-card', {
    hasText: 'Draw a three-dimensional cube',
  });
  await expect(card).toBeVisible({ timeout: 60_000 });
  await card.getByRole('button', { name: 'Run' }).click();
  await page.getByRole('tab', { name: 'Command Window' }).click();
  const transcript = page.getByRole('region', { name: 'Command Window transcript' });
  await expect(input).toBeHidden({ timeout: 10_000 });
  await expect(page.locator('.statusbar')).toContainText('Busy', { timeout: 10_000 });
  await expect
    .poll(async () => (await transcript.textContent().catch(() => '')) ?? '', { timeout: 10_000 })
    .not.toMatch(/(^|\n)>>\s*$/);
  await expect(transcript).not.toContainText(/run\('/);
  await page.waitForTimeout(500);
  await expect(input).toBeHidden();
  await expect(page.locator('.statusbar')).toContainText('Busy');
  await expect
    .poll(async () => (await transcript.textContent().catch(() => '')) ?? '', { timeout: 5_000 })
    .not.toMatch(/(^|\n)>>\s*$/);

  await page.keyboard.down('Control');
  await page.keyboard.press('c');
  await page.keyboard.up('Control');
  await expect(page.locator('.statusbar')).toContainText('Ready', { timeout: 60_000 });
  await expect(input).toBeEnabled();
  await expect(transcript).toContainText('Nelson evaluation interrupted', { timeout: 60_000 });
});
//=============================================================================
test('starts the 2D fluid demo through figure controls and renders live smoke', async ({
  page,
}) => {
  await openRuntime(page);
  await page.getByRole('tab', { name: 'Command Window' }).click();
  const input = page.getByRole('textbox', { name: 'Command input' });
  await input.fill(
    [
      'fluidFrameCount = Inf',
      'fluidGridSize = 64',
      'fluidJacobiIterations = 4',
      "run('/modules/graphics/examples/fluid_simulation_2d.m')",
    ].join('; ')
  );
  await input.press('Enter');
  await expect(page.getByRole('status')).toContainText('Busy', { timeout: 10_000 });
  await expect(figureTab(page, '2D fluid simulation')).toBeVisible({ timeout: 60_000 });
  await figureTab(page, '2D fluid simulation').click();

  const panel = page.locator('.figure-panel').first();
  await expect(panel.getByRole('button', { name: 'Start' })).toBeVisible({ timeout: 60_000 });
  await panel.getByRole('button', { name: 'Start' }).click();
  await expect(panel.getByRole('button', { name: 'Reset' })).toBeVisible({ timeout: 60_000 });

  const canvas = panel.locator('canvas').first();
  await expect(canvas).toBeVisible({ timeout: 60_000 });
  await expect
    .poll(async () => (await canvasImageStats(canvas, 8)).bright, { timeout: 60_000 })
    .toBeGreaterThan(20);
  await expect
    .poll(async () => (await canvasImageStats(canvas, 8)).colors, { timeout: 60_000 })
    .toBeGreaterThan(8);
  const frames = new Set<string>();
  for (let attempt = 0; attempt < 30 && frames.size < 3; attempt += 1) {
    frames.add(await canvasFingerprint(canvas));
    await page.waitForTimeout(250);
  }
  expect(frames.size).toBeGreaterThan(2);

  await page.getByRole('button', { name: 'Stop the current execution' }).click();
  await expect(page.getByRole('status')).toContainText('Ready', { timeout: 60_000 });
});
//=============================================================================
test('opens representative 2D and 3D plots in desktop figure panels', async ({ page }) => {
  await openRuntime(page);
  await run(page, "f = figure('Visible', 'off'); plot([0 1 2], [0 1 4]); title('Wasm 2D');");
  await expect(page.getByRole('tab', { name: 'Figure 1' })).toBeVisible({ timeout: 60_000 });
  await expect(page.locator('.figure-panel canvas').first()).toBeVisible();

  await run(page, "f = figure('Visible', 'off'); surf(peaks);");
  await expect(page.getByRole('tab', { name: 'Figure 2' })).toBeVisible({ timeout: 60_000 });
  await expect(page.locator('.figure-panel canvas').first()).toBeVisible({ timeout: 60_000 });

  const rotate = page.locator('.figure-panel__rotate');
  await expect(rotate).toBeVisible({ timeout: 60_000 });
  const initialView = await rotate.getAttribute('data-view');
  const rotateBox = await rotate.boundingBox();
  if (!rotateBox) throw new Error('3D rotation surface has no box');
  await page.mouse.move(rotateBox.x + rotateBox.width / 2, rotateBox.y + rotateBox.height / 2);
  await page.mouse.down();
  await page.mouse.move(
    rotateBox.x + rotateBox.width / 2 + 80,
    rotateBox.y + rotateBox.height / 2 + 20,
    { steps: 4 }
  );
  await page.mouse.up();
  await expect(rotate).not.toHaveAttribute('data-view', initialView ?? '', { timeout: 60_000 });
});
//=============================================================================
test('routes figure pan, mouse, key, control, resize, and close actions through Wasm', async ({
  page,
}) => {
  await openRuntime(page);
  await run(
    page,
    "f = figure('Visible', 'off', 'Position', [20 30 420 300]); " +
      "ax = axes('Parent', f); plot(ax, [0 10], [0 20]); axis(ax, [0 10 0 20]); " +
      "button = uicontrol('Parent', f, 'Style', 'pushbutton', 'String', 'Apply', " +
      "'Position', [10 10 100 30], " +
      "'Callback', @(source, event) set(source, 'String', 'Applied')); " +
      "f.WindowButtonDownFcn = @(source, event) set(button, 'String', 'Mouse'); " +
      "f.WindowKeyPressFcn = @(source, event) set(button, 'String', ['Key ', event.Key]);"
  );

  const tab = page.getByRole('tab', { name: 'Figure 1' });
  await expect(tab).toBeVisible({ timeout: 60_000 });
  const panel = page.locator('.figure-panel');
  const canvas = panel.locator('canvas').first();
  await expect(canvas).toBeVisible();
  const control = panel.locator('button.figure-uicontrol');
  await expect(control).toHaveText('Apply');

  await control.click();
  await expect(control).toHaveText('Applied', { timeout: 60_000 });

  const canvasBox = await canvas.boundingBox();
  if (!canvasBox) throw new Error('Figure canvas has no box');
  await page.mouse.click(
    canvasBox.x + canvasBox.width * 0.65,
    canvasBox.y + canvasBox.height * 0.4
  );
  await expect(control).toHaveText('Mouse', { timeout: 60_000 });

  await panel.focus();
  await page.keyboard.press('k');
  await expect(control).toHaveText('Key K', { timeout: 60_000 });

  const beforePan = await canvasFingerprint(canvas);
  await showFigureBars(panel);
  await panel.getByRole('button', { name: 'Pan', exact: true }).click();
  const panSurface = panel.locator('.figure-panel__pan');
  const panBox = await panSurface.boundingBox();
  if (!panBox) throw new Error('Figure pan surface has no box');
  await page.mouse.move(panBox.x + panBox.width / 2, panBox.y + panBox.height / 2);
  await page.mouse.down();
  await page.mouse.move(panBox.x + panBox.width / 2 + 60, panBox.y + panBox.height / 2 + 20, {
    steps: 4,
  });
  await page.mouse.up();
  await expect
    .poll(() => canvasFingerprint(canvas), {
      timeout: 60_000,
    })
    .not.toBe(beforePan);

  await tab.click({ button: 'right' });
  await page.getByText('Close panel', { exact: true }).click();
  await expect(tab).toBeHidden({ timeout: 60_000 });
});
//=============================================================================
test('streams drawnow animation frames before evaluation completes', async ({ page }) => {
  await openRuntime(page);
  const input = page.getByRole('textbox', { name: 'Command input' });
  await input.fill(
    "f = figure('Visible', 'off'); h = plot([0 1], [0 0]); axis([0 1 0 16]); " +
      "for k = 1:15; set(h, 'YData', [0 k]); drawnow(); pause(0.1); end"
  );
  await input.press('Enter');
  await expect(page.getByRole('status')).toContainText('Busy', { timeout: 10_000 });
  await expect(page.getByRole('tab', { name: 'Figure 1' })).toBeVisible({ timeout: 60_000 });

  const canvas = page.locator('.figure-panel canvas').first();
  await expect(canvas).toBeVisible();
  const frames = new Set<string>();
  for (let attempt = 0; attempt < 30; attempt += 1) {
    frames.add(await canvasFingerprint(canvas));
    if ((await page.getByRole('status').textContent())?.includes('Ready')) break;
    await page.waitForTimeout(100);
  }

  expect(frames.size).toBeGreaterThan(1);
  await expect(page.getByRole('status')).toContainText('Ready', { timeout: 60_000 });
});
//=============================================================================
test('keeps Command Window input unavailable while a drawnow animation is running', async ({
  page,
}) => {
  await openRuntime(page);
  const input = page.getByRole('textbox', { name: 'Command input' });
  await input.fill(
    "f = figure('Visible', 'off'); h = plot([0 1], [0 0]); " +
      "for k = 1:50; set(h, 'YData', [0 k]); drawnow(); pause(0.1); end"
  );
  await input.press('Enter');
  await expect(page.getByRole('status')).toContainText('Busy', { timeout: 10_000 });
  await expect(page.getByRole('tab', { name: 'Figure 1' })).toBeVisible({ timeout: 60_000 });

  await page.getByRole('tab', { name: 'Command Window' }).click();
  const transcript = page.getByRole('region', { name: 'Command Window transcript' });
  await expect(input).toBeHidden();
  await expect
    .poll(async () => (await transcript.textContent().catch(() => '')) ?? '', { timeout: 5_000 })
    .not.toMatch(/(^|\n)>>\s*$/);

  await page.keyboard.down('Control');
  await page.keyboard.press('c');
  await page.keyboard.up('Control');
  await expect(page.getByRole('status')).toContainText('Ready', { timeout: 60_000 });
  await expect(input).toBeEnabled();
});
//=============================================================================
test('computes and plots a control-system step response', async ({ page }) => {
  await openRuntime(page);
  await run(
    page,
    "sys = tf(1, [1 0.8 1]); f = figure('Visible', 'off'); step(sys); title('Wasm control response');"
  );
  await expect(page.getByRole('tab', { name: 'Figure 1' })).toBeVisible({ timeout: 60_000 });
  await expect(page.locator('.figure-panel canvas').first()).toBeVisible();
  await run(page, 'x = lyap([-1 0; 0 -2], eye(2)); disp(x);');
  await expect(page.getByRole('region', { name: 'Command Window transcript' })).toContainText(
    '0.5000',
    { timeout: 60_000 }
  );
});
//=============================================================================
test('edits, saves, and runs a script in the virtual workspace', async ({ page }) => {
  await page.setViewportSize({ width: 1033, height: 920 });
  await openRuntime(page);
  await page.getByRole('button', { name: 'Text Editor' }).click();
  const editor = page.locator('.editor-panel').first();
  await expect(editor.locator('.monaco-editor')).toBeVisible({ timeout: 60_000 });
  await editor.locator('.view-lines').click();
  await page.keyboard.type('wasmEditorAnswer = 73;');
  await page.setViewportSize({ width: 1600, height: 920 });
  await editor.getByRole('button', { name: 'Save', exact: true }).click();
  await run(page, "run('/workspace/untitled.m'); disp(wasmEditorAnswer);");
  await expect(page.getByRole('region', { name: 'Command Window transcript' })).toContainText(
    '73',
    { timeout: 60_000 }
  );
});
//=============================================================================
test('opens and edits a numeric matrix in the variables editor', async ({ page }) => {
  await openRuntime(page);
  await run(page, 'wasmGrid = [1 2; 3 4];');
  await page.getByRole('cell', { name: 'wasmGrid', exact: true }).dblclick();
  const editor = page.locator('.var-editor');
  await expect(editor).toBeVisible({ timeout: 60_000 });
  await expect(editor.locator('.var-editor__bar')).toContainText('2×2 double');
  const firstCell = editor.locator('.grid tbody tr').first().locator('.grid__cell').first();
  await expect(firstCell).toHaveText('1');
  await firstCell.dblclick();
  await editor.locator('.grid__input').fill('9');
  await editor.locator('.grid__input').press('Enter');
  await expect(firstCell).toHaveText('9');
  await run(page, 'disp(wasmGrid(1, 1));');
  await expect(page.getByRole('region', { name: 'Command Window transcript' })).toContainText('9');
});
//=============================================================================
test('runs the portable NFlow simulation from the desktop prompt', async ({ page }) => {
  await openRuntime(page);
  const diagram = JSON.stringify({
    blocks: [
      { id: 'one', type: 'constant', inputs: 0, outputs: 1, params: { Value: 1 } },
      { id: 'sink', type: 'scope', inputs: 1, outputs: 0, params: {} },
    ],
    connections: [{ from: 'one', to: 'sink', fromIndex: 0, toIndex: 0 }],
    sampleTime: 0.1,
    duration: 0.2,
    variables: {},
  });
  await run(page, `disp(__nflow_simulate__('${diagram}'));`);
  await expect(page.getByRole('region', { name: 'Command Window transcript' })).toContainText(
    '"status":"done"',
    { timeout: 60_000 }
  );
});
//=============================================================================
test('opens demo() and runs a packaged plotting example', async ({ page }) => {
  await openRuntime(page);
  await page.getByRole('tab', { name: 'Command Window' }).click();
  const input = page.getByRole('textbox', { name: 'Command input' });
  await input.fill('demo()');
  await input.press('Enter');
  const gallery = page.locator('.examples-panel');
  await expect(gallery).toBeVisible({ timeout: 60_000 });
  const card = gallery.locator('.example-gallery-card', {
    hasText: 'Portable curves and peaks',
  });
  await expect(card).toBeVisible({ timeout: 60_000 });
  await card.getByRole('button', { name: 'Run' }).click();
  await expect(page.getByRole('tab', { name: 'Figure 1' })).toBeVisible({ timeout: 60_000 });
  await expect(page.getByRole('tab', { name: 'Figure 2' })).toBeVisible({ timeout: 60_000 });
});
//=============================================================================
test('opens a packaged example in the editor with its source content', async ({ page }) => {
  await openRuntime(page);
  await page.getByRole('tab', { name: 'Command Window' }).click();
  const input = page.getByRole('textbox', { name: 'Command input' });
  await input.fill('demo()');
  await input.press('Enter');
  const gallery = page.locator('.examples-panel');
  await expect(gallery).toBeVisible({ timeout: 60_000 });
  const card = gallery.locator('.example-gallery-card', {
    hasText: 'Portable curves and peaks',
  });
  await expect(card).toBeVisible({ timeout: 60_000 });
  await card.getByRole('button', { name: 'Open in editor' }).click();

  await expect(page.getByRole('tab', { name: 'portable_peaks.m' })).toBeVisible({
    timeout: 60_000,
  });
  const editor = page.locator('.editor-panel').first();
  await expect(editor.locator('.monaco-editor')).toBeVisible({ timeout: 60_000 });
  await expect(editor.locator('.view-lines')).toContainText('surf(peaks(49))', {
    timeout: 60_000,
  });
});
//=============================================================================
test('renders the existing 2D fluid simulation image', async ({ page }) => {
  await openRuntime(page);
  const input = page.getByRole('textbox', { name: 'Command input' });
  await input.fill(
    'fluidFrameCount=2; fluidGridSize=32; fluidJacobiIterations=2; ' +
      "fluidAutoStart=true; run('/modules/graphics/examples/fluid_simulation_2d.m');"
  );
  await input.press('Enter');
  await expect(page.getByRole('status')).toContainText('Busy', { timeout: 10_000 });
  await expect(figureTab(page, '2D fluid simulation')).toBeVisible({ timeout: 60_000 });
  await expect(page.getByRole('status')).toContainText('Ready', { timeout: 60_000 });

  const canvas = page.locator('.figure-panel canvas').first();
  await expect(canvas).toBeVisible({ timeout: 60_000 });
  const stats = await canvasImageStats(canvas, 4);
  expect(stats.opaque).toBeGreaterThan(100);
  expect(stats.bright).toBeGreaterThan(100);
  expect(stats.colors).toBeGreaterThan(8);
});
//=============================================================================
test('starts the existing 2D fluid simulation from its figure control', async ({ page }) => {
  await openRuntime(page);
  const input = page.getByRole('textbox', { name: 'Command input' });
  await input.fill(
    'fluidFrameCount=3; fluidGridSize=32; fluidJacobiIterations=2; ' +
      "fluidAutoStart=false; run('/modules/graphics/examples/fluid_simulation_2d.m');"
  );
  await input.press('Enter');
  await expect(page.locator('.statusbar')).toContainText('Busy', { timeout: 10_000 });
  const fluidTab = figureTab(page, '2D fluid simulation');
  await expect(fluidTab).toBeVisible({ timeout: 60_000 });
  await fluidTab.click();

  await page.getByRole('button', { name: 'Start' }).click({ timeout: 60_000 });
  await expect(page.locator('.statusbar')).toContainText('Ready', { timeout: 60_000 });

  const canvas = page.locator('.figure-panel canvas').first();
  await expect(canvas).toBeVisible({ timeout: 60_000 });
  const stats = await canvasImageStats(canvas, 4);
  expect(stats.opaque).toBeGreaterThan(100);
  expect(stats.bright).toBeGreaterThan(100);
  expect(stats.colors).toBeGreaterThan(8);
});
//=============================================================================
test('lists and runs HDF5-backed graphics examples', async ({ page, request }) => {
  const response = await request.get('/examples/catalog.json');
  expect(response.ok()).toBe(true);
  const catalog = (await response.json()) as {
    groups: { examples: { title?: string; file?: string }[] }[];
  };
  const examples = catalog.groups.flatMap((group) => group.examples);
  expect(examples).toEqual(
    expect.arrayContaining([
      expect.objectContaining({
        title: 'Nefertiti point cloud',
        file: '/modules/graphics/examples/nefertiti-mask/nefertiti_mask.m',
      }),
      expect.objectContaining({
        title: 'Teapot surface',
        file: '/modules/graphics/examples/utah-teapot/utah_teapot.m',
      }),
    ])
  );

  await openRuntime(page);
  await run(page, "close('all'); run('/modules/graphics/examples/utah-teapot/utah_teapot.m');");
  await page.getByRole('tab', { name: 'Figure 1' }).click();
  let canvas = page.locator('.figure-panel canvas').first();
  await expect(canvas).toBeVisible({ timeout: 60_000 });
  let stats = await canvasImageStats(canvas, 8);
  expect(stats.opaque).toBeGreaterThan(100);
  expect(stats.bright).toBeGreaterThan(100);

  await run(
    page,
    "close('all'); run('/modules/graphics/examples/nefertiti-mask/nefertiti_mask.m');"
  );
  await page.getByRole('tab', { name: 'Figure 1' }).click();
  canvas = page.locator('.figure-panel canvas').first();
  await expect(canvas).toBeVisible({ timeout: 60_000 });
  stats = await canvasImageStats(canvas, 8);
  expect(stats.opaque).toBeGreaterThan(100);
  expect(stats.bright).toBeGreaterThan(100);
  expect(stats.colors).toBeGreaterThan(16);
});
//=============================================================================
test('streams the existing raycaster with retained image textures', async ({ page }) => {
  await openRuntime(page);
  await page.evaluate(() => {
    window.__nelsonFigureCanvasProfileEnabled = true;
    window.__nelsonResetFigureCanvasProfile?.();
  });
  await run(
    page,
    'raycasterBatch=true; raycasterMaxFrames=12; raycasterWidth=400; ' +
      'raycasterHeight=225; raycasterAutopilot=true; raycasterStartFullscreen=false; ' +
      "run('/modules/graphics/examples/retro_raycaster.m');"
  );

  await expect(figureTab(page, 'Nelson retro raycaster')).toBeVisible({ timeout: 60_000 });
  const canvas = page.locator('.figure-panel canvas').first();
  await expect(canvas).toBeVisible({ timeout: 60_000 });
  const stats = await canvasImageStats(canvas, 8);
  expect(stats.opaque).toBeGreaterThan(100);
  expect(stats.colors).toBeGreaterThan(16);
  const profile = await page.evaluate(() => window.__nelsonGetFigureCanvasProfile?.() ?? null);
  expect(profile?.primitiveImages ?? 0).toBeGreaterThan(12);
  expect(profile?.rasterWebGlUploadCalls ?? 0).toBeLessThan(profile?.primitiveImages ?? 0);
});
//=============================================================================
test('starts the existing demoscene after generating procedural audio', async ({ page }) => {
  await openRuntime(page);
  await run(
    page,
    "close('all'); demoFigureVisible='off'; demoStartFullscreen=false; " +
      'demoEnableAudio=false; demoWidth=320; demoHeight=180; demoTargetFps=60; ' +
      "demoMaxFrames=1; demoMaxSeconds=Inf; demoOutputImage=''; " +
      "run('/modules/graphics/examples/demoscene.m');",
    45_000
  );
  // The sidebar button toggles the panel; the tab only brings it to the front.
  await page.getByRole('tab', { name: 'Command Window' }).click();
  const transcript = page.getByRole('region', { name: 'Command Window transcript' });
  await expect(transcript).toBeVisible({ timeout: 10_000 });
  await expect(transcript).toContainText(
    'Preparing demoscene: generating procedural music and timeline, please wait...'
  );
  await expect(transcript).toContainText('Starting demoscene.');
});
//=============================================================================
test('animates the existing demoscene and interrupts it from Command Window', async ({ page }) => {
  test.setTimeout(180_000);
  await openRuntime(page);
  await page.getByRole('tab', { name: 'Command Window' }).click();
  const input = page.getByRole('textbox', { name: 'Command input' });
  await input.fill(
    "close('all'); demoStartFullscreen=false; demoEnableAudio=true; " +
      'demoTargetFps=60; demoMaxFrames=Inf; demoMaxSeconds=Inf; ' +
      "run('/modules/graphics/examples/demoscene.m');"
  );
  await input.press('Enter');
  await expect(page.getByRole('status')).toContainText('Busy', { timeout: 10_000 });
  await expect(figureTab(page, 'Nelson demoscene')).toBeVisible({ timeout: 90_000 });
  await figureTab(page, 'Nelson demoscene').click();

  const canvas = page.locator('.figure-panel canvas').first();
  await expect(canvas).toBeVisible({ timeout: 60_000 });
  const frames = new Set<string>();
  for (let attempt = 0; attempt < 30 && frames.size < 3; attempt += 1) {
    frames.add(await canvasFingerprint(canvas));
    await page.waitForTimeout(250);
  }
  expect(frames.size).toBeGreaterThan(2);

  await page.getByRole('tab', { name: 'Command Window' }).click();
  await expect(input).toBeHidden({ timeout: 10_000 });
  await expect(page.getByRole('status')).toContainText('Busy');
  const transcript = page.getByRole('region', { name: 'Command Window transcript' });
  await expect
    .poll(async () => (await transcript.textContent().catch(() => '')) ?? '', { timeout: 5_000 })
    .not.toMatch(/(^|\n)>>\s*$/);

  await page.keyboard.down('Control');
  await page.keyboard.press('c');
  await page.keyboard.up('Control');
  await expect(page.getByRole('status')).toContainText('Ready', { timeout: 60_000 });
  await expect(input).toBeEnabled();
  await expect(transcript).toContainText('Nelson evaluation interrupted', { timeout: 60_000 });
});
//=============================================================================
test('renders the existing animated cube demo with a bounded duration', async ({ page }) => {
  await openRuntime(page);
  await run(
    page,
    "close('all'); setenv('NELSON_DEMO_DURATION', '0.1'); " +
      "run('/modules/graphics/examples/cube/demo_cube.m'); " +
      "setenv('NELSON_DEMO_DURATION', '');",
    60_000
  );

  await expect(page.getByRole('tab', { name: 'Figure 1' })).toBeVisible({ timeout: 60_000 });
  await page.getByRole('tab', { name: 'Figure 1' }).click();
  const canvas = page.locator('.figure-panel canvas').first();
  await expect(canvas).toBeVisible({ timeout: 60_000 });
  const stats = await canvasImageStats(canvas, 8);
  expect(stats.opaque).toBeGreaterThan(100);
  expect(stats.bright).toBeGreaterThan(20);
  expect(stats.colors).toBeGreaterThan(16);
});
//=============================================================================
test('runs sequential demos and keeps a slow animation visibly pending', async ({ page }) => {
  await openRuntime(page);
  const input = page.getByRole('textbox', { name: 'Command input' });
  await input.fill('demo()');
  await input.press('Enter');
  const gallery = page.locator('.examples-panel');
  await expect(gallery).toBeVisible({ timeout: 60_000 });

  const runExample = async (title: string, figureCount: number): Promise<void> => {
    await page.getByRole('tab', { name: 'Examples', exact: true }).click();
    const button = gallery
      .locator('.example-gallery-card', { hasText: title })
      .getByRole('button', { name: 'Run' });
    await button.click();
    await expectFigureCountAtLeast(page, figureCount);
    await expect(page.locator('.statusbar')).toContainText('Ready', { timeout: 60_000 });
  };

  await runExample('Time-domain response', 1);
  await runExample('Frequency-domain response', 2);

  await page.getByRole('tab', { name: 'Examples', exact: true }).click();
  const ballButton = gallery
    .locator('.example-gallery-card', { hasText: 'Ball-on-plate LQR tracking' })
    .getByRole('button', { name: 'Run' });
  await ballButton.click();
  await expect(ballButton).toHaveText('Running…');
  await page.waitForTimeout(2_000);
  await expect(ballButton).toHaveText('Running…');
  await expectFigureCountAtLeast(page, 3);

  await page.getByRole('button', { name: 'Stop the current execution' }).click();
  await expect(page.locator('.statusbar')).toContainText('Ready', { timeout: 60_000 });
});
//=============================================================================
test('opens virtual files through editor and edit', async ({ page }) => {
  await openRuntime(page);
  await run(page, "filewrite('/workspace/editor_builtin_demo.m', 'editorBuiltinValue = 42;');");
  await run(page, "editor('/workspace/editor_builtin_demo.m', 1);");
  await expect(page.getByRole('tab', { name: 'editor_builtin_demo.m' })).toBeVisible();
  let editor = page.locator('.editor-panel').first();
  await expect(editor.locator('.monaco-editor')).toBeVisible({ timeout: 60_000 });
  await expect(editor.locator('.view-lines')).toContainText('editorBuiltinValue = 42', {
    timeout: 60_000,
  });

  await run(page, "filewrite('/workspace/edit_macro_demo.m', 'editMacroValue = 73;');");
  await run(page, "edit('/workspace/edit_macro_demo');");
  await expect(page.getByRole('tab', { name: 'edit_macro_demo.m' })).toBeVisible();
  editor = page.locator('.editor-panel').first();
  await expect(editor.locator('.monaco-editor')).toBeVisible({ timeout: 60_000 });
  await expect(editor.locator('.view-lines')).toContainText('editMacroValue = 73', {
    timeout: 60_000,
  });
});
//=============================================================================
test('runs the existing rigid triple pendulum as a streamed animation', async ({ page }) => {
  test.setTimeout(240_000);
  await openRuntime(page);
  const input = page.getByRole('textbox', { name: 'Command input' });
  await input.fill('demo()');
  await input.press('Enter');
  const gallery = page.locator('.examples-panel');
  await expect(gallery).toBeVisible({ timeout: 60_000 });

  const card = gallery.locator('.example-gallery-card', { hasText: 'Rigid triple pendulum' });
  await expect(card).toBeVisible({ timeout: 60_000 });
  await card.getByRole('button', { name: 'Run' }).click();
  await expect(page.locator('.statusbar')).toContainText('Busy', { timeout: 10_000 });
  await expect(page.getByRole('tab', { name: 'Figure 1' })).toBeVisible({ timeout: 60_000 });

  const canvas = page.locator('.figure-panel canvas').first();
  await expect(canvas).toBeVisible();
  // The first figure frame is the loading screen while ode45 computes the
  // trajectory. Firefox's Wasm execution is slower here, so wait for a
  // control created immediately before playback instead of sampling that
  // static loading frame for an arbitrary interval.
  await expect(page.locator('button.figure-uicontrol').filter({ hasText: 'Pause' })).toBeVisible({
    timeout: 150_000,
  });
  const frames = new Set<string>();
  for (let attempt = 0; attempt < 50 && frames.size < 3; attempt += 1) {
    // Read the canvas backing store directly. Firefox can return an identical
    // element screenshot while canvas paints are queued, even though the
    // backing pixels and the visible page are already advancing.
    frames.add(await canvasFingerprint(canvas));
    await page.waitForTimeout(200);
  }
  expect(frames.size).toBeGreaterThan(2);

  await page.getByRole('button', { name: 'Stop the current execution' }).click();
  await expect(page.locator('.statusbar')).toContainText('Ready', { timeout: 60_000 });
});
//=============================================================================
test('opens the complete NFlow editor and simulates an imported model', async ({
  page,
  request,
}) => {
  await openRuntime(page);
  await run(page, 'nelsonWasmCancelSentinel = 73;');
  const editorPromise = page.context().waitForEvent('page');
  await page.getByRole('button', { name: 'NFlow', exact: true }).click();
  const editor = await editorPromise;

  await expect(editor).toHaveURL((url) => url.pathname.endsWith('/nflow/index.html'));
  await expect(editor.locator('#svgCanvas')).toBeVisible({ timeout: 60_000 });
  await expect(editor.locator('#blockLibraryGroups')).toBeVisible();

  await editor.setViewportSize({ width: 1033, height: 920 });
  await editor.getByRole('button', { name: 'Properties', exact: true }).click();
  await expect(editor.locator('.inspector-pane')).not.toHaveClass(/collapsed/);
  await editor.getByRole('button', { name: 'Library', exact: true }).click();
  await expect(editor.locator('.inspector-pane')).toHaveClass(/collapsed/);
  await expect(editor.locator('.sidebar')).not.toHaveClass(/collapsed/);
  await expect(editor.locator('#blockLibraryGroups')).toBeVisible();
  await expect
    .poll(
      () =>
        editor
          .locator('.tool-icon svg')
          .first()
          .locator('path, line, rect, circle, polyline, polygon, ellipse, text')
          .count(),
      { timeout: 10_000 }
    )
    .toBeGreaterThan(0);

  await editor.getByRole('button', { name: 'Examples', exact: true }).click();
  const examples = editor.getByRole('dialog', { name: 'Examples', exact: true });
  await expect(examples).toBeVisible({ timeout: 60_000 });
  // The gallery counts every packaged entry (models and companion scripts):
  // derive the expected total from the served catalog instead of hard-coding it.
  const nflowCatalog = (await (await request.get('/nflow/examples/catalog.json')).json()) as {
    groups: { examples: unknown[] }[];
  };
  const nflowExampleCount = nflowCatalog.groups.reduce(
    (count, group) => count + group.examples.length,
    0
  );
  expect(nflowExampleCount).toBeGreaterThan(100);
  await expect(examples).toContainText(`${nflowExampleCount} examples`);
  const pidDemo = examples.locator('.example-gallery-card', {
    hasText: 'PID Demo - P vs PI vs PID',
  });
  await expect(pidDemo).toBeVisible();
  await pidDemo.getByRole('button', { name: 'Open', exact: true }).click();
  await expect(examples).toBeHidden();
  await expect
    .poll(async () => editor.evaluate(() => window.__nf_serializeDiagram?.() ?? ''), {
      timeout: 60_000,
    })
    .toContain('PID Demo - P vs PI vs PID');

  const model = {
    version: '1.0.0',
    format: 'nflow-2',
    name: 'wasm-editor-model',
    blocks: [
      {
        id: 'one',
        type: 'constant',
        x: 160,
        y: 200,
        params: { Value: 1 },
      },
      { id: 'sink', type: 'scope', x: 440, y: 200, params: {} },
    ],
    connections: [{ from: 'one', to: 'sink', fromIndex: 0, toIndex: 0 }],
    variables: '',
    runtime: '0.2',
    sampleTime: 0.1,
  };
  await editor.locator('#fileOpenInput').setInputFiles({
    name: 'wasm-editor-model.nflow',
    mimeType: 'application/json',
    buffer: Buffer.from(JSON.stringify(model)),
  });
  await expect
    .poll(async () => editor.evaluate(() => window.__nf_serializeDiagram?.() ?? ''), {
      timeout: 60_000,
    })
    .toContain('wasm-editor-model');

  const result = await editor.evaluate(async () => {
    const run = window.__nf_runSimulationForTest;
    if (!run) throw new Error('NFlow simulation test bridge is unavailable');
    return await run();
  });
  expect(result).toMatchObject({ status: 'done' });

  await editor.evaluate(() => {
    const runtime = document.querySelector<HTMLInputElement>('#runtimeInput')!;
    runtime.value = '5';
    runtime.dispatchEvent(new Event('input', { bubbles: true }));
    runtime.dispatchEvent(new Event('change', { bubbles: true }));
    const mode = document.querySelector<HTMLSelectElement>('#realTimeMode')!;
    mode.value = 'real-time';
    mode.dispatchEvent(new Event('change', { bubbles: true }));
  });
  await editor.locator('#playPauseBtn').click();
  await expect(editor.locator('body')).toHaveClass(/simulation-active/, { timeout: 10_000 });
  await expect(editor.locator('#statusBarInfo')).toContainText(/Running.*%/, {
    timeout: 10_000,
  });
  const stop = editor.locator('#stopSimToolbarBtn');
  await expect(stop).toBeEnabled();
  await stop.click();
  await expect(editor.locator('body')).not.toHaveClass(/simulation-active/, { timeout: 60_000 });
  await expect(editor.locator('#statusBarInfo')).toContainText('Stopped', { timeout: 60_000 });

  const recovered = await editor.evaluate(async () => {
    const run = window.__nf_runSimulationForTest;
    if (!run) throw new Error('NFlow simulation test bridge is unavailable');
    return await run({ realTime: false });
  });
  expect(recovered).toMatchObject({ status: 'done' });
  await run(page, 'disp(nelsonWasmCancelSentinel);');
  await expect(page.getByRole('region', { name: 'Command Window transcript' })).toContainText(
    '73',
    { timeout: 60_000 }
  );
  await editor.close();
});
//=============================================================================
test('imports a browser file into the desktop virtual workspace', async ({ page }) => {
  await openRuntime(page);
  await page.locator('.browser input[type="file"]').setInputFiles({
    name: 'portable.txt',
    mimeType: 'text/plain',
    buffer: Buffer.from('portable browser file'),
  });
  await expect(page.getByText('portable.txt', { exact: true })).toBeVisible();
  await run(page, "disp(fileread('/workspace/portable.txt')); ");
  await expect(page.getByRole('region', { name: 'Command Window transcript' })).toContainText(
    'portable browser file',
    { timeout: 60_000 }
  );
});
//=============================================================================
test('loads the News feed through the browser-safe Wasm adapter', async ({ page }) => {
  await page.route(
    'https://api.github.com/repos/nelson-lang/nelson/releases?per_page=30',
    (route) =>
      route.fulfill({
        status: 200,
        contentType: 'application/json',
        body: JSON.stringify([
          {
            name: 'Nelson portable release',
            tag_name: 'portable-release',
            html_url: 'https://github.com/nelson-lang/nelson/releases/tag/portable-release',
            published_at: '2026-09-27T10:00:00Z',
            body: 'Portable release notes',
          },
        ]),
      })
  );
  await openRuntime(page);

  await page.getByRole('button', { name: 'News feed' }).click();
  await expect(page.locator('.newsfeed__title')).toHaveText('Nelson releases');
  await expect(page.locator('.newsfeed__item')).toHaveCount(1);
  await expect(page.locator('.newsfeed__item')).toContainText('Nelson portable release');
  await expect(page.locator('.newsfeed__status')).toContainText('1 item');
});
//=============================================================================
test('reads and saves web resources through the browser network stack', async ({ page }) => {
  await openRuntime(page);
  const transcript = page.getByRole('region', { name: 'Command Window transcript' });
  // Same-origin JSON: webread decodes it into a struct.
  await run(
    page,
    "wasmWebManifest = webread('/manifest.json'); " +
      "disp(['web formatVersion=' num2str(wasmWebManifest.formatVersion)]);"
  );
  await expect(transcript).toContainText('web formatVersion=1', { timeout: 60_000 });
  // websave stores the body in the virtual workspace and returns the absolute path.
  await run(
    page,
    "wasmWebFile = websave('/workspace/manifest-copy.json', '/manifest.json'); " +
      'wasmWebInfo = dir(wasmWebFile); ' +
      "disp(['web saved=' wasmWebFile ' bytes>0=' mat2str(wasmWebInfo.bytes > 0)]);"
  );
  await expect(transcript).toContainText('web saved=/workspace/manifest-copy.json bytes>0=true', {
    timeout: 60_000,
  });
  // HTTP failures surface as the usual webtools messages.
  await run(
    page,
    "try; webread('/this-file-does-not-exist.json'); catch wasmWebError; " +
      "disp(['web error=' wasmWebError.message]); end"
  );
  await expect(transcript).toContainText('web error=Not Found (404)', { timeout: 60_000 });
});

test('persists the virtual workspace across a full page reload', async ({ page }) => {
  await openRuntime(page);
  await page.locator('.browser input[type="file"]').setInputFiles({
    name: 'persistent-reload.txt',
    mimeType: 'text/plain',
    buffer: Buffer.from('persistent reload file'),
  });
  await expect(page.getByText('persistent-reload.txt', { exact: true })).toBeVisible();

  await openRuntime(page);
  await expect(page.getByText('persistent-reload.txt', { exact: true })).toBeVisible({
    timeout: 60_000,
  });
  await run(page, "disp(fileread('/workspace/persistent-reload.txt')); ");
  await expect(page.getByRole('region', { name: 'Command Window transcript' })).toContainText(
    'persistent reload file',
    { timeout: 60_000 }
  );

  await run(page, "rmfile('/workspace/persistent-reload.txt');");
  await openRuntime(page);
  await expect(page.getByText('persistent-reload.txt', { exact: true })).toBeHidden({
    timeout: 60_000,
  });
});
//=============================================================================
