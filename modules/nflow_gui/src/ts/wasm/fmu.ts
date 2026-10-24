//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { readFmuArchive, type WasmFmuInfo } from './fmu-archive.js';
import type { NFlowWasmFmuProvider } from '../app/nflow-host.js';
import { parseFmuInspection, recomputeFmuPortWidths } from '../libraries/fmi/fmu-inspect.js';
//=============================================================================
export interface WasmFmuModel {
  path: string;
  token: string;
  info: WasmFmuInfo;
  source: string;
  files: Array<[string, Uint8Array]>;
  wasiUrl: string;
}
//=============================================================================
export async function compileFmu(bytes: Uint8Array): Promise<WasmFmuModel> {
  const { component, token, info } = readFmuArchive(bytes);
  const tools = await import('@bytecodealliance/jco-transpile/component');
  await tools.$init;
  const generated = tools.generate(component, {
    name: 'fmu',
    noTypescript: true,
    noNodejsCompat: true,
    instantiation: { tag: 'async' },
    base64Cutoff: 0,
  });
  const source = generated.files.find(([name]) => name === 'fmu.js')?.[1];
  if (!source) throw new Error('Could not generate FMU component bindings.');
  const digest = await crypto.subtle.digest('SHA-256', bytes.slice().buffer);
  const id = Array.from(new Uint8Array(digest), (n) => n.toString(16).padStart(2, '0')).join('');
  return {
    path: `/workspace/fmus/${id}.fmu`,
    token,
    info,
    source: new TextDecoder().decode(source),
    files: generated.files.filter(([name]) => name.endsWith('.wasm')),
    wasiUrl: new URL('./fmu/wasi.js', window.location.href).href,
  };
}
//=============================================================================
function chooseFmu(): Promise<File | null> {
  return new Promise((resolve) => {
    const input = document.createElement('input');
    input.type = 'file';
    input.accept = '.fmu';
    input.hidden = true;
    const finish = (file: File | null) => {
      input.remove();
      resolve(file);
    };
    input.addEventListener('change', () => finish(input.files?.[0] || null), { once: true });
    input.addEventListener('cancel', () => finish(null), { once: true });
    document.body.appendChild(input);
    input.click();
  });
}
//=============================================================================
export function createFmuProvider(client: {
  registerNFlowFmu(model: WasmFmuModel): Promise<unknown>;
}): NFlowWasmFmuProvider {
  const models = new Map<string, WasmFmuModel>();
  const aliases = new Map<string, string>();
  async function load(path: string): Promise<WasmFmuModel> {
    const cached = models.get(aliases.get(path) || path);
    if (cached) return cached;
    const prefix = 'nflow-example:/';
    if (!path.startsWith(prefix))
      throw new Error('Select the WebAssembly .fmu archive with Browse FMU first.');
    const segments = path.slice(prefix.length).split('/');
    if (segments.some((part) => !part || part === '.' || part === '..') || !path.endsWith('.fmu'))
      throw new Error('Invalid FMU example path.');
    const url = new URL(
      `./examples/files/${segments.map(encodeURIComponent).join('/')}`,
      window.location.href
    );
    const response = await fetch(url);
    if (!response.ok) throw new Error(`FMU example could not be loaded: HTTP ${response.status}`);
    const model = await compileFmu(new Uint8Array(await response.arrayBuffer()));
    models.set(model.path, model);
    aliases.set(path, model.path);
    return model;
  }
  return {
    async pickFile() {
      const file = await chooseFmu();
      if (!file) return null;
      const model = await compileFmu(new Uint8Array(await file.arrayBuffer()));
      await client.registerNFlowFmu(model);
      models.set(model.path, model);
      return model.path;
    },
    async inspect(path) {
      const model = await load(path);
      await client.registerNFlowFmu(model);
      return JSON.stringify(model.info);
    },
    async prepareDiagram(json) {
      const diagram = JSON.parse(json);
      async function prepare(value: unknown): Promise<void> {
        if (!value || typeof value !== 'object') return;
        const object = value as Record<string, unknown>;
        if (
          ['fmu', 'fmuMe'].includes(String(object.type)) &&
          object.params &&
          typeof object.params === 'object'
        ) {
          const params = object.params as Record<string, unknown>;
          const model = await load(String(params.path || ''));
          await client.registerNFlowFmu(model);
          params.path = model.path;
          const info = parseFmuInspection(JSON.stringify(model.info));
          for (const [direction, variables] of [
            ['input', info.inputs],
            ['output', info.outputs],
          ] as const) {
            const names = params[`${direction}Names`] as string[] | undefined;
            const ports = names?.length
              ? names.map((name) => variables.find((v) => v.name === name))
              : variables;
            if (ports.some((port) => !port)) throw new Error('Unknown FMU port name.');
            params[`${direction}Widths`] = ports.map((port) => port!.arraySize ?? 1);
            params[`${direction}Dimensions`] = ports.map((port) => port!.dimensions ?? null);
          }
          params.parameterNames = info.parameters.map((p) => p.name);
          params.parameterVrs = info.parameters.map((p) => p.valueReference);
          params.parameterStructural = info.parameters.map((p) => p.structural === true);
          recomputeFmuPortWidths({ params });
          if (object.type === 'fmuMe') {
            params.numContinuousStates = model.info.numberOfContinuousStates;
            params.numEventIndicators = model.info.numberOfEventIndicators;
          }
        }
        for (const item of Object.values(object)) {
          if (Array.isArray(item)) {
            for (const child of item) await prepare(child);
          } else if (item && typeof item === 'object') await prepare(item);
        }
      }
      await prepare(diagram);
      return JSON.stringify(diagram);
    },
  };
}
//=============================================================================
