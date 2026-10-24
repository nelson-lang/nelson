//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { expect, test } from 'vitest';
import { zipSync, strToU8 } from 'fflate';
import { readFmuArchive } from './fmu-archive.js';
//=============================================================================
const ascii = (text: string) => strToU8(text, true);
const xml = `<fmiModelDescription fmiVersion="3.0" instantiationToken="test">
<CoSimulation modelIdentifier="adder"/>
<ModelVariables><Float64 name="a" valueReference="1" causality="input" start="2"/>
<Float64 name="sum" valueReference="3" causality="output"/></ModelVariables>
</fmiModelDescription>`;
const componentHeader = ascii('\0asm\r\0\x01\0');
test('reads the model metadata and selects its WASI Preview 2 binary', () => {
  const archive = zipSync({
    'modelDescription.xml': ascii(xml),
    'binaries/wasm32-wasip2/adder.wasm': componentHeader,
  });
  const model = readFmuArchive(archive);
  expect(model.token).toBe('test');
  expect(model.info).toMatchObject({
    coSimulation: true,
    modelExchange: false,
    modelIdentifier: 'adder',
  });
  expect(model.info.variables[0]).toMatchObject({ name: 'a', valueReference: 1, startValue: 2 });
  expect(model.component).toEqual(componentHeader);
});
//=============================================================================
test('explains why an archive with only native binaries cannot run', () => {
  expect(() =>
    readFmuArchive(
      zipSync({
        'modelDescription.xml': ascii(xml),
        'binaries/x86_64-linux/adder.so': ascii('x'),
      })
    )
  ).toThrow('no wasm32-wasip2 component');
});
//=============================================================================
test.each([
  [xml.replace('valueReference="1"', 'valueReference="-1"'), 'Invalid FMU variable'],
  [xml.replace('fmiVersion="3.0"', 'fmiVersion="2.0"'), 'FMI 3.0'],
  [
    xml.replace(
      'causality="input" start="2"/>',
      'causality="input"><Dimension start="0"/></Float64>'
    ),
    'Invalid FMU array dimension',
  ],
])('rejects malformed or incompatible metadata', (description, reason) => {
  expect(() =>
    readFmuArchive(
      zipSync({
        'modelDescription.xml': ascii(description),
        'binaries/wasm32-wasip2/adder.wasm': componentHeader,
      })
    )
  ).toThrow(reason);
});
//=============================================================================
test('accepts ME, typed scalar ports and array dimensions', () => {
  const description = xml
    .replace('CoSimulation', 'ModelExchange')
    .replace('Float64 name="a"', 'Int32 name="a"')
    .replace(
      'valueReference="1" causality="input" start="2"/>',
      'valueReference="1" causality="input" start="2"><Dimension start="3"/></Int32>'
    );
  const model = readFmuArchive(
    zipSync({
      'modelDescription.xml': ascii(description),
      'binaries/wasm32-wasip2/adder.wasm': componentHeader,
    })
  );
  expect(model.info).toMatchObject({ modelExchange: true, coSimulation: false });
  expect(model.info.variables[0]).toMatchObject({ dataType: 'Int32', arraySize: 3 });
});
//=============================================================================
test('rejects a core Wasm module without component bindings', () => {
  expect(() =>
    readFmuArchive(
      zipSync({
        'modelDescription.xml': ascii(xml),
        'binaries/wasm32-wasip2/adder.wasm': ascii('\0asm\x01\0\0\0'),
      })
    )
  ).toThrow('Component Model');
});
//=============================================================================
test.each(['BouncingBall', 'Dahlquist', 'Feedthrough', 'StateSpace', 'VanDerPol'])(
  'reads the compiled %s reference FMU',
  async (name) => {
    const { readFile } = await import('node:fs/promises');
    const bytes = new Uint8Array(await readFile(`../nflow_fmi/examples/${name}.fmu`));
    const model = readFmuArchive(bytes);
    expect(model.info).toMatchObject({
      modelIdentifier: name,
      coSimulation: true,
      modelExchange: true,
    });
    if (name === 'StateSpace') {
      expect(model.info.numberOfContinuousStates).toBe(3);
      expect(model.info.variables.find((v) => v.name === 'u')?.arraySize).toBe(3);
    }
    if (name === 'BouncingBall') {
      expect(model.info.numberOfContinuousStates).toBe(2);
      expect(model.info.numberOfEventIndicators).toBe(1);
    }
  }
);
//=============================================================================
