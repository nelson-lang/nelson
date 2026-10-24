//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { unzipSync, strFromU8 } from 'fflate';
//=============================================================================
export interface WasmFmuVariable {
  name: string;
  valueReference: number;
  causality: string;
  dataType: string;
  description: string;
  startValue?: number;
  arraySize: number;
  dimensions?: Array<{ start: number; vr?: number }>;
}
//=============================================================================
export interface WasmFmuInfo {
  success: true;
  fmiVersion: string;
  modelIdentifier: string;
  coSimulation: boolean;
  modelExchange: boolean;
  numberOfContinuousStates: number;
  numberOfEventIndicators: number;
  variables: WasmFmuVariable[];
}
//=============================================================================
function parseVariables(root: Element): WasmFmuVariable[] {
  const elements = Array.from(root.querySelector('ModelVariables')?.children || []);
  return elements.map((element) => {
    const variable: WasmFmuVariable = {
      name: element.getAttribute('name') || '',
      valueReference: Number(element.getAttribute('valueReference')),
      causality: element.getAttribute('causality') || 'local',
      dataType: element.localName,
      description: element.getAttribute('description') || '',
      arraySize: 1,
    };
    if (
      !variable.name ||
      !element.hasAttribute('valueReference') ||
      !Number.isInteger(variable.valueReference) ||
      variable.valueReference < 0 ||
      variable.valueReference > 0xffffffff
    )
      throw new Error('Invalid FMU variable metadata.');
    const start = element.getAttribute('start');
    if (start !== null && !start.includes(' ')) {
      const value = element.localName === 'Boolean' ? Number(start === 'true') : Number(start);
      if (!Number.isFinite(value)) throw new Error('Invalid FMU start value.');
      variable.startValue = value;
    }
    const dimensions = Array.from(element.querySelectorAll('Dimension')).map((dimension) => {
      const vrText = dimension.getAttribute('valueReference');
      const parameter = elements.find((item) => item.getAttribute('valueReference') === vrText);
      const size = Number(dimension.getAttribute('start') ?? parameter?.getAttribute('start'));
      if (!Number.isSafeInteger(size) || size < 1 || size > 1000000)
        throw new Error('Invalid FMU array dimension.');
      return vrText === null ? { start: size } : { start: size, vr: Number(vrText) };
    });
    if (dimensions.length) {
      variable.dimensions = dimensions;
      variable.arraySize = dimensions.reduce((size, dimension) => size * dimension.start, 1);
      if (!Number.isSafeInteger(variable.arraySize) || variable.arraySize > 1000000)
        throw new Error('FMU array exceeds 1000000 elements.');
    }
    return variable;
  });
}
//=============================================================================
export function readFmuArchive(bytes: Uint8Array): {
  component: Uint8Array;
  token: string;
  info: WasmFmuInfo;
} {
  if (bytes.byteLength > 32 * 1024 * 1024) throw new Error('FMU archive exceeds 32 MiB.');
  let expanded = 0;
  const files = unzipSync(bytes, {
    filter(file) {
      const keep =
        file.name === 'modelDescription.xml' ||
        /^binaries\/wasm32-wasip2\/[^/]+\.wasm$/.test(file.name);
      if (keep) {
        expanded += file.originalSize;
        if (expanded > 64 * 1024 * 1024) throw new Error('FMU contents exceed 64 MiB.');
      }
      return keep;
    },
  });
  const description = files['modelDescription.xml'];
  if (!description) throw new Error('FMU has no modelDescription.xml.');
  const doc = new DOMParser().parseFromString(strFromU8(description), 'application/xml');
  if (doc.querySelector('parsererror')) throw new Error('Invalid FMU modelDescription.xml.');
  const root = doc.documentElement;
  const cs = root.querySelector('CoSimulation');
  const me = root.querySelector('ModelExchange');
  if (!root.getAttribute('fmiVersion')?.startsWith('3.') || (!cs && !me))
    throw new Error(
      'The browser supports FMI 3.0 Co-Simulation and Model Exchange components only.'
    );
  const modelIdentifier = (cs || me)?.getAttribute('modelIdentifier') || '';
  const component = files[`binaries/wasm32-wasip2/${modelIdentifier}.wasm`];
  if (!component)
    throw new Error(
      'This FMU has no wasm32-wasip2 component. Native binaries cannot run in the browser.'
    );
  if (
    component[0] !== 0 ||
    component[1] !== 97 ||
    component[2] !== 115 ||
    component[3] !== 109 ||
    component[4] !== 13
  )
    throw new Error('The FMU binary must be a WebAssembly Component Model component.');
  const variables = parseVariables(root);
  const states = Array.from(root.querySelectorAll('ModelStructure > ContinuousStateDerivative'));
  const numberOfContinuousStates = states.reduce(
    (count, element) =>
      count +
      (variables.find(
        (variable) => variable.valueReference === Number(element.getAttribute('valueReference'))
      )?.arraySize || 0),
    0
  );
  return {
    component,
    token: root.getAttribute('instantiationToken') || '',
    info: {
      success: true,
      fmiVersion: root.getAttribute('fmiVersion') || '3.0',
      modelIdentifier,
      coSimulation: cs !== null,
      modelExchange: me !== null,
      numberOfContinuousStates,
      numberOfEventIndicators: root.querySelectorAll('ModelStructure > EventIndicator').length,
      variables,
    },
  };
}
//=============================================================================
