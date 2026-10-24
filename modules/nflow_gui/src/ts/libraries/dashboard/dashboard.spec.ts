//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { describe, expect, test } from 'vitest';
import { existsSync, readFileSync } from 'node:fs';
import path from 'node:path';
import contract from '../../../../../nflow_blocks/libraries/dashboard/contract.json';
import { blockDefinitions, dashboardManifest } from './blocks.js';
//=============================================================================
const repositoryDirectory =
  path.basename(process.cwd()) === 'nflow_gui'
    ? path.resolve(process.cwd(), '..', '..')
    : process.cwd();
const iconDirectory = path.join(
  repositoryDirectory,
  'modules',
  'nflow_blocks',
  'libraries',
  'dashboard',
  'exports'
);
//=============================================================================
describe('dashboard contract', () => {
  test('registers the complete standard inventory without regular ports', () => {
    const blocks = dashboardManifest.blocks;
    expect(blocks.map((block) => block.type)).toEqual(contract.types);
    expect(Object.keys(blockDefinitions)).toEqual(contract.types);
    expect(blocks.every((block) => block.inputs.length === 0 && block.outputs.length === 0)).toBe(
      true
    );
  });

  test('keeps the locally extracted defaults stable', () => {
    for (const expected of contract.blocks) {
      const block = dashboardManifest.blocks.find((candidate) => candidate.type === expected.type);
      expect(block).toBeDefined();
      expect(block?.width).toBe(expected.width);
      expect(block?.height).toBe(expected.height);
      expect(Object.keys(block?.defaultParams ?? {}).sort()).toEqual(
        Object.keys(expected.properties).sort()
      );
      for (const [name, property] of Object.entries(expected.properties)) {
        expect(block?.defaultParams[name], `${expected.type}.${name}`).toEqual(property.default);
        if (
          property.source === 'dialog' &&
          (property.type === 'enum' || property.type === 'boolean')
        ) {
          expect(property.invalidRejected, `${expected.type}.${name}`).toBe(true);
        }
      }
    }
  });

  test('records the complete local enumeration domains', () => {
    const enumerations = contract.blocks.flatMap((block) =>
      Object.entries(block.properties)
        .filter(([, property]) => property.type === 'enum')
        .map(([name, property]) => ({ id: `${block.type}.${name}`, values: property.enum }))
    );
    expect(enumerations.length).toBeGreaterThan(0);
    expect(enumerations.every((entry) => entry.values.length > 0)).toBe(true);
    expect(enumerations.find((entry) => entry.id === 'dashboardDisplay.Format')?.values).toContain(
      'Custom'
    );
    expect(enumerations.find((entry) => entry.id === 'dashboardSlider.ScaleType')?.values).toEqual([
      'Linear',
      'Log',
    ]);
  });

  test('ships a distinct SVG palette icon for every dashboard block', () => {
    const icons = dashboardManifest.blocks.map((block) => block.icon);
    expect(icons.every((icon) => typeof icon === 'string' && icon.endsWith('.svg'))).toBe(true);
    expect(new Set(icons).size).toBe(contract.types.length);

    for (const block of dashboardManifest.blocks) {
      const filename = path.join(iconDirectory, String(block.icon));
      expect(existsSync(filename), `${block.type} icon`).toBe(true);
      const source = readFileSync(filename, 'utf8');
      expect(source, `${block.type} SVG root`).toMatch(/^<svg\b/);
      expect(source, `${block.type} SVG viewBox`).toContain('viewBox="0 0 72 72"');
    }
  });
});
//=============================================================================
