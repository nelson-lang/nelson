//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { existsSync, readdirSync, readFileSync } from 'node:fs';
import { extname, join } from 'node:path';
import { expect, test } from 'vitest';
//=============================================================================
const moduleRoot = existsSync(join(process.cwd(), 'src', 'ts', 'libraries'))
  ? process.cwd()
  : join(process.cwd(), 'modules', 'nflow_gui');
function filesUnder(root: string, fileNameOrExtension: string): string[] {
  return readdirSync(root, { withFileTypes: true }).flatMap((entry) => {
    const path = join(root, entry.name);
    if (entry.isDirectory()) return filesUnder(path, fileNameOrExtension);
    return entry.name === fileNameOrExtension || extname(entry.name) === fileNameOrExtension
      ? [path]
      : [];
  });
}
//=============================================================================
function blockTypesFromManifest(manifestPath: string): string[] {
  const manifest = JSON.parse(readFileSync(manifestPath, 'utf8')) as {
    blocks?: Array<{ type?: string }>;
  };
  return (manifest.blocks || []).map((block) => block.type || '').filter(Boolean);
}
//=============================================================================
function dashboardBlockTypes(): string[] {
  return blockTypesFromManifest(
    join(moduleRoot, '..', 'nflow_blocks', 'libraries', 'dashboard', 'library.json')
  );
}
//=============================================================================
function allBlockTypes(): string[] {
  const librariesRoot = join(moduleRoot, '..', 'nflow_blocks', 'libraries');
  return filesUnder(librariesRoot, 'library.json').flatMap((path) => blockTypesFromManifest(path));
}
//=============================================================================
function documentationXml(locale: string): string {
  const roots = [
    join(moduleRoot, '..', 'nflow_blocks', 'help', locale, 'xml'),
    join(moduleRoot, 'help', locale, 'xml'),
  ];
  return roots
    .flatMap((root) => filesUnder(root, '.xml'))
    .map((path) => readFileSync(path, 'utf8'))
    .join('\n');
}
//=============================================================================
test.each(['en_US', 'fr_FR'])('documents every dashboard block type in %s', (locale) => {
  const types = dashboardBlockTypes();
  const xml = documentationXml(locale);
  const missing = types.filter(
    (type) => !xml.includes(`<keyword_alias>block:${type}</keyword_alias>`)
  );

  expect(types).toHaveLength(20);
  expect(missing).toEqual([]);
});
//=============================================================================
test.each(['en_US', 'fr_FR'])('documents every block type with a help alias in %s', (locale) => {
  const types = allBlockTypes();
  const xml = documentationXml(locale);
  const missing = types.filter(
    (type) => !xml.includes(`<keyword_alias>block:${type}</keyword_alias>`)
  );

  expect(types.length).toBeGreaterThan(0);
  expect(missing).toEqual([]);
});
//=============================================================================
function pngDimensions(filePath: string): { width: number; height: number } {
  const data = readFileSync(filePath);
  expect(data.subarray(1, 4).toString('ascii')).toBe('PNG');
  return { width: data.readUInt32BE(16), height: data.readUInt32BE(20) };
}
//=============================================================================
test.each(['en_US', 'fr_FR'])('dashboard help has readable generated images in %s', (locale) => {
  const xmlPath = join(moduleRoot, 'help', locale, 'xml', 'nflow_dashboard.xml');
  const imageDir = join(moduleRoot, 'help', locale, 'images');
  const xml = readFileSync(xmlPath, 'utf8');
  const names = ['nflow_dashboard_library.png', 'nflow_dashboard_binding.png'];

  for (const name of names) {
    expect(xml).toContain(`../images/${name}`);
    const dimensions = pngDimensions(join(imageDir, name));
    expect(dimensions.width).toBeGreaterThanOrEqual(600);
    expect(dimensions.height).toBeGreaterThanOrEqual(500);
  }
});
//=============================================================================
