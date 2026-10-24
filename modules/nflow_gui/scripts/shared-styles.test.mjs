// ============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
// ============================================================================
// This file is part of Nelson.
// ============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
// ============================================================================
import assert from 'node:assert/strict';
import test from 'node:test';
import { inlineStylesheetImport } from './shared-styles.mjs';

const importRule = "@import url('../../../../webview/frontend/examples/gallery-shared.css');";

test('inlines the shared gallery stylesheet for distribution', () => {
  const result = inlineStylesheetImport(
    `${importRule}\n\n.local { color: blue; }\n`,
    importRule,
    '.shared { color: red; }\n'
  );
  assert.equal(result, '.shared { color: red; }\n\n.local { color: blue; }\n');
});

test('rejects a missing or duplicated shared stylesheet import', () => {
  assert.throws(() => inlineStylesheetImport('.local {}', importRule, '.shared {}'));
  assert.throws(() =>
    inlineStylesheetImport(`${importRule}\n${importRule}`, importRule, '.shared {}')
  );
});
