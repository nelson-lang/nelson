//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import {
  bindDocumentViewPreferences,
  _resetDocumentViewPreferencesForTests,
} from './document-view-preferences.js';
import { setCurrentFilePath } from './state.js';
import { getViewBox, getZoomScale, setViewBox, setZoomScale } from './workspace-handlers.js';
import { _resetUiPrefsForTests } from './ui-prefs.js';
//=============================================================================
beforeEach(() => {
  _resetDocumentViewPreferencesForTests();
  _resetUiPrefsForTests();
  localStorage.clear();
  setCurrentFilePath(null);
});
//=============================================================================
afterEach(() => _resetDocumentViewPreferencesForTests());
test('restores a view only when the same document is opened again', () => {
  bindDocumentViewPreferences();
  setCurrentFilePath('C:/models/a.nflow');
  setZoomScale(2);
  setViewBox({ x: 120, y: 80, w: 640, h: 420 });

  setCurrentFilePath(null);
  setZoomScale(1);
  setViewBox({ x: 0, y: 0, w: 100, h: 100 });
  expect(getZoomScale()).toBe(1);

  setCurrentFilePath('C:/models/a.nflow');
  expect(getZoomScale()).toBe(2);
  expect(getViewBox()).toEqual({ x: 120, y: 80, w: 640, h: 420 });
});
//=============================================================================
test('does not store a view for an unsaved diagram', () => {
  bindDocumentViewPreferences();
  setCurrentFilePath(null);
  setViewBox({ x: 5, y: 5, w: 500, h: 300 });
  expect([...Array(localStorage.length)].map((_, i) => localStorage.key(i))).not.toContainEqual(
    expect.stringContaining('nf-document-view-')
  );
});
//=============================================================================
