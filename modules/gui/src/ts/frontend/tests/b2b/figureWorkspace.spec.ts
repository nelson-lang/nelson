//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { test } from '@playwright/test';
import { figureWorkspaceScenario } from '../figureWorkspaceScenario';
import { figureSubplotsScenario } from '../figureSubplotsScenario';
import { figureFitScenario } from '../figureFitScenario';
import { figureLivePanScenario } from '../figureLivePanScenario';
import { figureRestoreScenario } from '../figureRestoreScenario';
import { figureQuickActionsScenario } from '../figureQuickActionsScenario';
import { figureConcurrencyScenario } from '../figureConcurrencyScenario';
import { figure3dScalesScenario } from '../figure3dScalesScenario';
//=============================================================================
test('synchronizes the figure workspace with real graphics objects', async ({ page }) => {
  await figureWorkspaceScenario(page);
});
//=============================================================================
test('keeps subplot navigation, labels, printing and detached saving in sync', async ({ page }) => {
  await figureSubplotsScenario(page);
});
//=============================================================================
test('fits a moved 3D subplot without shifting its layout or other subplots', async ({ page }) => {
  await figureFitScenario(page);
});
//=============================================================================
test('updates 2D and 3D subplots throughout a held pan gesture', async ({ page }) => {
  await figureLivePanScenario(page);
});
//=============================================================================
test('targets subplots with double-click, indicators and focused keyboard shortcuts', async ({
  page,
}) => {
  await figureQuickActionsScenario(page);
});
//=============================================================================
test('restores independent views across tiled, logarithmic, reversed and detached axes', async ({
  page,
}) => {
  await figureRestoreScenario(page);
});
//=============================================================================
test('orders Fit and Restore during a held rotation and safely closes an active figure', async ({
  page,
}) => {
  await figureConcurrencyScenario(page);
});
//=============================================================================
test('navigates logarithmic reversed 3D surfaces and curves with both projections', async ({
  page,
}) => {
  await figure3dScalesScenario(page);
});
//=============================================================================
