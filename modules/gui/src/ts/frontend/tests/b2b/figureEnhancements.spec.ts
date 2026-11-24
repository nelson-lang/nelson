//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { test } from '@playwright/test';
import { figureEnhancementsScenario } from '../figureEnhancementsScenario';
import { figureColorbarPerformanceScenario } from '../figureColorbarPerformanceScenario';
import {
  figureMeshRotationScenario,
  figureUiLayoutScenario,
} from '../figureDemoRegressionsScenario';
//=============================================================================
test('supports enhanced inspection, history, preferences, isolation and property editors', async ({
  page,
}) => {
  await figureEnhancementsScenario(page);
});
//=============================================================================
test('keeps Colorbar layouts responsive with editable properties', async ({ page }) => {
  await figureColorbarPerformanceScenario(page);
});
//=============================================================================
test('keeps UI controls separate from the plot when properties open', async ({ page }) => {
  test.setTimeout(300_000);
  await figureUiLayoutScenario(page);
});
//=============================================================================
test('rotates the Bunny and Teapot meshes with hidden axes using the mouse', async ({ page }) => {
  test.setTimeout(300_000);
  await figureMeshRotationScenario(page);
});
//=============================================================================
