//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
export const CONFIRM_SAVE_EVENT = 'nflow:confirm-save';
export const ABOUT_SHOW_EVENT = 'nflow:about-show';
export const ABOUT_HIDE_EVENT = 'nflow:about-hide';
export const GRID_SETTINGS_SHOW_EVENT = 'nflow:grid-settings-show';
export const GRID_SETTINGS_HIDE_EVENT = 'nflow:grid-settings-hide';
export const MISSING_BLOCKS_EVENT = 'nflow:missing-blocks';
export const FILE_PICKER_EVENT = 'nflow:file-picker';
export const IMPORT_FMU_EVENT = 'nflow:import-fmu';
export const EXAMPLE_BROWSER_SHOW_EVENT = 'nflow:example-browser-show';
export const EXAMPLE_BROWSER_HIDE_EVENT = 'nflow:example-browser-hide';
// Request the main app to open a model by absolute path. Dispatched by the
// Example Browser and handled by menu-handlers, so the open (and the resulting
// currentFilePath) always lands in the app instance that Save reads from.
export const OPEN_MODEL_PATH_EVENT = 'nflow:open-model-path';
//=============================================================================
