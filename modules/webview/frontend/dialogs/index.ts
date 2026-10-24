//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Shared, Qt-free dialog components. They live in the webview module, which
// both web front ends (web_gui and nflow_gui) already depend on, so neither
// depends on the other. Consumed as source through the `@nelson/dialogs` alias.
export {
  FilePickerDialog,
  default as FilePickerDialogDefault,
} from './FilePickerDialog';
//=============================================================================
export type {
  FilePickerMode,
  FilePickerFsEntry,
  FilePickerListResult,
  FilePickerDialogProps,
} from './FilePickerDialog';
//=============================================================================
