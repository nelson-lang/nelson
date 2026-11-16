//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Shared, transport-agnostic AI assistant pane. Both web front ends (web_gui
// and nflow_gui) consume it as source through the `@nelson/assistant` alias, so
// neither depends on the other. Each host injects its own AssistantBackend.
export { AssistantPane } from "./AssistantPane";
export type {
  AssistantBackend,
  AssistantCodeAction,
  AssistantCodeActionHelpers,
  AssistantPaneProps,
} from "./AssistantPane";
//=============================================================================
