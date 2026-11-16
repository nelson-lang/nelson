//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "nlsNelson_manager_exports.h"
#include <functional>
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
// A sink that receives the assistant's live events (channel = reset/token/
// activity/answer/error, payloadJson = the event data). The Qt desktop's
// dockable assistant registers one; it is called from the interpreter thread,
// so the sink must marshal UI work to its own thread.
using AssistantSinkFn = std::function<void(const std::string& channel, const std::string& payloadJson)>;
//=============================================================================
NLSNELSON_MANAGER_IMPEXP void
SetAssistantSink(const AssistantSinkFn& sink);
//=============================================================================
NLSNELSON_MANAGER_IMPEXP void
AssistantSinkPush(const std::string& channel, const std::string& payloadJson);
//=============================================================================
// A second, independent assistant sink for the NFlow host: its embedded
// assistant pane registers one so the same live events (reset/token/activity/
// answer/error, and surface channels such as nflowApply) reach the NFlow window.
// Separate from the Qt dock sink above because NFlow is a distinct host window.
NLSNELSON_MANAGER_IMPEXP void
SetNFlowAssistantSink(const AssistantSinkFn& sink);
//=============================================================================
NLSNELSON_MANAGER_IMPEXP void
NFlowAssistantSinkPush(const std::string& channel, const std::string& payloadJson);
//=============================================================================
// Dedicated assistant Stop flag, independent of the terminal interrupt: set from
// any Stop control (web pane term.interrupt, Qt dock button), consumed by the
// assistant's async poll loop. Decoupled because the loop runs off the prompt,
// where the terminal interrupt path does not reach it.
NLSNELSON_MANAGER_IMPEXP void
RequestAssistantStop();
//=============================================================================
NLSNELSON_MANAGER_IMPEXP bool
ConsumeAssistantStop();
//=============================================================================
// Per-surface routing for isolated assistant sessions. Each surface registers
// its sink under a surfaceId ("qt" / "web" / "nflow" / "notebook"); events and
// Stop then target one surface only, so concurrent prompts on different surfaces
// no longer collide on a single shared session.
NLSNELSON_MANAGER_IMPEXP void
RegisterAssistantSink(const std::string& surfaceId, const AssistantSinkFn& sink);
//=============================================================================
NLSNELSON_MANAGER_IMPEXP void
UnregisterAssistantSink(const std::string& surfaceId);
//=============================================================================
NLSNELSON_MANAGER_IMPEXP void
AssistantSinkPushTo(
    const std::string& surfaceId, const std::string& channel, const std::string& payloadJson);
//=============================================================================
// Push an event to EVERY registered sink (used for host-agnostic, surface-less
// tool actions such as editorInsert / notebookInsert, which must reach whichever
// host editor is listening); each sink filters by channel.
NLSNELSON_MANAGER_IMPEXP void
AssistantSinkBroadcast(const std::string& channel, const std::string& payloadJson);
//=============================================================================
NLSNELSON_MANAGER_IMPEXP void
RequestAssistantStop(const std::string& surfaceId);
//=============================================================================
NLSNELSON_MANAGER_IMPEXP bool
ConsumeAssistantStop(const std::string& surfaceId);
//=============================================================================
} // namespace Nelson
//=============================================================================
