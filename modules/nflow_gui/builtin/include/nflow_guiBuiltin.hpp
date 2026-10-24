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
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
class Evaluator;
//=============================================================================
} // namespace Nelson
//=============================================================================
namespace Nelson::NFlowGateway {
//=============================================================================
// Needs the Evaluator to install the workspace bridge used by the
// fromWorkspace / toWorkspace / nelsonFunction blocks.
ArrayOfVector
nflow_guiBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
//=============================================================================
// Stops and joins every simulation worker, closes every editor window still
// open and removes its bootstrap file. While joining, pending workspace bridge
// calls are processed by the interpreter that owns eval.
void
closeAllNFlowWindows(Evaluator* eval);
//=============================================================================
// Push a one-way host->editor event (highlight and similar) to every open editor
// window. payloadJson is delivered verbatim to window.__nelsonPush(channel, payloadJson).
void
broadcastNFlowPush(const std::string& channel, const std::string& payloadJson);
//=============================================================================
} // namespace Nelson
//=============================================================================
