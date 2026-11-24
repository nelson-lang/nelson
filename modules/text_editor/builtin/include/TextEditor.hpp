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
namespace Nelson {
//=============================================================================
class Evaluator;
//=============================================================================
bool
isTextEditorVisible();
void
showTextEditor();
bool
closeTextEditor();
// No native editor in this build: the assistant 'editorInsert' router is a no-op.
void
installTextEditorAssistantRouter(Evaluator* eval);
//=============================================================================
} // namespace Nelson
//=============================================================================
