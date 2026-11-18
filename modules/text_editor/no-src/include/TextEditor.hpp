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
#include "nlsText_editor_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class Evaluator;
//=============================================================================
NLSTEXT_EDITOR_IMPEXP bool
isTextEditorVisible();
NLSTEXT_EDITOR_IMPEXP void
showTextEditor();
NLSTEXT_EDITOR_IMPEXP bool
closeTextEditor();
// No native editor in this build: the assistant 'editorInsert' router is a no-op.
NLSTEXT_EDITOR_IMPEXP void
installTextEditorAssistantRouter(Evaluator* eval);
//=============================================================================
} // namespace Nelson
//=============================================================================
