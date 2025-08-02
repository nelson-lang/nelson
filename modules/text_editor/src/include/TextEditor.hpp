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
#include "Evaluator.hpp"
#include "nlsText_editor_exports.h"
#include <string>
//=============================================================================
namespace Nelson {
NLSTEXT_EDITOR_IMPEXP bool
textEditor(Evaluator* eval);
NLSTEXT_EDITOR_IMPEXP bool
textEditor(Evaluator* eval, const std::wstring& filename);
NLSTEXT_EDITOR_IMPEXP bool
textEditor(Evaluator* eval, const wstringVector& filenames);
NLSTEXT_EDITOR_IMPEXP bool
textEditor(Evaluator* eval, bool importSharedText);
NLSTEXT_EDITOR_IMPEXP bool
closeTextEditor();
//=============================================================================
NLSTEXT_EDITOR_IMPEXP bool
isTextEditorVisible();
NLSTEXT_EDITOR_IMPEXP void
showTextEditor();
} // namespace Nelson
//=============================================================================
