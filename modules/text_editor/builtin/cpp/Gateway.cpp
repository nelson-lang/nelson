//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NelsonGateway.hpp"
#include "TextEditor.hpp"
#include "editorBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"text_editor";
//=============================================================================
static const nlsGateway gateway[] = {
    { "editor", (ptrBuiltin)Nelson::TextEditorGateway::editorBuiltin, 0, 0,
        CPP_BUILTIN_WITH_EVALUATOR },
};
//=============================================================================
NLS_GATEWAY_FUNC(gateway)
//=============================================================================
static bool
finishTextEditorModule(Nelson::Evaluator* eval)
{
    closeTextEditor();
    return true;
}
//=============================================================================
NLS_GATEWAY_REMOVE_EXTENDED(gateway, (void*)finishTextEditorModule)
//=============================================================================
