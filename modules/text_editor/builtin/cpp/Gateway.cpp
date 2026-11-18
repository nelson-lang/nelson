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
#include "NelsonConfiguration.hpp"
#include "NelSon_engine_mode.h"
#include "TextEditor.hpp"
#include "editorBuiltin.hpp"
#include "editorHookBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"text_editor";
//=============================================================================
static const nlsGateway gateway[] = {
    { "editor", (ptrBuiltin)Nelson::TextEditorGateway::editorBuiltin, 0, 0,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "__editor__", (ptrBuiltin)Nelson::TextEditorGateway::editorHookBuiltin, -1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
};
//=============================================================================
static bool
initializeTextEditorModule(Nelson::Evaluator* eval)
{
    // Only the Qt desktop has a native editor window and a QApplication to host it.
    // Install the persistent assistant 'editorInsert' router here so an assistant
    // insert can open the editor on demand even before any editor window exists.
    if (NelsonConfiguration::getInstance()->getNelsonEngineMode() == GUI) {
        installTextEditorAssistantRouter(eval);
    }
    return true;
}
//=============================================================================
NLS_GATEWAY_FUNC_EXTENDED(gateway, (void*)initializeTextEditorModule)
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
