//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Evaluator.hpp"
#include "NelsonGateway.hpp"
#include "notebookBuiltin.hpp"
#include "NotebookEditor.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"notebook";
//=============================================================================
static const nlsGateway gateway[] = {
    { "notebook", (ptrBuiltin)Nelson::NotebookGateway::notebookBuiltin, 1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
};
//=============================================================================
static bool
finishNotebookModule(Evaluator*)
{
    Nelson::NotebookGateway::closeNotebookWindow();
    return true;
}
//=============================================================================
NLS_GATEWAY_FUNC(gateway)
NLS_GATEWAY_REMOVE_EXTENDED(gateway, (void*)finishNotebookModule)
//=============================================================================
