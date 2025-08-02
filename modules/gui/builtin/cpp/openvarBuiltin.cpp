//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "openvarBuiltin.hpp"
#include "Error.hpp"
#include "PredefinedErrorMessages.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "VariablesEditor.hpp"
#include "NelsonConfiguration.hpp"
#include "NelSon_engine_mode.h"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::GuiGateway::openvarBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 0);
    auto engineMode = NelsonConfiguration::getInstance()->getNelsonEngineMode();
    if (engineMode != GUI) {
        return {};
    }
    std::wstring variableName = argIn[0].getContentAsWideString();
    if (!VariablesEditor::getVariablesEditor()) {
        VariablesEditor::createVariablesEditor(eval);
    }
    VariablesEditor::openVariable(variableName);
    VariablesEditor::showVariablesEditor();
    return {};
}
//=============================================================================
