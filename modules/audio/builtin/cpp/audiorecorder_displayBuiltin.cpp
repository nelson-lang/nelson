//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "audiorecorder_displayBuiltin.hpp"
#include "AudiorecorderObject.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "DisplayVariableHelpers.hpp"
#include "NelsonConfiguration.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static void
displayVariableName(Interface* io, const std::wstring& name)
{
    if (name.empty()) {
        return;
    }
    if (NelsonConfiguration::getInstance()->getLineSpacingDisplay() == NLS_LINE_SPACING_LOOSE) {
        io->outputMessage(L"\n");
    }
    io->outputMessage(name + L" =\n");
}
//=============================================================================
ArrayOfVector
Nelson::AudioGateway::audiorecorder_dispBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 1, 1);
    ArrayOf param1 = argIn[0];
    if (param1.isHandle()) {
        std::wstring name;
        Interface* io = eval->getInterface();
        if (param1.isScalar()) {
            if (param1.handleCategory() != NLS_HANDLE_AUDIORECORDER_CATEGORY_STR) {
                Error(L"Nelson:audiorecorder:invalidHandle", L"audiorecorder handle expected.");
            }
            auto* objPlayer = (AudiorecorderObject*)param1.asHandleScalar();
            objPlayer->disp(io);
        } else {
            DisplayVariableHeader(io, param1, name, false);
        }
        DisplayVariableFooter(io, name.empty());
    } else {
        Error(L"Nelson:audiorecorder:invalidHandle", L"audiorecorder handle expected.");
    }
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::AudioGateway::audiorecorder_displayBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 1, 2);
    ArrayOf param1 = argIn[0];
    if (param1.isHandle()) {
        std::wstring name = argIn[0].wname();
        if (argIn.size() == 2) {
            name = argIn[1].asWideString();
        }
        Interface* io = eval->getInterface();
        if (param1.isScalar()) {
            displayVariableName(io, name);
            if (param1.handleCategory() != NLS_HANDLE_AUDIORECORDER_CATEGORY_STR) {
                Error(L"Nelson:audiorecorder:invalidHandle", L"audiorecorder handle expected.");
            }
            auto* objPlayer = (AudiorecorderObject*)param1.asHandleScalar();
            objPlayer->disp(io);
        } else {
            DisplayVariableHeader(io, param1, name, false);
        }
        DisplayVariableFooter(io, name.empty());
    } else {
        Error(L"Nelson:audiorecorder:invalidHandle", L"audiorecorder handle expected.");
    }
    return retval;
}
//=============================================================================
