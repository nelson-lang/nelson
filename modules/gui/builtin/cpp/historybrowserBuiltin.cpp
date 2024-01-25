//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "historybrowserBuiltin.hpp"
#include "Error.hpp"
#include "PredefinedErrorMessages.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "HistoryBrowser.hpp"
#include "NelsonConfiguration.hpp"
#include "NelSon_engine_mode.h"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::GuiGateway::historybrowserBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 2);
    nargoutcheck(nLhs, 0, 0);
    auto engineMode = NelsonConfiguration::getInstance()->getNelsonEngineMode();
    if (engineMode != GUI) {
        return {};
    }
    switch (argIn.size()) {
    case 0: {
        HistoryBrowser::showHistoryBrowser();
    } break;
    case 1: {
        std::wstring param = argIn[0].getContentAsWideString();
        if (param == L"sync") {
            HistoryBrowser::synchronizeHistoryBrowser();
        } else {
            Error(_W("Wrong value for #1 argument."));
        }
    } break;
    case 2: {
        std::wstring param1 = argIn[0].getContentAsWideString();
        if (param1 == L"visible") {
            if (argIn[1].isRowVectorCharacterArray()) {
                std::wstring param2 = argIn[1].getContentAsWideString();
                if (param2 == L"toggle") {
                    HistoryBrowser::toggleVisibilityHistoryBrowser();
                } else {
                    Error(_W("Wrong value for #2 argument."));
                }
            } else {
                logical visibility = argIn[1].getContentAsLogicalScalar();
                if (visibility) {
                    HistoryBrowser::showHistoryBrowser();
                } else {
                    HistoryBrowser::hideHistoryBrowser();
                }
            }
        } else {
            Error(_W("Wrong value for #1 argument."));
        }
    } break;
    }
    return {};
}
//=============================================================================
