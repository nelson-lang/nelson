//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nflowBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "WebDesktopBridge.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace Nelson::NFlowGateway {
//=============================================================================
ArrayOfVector
nflowBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 1);
    nargoutcheck(nLhs, 0, 0);

    std::wstring modelPath;
    if (!argIn.empty()) {
        if (!argIn[0].isRowVectorCharacterArray() && !argIn[0].isScalarStringArray()) {
            Error(L"Nelson:nflow:invalidModelFilename",
                L"nflow: the model filename must be a string.");
        }
        modelPath = utf8_to_wstring(argIn[0].asUtf8String());
    }

    if (!WebDesktopBridge::getInstance()->openWebNFlow(eval, modelPath)) {
        Error("Nelson:nflow:unavailable",
            "The NFlow graphical editor is not available in this runtime.");
    }
    return {};
}
//=============================================================================
} // namespace Nelson::NFlowGateway
//=============================================================================
