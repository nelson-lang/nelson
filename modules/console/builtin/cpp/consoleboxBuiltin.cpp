//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "consoleboxBuiltin.hpp"
#include "ConsoleBox.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "Warning.hpp"
#include "ErrorID.hpp"
#ifdef _MSC_VER
#include "NelsonConfiguration.hpp"
#include "NelSon_engine_mode.h"
#endif
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ConsoleGateway::consoleboxBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 1);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;
#ifdef _MSC_VER
    if (NelsonConfiguration::getInstance()->getNelsonEngineMode() != GUI) {
        Warning(L"Nelson:consolebox:guiModeOnly",
            L"consolebox builtin is only available in GUI mode on Windows.");
        retval << ArrayOf::logicalScalar(false);
        return retval;
    }
    if (argIn.size() == 0) {
        bool res = ConsoleBoxToggle();
        retval << ArrayOf::logicalScalar(res);
    } else {
        if (argIn[0].isRowVectorCharacterArray() || argIn[0].isScalarStringArray()) {
            std::wstring str = argIn[0].asWideString();
            if (str == L"on" || str == L"show") {
                bool res = ConsoleBoxShow(true);
                retval << ArrayOf::logicalScalar(res);
                return retval;
            }
            if (str == L"off" || str == L"hide") {
                bool res = ConsoleBoxShow(false);
                retval << ArrayOf::logicalScalar(res);
                return retval;
            }
            if (str == L"status") {
                bool res = ConsoleBoxIsVisible();
                retval << ArrayOf::logicalScalar(res);
                return retval;
            }
            if (str == L"toggle") {
                bool res = ConsoleBoxToggle();
                retval << ArrayOf::logicalScalar(res);
                return retval;
            }
            Error(L"Nelson:consolebox:invalidValue", L"Wrong value for #%d argument.", 1);
        }
        if (argIn[0].isLogical() && argIn[0].isScalar()) {
            bool show = argIn[0].asLogicalScalar();
            bool res = ConsoleBoxShow(show);
            retval << ArrayOf::logicalScalar(res);
            return retval;
        }
        Error(L"Nelson:types:wrongArgumentType", L"Wrong type for #%d argument.", 1);
    }
#else
    Warning(L"Nelson:consolebox:notImplemented",
        L"consolebox builtin is not implemented for this platform.");
    retval << ArrayOf::logicalScalar(false);
#endif
    return retval;
}
//=============================================================================
