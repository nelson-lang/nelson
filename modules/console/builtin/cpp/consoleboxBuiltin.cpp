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
        Warning(_W("consolebox builtin is only available in GUI mode on Windows."));
        retval << ArrayOf::logicalConstructor(false);
        return retval;
    }
    if (argIn.size() == 0) {
        // status
        bool res = ConsoleBoxToggle();
        retval << ArrayOf::logicalConstructor(res);
    } else {
        // show/hide/toggle/status
        if (argIn[0].isRowVectorCharacterArray() || argIn[0].isScalarStringArray()) {
            std::wstring str = argIn[0].getContentAsWideString();
            if (str == L"on" || str == L"show") {
                bool res = ConsoleBoxShow(true);
                retval << ArrayOf::logicalConstructor(res);
                return retval;
            }
            if (str == L"off" || str == L"hide") {
                bool res = ConsoleBoxShow(false);
                retval << ArrayOf::logicalConstructor(res);
                return retval;
            }
            if (str == L"status") {
                bool res = ConsoleBoxIsVisible();
                retval << ArrayOf::logicalConstructor(res);
                return retval;
            }
            if (str == L"toggle") {
                bool res = ConsoleBoxToggle();
                retval << ArrayOf::logicalConstructor(res);
                return retval;
            }
            Error(_W("Wrong value for #1 argument."));
        }
        if (argIn[0].isLogical() && argIn[0].isScalar()) {
            bool show = argIn[0].getContentAsLogicalScalar();
            bool res = ConsoleBoxShow(show);
            retval << ArrayOf::logicalConstructor(res);
            return retval;
        }
        Error(_W("Wrong type for #1 argument."));
    }
#else
    Warning(_W("consolebox builtin is not implemented for this platform."));
    retval << ArrayOf::logicalConstructor(false);
#endif
    return retval;
}
//=============================================================================
