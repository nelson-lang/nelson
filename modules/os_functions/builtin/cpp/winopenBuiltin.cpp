//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "winopenBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "WinOpen.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::OsFunctionsGateway::winopenBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
#ifdef _MSC_VER
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 0);
    std::wstring cmd;
    if (argIn[0].isRowVectorCharacterArray() || argIn[0].isScalarStringArray()) {
        cmd = argIn[0].asWideString();
    } else {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"string");
    }
    if (!WinOpen(cmd)) {
        Error(L"Nelson:winopen:noAssociatedApplication",
            L"Filename not associated to an application.");
    }
#else
    Error(L"Nelson:os_functions:notImplemented", L"Not implemented on this platform.");
#endif
    return retval;
}
//=============================================================================
