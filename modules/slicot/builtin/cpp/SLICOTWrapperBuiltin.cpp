//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "SLICOTWrapperBuiltin.hpp"
#include "SlicotWrapper.hpp"
#include "Error.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::SlicotGateway::SLICOTWrapperBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    switch (argIn.size()) {
    case 1: {
        std::wstring param1 = argIn[0].getContentAsWideString();
        if (param1 == L"load") {
            retval << ArrayOf::logicalConstructor(loadSlicotLibrary());
        } else if (param1 == L"free") {
            retval << ArrayOf::logicalConstructor(freeSlicotLibrary());
        } else {
            Error(_W("Wrong value for #1: 'load' or 'free' expected."));
        }
    } break;
    case 2: {
        std::wstring param1 = argIn[0].getContentAsWideString();
        if (param1 == L"load") {
            std::wstring slicotName = argIn[0].getContentAsWideString();
            retval << ArrayOf::logicalConstructor(loadSlicotLibrary(slicotName));
        } else {
            Error(_W("Wrong value for #1: 'load' expected."));
        }
    } break;
    default: {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } break;
    }
    return retval;
}
//=============================================================================
