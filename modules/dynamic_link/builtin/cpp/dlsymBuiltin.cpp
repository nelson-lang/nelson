//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "dlsymBuiltin.hpp"
#include "CreateDynamicLinkLibraryObject.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DynamicLinkGateway::dlsymBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 4, 4);
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    std::wstring symbolName = param2.getContentAsWideString();
    ArrayOf param3 = argIn[2];
    std::wstring returnTypeString = param3.getContentAsWideString();
    ArrayOf param4 = argIn[3];
    wstringVector argumentsString = param4.getContentAsWideStringVector(true);
    retval << createDynamicLinkSymbolObject(param1, symbolName, returnTypeString, argumentsString);
    return retval;
}
//=============================================================================
