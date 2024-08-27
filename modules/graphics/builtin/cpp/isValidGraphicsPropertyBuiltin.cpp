//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "isValidGraphicsPropertyBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "IsValidGraphicsProperty.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
GraphicsGateway::isValidGraphicsPropertyBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);

    std::wstring GOTypename = argIn[0].getContentAsWideString();
    std::wstring GOPropertyName = argIn[1].getContentAsWideString();

    bool isValidPropertyName = IsValidGraphicsProperty(GOTypename, GOPropertyName);

    retval << ArrayOf::logicalConstructor(isValidPropertyName);
    return retval;
}
//=============================================================================
