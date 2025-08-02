//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "h5readattBuiltin.hpp"
#include "Error.hpp"
#include "h5ReadAttribute.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// attval = h5readatt(filename, location, attr)
//=============================================================================
ArrayOfVector
Nelson::Hdf5Gateway::h5readattBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 3, 3);
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    ArrayOf param3 = argIn[2];
    std::wstring filename = param1.getContentAsWideString();
    std::wstring location = param2.getContentAsWideString();
    std::wstring attribute = param3.getContentAsWideString();
    retval << h5ReadAttribute(filename, location, attribute);
    return retval;
}
//=============================================================================
