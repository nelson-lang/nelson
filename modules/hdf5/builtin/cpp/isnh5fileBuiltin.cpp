//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "isnh5fileBuiltin.hpp"
#include "isNh5File.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// isnh5file(filename)
//=============================================================================
ArrayOfVector
Nelson::Hdf5Gateway::isnh5fileBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 3);
    nargincheck(argIn, 1, 1);
    wstringVector filenames = argIn[0].getContentAsWideStringVector(true);
    ArrayOf isNh5;
    ArrayOf nh5Versions;
    ArrayOf nh5Headers;
    isNh5File(filenames, isNh5, nh5Versions, nh5Headers);
    retval << isNh5;
    if (nLhs > 1) {
        retval << nh5Versions;
    }
    if (nLhs > 2) {
        retval << nh5Headers;
    }
    return retval;
}
//=============================================================================
