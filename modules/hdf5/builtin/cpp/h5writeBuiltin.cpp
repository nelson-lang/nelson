//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "h5writeBuiltin.hpp"
#include "h5WriteDataset.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// h5write(filename, location, data)
//=============================================================================
ArrayOfVector
Nelson::Hdf5Gateway::h5writeBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 3, 3);
    ArrayOf param1 = argIn[0];
    std::wstring filename = param1.getContentAsWideString();
    ArrayOf param2 = argIn[1];
    std::wstring location = param2.getContentAsWideString();
    ArrayOf data = argIn[2];
    h5WriteDataset(filename, location, data);
    return retval;
}
//=============================================================================
