//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "h5readBuiltin.hpp"
#include "Error.hpp"
#include "h5ReadDataset.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// data = h5read(filename, datasetname)
//=============================================================================
ArrayOfVector
Nelson::Hdf5Gateway::h5readBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    std::wstring filename;
    std::wstring datasetname;
    switch (argIn.size()) {
    case 2: {
        ArrayOf param1 = argIn[0];
        ArrayOf param2 = argIn[1];
        filename = param1.getContentAsWideString();
        datasetname = param2.getContentAsWideString();
    } break;
    default: {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } break;
    }
    retval << h5ReadDataset(filename, datasetname);
    return retval;
}
//=============================================================================
