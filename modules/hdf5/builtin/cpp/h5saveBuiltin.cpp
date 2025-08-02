//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "h5saveBuiltin.hpp"
#include "h5Save.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOfVector
Nelson::Hdf5Gateway::h5saveBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 1);
    std::wstring filename = argIn[0].getContentAsWideString();
    wstringVector names;
    bool bAppend = false;
    bool bNoCompression = false;
    for (size_t k = 1; k < argIn.size(); k++) {
        ArrayOf paramK = argIn[k];
        std::wstring param = paramK.getContentAsWideString();
        if (param == L"-append") {
            bAppend = true;
        } else if (param == L"-nocompression") {
            bNoCompression = true;
        } else {
            names.push_back(param);
        }
    }
    h5Save(eval, filename, names, bAppend, bNoCompression);
    return retval;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
