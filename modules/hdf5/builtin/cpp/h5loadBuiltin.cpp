//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "h5loadBuiltin.hpp"
#include "h5Load.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOfVector
Nelson::Hdf5Gateway::h5loadBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1);
    std::wstring filename = argIn[0].getContentAsWideString();
    wstringVector names;
    for (size_t k = 1; k < argIn.size(); k++) {
        names.push_back(argIn[k].getContentAsWideString());
    }
    ArrayOf st = h5Load(eval, filename, names, nLhs == 1);
    if (nLhs == 1) {
        retval << st;
    }
    return retval;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
