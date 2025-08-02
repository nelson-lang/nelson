//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "whonh5Builtin.hpp"
#include "whoNh5File.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOfVector
Nelson::Hdf5Gateway::whonh5Builtin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1);
    std::wstring filename = argIn[0].getContentAsWideString();
    wstringVector names;
    for (indexType k = 1; k < (indexType)argIn.size(); k++) {
        names.push_back(argIn[k].getContentAsWideString());
    }
    Interface* io = eval->getInterface();
    ArrayOf ce = whoNh5File(io, filename, names, nLhs == 1);
    if (nLhs == 1) {
        retval << ce;
    }
    return retval;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
