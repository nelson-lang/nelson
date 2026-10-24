//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FileSystemWrapper.hpp"
#include "diff_fileBuiltin.hpp"
#include "Error.hpp"
#include "FileDiff.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FilesFoldersGateway::diff_fileBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    logical eolCompare = true;
    std::wstring filename1 = param1.asWideString();
    std::wstring filename2 = param2.asWideString();
    if (argIn.size() == 3) {
        ArrayOf param3 = argIn[2];
        eolCompare = param3.asLogicalScalar();
    }
    std::wstring res;
    FileDiff(filename1, filename2, eolCompare, res);
    retval << ArrayOf::characterArray(res);
    return retval;
}
//=============================================================================
