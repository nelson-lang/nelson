//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "zipBuiltin.hpp"
#include "Error.hpp"
#include "Zip.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FileArchiverGateway::zipBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 3);
    nargoutcheck(nLhs, 0, 1);
    std::wstring rootPath = L".";
    std::wstring zipFilename = argIn[0].getContentAsWideString();
    wstringVector names = argIn[1].getContentAsWideStringVector();
    wstringVector filenames;
    if (argIn.size() > 2) {
        rootPath = argIn[2].getContentAsWideString();
        if (rootPath.empty()) {
            rootPath = L".";
        }
    }
    Zip(zipFilename, names, rootPath, filenames);
    if (nLhs > 0) {
        retval << ArrayOf::toCellArrayOfCharacterRowVectors(filenames);
    }
    return retval;
}
//=============================================================================
