//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "fullpathBuiltin.hpp"
#include "Error.hpp"
#include "FileSystemWrapper.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FilesFoldersGateway::fullpathBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf param1 = argIn[0];
    if (param1.isCellArrayOfCharacterVectors() || param1.isStringArray()) {
        Dimensions dims = param1.getDimensions();
        wstringVector paths = param1.getContentAsWideStringVector();
        wstringVector normalizedPaths;
        normalizedPaths.reserve(dims.getElementCount());
        for (const std::wstring& s : paths) {
            normalizedPaths.push_back(FileSystemWrapper::Path::normalize(s));
        }
        if (param1.isStringArray()) {
            retval << ArrayOf::stringArrayConstructor(normalizedPaths, dims);
        } else {
            retval << ArrayOf::toCellArrayOfCharacterColumnVectors(normalizedPaths);
        }
    } else {
        std::wstring path = argIn[0].getContentAsWideString();
        retval << ArrayOf::characterArrayConstructor(FileSystemWrapper::Path::normalize(path));
    }
    return retval;
}
//=============================================================================
