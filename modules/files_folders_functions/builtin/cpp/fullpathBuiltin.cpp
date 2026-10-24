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
        Dimensions dims = param1.dimensions();
        wstringVector paths = param1.asWideStringVector();
        wstringVector normalizedPaths;
        normalizedPaths.reserve(dims.elementCount());
        for (const std::wstring& s : paths) {
            normalizedPaths.push_back(FileSystemWrapper::Path::normalize(s));
        }
        if (param1.isStringArray()) {
            retval << ArrayOf::stringArray(normalizedPaths, dims);
        } else {
            retval << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(normalizedPaths);
        }
    } else {
        std::wstring path = argIn[0].asWideString();
        retval << ArrayOf::characterArray(FileSystemWrapper::Path::normalize(path));
    }
    return retval;
}
//=============================================================================
