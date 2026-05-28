//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "unzipBuiltin.hpp"
#include "Error.hpp"
#include "Unzip.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include <algorithm>
#include <cwctype>
//=============================================================================
using namespace Nelson;
//=============================================================================
static std::wstring
toLower(const std::wstring& text)
{
    std::wstring result = text;
    std::transform(result.begin(), result.end(), result.begin(), ::towlower);
    return result;
}
//=============================================================================
static bool
isTextScalar(const ArrayOf& arg)
{
    return arg.isRowVectorCharacterArray() || arg.isScalarStringArray();
}
//=============================================================================
static bool
isPasswordKey(const ArrayOf& arg)
{
    if (!isTextScalar(arg)) {
        return false;
    }
    return toLower(arg.getContentAsWideString()) == L"password";
}
//=============================================================================
ArrayOfVector
Nelson::FileArchiverGateway::unzipBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 4);
    nargoutcheck(nLhs, 0, 1);
    std::wstring zipFilename = argIn[0].getContentAsWideString();
    std::wstring rootPath = L".";
    std::wstring password;
    size_t nameValuePosition = 1;
    if (argIn.size() > 1 && !isPasswordKey(argIn[1])) {
        rootPath = argIn[1].getContentAsWideString();
        nameValuePosition = 2;
    }
    if (((argIn.size() - nameValuePosition) % 2) != 0) {
        Error(_W("Wrong number of input arguments."));
    }
    for (size_t k = nameValuePosition; k < argIn.size(); k += 2) {
        if (!isPasswordKey(argIn[k])) {
            Error(_W("Invalid option name."));
        }
        password = argIn[k + 1].getContentAsWideString();
    }
    wstringVector filenames;

    UnZip(zipFilename, rootPath, password, filenames);
    if (nLhs > 0) {
        retval << ArrayOf::toCellArrayOfCharacterRowVectors(filenames);
    }
    return retval;
}
//=============================================================================
