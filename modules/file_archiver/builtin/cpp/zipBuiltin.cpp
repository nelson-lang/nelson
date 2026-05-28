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
isNameValueKey(const ArrayOf& arg)
{
    if (!isTextScalar(arg)) {
        return false;
    }
    std::wstring key = toLower(arg.getContentAsWideString());
    return key == L"password" || key == L"encryptionmethod";
}
//=============================================================================
ArrayOfVector
Nelson::FileArchiverGateway::zipBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 7);
    nargoutcheck(nLhs, 0, 1);
    std::wstring rootPath = L".";
    std::wstring zipFilename = argIn[0].getContentAsWideString();
    wstringVector names = argIn[1].getContentAsWideStringVector();
    std::wstring password;
    std::wstring encryptionMethod;
    wstringVector filenames;
    size_t nameValuePosition = 2;
    if (argIn.size() > 2 && !isNameValueKey(argIn[2])) {
        rootPath = argIn[2].getContentAsWideString();
        if (rootPath.empty()) {
            rootPath = L".";
        }
        nameValuePosition = 3;
    }
    if (((argIn.size() - nameValuePosition) % 2) != 0) {
        Error(_W("Wrong number of input arguments."));
    }
    for (size_t k = nameValuePosition; k < argIn.size(); k += 2) {
        if (!isTextScalar(argIn[k])) {
            Error(_W("Invalid option name."));
        }
        std::wstring key = toLower(argIn[k].getContentAsWideString());
        std::wstring value = argIn[k + 1].getContentAsWideString();
        if (key == L"password") {
            password = value;
        } else if (key == L"encryptionmethod") {
            encryptionMethod = value;
        } else {
            Error(_W("Invalid option name."));
        }
    }
    Zip(zipFilename, names, rootPath, password, encryptionMethod, filenames);
    if (nLhs > 0) {
        retval << ArrayOf::toCellArrayOfCharacterRowVectors(filenames);
    }
    return retval;
}
//=============================================================================
