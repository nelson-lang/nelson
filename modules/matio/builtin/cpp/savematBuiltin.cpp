//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "savematBuiltin.hpp"
#include "SaveMatioFile.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isOption(const std::wstring& param)
{
    return param.size() > 2 && param[0] == L'-';
}
//=============================================================================
static bool
isValidOption(const std::wstring& option)
{
    return option == L"-append" || option == L"-nocompression" || option == L"-v4"
        || option == L"-v6" || option == L"-v7" || option == L"-v7.3";
}
//=============================================================================
ArrayOfVector
Nelson::MatioGateway::savematBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 1);
    std::wstring filename = argIn[0].getContentAsWideString();
    wstringVector names;
    std::wstring matFileVersion = L"-v7.3";
    bool bAppend = false;
    bool bNoCompression = false;
    for (size_t k = 1; k < argIn.size(); k++) {
        ArrayOf paramK = argIn[k];
        std::wstring param = paramK.getContentAsWideString();
        if (isOption(param)) {
            if (isValidOption(param)) {
                if (param == L"-append") {
                    bAppend = true;
                }
                if (param == L"-nocompression") {
                    bNoCompression = true;
                }
                if (param == L"-v4") {
                    matFileVersion = param;
                }
                if (param == L"-v6") {
                    matFileVersion = param;
                }
                if (param == L"-v7") {
                    matFileVersion = param;
                }
                if (param == L"-v7.3") {
                    matFileVersion = param;
                }
            } else {
                Error(_W("Invalid option:") + param);
            }
        } else {
            names.push_back(param);
        }
    }
    SaveMatioFile(eval, filename, names, matFileVersion, bAppend, bNoCompression);
    return retval;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
