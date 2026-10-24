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
#include "isdirBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FilesFoldersGateway::isdirBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    if (argIn[0].isCellArrayOfCharacterVectors() || argIn[0].isStringArray()) {
        Dimensions dim = argIn[0].dimensions();
        if (argIn[0].isEmpty()) {
            retval << ArrayOf::empty(dim);
        } else {
            ArrayOf cell(argIn[0]);
            logical* bmat = static_cast<logical*>(ArrayOf::allocateArrayOf(
                NLS_LOGICAL, argIn[0].elementCount(), stringVector(), false));
            indexType elementCount = dim.elementCount();
            for (indexType k = 0; k < elementCount; k++) {
                ArrayOf cell(argIn[0]);
                auto* arg = (ArrayOf*)(cell.dataPointer());
                if (arg[k].isRowVectorCharacterArray()) {
                    bool permissionDenied;
                    bmat[k] = static_cast<Nelson::logical>(FileSystemWrapper::Path::is_directory(
                        arg[k].asWideString(), permissionDenied));
                    if (permissionDenied) {
                        Error(L"Nelson:files_folders_functions:permissionDenied",
                            L"Permission denied.");
                    }
                } else {
                    bmat[k] = static_cast<Nelson::logical>(false);
                }
            }
            ArrayOf res = ArrayOf(NLS_LOGICAL, dim, bmat, false);
            retval << res;
        }
        return retval;
    }
    if (argIn[0].isRowVectorCharacterArray()) {
        std::wstring wpath = argIn[0].asWideString();
        bool permissionDenied;
        retval << ArrayOf::logicalScalar(
            FileSystemWrapper::Path::is_directory(wpath, permissionDenied));
        if (permissionDenied) {
            Error(L"Nelson:files_folders_functions:permissionDenied", L"Permission denied.");
        }
        return retval;
    }
    if (argIn[0].isEmpty()) {
        retval << ArrayOf::logicalScalar(false);
        return retval;
    }
    Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
        L"string or cell");
    return retval;
}
//=============================================================================
