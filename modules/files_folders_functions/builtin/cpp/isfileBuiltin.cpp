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
#include "isfileBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "ArrayOfView.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FilesFoldersGateway::isfileBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    if (argIn[0].isEmpty()) {
        retval << ArrayOf::logicalScalar(false);
    } else if (argIn[0].isRowVectorCharacterArray() || argIn[0].isScalarStringArray()) {
        std::wstring wpath = argIn[0].asWideString();
        bool permissionDenied;
        bool bIsFile = FileSystemWrapper::Path::is_regular_file(wpath, permissionDenied);
        if (permissionDenied) {
            Error(L"Nelson:files_folders_functions:permissionDenied", L"Permission denied.");
        }
        retval << ArrayOf::logicalScalar(bIsFile);
    } else if (argIn[0].dataClass() == NLS_CELL_ARRAY || argIn[0].isStringArray()) {
        Dimensions dim = argIn[0].dimensions();
        if (argIn[0].isEmpty()) {
            retval << ArrayOf::empty(dim);
        } else {
            ArrayOf cell(argIn[0]);
            logical* bmat = static_cast<logical*>(ArrayOf::allocateArrayOf(
                NLS_LOGICAL, argIn[0].elementCount(), stringVector(), false));
            indexType elementCount = dim.elementCount();
            ArrayOfView<ArrayOf> arg(cell, ArrayOfViewAssumeValidated {});
            for (indexType k = 0; k < elementCount; k++) {
                if (arg[k].isRowVectorCharacterArray()) {
                    bool permissionDenied;
                    bool bIsFile = FileSystemWrapper::Path::is_regular_file(
                        arg[k].asWideString(), permissionDenied);
                    if (permissionDenied) {
                        Error(L"Nelson:files_folders_functions:permissionDenied",
                            L"Permission denied.");
                    }
                    bmat[k] = static_cast<Nelson::logical>(bIsFile);
                } else {
                    delete[] bmat;
                    Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
                        L"string or cell");
                }
            }
            ArrayOf res = ArrayOf(NLS_LOGICAL, dim, bmat, false);
            retval << res;
        }
    } else {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
            L"string or cell");
    }
    return retval;
}
//=============================================================================
