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
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
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
        Dimensions dim = argIn[0].getDimensions();
        if (argIn[0].isEmpty()) {
            retval << ArrayOf::emptyConstructor(dim);
        } else {
            ArrayOf cell(argIn[0]);
            logical* bmat = static_cast<logical*>(ArrayOf::allocateArrayOf(
                NLS_LOGICAL, argIn[0].getElementCount(), stringVector(), false));
            indexType elementCount = dim.getElementCount();
            for (indexType k = 0; k < elementCount; k++) {
                ArrayOf cell(argIn[0]);
                auto* arg = (ArrayOf*)(cell.getDataPointer());
                if (arg[k].isRowVectorCharacterArray()) {
                    bool permissionDenied;
                    bmat[k] = static_cast<Nelson::logical>(FileSystemWrapper::Path::is_directory(
                        arg[k].getContentAsWideString(), permissionDenied));
                    if (permissionDenied) {
                        Error(_W("Permission denied."));
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
        std::wstring wpath = argIn[0].getContentAsWideString();
        bool permissionDenied;
        retval << ArrayOf::logicalConstructor(
            FileSystemWrapper::Path::is_directory(wpath, permissionDenied));
        if (permissionDenied) {
            Error(_W("Permission denied."));
        }
        return retval;
    }
    if (argIn[0].isEmpty()) {
        retval << ArrayOf::logicalConstructor(false);
        return retval;
    }
    Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_OR_CELL_EXPECTED);
    return retval;
}
//=============================================================================
