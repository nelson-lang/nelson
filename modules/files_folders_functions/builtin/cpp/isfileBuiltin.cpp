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
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
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
        retval << ArrayOf::logicalConstructor(false);
    } else if (argIn[0].isRowVectorCharacterArray() || argIn[0].isScalarStringArray()) {
        std::wstring wpath = argIn[0].getContentAsWideString();
        bool permissionDenied;
        bool bIsFile = FileSystemWrapper::Path::is_regular_file(wpath, permissionDenied);
        if (permissionDenied) {
            Error(_W("Permission denied."));
        }
        retval << ArrayOf::logicalConstructor(bIsFile);
    } else if (argIn[0].getDataClass() == NLS_CELL_ARRAY || argIn[0].isStringArray()) {
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
                    bool bIsFile = FileSystemWrapper::Path::is_regular_file(
                        arg[k].getContentAsWideString(), permissionDenied);
                    if (permissionDenied) {
                        Error(_W("Permission denied."));
                    }
                    bmat[k] = static_cast<Nelson::logical>(bIsFile);
                } else {
                    delete[] bmat;
                    Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_OR_CELL_EXPECTED);
                }
            }
            ArrayOf res = ArrayOf(NLS_LOGICAL, dim, bmat, false);
            retval << res;
        }
    } else {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_OR_CELL_EXPECTED);
    }
    return retval;
}
//=============================================================================
