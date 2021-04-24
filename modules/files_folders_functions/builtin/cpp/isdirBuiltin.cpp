//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "isdirBuiltin.hpp"
#include "Error.hpp"
#include "IsDirectory.hpp"
#include "IsCellOfStrings.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FilesFoldersGateway::isdirBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    if (IsCellOfString(argIn[0]) || argIn[0].isStringArray()) {
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
                    bmat[k] = static_cast<Nelson::logical>(
                        IsDirectory(arg[k].getContentAsWideString()));
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
        retval << ArrayOf::logicalConstructor(IsDirectory(wpath));
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
