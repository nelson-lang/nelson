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
#include "structBuiltin.hpp"
#include "Error.hpp"
#include "IsValidFieldname.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DataStructuresGateway::structBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.empty()) {
        retval.push_back(ArrayOf::emptyStructWithoutFields());
        return retval;
    }
    if (argIn.size() == 1) {
        if (argIn[0].isClassStruct()) {
            ArrayOf asStruct = argIn[0];
            asStruct.ensureSingleOwner();
            asStruct.setStructType(NLS_STRUCT_ARRAY_STR);
            retval.push_back(asStruct);
            return retval;
        } else if (!argIn[0].isEmpty()) {
            Error(_W("struct([]) expected."));
        }
        Dimensions dim = argIn[0].getDimensions();
        wstringVector fieldnames;
        retval.push_back(ArrayOf::emptyStructConstructor(fieldnames, dim));
    } else {
        if (argIn.size() % 2) {
            Error(_W("requires pairs of field names and values."));
        }
        size_t pairCount = argIn.size() / 2;
        stringVector names;
        ArrayOfVector values;
        for (size_t i = 0; i < pairCount; i++) {
            values.push_back(ArrayOf());
        }
        for (size_t i = 0; i < pairCount * 2; i += 2) {
            if (!(argIn[i].isRowVectorCharacterArray())) {
                Error(_W("requires pairs of field names and values."));
            }
            std::string field = argIn[i].getContentAsCString();
            if (!IsValidFieldname(field)) {
                Error(_W("requires a valid fieldname."));
            }
            names.push_back(field);
            values[i / 2] = argIn[i + 1];
        }
        retval.push_back(ArrayOf::structConstructor(names, values));
    }
    return retval;
}
//=============================================================================
